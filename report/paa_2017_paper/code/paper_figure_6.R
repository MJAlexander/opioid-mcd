## Paper figure 6 -- age-specific opioid mortality by race
library(tidyverse)
source('./code/functions/helpers.R')
mkdir_p('./report/paa_2017_paper/plots')

## Load data 
race_age_uc <- read_csv("./data/race_age_uc.csv")
pop_data <- make_superpop()

age_specific_all <- race_age_uc %>%
    mutate(age = convert_age_to_10(age)) %>% 
    group_by(race, age, year) %>% 
    summarise(death_sum  = sum(uc_deaths)) %>% 
    left_join(pop_data, by = c("age", "year", "race")) %>% 
    mutate(race_cat   = categorize_race(race), 
           age_cat    = categorize_age(age), 
           death_rate = death_sum / pop_year * 1e5, 
           vari       = (death_rate/1e5)^2 / death_sum, 
           hi_ci      = death_rate + (1.96 * sqrt(vari) * 1e5), 
           lo_ci      = death_rate - (1.96 * sqrt(vari) * 1e5))

## Plot -- age-specific
age_spec_over_time_race <- ggplot(data = age_specific_all, 
                                  aes(x = year, y = death_rate, 
                                      group = race_cat, color = race_cat)) + 
    geom_point(size = 1.15, alpha = .9) +
    geom_line(size = 1, alpha = .9) + 
    facet_wrap(~ age_cat) + 
    scale_color_brewer(NULL, type = "qual", palette = "Set1") + 
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.02, 0), limits = c(0, NA)) + 
    mk_classic() +
    mk_x90() + 
    theme(legend.background = element_rect(fill = "transparent")) + 
    labs(title = "Age-specific opioid-related mortality rate, by race", 
         x = NULL, y = "Mortality rate (per 100,000)")
ggsave(age_spec_over_time_race, 
       filename = './report/paa_2017_paper/plots/paper_fig6_age_spec_by_race.pdf', 
       width = 8, height = 5.5, scale = 1)

age_spec_over_time_race_ci <- add_ci(age_spec_over_time_race)
ggsave(age_spec_over_time_race_ci, 
       filename = './report/paa_2017_paper/plots/paper_fig6_age_spec_by_race_ci.pdf', 
       width = 8, height = 5.5, scale = 1)
