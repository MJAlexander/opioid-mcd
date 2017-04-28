## Paper figure 7 -- age-specific mortality rate by opioid type
library(tidyverse)
source('./code/functions/helpers.R')
mkdir_p('./report/paa_2017_paper/plots')

## Load data 
race_age_uc_ac <- read_csv("./data/race_age_uc_ac.csv")
pop_data <- make_superpop()

t4_age_spec <- race_age_uc_ac %>% 
    mutate(t_code = ifelse(code %in% make_pois_codes(), 
                           code, "Other"), 
           age = convert_age_to_10(age)) %>% 
    group_by(race, age, year, t_code) %>% 
    summarize(death_sum  = sum(uc_ac_deaths)) %>% 
    left_join(pop_data, by = c("age", "year", "race")) %>% 
    mutate(death_rate = death_sum / pop_year * 1e5, 
           race_cat = categorize_race(race), 
           age_cat  = categorize_age(age), 
           t_cat    = categorize_tcode(t_code), 
           vari     = (death_rate/1e5)^2 / death_sum, 
           hi_ci    = death_rate + (1.96 * sqrt(vari) * 1e5),
           lo_ci    = death_rate - (1.96 * sqrt(vari) * 1e5))
sub_t4_age <- filter(t4_age_spec, t_code != "Other")

age_t40_3 <- ggplot(data = sub_t4_age, 
                    aes(x = year, y = death_rate, 
                        group = interaction(t_code, race), 
                        color = race_cat)) + 
    geom_point(size = .3) + 
    geom_line() + 
    scale_color_brewer(NULL, palette = "Set1") + 
    scale_y_continuous("Mortality rate (per 100,000)") + 
    scale_x_continuous("") + 
    mk_classic() + 
    mk_x90() + 
    theme(legend.position = c(1, 1), 
          legend.justification = c(1, 1)) +
    labs(title = "Age-specific mortality rate by opioid type, 2000-2015") + 
    facet_grid(t_cat ~ age_cat)
ggsave(age_t40_3, 
       filename = './report/paa_2017_paper/plots/paper_fig7_t40_notpretty.pdf', 
       width = 8, height = 6, scale = 1.3)

# age_t40_3_ci <- add_ci(age_t40_3)
# ggsave(age_t40_3_ci, 
#        filename = './report/paa_2017_paper/plots/paper_fig7_t40_notpretty_ci.pdf', 
#        width = 8, height = 6, scale = 1.3)
