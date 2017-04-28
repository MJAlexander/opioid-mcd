## Paper figure 3 -- opioid related mortality by top 4 ucod
## Imports 
library(tidyverse)
source('./code/functions/helpers.R')
mkdir_p('./report/paa_2017_paper/plots')

## Load data 
race_age_uc <- read_csv("./data/race_age_uc.csv")

## Break down by UCOD
adj_deaths_ucod <- race_age_uc %>% 
    mutate(age = convert_age_to_10(age)) %>% 
    filter(uc != "Other") %>% 
    group_by(race, age, year, ucod) %>% 
    summarise(death_sum  = sum(uc_deaths)) %>% 
    left_join(pop_data, by = c("age", "year", "race")) %>% 
    mutate(death_rate = death_sum / pop_year, 
           race_cat   = categorize_race(race), 
           vari       = death_rate^2 / death_sum) %>% 
    group_by(race, year, ucod) %>% 
    summarize(race_cat   = first(race_cat), 
              vari       = weighted.mean(vari, death_sum),
              death_sum  = sum(death_sum),
              unadj_rate = weighted.mean(death_rate, pop_year) * 1e5, 
              adj_rate   = weighted.mean(death_rate, pop_std) * 1e5, 
              hi_ci      = adj_rate + (1.96 * sqrt(vari)) * 1e5, 
              lo_ci      = adj_rate - (1.96 * sqrt(vari)) * 1e5)

adj_ucod <- ggplot(data = filter(adj_deaths_ucod, 
                                 ucod %in% c("X42", "X44", "X64", "Y12")),
                   aes(x = year, y = adj_rate)) + 
    geom_line(size = 1, alpha = .9, aes(group = ucod, color = ucod)) + 
    geom_point(size = 1.5, alpha = 1, aes(group = ucod), color = "white") + 
    geom_point(size = 1.25, alpha = .9, aes(group = ucod, color = ucod)) + 
    scale_color_brewer("Underlying\nCause of Death", palette = "Dark2") + 
    scale_x_continuous(expand = c(.01, 0)) + 
    scale_y_continuous(expand = c(.02, 0)) + 
    facet_wrap(~ race_cat) + 
    mk_classic() + 
    mk_x90() +
    mk_legend_ur() + 
    labs(title = paste("Opioid-related mortality rate, by race and", 
                       "underlying cause of death"), 
         x = NULL, y = "Mortality rate (per 100,000)", 
         subtitle = "Top 4 underlying causes only, 2000-2015")
ggsave(adj_ucod, filename = './report/paa_2017_paper/plots/paper_fig3_ucod_adjusted.pdf', 
       height = 4, width = 8, scale = 1)

adj_ucod_ci <- add_ci(adj_ucod, fill_var = "ucod", palette = "Dark2", 
                      p_title = "Underlying\nCause of Death")
ggsave(adj_ucod_ci, filename = './report/paa_2017_paper/plots/paper_fig3_ucod_adjusted_ci.pdf', 
       height = 4, width = 8, scale = 1)

adj_ucod_v <- add_vline(adj_ucod)
ggsave(adj_ucod_v, filename = './report/paa_2017_paper/plots/paper_fig3_ucod_adjusted_v.pdf', 
       height = 4, width = 8, scale = 1)

adj_ucod_ci_v <- add_ci(adj_ucod_v, fill_var = "ucod", palette = "Dark2", 
                      p_title = "Underlying\nCause of Death")
ggsave(adj_ucod_ci, filename = './report/paa_2017_paper/plots/paper_fig3_ucod_adjusted_ci_v.pdf', 
       height = 4, width = 8, scale = 1)
