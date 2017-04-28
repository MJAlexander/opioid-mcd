## Figure 2a and 2b -- Opioid-related mortality by race (and relative risk)
## Imports 
library(tidyverse)
source('./code/functions/helpers.R')
mkdir_p('./report/paa_2017_paper/plots')

## Load data 
race_age_uc    <- read_csv("./data/race_age_uc.csv")
pop_data <- make_superpop()

adj_deaths_overall <- race_age_uc %>% 
    mutate(age = convert_age_to_10(age)) %>% 
    group_by(race, age, year) %>% 
    summarize(death_sum  = sum(uc_deaths)) %>% 
    left_join(pop_data, by = c("age", "year", "race")) %>% 
    mutate(death_rate = death_sum / pop_year, 
           race_cat   = categorize_race(race), 
           vari       = death_rate^2 / death_sum) %>% 
    group_by(race, year) %>% 
    summarize(race_cat   = first(race_cat),
              vari       = weighted.mean(vari, death_sum), 
              death_sum  = sum(death_sum),
              unadj_rate = weighted.mean(death_rate, pop_year) * 1e5, 
              adj_rate   = weighted.mean(death_rate, pop_std) * 1e5, 
              pop_std    = first(pop_std), 
              pop_year   = first(pop_year), 
              hi_ci      = adj_rate + (1.96 * sqrt(vari)) * 1e5, 
              lo_ci      = adj_rate - (1.96 * sqrt(vari)) * 1e5)

adj_overall <- ggplot(data = adj_deaths_overall, 
                      aes(x = year, y = adj_rate, 
                          group = race_cat, 
                          color = race_cat)) + 
    geom_line(size = 1, alpha = .9) + 
    geom_point(size = 1.25, alpha = .9) + 
    scale_color_brewer(NULL, palette = "Set1") + 
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) + 
    mk_classic() + 
    labs(title = "Opioid-related mortality rate", 
         subtitle = "By race/ethnicity, 2000-2015",
         x = NULL, y = "Mortality rate (per 100,000)")
ggsave(adj_overall, filename = './report/paa_2017_paper/plots/paper_fig2_overall_adjusted.pdf', 
       height = 4, width = 4, scale = 1)

adj_overall_v <- add_vline(adj_overall)
ggsave(adj_overall_v, 
       filename = './report/paa_2017_paper/plots/paper_fig2_overall_adjusted_v.pdf', 
       height = 4, width = 4, scale = 1)

# adj_overall_ci <- add_ci(adj_overall)
# ggsave(adj_overall_ci, 
#        filename = './report/paa_2017_paper/plots/paper_fig2_overall_adjusted_ci.pdf', 
#        height = 4, width = 4, scale = 1)

# adj_overall_ci_v <- add_vline(adj_overall_ci)
# ggsave(adj_overall_ci_v, 
#        filename = './report/paa_2017_paper/plots/paper_fig2_overall_adjusted_ci_v.pdf', 
#        height = 4, width = 4, scale = 1)

 ## RELATIVE
rel_adj_deaths_overall <- relativizer(adj_deaths_overall)
rel_adj_overall <- ggplot(data = rel_adj_deaths_overall, 
                          aes(x = year, y = relrisk)) + 
    geom_line(size = 1, alpha = .9) + 
    geom_point(size = 1.25, alpha = .9) +  
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) + 
    mk_classic() + 
    labs(title = "Relative risk of opioid-related mortality",
         subtitle = "2000-2015",
         x = NULL, 
         y = "Relative risk (non-Hispanic white / non-Hispanic black)")
# ggsave(rel_adj_overall, 
#        filename = './report/paa_2017_paper/plots/paper_fig2_relative_risk.pdf', 
#        height = 4, width = 4, scale = 1)

# rel_adj_overall_ci <- add_ci(rel_adj_overall, fill_var = NULL)
# ggsave(rel_adj_overall_ci, 
#        filename = './report/paa_2017_paper/plots/paper_fig2_relative_risk_ci.pdf', 
#        height = 4, width = 4, scale = 1)

rel_adj_overall_v <- add_vline(rel_adj_overall)
ggsave(rel_adj_overall_v, 
       filename = './report/paa_2017_paper/plots/paper_fig2_relative_risk_v.pdf', 
       height = 4, width = 4, scale = 1)

# rel_adj_overall_ci_v <- add_vline(rel_adj_overall_ci)
# ggsave(rel_adj_overall_ci_v, 
#        filename = './report/paa_2017_paper/plots/paper_fig2_relative_risk_ci_v.pdf', 
#        height = 4, width = 4, scale = 1)
