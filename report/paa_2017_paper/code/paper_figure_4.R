## Figure 4 -- mortality rate by race and underlying type of opioid
## Imports 
library(tidyverse)
library(scales)
source('./code/functions/helpers.R')
mkdir_p('./report/paa_2017_paper/plots')

## Data
race_age_uc_ac <- read_csv("./data/race_age_uc_ac.csv")
pop_data <- make_superpop()

t4_age_spec <- race_age_uc_ac %>% 
    mutate(t_code = ifelse(code %in% make_pois_codes(), 
                           code, "Other"), 
           age = convert_age_to_10(age)) %>% 
    group_by(race, age, year, t_code) %>% 
    summarize(death_sum  = sum(uc_ac_deaths)) %>% 
    left_join(pop_data, by = c("age", "year", "race")) %>% 
    mutate(death_rate = death_sum / pop_year, 
           vari       = death_rate^2 / death_sum, 
           race_cat   = categorize_race(race), 
           age_cat    = categorize_age(age), 
           t_cat      = categorize_tcode(t_code))

t4_age_adj <- t4_age_spec %>% 
    group_by(race, year, t_cat, t_code) %>% 
    summarize(race_cat   = first(race_cat),
              unadj_rate = weighted.mean(death_rate, pop_year) * 1e5, 
              adj_rate   = weighted.mean(death_rate, pop_std) * 1e5,
              vari       = weighted.mean(vari, death_sum), 
              hi_ci      = adj_rate + (1.96 * sqrt(vari)) * 1e5,
              lo_ci      = adj_rate - (1.96 * sqrt(vari)) * 1e5,
              death_sum  = sum(death_sum)) %>% 
    ungroup() %>%
    filter(t_code != "Other")


t4_adj_plots_race_lines <- ggplot(data = t4_age_adj,
                                  aes(x = year, y = adj_rate, 
                                      group = interaction(t_code, race_cat), 
                                      color = race_cat)) + 
    geom_point(size = 1.25, alpha = .9) + 
    geom_line(size = 1, alpha = .9) + 
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) +
    scale_color_brewer(NULL, palette = "Set1") +
    facet_wrap(~ t_cat, nrow = 1) + 
    mk_classic() +
    mk_x90() + 
    theme(legend.position = c(1, 1), 
          legend.justification = c(1, 1)) + 
    labs(title = "Mortality rate, by race and underlying type of opioid, 2000-2015", 
         x = NULL, y = "Mortality rate (per 100,000)")
ggsave(t4_adj_plots_race_lines, 
       filename = './report/paa_2017_paper/plots/paper_fig4_t40_adjusted_race_lines.pdf', 
       height = 4, width = 8, scale = 1)

t4_adj_plots_race_lines_ci <- add_ci(t4_adj_plots_race_lines)
ggsave(t4_adj_plots_race_lines_ci, 
       filename = './report/paa_2017_paper/plots/paper_fig4_t40_adjusted_race_lines_ci.pdf', 
       height = 4, width = 8, scale = 1)

t4_adj_plots_race_lines_v <- add_vline(t4_adj_plots_race_lines)
ggsave(t4_adj_plots_race_lines_v, 
       filename = './report/paa_2017_paper/plots/paper_fig4_t40_adjusted_race_lines_v.pdf', 
       height = 4, width = 8, scale = 1)

t4_adj_plots_race_lines_v_ci <- add_ci(t4_adj_plots_race_lines_v)
ggsave(t4_adj_plots_race_lines_v_ci, 
       filename = './report/paa_2017_paper/plots/paper_fig4_t40_adjusted_race_lines_v_ci.pdf', 
       height = 4, width = 8, scale = 1)
