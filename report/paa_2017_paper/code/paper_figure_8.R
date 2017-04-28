## Paper - figure 8 -- proportion of deaths with two or more opioids
library(tidyverse)
library(RColorBrewer)
library(binom)
source('./code/functions/helpers.R')
mkdir_p('./report/paa_2017_paper/plots')

d <- read.csv("./data/race_age_tc.csv")

prop <- d %>%  
    mutate(race_cat = categorize_race(race)) %>% 
    group_by(race, race_cat, year) %>% 
    summarise(sum_deaths    = sum(n_deaths), 
              sum_opioids_1 = sum(opioids_1), 
              sum_opioids_2 = sum(opioids_2), 
              sum_opioids_3 = sum(opioids_3_more), 
              sum_opioids_p = sum_opioids_2 + sum_opioids_3, 
              prop_opioid_1 = sum(opioids_1)/sum(n_deaths), 
              prop_opioid_2 = sum(opioids_2)/sum(n_deaths),
              prop_opioid_3 = sum(opioids_3_more)/sum(n_deaths),
              prop_opioid_2_more = (sum(opioids_2) + sum(opioids_3_more)) / 
                  sum(n_deaths), 
              prop_o1_bound = 1.96 * 
                  sqrt(1/sum(n_deaths) * prop_opioid_1 * (1 - prop_opioid_1)),
              prop_o2_bound = 1.96 * 
                  sqrt(1/sum(n_deaths) * prop_opioid_2 * (1 - prop_opioid_2)),
              prop_o3_bound = 1.96 * 
                  sqrt(1/sum(n_deaths) * prop_opioid_3 * (1 - prop_opioid_3)),
              prop_2p_bound = 1.96 * 
                  sqrt(1/sum(n_deaths) * prop_opioid_2_more * 
                           (1 - prop_opioid_2_more))) %>% 
    ungroup() %>% 
    mutate(ac_o1_lower = binom.confint(sum_opioids_1, 
                                       sum_deaths, 
                                       methods = "wilson")$lower, 
           ac_o1_upper = binom.confint(sum_opioids_1, 
                                       sum_deaths, 
                                       methods = "wilson")$upper, 
           ac_o2_lower = binom.confint(sum_opioids_2, 
                                       sum_deaths, 
                                       methods = "wilson")$lower, 
           ac_o2_upper = binom.confint(sum_opioids_2, 
                                       sum_deaths, 
                                       methods = "wilson")$upper,
           ac_o3_lower = binom.confint(sum_opioids_3, 
                                       sum_deaths, 
                                       methods = "wilson")$lower, 
           ac_o3_upper = binom.confint(sum_opioids_3, 
                                       sum_deaths, 
                                       methods = "wilson")$upper,
           ac_om_lower = binom.confint(sum_opioids_p, 
                                       sum_deaths, 
                                       methods = "wilson")$lower, 
           ac_om_upper = binom.confint(sum_opioids_p, 
                                       sum_deaths, 
                                       methods = "wilson")$upper)

p_ma_2_or_more <- ggplot(prop, 
                         aes(year, prop_opioid_2_more, 
                             group = race, color = race_cat)) + 
    geom_line(size = 1, alpha = .9) + 
    geom_point(size = 1.25, alpha = .9) + 
    scale_color_brewer(NULL, palette = "Set1") + 
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) + 
    mk_classic() + 
    labs(y = "Proportion", 
         x = NULL, 
         title = "Proportion of deaths with two or more opioids")
ggsave(p_ma_2_or_more, filename =  "./report/paa_2017_paper/plots/paper_fig8_prop_2_more.pdf", 
       height = 4, width = 4, scale = 1)

p_ma_2_or_more_v <- add_vline(p_ma_2_or_more)
ggsave(p_ma_2_or_more_v, filename =  "./report/paa_2017_paper/plots/paper_fig8_prop_2_more_v.pdf", 
       height = 4, width = 4, scale = 1)

p_ma_2_or_more_ci <- ggplot(prop, 
                         aes(year, prop_opioid_2_more, 
                             group = race, color = race_cat)) + 
    geom_ribbon(aes(ymax = ac_om_upper, 
                    ymin = ac_om_lower, 
                    fill = race_cat, group = race_cat),
                alpha = .25, color = NA) + 
    geom_line(size = 1, alpha = .9) + 
    geom_point(size = 1.25, alpha = .9) + 
    scale_color_brewer(NULL, palette = "Set1") + 
    scale_fill_brewer(NULL, palette = "Set1") + 
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) + 
    mk_classic() + 
    labs(y = "Proportion", 
         x = NULL, 
         title = "Proportion of deaths with two or more opioids")
ggsave(p_ma_2_or_more_ci, filename =  "./report/paa_2017_paper/plots/paper_fig8_prop_2_more_ci.pdf", 
       height = 4, width = 4, scale = 1)

p_ma_2_or_more_ci_v <- add_vline(p_ma_2_or_more_ci)
ggsave(p_ma_2_or_more_ci_v, filename =  "./report/paa_2017_paper/plots/paper_fig8_prop_2_more_ci_v.pdf", 
       height = 4, width = 4, scale = 1)
