## Paper figure 9 -- proportion of heroin deaths involving other opioids
library(tidyverse)
library(RColorBrewer)
library(binom)
source('./code/functions/helpers.R')
mkdir_p('./report/paa_2017_paper/plots')

d <- read.csv("./data/race_age_tc.csv")

## We need to reshape this later so it is easier to calculate the bounds in 
## a different dataframe and then perform a left_join. We rename the values of
## `comb_401` to make it easier to match later. 
prop <- d %>%
    mutate(race_cat = categorize_race(race)) %>%
    group_by(race, year, race_cat) %>%
    summarise(sum_t401_404 = sum(t401_404), 
              sum_t401_402 = sum(t401_402), 
              sum_t401_403 = sum(t401_403),
              sum_t404 = sum(t404), 
              sum_t402 = sum(t402), 
              sum_t403 = sum(t403), 
              ras_t404401_404 = sum(t401_404)/sum(t404),
              ras_t402401_402 = sum(t401_402)/sum(t402),
              ras_t403401_403 = sum(t401_403)/sum(t403), 
              bound_t404401 = 1.96 * 
                  sqrt(1/sum(t404) * ras_t404401_404 * (1 - ras_t404401_404)), 
              bound_t402401 = 1.96 * 
                  sqrt(1/sum(t402) * ras_t402401_402 * (1 - ras_t402401_402)), 
              bound_t403401 = 1.96 * 
                  sqrt(1/sum(t403) * ras_t403401_403 * (1 - ras_t403401_403))) %>%
    mutate(t401_404_lower = binom.confint(sum_t401_404, 
                                          sum_t404, 
                                          methods = "wilson")$lower, 
           t401_404_upper = binom.confint(sum_t401_404, 
                                          sum_t404, 
                                          methods = "wilson")$upper, 
           t401_402_lower = binom.confint(sum_t401_402, 
                                          sum_t402, 
                                          methods = "wilson")$lower, 
           t401_402_upper = binom.confint(sum_t401_402, 
                                          sum_t402, 
                                          methods = "wilson")$upper, 
           t401_403_lower = binom.confint(sum_t401_403, 
                                          sum_t403, 
                                          methods = "wilson")$lower, 
           t401_403_upper = binom.confint(sum_t401_403, 
                                          sum_t403, 
                                          methods = "wilson")$upper)

lowers <- prop %>% 
    select(race, race_cat, year, ends_with("lower")) %>% 
    gather(key = comb_401, value = lower, 
           t401_404_lower:t401_403_lower) %>% 
    mutate(comb_401 = ifelse(comb_401 == "t401_404_lower", 
                             "ras_t404401_404", 
                             ifelse(comb_401 == "t401_403_lower", 
                                    "ras_t403401_403", 
                                    "ras_t402401_402")))

uppers <- prop %>% 
    select(race, race_cat, year, ends_with("upper")) %>% 
    gather(key = comb_401, value = upper, 
           t401_404_upper:t401_403_upper) %>% 
    mutate(comb_401 = ifelse(comb_401 == "t401_404_upper", 
                             "ras_t404401_404", 
                             ifelse(comb_401 == "t401_403_upper", 
                                    "ras_t403401_403", 
                                    "ras_t402401_402")))

## Now do the normal calculations and left_join with our bounds from above.
reshaped_prop <- prop %>%
    select(race, race_cat, year,
           ras_t404401_404, ras_t402401_402, ras_t403401_403) %>%
    gather(key = comb_401, value = propor,
           ras_t404401_404:ras_t403401_403) %>% 
    left_join(lowers, by = c("race", "race_cat", "year", "comb_401")) %>%
    left_join(uppers, by = c("race", "race_cat", "year", "comb_401")) %>%
    mutate(comb_401 = factor(comb_401,
                             levels = c("ras_t402401_402",
                                        "ras_t403401_403",
                                        "ras_t404401_404"),
                             labels = c("Natural / semi-synthetic (T402)",
                                        "Methadone (T403)",
                                        "Other synthetic (T404)"),
                             ordered = TRUE)) 

## Plotting
p_ma_t401_combo <- ggplot(data = reshaped_prop, 
                          aes(x = year, y = propor, 
                              group = race, color = race_cat)) + 
    geom_line(size = 1, alpha = .9) + 
    geom_point(size = 1.25, alpha = .9) + 
    scale_color_brewer(NULL, palette = "Set1") + 
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) + 
    mk_classic() + 
    mk_x90() + 
    theme(legend.background = element_rect(fill = "transparent", color = NA)) + 
    labs(y = "Proportion", 
         x = NULL, 
         title = "Proportion of heroin (T401) deaths involving other opioids")
ggsave(p_ma_t401_combo + facet_wrap(~ comb_401, nrow = 1), 
       filename =  "./report/paa_2017_paper/plots/paper_fig9_t401_combos.pdf", 
       height = 4, width = 8, scale = 1)

ggsave(p_ma_t401_combo + facet_wrap(~ comb_401, nrow = 1, scales = "free_y"), 
       filename =  "./report/paa_2017_paper/plots/paper_fig9_t401_combos_free.pdf", 
       height = 4, width = 8, scale = 1)

## Add vline version
p_ma_t401_combo_v <- add_vline(p_ma_t401_combo)
ggsave(p_ma_t401_combo_v + facet_wrap(~ comb_401, nrow = 1), 
       filename =  "./report/paa_2017_paper/plots/paper_fig9_t401_combos_v.pdf", 
       height = 4, width = 8, scale = 1)

ggsave(p_ma_t401_combo_v + facet_wrap(~ comb_401, nrow = 1, scales = "free_y"), 
       filename =  "./report/paa_2017_paper/plots/paper_fig9_t401_combos_free_v.pdf", 
       height = 4, width = 8, scale = 1)

## CI
## Paper figure 9 -- proportion of heroin deaths involving other opioids
p_ma_t401_combo <- ggplot(data = reshaped_prop, 
                          aes(x = year, y = propor, 
                              group = race, color = race_cat)) + 
    geom_ribbon(aes(ymax = upper, 
                    ymin = lower, 
                    fill = race_cat, 
                    group = race_cat), 
                color = NA, alpha = .25) + 
    geom_line(size = 1, alpha = .9) + 
    geom_point(size = 1.25, alpha = .9) + 
    scale_fill_brewer(NULL, palette = "Set1") + 
    scale_color_brewer(NULL, palette = "Set1") + 
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) + 
    mk_classic() + 
    theme(legend.background = element_rect(fill = "transparent", color = NA)) + 
    labs(y = "Proportion", 
         x = NULL, 
         title = "Proportion of heroin (T401) deaths involving other opioids") + facet_wrap(~ comb_401, nrow = 1)
ggsave(p_ma_t401_combo + facet_wrap(~ comb_401, nrow = 1), 
       filename =  "./report/paa_2017_paper/plots/paper_fig9_t401_combos_ci.pdf", 
       height = 4, width = 8, scale = 1)

ggsave(p_ma_t401_combo + facet_wrap(~ comb_401, nrow = 1, scales = "free_y"), 
       filename =  "./report/paa_2017_paper/plots/paper_fig9_t401_combos_free_ci.pdf", 
       height = 4, width = 8, scale = 1)

## Add vline version
p_ma_t401_combo_v <- add_vline(p_ma_t401_combo)
ggsave(p_ma_t401_combo_v + facet_wrap(~ comb_401, nrow = 1), 
       filename =  "./report/paa_2017_paper/plots/paper_fig9_t401_combos_v_ci.pdf", 
       height = 4, width = 8, scale = 1)

ggsave(p_ma_t401_combo_v + facet_wrap(~ comb_401, nrow = 1, scales = "free_y"), 
       filename =  "./report/paa_2017_paper/plots/paper_fig9_t401_combos_free_v_ci.pdf", 
       height = 4, width = 8, scale = 1)
