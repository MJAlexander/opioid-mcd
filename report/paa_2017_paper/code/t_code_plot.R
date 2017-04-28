## Tcode plot
## Imports 
library(tidyverse)
library(viridis)
source('./code/functions/helpers.R')
mkdir_p('./report/paa_2017_paper/plots')

## Load data 
race_age_tc_uc_ac <- read_csv("./data/race_age_tc_uc_ac.csv")
pop_data <- make_superpop()

sub_df <- race_age_tc_uc_ac %>% 
    filter(ucod != code, 
           substr(code, 1, 3) %in% paste0("T", 36:65),
           opioid_code == "T401") %>% 
    mutate(age = convert_age_to_10(age), 
           race_cat = categorize_race(race)) %>% 
    select(-pop_std, -pop_year) %>% 
    left_join(pop_data, by = c("race", "age", "year")) %>% 
    group_by(race_cat, race, year, code) %>% 
    summarize(prop_ac = weighted.mean(uc_tc_ac_deaths, pop_std) / 
                  weighted.mean(ac_deaths, pop_std)) %>% 
    arrange(race, code, year) %>% 
    group_by(race, code) %>% 
    mutate(keep_code = year - lag(year, 3) == 3, 
           code_cat = add_tplot_labels(code))

good_codes <- sub_df %>% 
    ungroup() %>% 
    filter(keep_code) %>% 
    select(code) %>% 
    unique()

sub_df2 <- filter(sub_df, code %in% good_codes$code)

t_code_plot <- ggplot(data = sub_df2, 
                      aes(x = year, y = code_cat, 
                          fill = log10(prop_ac))) + 
    geom_tile(color = "white") + 
    scale_fill_viridis(expression(Proportion), 
                       direction = -1, 
                       breaks = -3:0,
                       labels = c(expression(10^-3), 
                                  expression(10^-2), 
                                  expression(10^-1), 
                                  expression(10^0))) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = seq(2000, 2015, 2)) + 
    scale_y_discrete(limits = rev(levels(sub_df2$code_cat))) +
    theme_classic() + 
    theme(title = element_text(family = "serif"), 
          legend.position = "bottom", 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), 
          axis.text.y = element_text(size = 12)) + 
    facet_wrap(~ race_cat) + 
    labs(title = "Proportion of T36-T65 codes associated with heroin deaths, by race", 
         y = NULL, 
         x = NULL)
ggsave(t_code_plot, filename = './report/paa_2017_paper/plots/t_code_plot.pdf', 
       width = 8, height = 10, scale = 1)




