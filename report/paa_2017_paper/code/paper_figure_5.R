## Paper figure 5 -- age-specific opioid-mortality by race and year
library(tidyverse)
source('./code/functions/helpers.R')
mkdir_p('./report/paa_2017_paper/plots')

## Load data 
race_age_uc    <- read_csv("./data/race_age_uc.csv")
pop_data <- make_superpop()

age_specific_all <- race_age_uc %>% 
    mutate(age = convert_age_to_10(age)) %>% 
    group_by(race, age, year) %>% 
    summarise(death_sum  = sum(uc_deaths)) %>% 
    left_join(pop_data, by = c("age", "year", "race")) %>% 
    mutate(death_rate = death_sum / pop_year * 1e5, 
           race_cat   = categorize_race(race), 
           age_cat    = categorize_age(age), 
           vari       = (death_rate/1e5)^2 / death_sum, 
           hi_ci      = death_rate + (1.96 * sqrt(vari) * 1e5), 
           lo_ci      = death_rate - (1.96 * sqrt(vari) * 1e5))

asmr_by_race_2_f <- ggplot(data = age_specific_all, 
                           aes(x = age_cat, y = death_rate, 
                               group = year, color = year)) + 
    geom_point(size = 1.25, alpha = .9) + 
    geom_line(size = 1, alpha = .9) + 
    facet_wrap(~ race_cat) + 
    scale_y_continuous("Mortality rate (per 100,000)", 
                       expand = c(.01, 0)) + 
    scale_x_discrete("Age group", expand = c(.01, 0)) + 
    scale_color_distiller(NULL, palette = "Purples", direction = 1) + 
    mk_classic() + mk_x90() + mk_legend_ur() + 
    labs(title = "Age-specific opioid-mortality, by race and year")
ggsave(asmr_by_race_2_f, 
       filename = "./report/paa_2017_paper/plots/paper_fig5_asmr_race_all.pdf", 
       width = 8, height = 4, scale = 1)

# asmr_by_race_2_f_w <- asmr_by_race_2_f + 
#     geom_errorbar(aes(ymin = lo_ci, ymax = hi_ci, color = year), width = .25) 
# ggsave(asmr_by_race_2_f_w, 
#        filename = "./report/paa_2017_paper/plots/paper_fig5_asmr_race_all_errorbars.pdf", 
#        width = 8, height = 4, scale = 1)
