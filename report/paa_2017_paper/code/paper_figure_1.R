## Figure 1 -- Overall drug-related mortality rate (opioid vs nonopioid)
library(tidyverse)
library(RColorBrewer)
source('./code/functions/helpers.R')
mkdir_p('./report/paa_2017_paper/plots')

## Opioid vs nonopioid deaths
pop_data <- make_superpop(age_ten = FALSE)

d <- read_csv("./data/df_deaths_tot_pop_overdose.csv")
prop_tot <- d %>% 
    select(-pop_std, -pop_year) %>% 
    left_join(pop_data %>% 
                  filter(race == "total") %>% 
                  select(-race), 
              by = c("age", "year")) %>% 
    mutate(asmr_opioid = opioid_deaths / pop_year,
           asmr_nood = nood_deaths / pop_year, 
           opioid_vari = asmr_opioid^2 / opioid_deaths, 
           nood_vari = asmr_nood^2 / nood_deaths) %>% 
    group_by(year) %>% 
    summarise(dras_opioid = weighted.mean(asmr_opioid, pop_std, na.rm = T) * 10^5,
              dras_nood = weighted.mean(asmr_nood, pop_std, na.rm = T) * 10^5, 
              opioid_vari = weighted.mean(opioid_vari, opioid_deaths), 
              nood_vari = weighted.mean(nood_vari, nood_deaths),
              opioid_bound = 1.96 * sqrt(opioid_vari) * 1e5, 
              nood_bound = 1.96 * sqrt(nood_vari) * 1e5)

prop_bounds <- prop_tot %>% 
    gather(measure, bound, opioid_bound:nood_bound)  %>% 
    mutate(measure = ifelse(measure == "opioid_bound", 
                            "dras_opioid", "dras_nood")) %>% 
    select(year, measure, bound)

prop_total_deaths <- prop_tot %>% 
    gather(measure, value, dras_opioid:dras_nood) %>% 
    left_join(prop_bounds, by = c("year", "measure")) %>% 
    mutate(hi_ci = value + bound, 
           lo_ci = value - bound,
           measure_cat = factor(measure, 
                                levels = c("dras_opioid", "dras_nood"), 
                                labels = c("Opioid", "Non-opioid"), 
                                ordered = TRUE))

nood_plot <- ggplot(data = prop_total_deaths, 
                    aes(year, value, group = measure, color = measure_cat)) +
    geom_line(size = 1, alpha = .9) + 
    geom_point(size = 1.25, alpha = .9) + 
    scale_color_brewer(NULL, palette = "Dark2") + 
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) + 
    mk_classic() + 
    labs(title = "Drug-related mortality rate", 
         subtitle = "By Opioid/non-opioid, 2000-2015",
         x = NULL, y = "Mortality rate (per 100,000)")
# ggsave(nood_plot, filename = "./report/paa_2017_paper/plots/paper_fig1_nood.pdf", 
#        height = 4, width = 4, scale = 1)

## Add vline
nood_plot_v <- add_vline(nood_plot)
ggsave(nood_plot_v, filename = "./report/paa_2017_paper/plots/paper_fig1_nood_v.pdf", 
       height = 4, width = 4, scale = 1)

## CI versions
# nood_plot_ci <- add_ci(nood_plot, fill_var = "measure_cat", palette = "Dark2")
# ggsave(nood_plot_ci, filename = "./report/paa_2017_paper/plots/paper_fig1_nood_ci.pdf", 
#        height = 4, width = 4, scale = 1)

# nood_plot_v_ci <- add_vline(nood_plot_ci)
# ggsave(nood_plot_v_ci, filename = "./report/paa_2017_paper/plots/paper_fig1_nood_v_ci.pdf", 
#        height = 4, width = 4, scale = 1)
