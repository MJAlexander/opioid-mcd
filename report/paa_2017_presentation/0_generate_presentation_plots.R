## Plots for PAA Presentation
## Figure 1 -- Overall drug-related mortality rate (opioid vs nonopioid) ----
library(tidyverse)
library(RColorBrewer)
source('./code/functions/helpers.R')
mkdir_p('./report/paa_2017_presentation/plots/')

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

last_points <- prop_total_deaths %>% 
    filter(year == max(year)) %>% 
    mutate(text_lab = sprintf("%s (%2.1f)", measure_cat, round(value, 2)))

nood_plot <- ggplot(data = prop_total_deaths, 
                    aes(year, value, group = measure, color = measure_cat)) +
    geom_hline(yintercept = min(prop_total_deaths$lo_ci), color = "grey50", size = .5) + 
    geom_line(size = 1, alpha = .9) + 
    geom_point(data = last_points, aes(year, value), 
               color = "white", size = 3.5) + 
    geom_point(data = last_points, 
               aes(year, value, color = measure_cat), size = 2) + 
    geom_text(data = last_points, 
              aes(year, value, label = text_lab, color = measure_cat), 
              hjust = -.05, vjust = .5, nudge_x = 0) + 
    scale_color_brewer(NULL, palette = "Dark2") + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) + 
    mk_whites(legend.position = "none", 
              plot.margin = unit(c(0.35, 2.9, 0.3, 2.9), "cm")) + 
    labs(title = "Drug-related mortality rate", 
         subtitle = "By opioid/non-opioid, 2000-2015",
         x = NULL, y = "Mortality rate (per 100,000)")
nood_plot <- add_ci(nood_plot, fill_var = "measure_cat", palette = "Dark2")
nood_plot <- add_vline(nood_plot)
nood_plot <- turn_off_clipping(nood_plot, draw = FALSE)
ggsave(nood_plot, filename = "./report/paa_2017_presentation/plots/fig1_nood_v_ci.pdf", 
       height = 5.5, width = 10, scale = .95, device = cairo_pdf)


## Figure 2a and 2b -- Opioid-related mortality by race (and relative risk) ----
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
    geom_hline(yintercept = min(adj_deaths_overall$lo_ci), color = "grey50", size = .5) + 
    geom_line(size = 1, alpha = .9) + 
    geom_point(size = 1.75, alpha = 1, aes(group = race_cat), color = "white") + 
    geom_point(size = 1.25, alpha = .9) + 
    scale_color_brewer(NULL, palette = "Set1") + 
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) + 
    mk_whites() + 
    labs(title = "Opioid-related mortality rate", 
         subtitle = "By race/ethnicity, 2000-2015",
         x = NULL, y = "Mortality rate (per 100,000)")

adj_overall_ci <- add_ci(adj_overall)
adj_overall_ci_v <- add_vline(adj_overall_ci, ltype = "dashed")
ggsave(adj_overall_ci_v, 
       filename = './report/paa_2017_presentation/plots/fig2_overall_adjusted_ci_v.pdf', 
       height = 5, width = 4, scale = 1, device = cairo_pdf)

## RELATIVE
rel_adj_deaths_overall <- relativizer(adj_deaths_overall)
rel_adj_overall <- ggplot(data = rel_adj_deaths_overall, 
                          aes(x = year, y = relrisk)) + 
    geom_hline(yintercept = min(rel_adj_deaths_overall$lo_ci), 
               color = "grey50", size = .5) + 
    geom_line(size = 1, alpha = .9) + 
    geom_point(size = 1.75, alpha = 1, color = "white") + 
    geom_point(size = 1.25, alpha = .9) +  
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) + 
    mk_whites() + 
    labs(title = "Relative risk",
         subtitle = "Opioid-related mortality, 2000-2015",
         x = NULL, 
         y = "Relative risk (non-Hispanic white / non-Hispanic black)")

rel_adj_overall_ci <- add_ci(rel_adj_overall, fill_var = NULL)
rel_adj_overall_ci_v <- add_vline(rel_adj_overall_ci, ltype = "dashed")
ggsave(rel_adj_overall_ci_v, 
       filename = './report/paa_2017_presentation/plots/fig2_relative_risk_ci_v.pdf', 
       height = 5, width = 4, scale = 1, device = cairo_pdf)


## Paper figure 3 -- opioid related mortality by top 4 ucod ----
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
    geom_hline(yintercept = 0, color = "grey50", size = .5) + 
    geom_line(size = 1, alpha = .9, aes(group = ucod, color = ucod)) + 
    geom_point(size = 1.75, alpha = 1, aes(group = ucod), color = "white") + 
    geom_point(size = 1.25, alpha = .9, aes(group = ucod, color = ucod)) + 
    scale_color_brewer("Underlying\nCause of Death", palette = "Dark2") + 
    scale_x_continuous(expand = c(.01, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    facet_wrap(~ race_cat) + 
    mk_whites(strip.background = element_blank(), 
              strip.text = element_text(size = 11)) + 
    mk_x90() +
    labs(title = paste("Opioid-related mortality rate, by race and", 
                       "underlying cause of death"), 
         x = NULL, y = "Mortality rate (per 100,000)", 
         subtitle = "Top 4 underlying causes only, 2000-2015")

adj_ucod_v <- add_vline(adj_ucod, ltype = "dashed")
adj_ucod_ci_v <- add_ci(adj_ucod_v, fill_var = "ucod", palette = "Dark2", 
                        p_title = "Underlying\nCause of Death")
ggsave(adj_ucod_ci_v, 
       filename = './report/paa_2017_presentation/plots/fig3_ucod_adjusted_ci_v.pdf',
       height = 5.5, width = 10, scale = 1, device = cairo_pdf)


## Figure 4 -- mortality rate by race and underlying type of opioid ----
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
    geom_hline(yintercept = 0, color = "grey50", size = .5) + 
    geom_point(size = 1.75, alpha = 1, color = "white") + 
    geom_point(size = 1.25, alpha = .9) + 
    geom_line(size = 1, alpha = .9) + 
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) +
    scale_color_brewer(NULL, palette = "Set1") +
    facet_wrap(~ t_cat, nrow = 1) + 
    mk_whites(strip.background = element_blank(), 
              strip.text = element_text(size = 11)) +
    mk_x90() + 
    theme(legend.position = c(0, 1), 
          legend.justification = c(0, 1)) + 
    labs(title = "Mortality rate, by race and underlying type of opioid, 2000-2015", 
         x = NULL, y = "Mortality rate (per 100,000)")

t4_adj_plots_race_lines_v <- add_vline(t4_adj_plots_race_lines, ltype = "dashed")
t4_adj_plots_race_lines_v_ci <- add_ci(t4_adj_plots_race_lines_v)
ggsave(t4_adj_plots_race_lines_v_ci, 
       filename = './report/paa_2017_presentation/plots/fig4_t40_adjusted_race_lines_v_ci.pdf', 
       height = 5.5, width = 10, scale = 1, device = cairo_pdf)

## Paper figure 5 -- age-specific opioid-mortality by race and year ----
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
           lo_ci      = death_rate - (1.96 * sqrt(vari) * 1e5)) %>%
    group_by(race, year) %>%
    mutate(death_rate_proportion = death_rate/sum(death_rate),
           se_prop  = sqrt(death_rate_proportion*(1-death_rate_proportion)/sum(death_sum)),
           hi_ci_prop = death_rate_proportion + (1.96 * se_prop), 
           lo_ci_prop = death_rate_proportion - (1.96 * se_prop)
           )

asmr_by_race_2_f <- ggplot(data = age_specific_all, 
                           aes(x = age_cat, y = death_rate, 
                               group = year, color = year)) + 
    geom_hline(yintercept = 0, color = "grey50", size = .5) + 
    geom_point(size = 1.75, alpha = 1, color = "white") + 
    geom_point(size = 1.25, alpha = .9) + 
    geom_line(size = 1, alpha = .9) + 
    facet_wrap(~ race_cat) + 
    scale_y_continuous("Mortality rate (per 100,000)", 
                       expand = c(.01, 0)) + 
    scale_x_discrete("Age group", expand = c(.01, 0)) + 
    scale_color_distiller(NULL, palette = "Purples", direction = 1) + 
    mk_whites(legend.background = 
                  element_rect(fill = alpha("white", .75), color = NA), 
              strip.background = element_blank(), 
               strip.text = element_text(size = 11)) + 
    mk_x90() + 
    mk_legend_ur() + 
    labs(title = "Age-specific opioid-mortality, by race and year")

asmr_by_race_2_f_w <- asmr_by_race_2_f + 
    geom_errorbar(aes(ymin = lo_ci, ymax = hi_ci, color = year), width = .15) 
ggsave(asmr_by_race_2_f_w, 
       filename = "./report/paa_2017_presentation/plots/fig5_asmr_race_all_errorbars.pdf", 
       width = 10, height = 5.5, scale = 1, device = cairo_pdf)


## Alternative: y axis is proportion of total deaths
asmr_by_race_prop <- ggplot(data = age_specific_all, 
                           aes(x = age_cat, y = death_rate_proportion, 
                               group = year, color = year)) + 
  geom_hline(yintercept = 0, color = "grey50", size = .5) + 
  geom_point(size = 1.75, alpha = 1, color = "white") + 
  geom_point(size = 1.25, alpha = .9) + 
  geom_line(size = 1, alpha = .9) + 
  facet_wrap(~ race_cat) + 
  scale_y_continuous("Proportion of total deaths", 
                     expand = c(.01, 0)) + 
  scale_x_discrete("Age group", expand = c(.01, 0)) + 
  scale_color_distiller(NULL, palette = "Purples", direction = 1) + 
  mk_whites(legend.background = 
              element_rect(fill = alpha("white", .75), color = NA)) + 
  mk_x90() + 
  mk_legend_ur() + 
  labs(title = "Age-specific opioid-mortality, by race and year")

asmr_by_race_prop_w <- asmr_by_race_prop + 
  geom_errorbar(aes(ymin = lo_ci_prop, ymax = hi_ci_prop, color = year), width = .15) 
ggsave(asmr_by_race_prop_w, 
       filename = "./report/paa_2017_presentation/plots/fig5_ageprop_race_all_errorbars.pdf", 
       width = 8, height = 4, scale = 1, device = cairo_pdf)


## Paper figure 7 -- age-specific mortality rate by opioid type ----
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
           lo_ci    = death_rate - (1.96 * sqrt(vari) * 1e5)
    )
sub_t4_age <- filter(t4_age_spec, t_code != "Other")

age_t40_3 <- ggplot(data = sub_t4_age, 
                    aes(x = year, y = death_rate, 
                        group = interaction(t_code, race), 
                        color = race_cat)) + 
    geom_hline(yintercept = 0, color = "grey50", size = .5) + 
    geom_point(size = .5, color = "white") + 
    geom_point(size = .3) + 
    geom_line() + 
    scale_color_brewer(NULL, palette = "Set1") + 
    scale_y_continuous("Mortality rate (per 100,000)") + 
    scale_x_continuous("") + 
    mk_whites(legend.background = 
                  element_rect(fill = alpha("white", .75), color = NA)) + 
    mk_x90() + 
    theme(legend.position = c(1, 1), 
          legend.justification = c(1, 1)) +
    labs(title = "Age-specific mortality rate by opioid type, 2000-2015") + 
    facet_grid(t_cat ~ age_cat)

age_t40_3_ci <- add_ci(age_t40_3)
ggsave(age_t40_3_ci, 
       filename = './report/paa_2017_presentation/plots/fig7_t40_notpretty_ci.pdf', 
       width = 8, height = 6, scale = 1.3, device = cairo_pdf)

## Heroin only version of plot 7 ----
heroin_only <- filter(sub_t4_age, t_code == "T401") %>% 
    mutate(lo_ci = max(0, lo_ci))

age_t40_3h <- ggplot(data = heroin_only, 
                    aes(x = year, y = death_rate, 
                        group = race, 
                        color = race_cat)) + 
    geom_hline(yintercept = 0, color = "grey50", size = .5) + 
    geom_point(size = .5, color = "white") + 
    geom_point(size = .3) + 
    geom_line() + 
    scale_color_brewer(NULL, palette = "Set1") + 
    scale_y_continuous("Mortality rate (per 100,000)", expand = c(0, 0)) + 
    scale_x_continuous(NULL, expand = c(.02, 0)) + 
    mk_whites(legend.background = 
                  element_rect(fill = alpha("white", .75), color = NA)) + 
    mk_x90() + 
    theme(legend.position = c(1, .99), 
          legend.justification = c(1, 1)) +
    labs(title = "Heroin-related age-specific mortality rate, 2000-2015") + 
    facet_grid(~ age_cat)

age_t40_3h_ci <- add_ci(age_t40_3h)
ggsave(age_t40_3h_ci, 
       filename = './report/paa_2017_presentation/plots/fig7_t40_heroin_ci.pdf', 
       width = 8, height = 4, scale = 1, device = cairo_pdf)



# 
# 
# heroin_only_purple_plot <- ggplot(data = heroin_only, 
#        aes(x = age_cat, y = death_rate, 
#            group = year, color = year)) + 
#     geom_hline(yintercept = 0, color = "grey50", size = .5) + 
#     geom_errorbar(aes(ymin = lo_ci, ymax = hi_ci, 
#                       color = year), width = .15) +
#     geom_point(size = 1.75, alpha = 1, color = "white") + 
#     geom_point(size = 1.25, alpha = .9) + 
#     geom_line(size = 1, alpha = .9) + 
#     facet_wrap(~ race_cat) + 
#     scale_y_continuous("Mortality rate (per 100,000)", 
#                        expand = c(.01, 0)) + 
#     scale_x_discrete("Age group", expand = c(.01, 0)) + 
#     scale_color_distiller(NULL, palette = "Purples", direction = 1) + 
#     mk_whites(legend.background = 
#                   element_rect(fill = alpha("white", .75), color = NA), 
#               strip.background = element_blank(), 
#               strip.text = element_text(size = 11)) + 
#     mk_x90() + 
#     mk_legend_ur() + 
#     labs(title = "Age-specific heroin-related mortality, by race and year")
# ggsave(heroin_only_purple_plot, 
#        filename = './report/paa_2017_presentation/plots/fig7_t40_heroin_purple.pdf', 
#        width = 10, height = 5.5, scale = 1, device = cairo_pdf)







## Using only one year
asmr_heroin <- ggplot(data = heroin_only %>% filter(year==2015), 
                            aes(x = age_cat, y = death_rate, 
                                group = race_cat, color = race_cat)) + 
  geom_hline(yintercept = 0, color = "grey50", size = .5) + 
  geom_point(size = 1.75, alpha = 1, color = "white") + 
  geom_point(size = 1.25, alpha = .9) + 
  geom_line(size = 1, alpha = .9) + 
  #facet_wrap(~year)+
  scale_y_continuous("Mortality rate (per 100,000)", 
                     expand = c(.01, 0)) + 
  scale_x_discrete("Age group", expand = c(.01, 0)) + 
  scale_color_brewer(NULL, palette = "Set1") + 
  scale_fill_brewer(NULL, palette = "Set1") + 
  mk_whites(legend.background = 
              element_rect(fill = alpha("white", .75), color = NA)) + 
  mk_x90() + 
  mk_legend_ur() + 
  labs(title = "Heroin-related age-specific mortality rate, 2015")

asmr_heroin_w <- asmr_heroin + 
  geom_ribbon(aes(ymin = lo_ci, ymax = hi_ci, fill = race_cat), 
              color = NA, alpha = .25) 
ggsave(asmr_heroin_w, 
       filename = './report/paa_2017_presentation/plots/fig7_heroin_2015_ci.pdf', 
       width = 10, height = 5.5, scale = .9, device = cairo_pdf)

## Paper - figure 8 -- proportion of deaths with two or more opioids ----
library(binom)

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

p_ma_2_or_more_ci <- ggplot(prop, 
                            aes(year, prop_opioid_2_more, 
                                group = race, color = race_cat))+ 
    geom_hline(yintercept = min(prop$ac_om_lower), 
               color = "grey50", size = .5) + 
    geom_ribbon(aes(ymax = ac_om_upper, 
                    ymin = ac_om_lower, 
                    fill = race_cat, group = race_cat),
                alpha = .25, color = NA) + 
    geom_line(size = 1, alpha = .9) + 
    geom_point(size = 1.75, alpha = 1, color = "white") + 
    geom_point(size = 1.25, alpha = .9) + 
    scale_color_brewer(NULL, palette = "Set1") + 
    scale_fill_brewer(NULL, palette = "Set1") + 
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) + 
    mk_whites(strip.background = element_blank(), 
              strip.text = element_text(size = 11)) + 
    labs(y = "Proportion", 
         x = NULL, 
         title = "Proportion of opioid deaths with two or more opioids")

p_ma_2_or_more_ci_v <- add_vline(p_ma_2_or_more_ci, ltype = "dashed")
ggsave(p_ma_2_or_more_ci_v, 
       filename =  "./report/paa_2017_presentation/plots/fig8_prop_2_more_ci_v.pdf", 
       height = 5.5, width = 10, scale = .9, device = cairo_pdf)


## Paper figure 9 -- proportion of heroin deaths involving other opioids ----
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
    geom_hline(yintercept = 0, color = "grey50", size = .5) + 
    geom_ribbon(aes(ymax = upper, 
                    ymin = lower, 
                    fill = race_cat, 
                    group = race_cat), 
                color = NA, alpha = .25) + 
    geom_line(size = 1, alpha = .9) + 
    geom_point(size = 1.75, alpha = 1, color = "white") + 
    geom_point(size = 1.25, alpha = .9) + 
    scale_fill_brewer(NULL, palette = "Set1") + 
    scale_color_brewer(NULL, palette = "Set1") + 
    scale_x_continuous(expand = c(.02, 0)) + 
    scale_y_continuous(expand = c(.01, 0)) + 
    mk_whites(legend.background = 
                  element_rect(fill = alpha("white", .75), color = NA), 
              strip.background = element_blank(), 
              strip.text = element_text(size = 11)) + 
    mk_x90() + 
    labs(y = "Proportion", 
         x = NULL, 
         title = "Proportion of heroin (T401) deaths involving other opioids") +
    facet_wrap(~ comb_401, nrow = 1)

## Add vline version
p_ma_t401_combo_v <- add_vline(p_ma_t401_combo, ltype = "dashed")
ggsave(p_ma_t401_combo_v, 
       filename =  "./report/paa_2017_presentation/plots/fig9_t401_combos_v_ci.pdf", 
       height = 5.5, width = 10, scale = .9, device = cairo_pdf)

