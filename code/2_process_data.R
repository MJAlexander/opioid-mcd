## process multiple cause of death files to  get useable datasets for  ----
## opioid analysis
library(tidyverse)
source("./code/functions/helpers.R")

## Define the folder with raw RDS files (defaul: ./raw_data/) and to save
## csv files
data_folder <- './raw_data'
csv_folder  <- './data'
dir.create(csv_folder, showWarnings = FALSE)

## Read in the population data for race/year and all the standard populations
pop_data <- read_csv(paste0(csv_folder, '/pop_data.csv'))
us_2000  <- make_us2000_std_pop(paste0(csv_folder, '/standard_pops.csv'))

## Loop through all the raw data files and concatenate into processed files.
i <- 1
for(y in 2000:2015){
    print(y)
    
    ## Read in data
    data_year <- readRDS(sprintf('%s/mort%s_full.rds', data_folder, y))
    
    ## Subset twice so we don't need to do it again later -- once with
    ## racial filtering and once without it.
    sub_data  <- subset_raw_data(data_year)
    sub_data_all_races <- subset_raw_data(data_year, filter_race = FALSE)
    
    ## Garbage collection -- datasets are relatively small (800 MB), but I
    ## keep getting segfaults so hoping this helps.
    rm(data_year)
    gc()
    
    ## data_list contains df, df_long, duc, ducx, dac
    ## data_list_all_races is the same, but doesn't filter out NHW and NHB
    data_list <- make_df(sub_data, year = y,
                         df_only = FALSE, return_long = TRUE)
    ## Make a data_list with all races
    data_list_all_races <- make_df(sub_data_all_races, year = y,
                                   df_only = FALSE, return_long = TRUE)
    
    ## Now we do the main processing for each file
    ## Age-specific rates of UC ----
    race_age_uc <- data_list$df %>%
        group_by(race, age, ucod, uc) %>% 
        summarize(uc_deaths = n()) %>% 
        ungroup() %>% 
        mutate(race     = ifelse(race == 1, "white", "black"), 
               race_cat = categorize_race(race), 
               age_cat  = categorize_age_5(age), 
               year     = y) %>% 
        left_join(pop_data, by = c("year", "age", "race")) %>% 
        left_join(us_2000, by = "age") %>% 
        mutate(uc_rate = uc_deaths / pop_year)
    
    ## UC / AC relations ----
    race_age_uc_ac <- data_list$df_long %>% 
        group_by(race, age, ucod, code) %>% 
        summarise(uc_ac_deaths = n()) %>% 
        arrange(race, age, (ucod),desc(uc_ac_deaths)) %>% 
        ungroup() %>% 
        mutate(race     = ifelse(race == 1, "white", "black")) %>% 
        left_join(race_age_uc %>% 
                      select(race, age, ucod, uc_deaths, pop_std, pop_year), 
                  by = c("race", "age", "ucod")) %>% 
        mutate(race_cat = categorize_race(race), 
               age_cat  = categorize_age_5(age), 
               year     = y, 
               prop_ac = uc_ac_deaths / uc_deaths) %>% 
        filter(!is.na(code))
        
    ## Focus on AC ----
    # want to rearrange data frame such that opioid AC is reference, then uc, 
    # then all other associated causes
    # TODO: do this for all deaths, not just AC
    # 
    # MK: Not going to lie, I don't understand what is happening here so I just
    # left it as is. Assuming if it needs mods, you're going to do it.
    df.torearrange <- filter(data_list$df, uc != "Mental")
    dac.rearrange <- data.frame(sex  = df.torearrange$sex, 
                                age  = df.torearrange$age, 
                                race = df.torearrange$race, 
                                uc   = df.torearrange$uc, 
                                opioid_code = NA, 
                                ucod = df.torearrange$ucod)
    
    # for remaining associated causes
    dac.rearrange <- cbind(dac.rearrange, 
                           matrix(NA, nrow = nrow(df.torearrange), 
                                  ncol = 19))
    colnames(dac.rearrange)[7:ncol(dac.rearrange)] <- paste0("ac", 1:19)
    
    # assumption: just take the first associated cause that is opioid related
    oid <- c(sapply(1:nrow(df.torearrange), 
                    function(i) {
                        grep(paste(make_pois_codes_with_opium(), 
                                   collapse = "|"), 
                             df.torearrange[i, ], value = TRUE)[1]
                        }))
    names(oid) <- NULL
    
    r120 <- paste0("record_", 1:20)
    dac.rearrange$opioid_code <- oid
    acs <- t(sapply(1:nrow(df.torearrange), 
                    function(i) { as.data.frame(
                        df.torearrange[i, 
                                       r120])[df.torearrange[i, 
                                                             r120] != oid[i]] 
                        }))
    dac.rearrange[, paste0('ac', 1:19)] <- acs
    dfa <- dac.rearrange
    
    dfa$year <- y
    dfa$id <- 1:nrow(dfa)
    dfa <- dfa[, c("id","sex","age", "race", "uc","opioid_code", 
                   "ucod", paste0("ac", 1:19))] 
    dfa.long <- gather(dfa, cause_position, code, ac1:ac19, 
                       factor_key = TRUE)
    
    ## Age-specific rates of AC ----
    race_age_ac <- dfa %>% 
        group_by(race, age, opioid_code) %>% 
        summarise(ac_deaths = n()) %>% 
        ungroup() %>% 
        mutate(race = ifelse(race == 1, "white", "black"), 
               year = y) %>% 
        left_join(pop_data, by = c("year", "age", "race")) %>% 
        left_join(us_2000, by = "age") %>% 
        mutate(ac_rate  = ac_deaths / pop_year, 
               race_cat = categorize_race(race), 
               age_cat  = categorize_age_5(age))
    
    ## Opioid AC and other AC relations ----
    race_age_ac_ac <- dfa.long %>% 
        group_by(race, age, opioid_code, ucod, code) %>% 
        summarise(uc_tc_ac_deaths = n()) %>% 
        arrange(race, age, opioid_code,desc(uc_tc_ac_deaths)) %>% 
        ungroup() %>% 
        mutate(race = ifelse(race == 1, "white", "black"), 
               year = y) %>% 
        left_join(race_age_ac %>% 
                      select(race, age, opioid_code, ac_deaths, 
                             pop_std, pop_year), 
                  by = c("race", "age", "opioid_code")) %>% 
        mutate(prop_ac = uc_tc_ac_deaths / ac_deaths, 
               race_cat = categorize_race(race), 
               age_cat = categorize_age_5(age)) %>% 
        filter(!is.na(code))
    
    ## Counts of Opioid / Non-opioid deaths by age and race ----
    # counts of all opioid deaths
    opioid_deaths <- race_age_uc %>% 
        group_by(race, age) %>% 
        summarise(opioid_deaths = sum(uc_deaths))
    
    # counts by opioid type
    opioid_type_deaths <- race_age_uc_ac %>% 
        filter(grepl(paste(make_pois_codes_with_opium(), 
                           collapse="|"), code)) %>% 
        group_by(race, age, code) %>% 
        summarise(deaths = sum(uc_ac_deaths)) %>%
        spread(code, deaths)
    
    df_opioid <- opioid_deaths %>% 
        left_join(opioid_type_deaths, by = c("race", "age")) 
    
    if ("T400" %in% names(df_opioid)) {
        df_opioid <- df_opioid %>% 
            select(-T400)
    }
    
    ## Non opioid
    dx_nonopioid <-make_dx_nonopioid(sub_data, year = y)
    
    # counts of non-opioid drug overdose
    nood_deaths <- dx_nonopioid %>% 
        mutate(race = ifelse(race == 1, "white", "black")) %>% 
        group_by(race, age) %>% 
        summarise(nood_deaths = n()) 
    
    # counts of all deaths
    all_deaths <- sub_data %>% 
        mutate(race = ifelse(race == 1, "white", "black")) %>% 
        group_by(race, age) %>% 
        summarise(total_deaths = n())
    df_deaths <- all_deaths %>% 
        left_join(nood_deaths, by = c("race", "age")) %>% 
        left_join(df_opioid, by = c("race", "age")) %>% 
        mutate(year = y) %>% 
        left_join(pop_data, by = c("year", "age", "race")) %>% 
        left_join(us_2000, by = "age")
    
    ## Multiple causes
    d_mc <- data_list$df_long %>% 
        filter(uc != "Mental") %>% 
        group_by(id, year, age, race) %>% 
        summarise(n_deaths = 1,
                  n_opioids = sum(code %in% make_pois_codes_with_opium(), 
                                  na.rm = TRUE),
                  opioids_1      = n_opioids == 1,
                  opioids_2      = n_opioids == 2,
                  opioids_3_more = n_opioids > 2,
                  n_tcodes       = sum(grepl("T", code), na.rm = TRUE),
                  n_nonopiods    = n_tcodes - n_opioids,
                  has_nonopiods  = n_nonopiods > 0,
                  t400 = sum(code == "T400", na.rm = TRUE),
                  t401 = sum(code == "T401", na.rm = TRUE),
                  t402 = sum(code == "T402", na.rm = TRUE),
                  t403 = sum(code == "T403", na.rm = TRUE),
                  t404 = sum(code == "T404", na.rm = TRUE),
                  t406 = sum(code == "T405", na.rm = TRUE),
                  t401_400 = sum(code =="T401" | 
                                     code == "T400", na.rm = TRUE) == 2,
                  t401_402 = sum(code =="T401" | 
                                     code == "T402", na.rm = TRUE) == 2,
                  t401_403 = sum(code =="T401" | 
                                     code == "T403", na.rm = TRUE) == 2,
                  t401_404 = sum(code =="T401" | 
                                     code == "T404", na.rm = TRUE) == 2,
                  t402_400 = sum(code == "T402" | 
                                     code == "T400", na.rm = TRUE) == 2,
                  t402_403 = sum(code == "T402" | 
                                     code == "T403", na.rm = TRUE) == 2,
                  t402_404 = sum(code == "T402" | 
                                     code == "T404", na.rm = TRUE) == 2, 
                  t403_400 = sum(code == "T403" | 
                                     code == "T400", na.rm = TRUE) == 2,
                  t403_404 = sum(code == "T403" | 
                                     code == "T404", na.rm = TRUE) == 2,
                  t404_400 = sum(code == "T403" | 
                                     code == "T404", na.rm = TRUE) == 2)
    
    d_mc <- d_mc %>% mutate(t400_only = opioids_1 == TRUE & t400 == 1,
                            t401_only = opioids_1 == TRUE & t401 == 1,
                            t402_only = opioids_1 == TRUE & t402 == 1,
                            t403_only = opioids_1 == TRUE & t403 == 1,
                            t404_only = opioids_1 == TRUE & t404 == 1,
                            t406_only = opioids_1 == TRUE & t406 == 1)
    
    race_age_tc <- d_mc %>% 
        group_by(age, race) %>% 
        summarise_each(funs(sum)) %>% 
        select(-id,-year) %>%
        mutate(year = y, 
               ave_opioids = n_opioids / n_deaths, 
               ave_drugs = n_tcodes / n_deaths,
               ave_nonopiods = n_nonopiods / n_deaths, 
               race = ifelse(race == 1, "white", "black")) %>% 
        left_join(pop_data, by = c("year", "age", "race")) %>% 
        left_join(us_2000, by = "age")
        
    
    ## Drug deaths
    ## Non opioid drugs
    dx_nonopioid_r <-make_dx_nonopioid(sub_data_all_races, year = y)
    
    # counts of non-opioid drug overdose
    nood_deaths_r <- dx_nonopioid_r %>% 
        group_by(age, year) %>% 
        summarise(nood_deaths = n())
    
    
    opioid_deaths_r <- data_list_all_races$df %>% 
        filter(uc == "Overdose") %>% 
        group_by(age, year) %>% 
        summarise(opioid_deaths = n())
    
    # counts of all deaths
    drug_deaths <- merge(nood_deaths_r, opioid_deaths_r) %>% 
        left_join(us_2000, by = "age") %>% 
        left_join(pop_data %>% 
                      filter(year == y) %>% 
                      group_by(age) %>% 
                      summarize(pop_year = sum(pop_year)), 
                  by = "age")
    
    res_year <- list(race_age_uc       = race_age_uc,
                     race_age_uc_ac    = race_age_uc_ac,
                     race_age_ac       = race_age_ac, 
                     race_age_tc_uc_ac = race_age_ac_ac,
                     df_deaths         = df_deaths, 
                     race_age_tc       = race_age_tc, 
                     df_deaths_tot_pop_overdose = drug_deaths)
    
    if (i == 1) {
        res_final <- res_year
    } else {
        res_final <- mapply(rbind, res_final, res_year)
    }
    i <- i + 1
}

## Bind and save
lapply(1:length(res_final), 
       function(i) {
           write_csv(res_final[[i]], 
                     path = paste0(csv_folder, '/', 
                                   names(res_final)[i],".csv"))
           })
