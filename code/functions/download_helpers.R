## MK Helper functions for Multiple Cause of Death Paper (PAA 2017) ----
library(tidyverse)

## Download helpers
download_mcod_zip <- function(year, download_dir = './raw_data/', 
                              delete_zip = TRUE) {
    ## Downloads the raw data for specified year, unzips it, deletes.
    ## Source: print(paste0('http://www.nber.org/data/vital-statistics', 
    ##                      '-mortality-data-multiple-cause-of-death.html'))
    
    ## Create URL
    base_url  <- "http://www.nber.org/mortality"
    file_name <- sprintf('mort%s.csv', year)
    file_url  <- sprintf("%s/%s/%s.zip", base_url, year, file_name)
    dest_file <- sprintf('%s%s.zip', download_dir, file_name)
    
    ## mkdir -p
    if (download_dir != './'){
        dir.create(download_dir, showWarnings = FALSE)
    }
    
    ## Get, unzip
    download.file(file_url, dest_file)
    unzip(dest_file, exdir = download_dir)
    
    ## Clean up
    if (delete_zip) {
        file.remove(dest_file)
    }
}


download_population_data <- function(save_dir = './data', return_df = TRUE) {
    ## Downloads data files for the 2000-2015 nonHispanic black and white 
    ## populations by age groups. Saves as a csv file in specified folder.
    ## Source: print(paste0('https://www2.census.gov/programs-surveys/', 
    ##                      'popest/datasets/2010-2015/state/asrh/', 
    ##                      'sc-est2015-alldata6.pdf'))
    
    ## This one is the state-level race, origin, age file. As far as I can tell,
    ## there is no national file so we just aggregate up.
    url_2010_2015 <- paste0('https://www2.census.gov/programs-surveys/', 
                            'popest/datasets/2010-2015/state/asrh/', 
                            'sc-est2015-alldata6.csv')
    
    ## This is the national file but age is in 5 year bins with 21 groups.
    url_2000_2010 <- paste0('https://www2.census.gov/programs-surveys/', 
                            'popest/datasets/2000-2010/intercensal/', 
                            'national/us-est00int-alldata-5yr.csv')
    
    ## First, 2000-2010. Read in and only take nonhispanic whites and blacks
    df_2000 <- read_csv(url_2000_2010) %>% 
        setNames(tolower(names(.))) %>% 
        filter(agegrp > 0, 
               month == 7) %>% 
        select(year, agegrp, nhwa_male, nhwa_female, 
               nhba_male, nhba_female, tot_pop) 
    
    ## Now take sum of male and female and gather up columns
    df_2000 <- df_2000 %>% 
        mutate(white = nhwa_male + nhwa_female, 
               black = nhba_male + nhba_female, 
               total = tot_pop) %>% 
        select(year, agegrp, white, black, total) %>% 
        gather(key = race, value = pop, white:total)
    
    ## Now fix age groupings to match our 5-year groupings
    ## Also take out 2010 since we have it in next df
    df_2000 <- df_2000 %>% 
        ungroup() %>% 
        mutate(age = (agegrp - 1) * 5, 
               age = ifelse(age > 85, 85, age)) %>% 
        group_by(year, age, race) %>% 
        summarize(pop = sum(pop)) %>% 
        filter(year != 2010)
    
    ## Now we do 2010 - 2015
    ## Import and immediately filter out sex, limit to nonHispanic white and
    ## black.
    df_2010 <- read_csv(url_2010_2015) %>% 
        setNames(tolower(names(.))) %>% 
        select(sex, origin, race, age, starts_with("popestimate"))
    
    df_2010_bw <- df_2010 %>%
        filter(sex == 0, 
               origin == 1, 
               race %in% 1:2) %>% 
        select(-origin, -sex) %>% 
        group_by(race, age) %>% 
        summarize_all(sum)
    
    df_2010_tot <- df_2010 %>% 
        filter(sex == 0, 
               origin == 0) %>% 
        select(-origin, -sex) %>% 
        group_by(age) %>% 
        summarize_all(sum) %>% 
        mutate(race = 0)
    
    df_2010_all <- rbind(as.data.frame(df_2010_bw), 
                         as.data.frame(df_2010_tot)) %>% tbl_df()
    
    ## Change single year ages to 5-year grouping (0-4, 5-9, etc.)
    df_2010_all <- df_2010_all %>% 
        mutate(age = (findInterval(age, c(seq(0, 85, 5), 1000)) - 1) * 5)
    
    ## Now group by age_cat and race and sum columns (years)
    df_2010_all <- df_2010_all %>% 
        group_by(race, age) %>% 
        summarize_at(vars(starts_with("popestimate")), funs(sum)) %>% 
        gather(key = year, value = pop, popestimate2010:popestimate2015) %>%
        ungroup() %>% 
        mutate(year = as.integer(gsub(x = year, "popestimate", "")), 
               race = ifelse(race == 0, "total", 
                             ifelse(race == 1, "white", "black"))) %>% 
        select(year, pop, race, age)
    
    df <- rbind(as.data.frame(df_2000), 
                as.data.frame(df_2010_all)) %>% 
        rename(pop_year = pop)
    
    dir.create(save_dir, showWarnings = FALSE)
    write_csv(df, paste0(save_dir, "/pop_data.csv"))
    
    if (return_df) {
        return(df)
    }
}


download_standard_pops <- function(save_dir = './data', return_df = TRUE) {
    ## Downloads different standard populations from SEER website using the
    ## 18 age group coding. Performs minimal manipulation to make age and 
    ## standard factors readable. 
    ## 
    ## NOTE: Stores all standards so you'll need to filter to a specific 
    ## standard and then left_join() on age_cat.
    
    
    ## Download the 18 (0-4 year old) grouping
    pop_url <- "https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt"
    
    ## Make a dictionary for factor values of standard population
    ## This includes codes for the 19 age group coding -- keep them in case
    ## we decide to use that instead.
    standards_dict <- list(
        s6   = "World (Segi 1960) Std Million (19 age groups)", 
        s7   = "1991 Canadian Std Million (19 age groups)", 
        s5   = "European (Scandinavian 1960) Std Million (19 age groups)", 
        s8   = "1996 Canadian Std Million (19 age groups)", 
        s10  = "World (WHO 2000-2025) Std Million (19 age groups)", 
        s141 = "1940 US Std Million (19 age groups)", 
        s151 = "1950 US Std Million (19 age groups)", 
        s161 = "1960 US Std Million (19 age groups)", 
        s171 = "1970 US Std Million (19 age groups)", 
        s181 = "1980 US Std Million (19 age groups)", 
        s191 = "1990 US Std Million (19 age groups)", 
        s201 = "2000 US Std Million (19 age groups)", 
        s203 = "2000 US Std Population (19 age groups - Census P25-1130)", 
        s202 = "2000 US Std Population (single ages to 84 - Census P25-1130)", 
        s205 = "2000 US Std Population (single ages to 99 - Census P25-1130)", 
        s11  = "World (WHO 2000-2025) Std Million (single ages to 84)", 
        s12  = "World (WHO 2000-2025) Std Million (single ages to 99)", 
        s1   = "World (Segi 1960) Std Million (18 age groups)", 
        s2   = "1991 Canadian Std Million (18 age groups)", 
        s3   = "European (Scandinavian 1960) Std Million (18 age groups)", 
        s4   = "1996 Canadian Std Million (18 age groups)", 
        s9   = "World (WHO 2000-2025) Std Million (18 age groups)", 
        s140 = "1940 US Std Million (18 age groups)", 
        s150 = "1950 US Std Million (18 age groups)", 
        s160 = "1960 US Std Million (18 age groups)", 
        s170 = "1970 US Std Million (18 age groups)", 
        s180 = "1980 US Std Million (18 age groups)", 
        s190 = "1990 US Std Million (18 age groups)", 
        s200 = "2000 US Std Million (18 age groups)", 
        s204 = "2000 US Std Population (18 age groups - Census P25-1130)")
    
    standard_pop <- read_fwf(pop_url, 
                             fwf_widths(c(3, 3, 8), 
                                        c("standard", "age", "pop")), 
                             col_types = "iii") %>%
        mutate(standard = paste0("s", standard), 
               standard_cat = factor(standard, 
                                     levels = names(standards_dict), 
                                     labels = unname(unlist(standards_dict)), 
                                     ordered = TRUE), 
               age = (age - 1) * 5,
               age_cat = categorize_age_5(age)) %>% 
        select(age_cat, standard_cat, pop_std = pop, everything())
    
    dir.create(save_dir, showWarnings = FALSE)
    write_csv(standard_pop, paste0(save_dir, "/standard_pops.csv"))
    
    if (return_df) {
        return(standard_pop)
    }
}
