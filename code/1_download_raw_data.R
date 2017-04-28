## Checks to make sure all the raw data exists, and if not, downloads it.
library(tidyverse)
source('./code/functions/download_helpers.R')
source('./code/functions/helpers.R')

## Define parameters ---- 
raw_folder <- './raw_data'
csv_folder <- './data'
start_year <- 2000
end_year   <- 2015

## Get all the multiple cause of death data. ----
## Save it as RDS (compression) and then delete CSVs
## NOTE: Some years use "Z" an "ZZZ" as indicator of foreign residence. 
##       Turn into NA. Won't use columns anyways, but just for consistency.
for (year in start_year:end_year) {
    ## Check if we've already made this RDS file.
    ## If not, download original zip, unzip, import, save, and delete zip/csv
    if (!file.exists(sprintf('%s/mort%s_full.rds', raw_folder, year))) {
        print(year)
        download_mcod_zip(year)
        temp <- read_csv(sprintf('%s/mort%s.csv', raw_folder, year), 
                         na = c("", "NA", "Z", "ZZZ"))
        saveRDS(temp, file = sprintf('%s/mort%s_full.rds', raw_folder, year))
        file.remove(sprintf('%s/mort%s.csv', raw_folder, year))
    }
}


## Get population data for 2000-2015
download_population_data(save_dir = csv_folder, return_df = FALSE)


## Get standard populations
download_standard_pops(save_dir = csv_folder, return_df = FALSE)
