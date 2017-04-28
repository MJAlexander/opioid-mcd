## A script that will download raw data, process it into working datasets, 
## and then generate the plots associated with the paper.

processed_files <- c("df_deaths_tot_pop_overdose.csv", "df_deaths.csv", 
                     "pop_data.csv", "race_age_ac.csv", "race_age_tc_uc_ac.csv",
                     "race_age_tc.csv", "race_age_uc_ac.csv", 
                     "race_age_uc.csv", "standard_pops.csv")

## Make sure all processed files already exist. If not, run processing code.
if (sum(processed_files %in% list.files('./data')) != 
    length(processed_files)) {
    source('./code/1_download_raw_data.R')
    source('./code/2_process_data.R')
}

## Paper plots
source('./report/paa_2017_paper/code/0_generate_paper_plots.R')

## Presentation plots
source('./report/paa_2017_presentation/0_generate_presentation_plots.R')