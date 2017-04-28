## MK Helper functions for Multiple Cause of Death Paper (PAA 2017) ----
library(tidyverse)
library(scales)
library(grid)

## Processing dataframes ----
make_ocodes <- function() {
    ## Just a helper for defining codes we are interested in.
    ocodes <- c(paste0("T40", c(0:4,6)), # opioid poisoning codes
                paste0("X4",0:4),        # unintentional overdose
                paste0("X6",0:4),        # suicide
                paste0("Y1",0:4),        # drug poisonings of unknown intent
                "X85",                   # homocide
                paste0("F11", 0:9)       # mental/behvaioral disorders
                #"Y450                   # complications of med / surg care
    )
    return(ocodes)
}


make_pois_codes <- function() {
    ## Allows us to quickly change the T codes we want to look at
    return(paste0("T40", c(1:4,6)))
}


make_pois_codes_with_opium <- function() {
    ## Same as make_pois_codes() but with opium
    return(paste0("T40", c(0:4,6)))
}


subset_raw_data <- function(df_raw, filter_race = TRUE) {
    ## Takes the raw data and subsets to the columns we want, then mutates
    ## race to codes we want and subsets, and finally fixes 27 age categories
    ## to match with our 5-year categories.
    sub_df <- df_raw %>% 
        select(sex, ager27, ucod, starts_with("record_"), 
               race = hspanicr) %>%
        mutate(race = race - 5, 
               age = (findInterval(ager27, c(0, 7:23, 50)) - 1) * 5) %>%  
        select(sex, ager27, race, ucod, starts_with("record_"), age)
    
    if (filter_race) {
        sub_df <- filter(sub_df, race %in% 1:2)
    }
    return(sub_df)
}


make_duc <- function(df, year, create_subset_df = FALSE) {
    ## Takes a raw df or (preferably) an already subsetted df and converts it 
    ## to our `duc` df.
    ## 
    ## If using the raw dataframe, use create_subset_df = TRUE
    
    if (create_subset_df) {
        sub_df <- subset_raw_data(df)
    } else {
        sub_df <- df
    }
    
    duc <- sub_df %>% 
        filter(ucod %in% make_ocodes()) %>% 
        mutate(uc = "Mental", 
               year = year) %>% 
        arrange(race, year, sex, ucod, 
                record_1, record_2, record_3, record_4, 
                record_5, record_6, record_7, record_8, 
                record_9, record_10, record_11, record_12, 
                record_13, record_14, record_15, record_16, 
                record_17, record_18, record_19, record_20) %>%
        mutate(id = 1:n()) %>% 
        select(sex, ager27, age, race, uc, ucod, 
               starts_with("record_"), year, id) 
    
    ducx <- duc %>% 
        filter(grepl("F|T", ucod) == FALSE) %>% 
        unite(record_cat, record_1:record_20, sep = " ", remove = FALSE) %>% 
        mutate(record_cat = gsub(x = record_cat, 
                                 pattern = " NA", 
                                 replacement = ""), 
               contains_ocode = grepl(pattern = 
                                          paste(make_pois_codes_with_opium(), 
                                                collapse = "|"), 
                                      x = record_cat), 
               uc = ifelse(contains_ocode, "Overdose", uc)) %>% 
        filter(contains_ocode) %>% 
        select(-contains_ocode, -record_cat)
    
    bind_duc <- rbind(filter(duc, grepl("F|T", ucod)), ducx) 
    
    return(bind_duc)
}


make_dx_nonopioid <- function(df, year, create_subset_df = FALSE) {
    ## Takes a raw df or (preferably) an already subsetted df and converts it 
    ## to the dx_nonopioid df. It gets its own function because make_duc()
    ## performs an rbind() too early.
    ## 
    ## If using the raw dataframe, use create_subset_df = TRUE
    
    if (create_subset_df) {
        sub_df <- subset_raw_data(df)
    } else {
        sub_df <- df
    }
    
    duc <- sub_df %>% 
        filter(ucod %in% make_ocodes()) %>% 
        mutate(uc = "Mental", 
               year = year) %>% 
        arrange(race, year, sex, ucod, 
                record_1, record_2, record_3, record_4, 
                record_5, record_6, record_7, record_8, 
                record_9, record_10, record_11, record_12, 
                record_13, record_14, record_15, record_16, 
                record_17, record_18, record_19, record_20) %>%
        mutate(id = 1:n()) %>% 
        select(sex, ager27, age, race, uc, ucod, 
               starts_with("record_"), year, id) 
    
    ducx <- duc %>% 
        filter(grepl("F|T", ucod) == FALSE) %>% 
        unite(record_cat, record_1:record_20, sep = " ", remove = FALSE) %>% 
        mutate(record_cat = gsub(x = record_cat, 
                                 pattern = " NA", 
                                 replacement = ""), 
               contains_ocode = grepl(pattern = 
                                          paste(make_pois_codes_with_opium(), 
                                                collapse = "|"), 
                                      x = record_cat), 
               uc = ifelse(contains_ocode, "Overdose", uc)) %>% 
        filter(contains_ocode) %>% 
        select(-contains_ocode, -record_cat)
    
    dx_nonopioid <- duc %>% 
        filter(grepl("X|Y", ucod), !(id %in% ducx$id))
    dx_nonopioid$uc <- "Overdose, non-opioid"
    dx_nonopioid <- dx_nonopioid %>% select(-id)
    
    return(dx_nonopioid)
}


make_dac <- function(df, year, create_subset_df = FALSE) {
    ## Takes a raw df or (preferably) an already subsetted df and converts it 
    ## to our `dac` df.
    ## 
    ## ## If using the raw dataframe, use create_subset_df = TRUE
    if (create_subset_df) {
        sub_df <- subset_raw_data(df)
    } else {
        sub_df <- df
    }
    
    dac <- sub_df %>% 
        filter(!(ucod %in% make_ocodes())) %>% 
        unite(record_cat, record_1:record_20, sep = " ", remove = FALSE) %>% 
        mutate(record_cat = gsub(x = record_cat, 
                                 pattern = " NA", replacement = ""), 
               contains_ocode = grepl(pattern = 
                                          paste(make_pois_codes_with_opium(), 
                                                collapse = "|"), 
                                      x = record_cat), 
               uc = ifelse(contains_ocode, "Other", NA), 
               year = year) %>% 
        select(sex, ager27, age, race, uc, ucod, 
               starts_with("record_"), year, everything()) %>% 
        filter(contains_ocode == TRUE) %>% 
        select(-contains_ocode, -record_cat)
    
    return(dac)
}


make_df <- function(df, year, create_subset_df = FALSE, 
                    df_only = TRUE, return_long = FALSE) {
    ## Takes a raw df or (preferably) an already subsetted df and converts it 
    ## to our the df we use for most manipulations. If you want everything 
    ## (df, df_long, duc, dac), use df_only = FALSE and return_long = TRUE.
    ## 
    ## If using raw data, use create_subset_df = TRUE.
    if (create_subset_df) {
        sub_df <- subset_raw_data(df)
    } else {
        sub_df <- df
    }
    
    duc  <- make_duc(sub_df, year)
    dac  <- make_dac(sub_df, year)
    df   <- rbind(select(duc, -id), 
                  dac)
    
    df <- df %>% 
        arrange(year, race, ager27, age, sex, ucod, 
                record_1, record_2, record_3, record_4, 
                record_5, record_6, record_7, record_8, 
                record_9, record_10, record_11, record_12, 
                record_13, record_14, record_15, record_16, 
                record_17, record_18, record_19, record_20) %>% 
        mutate(id = 1:n()) %>% 
        select(id, year, sex, ager27, age, race, uc, 
               ucod, starts_with("record_"), everything())
    
    ## NOTE: There's probably a better way to do this, but rowwise() is very
    ## slow.
    ## For example: ... %>% rowwise() %>% mutate(sum.missing = sum(is.na(.)))
    df$sum.missing <- sapply(1:nrow(df), 
                             function(i) {
                                 sum(is.na(df[i, paste0("record_", 2:20)]))
                             })
    
    if (df_only) {
        return(df)
    } else if (return_long) {
        df_long <- gather(df, cause_position, code, 
                          record_1:record_20, factor_key=TRUE)
        return(list(df = df, df_long = df_long, 
                    dac = dac, duc = duc))
    } else {
        return(list(df = df, dac = dac, duc = duc))
    }
}


make_us2000_std_pop <- function(std_pops_file = './data/standard_pops.csv') {
    ## Just a quick convenience function to bring up the 2000 std population
    
    us_2000 <- read_csv('./data/standard_pops.csv') %>% 
        filter(standard == "s204") %>% 
        select(pop_std, age)
    
    return(us_2000)
}


make_superpop <- function(age_ten = TRUE, pop_file = './data/pop_data.csv', 
                          std_pops_file = './data/standard_pops.csv') {
    us_2000  <- make_us2000_std_pop(std_pops_file = std_pops_file)
    pop_data <- read_csv(pop_file)
    
    pop_df <- pop_data %>% 
        left_join(us_2000, by = "age")
    
    if (age_ten) {
        pop_df <- pop_df %>% 
            mutate(age = convert_age_to_10(age)) %>% 
            group_by(age, year, race) %>% 
            summarize(pop_std  = sum(pop_std), 
                      pop_year = sum(pop_year))
    }
    
    return(pop_df)
}


## Plotting helpers ----
## Factorize functions so we can change labels in one place and have
## changes propogate throughout plots.
categorize_race <- function(race_column) {
    x <- factor(race_column, 
                levels = c("white", "black", "both"), 
                labels = c("non-Hispanic white", "non-Hispanic black", "Both"), 
                ordered = TRUE)
    return(x)
}


categorize_uc <- function(uc_column) {
    x <- factor(uc_column, 
                levels = c("Overdose", "Mental", "Other"), 
                ordered = TRUE)
    return(x)
}


categorize_age_5 <- function(age_column) {
    x <- factor(age_column, 
                levels = seq(0, 85, 5), 
                labels = c(paste0(seq(0, 84, 5), "-", 
                                  seq(4, 84, 5)), "85+"), 
                ordered = TRUE)
    return(x)
}


categorize_age <- function(age_column_10) {
    x <- factor(age_column_10, 
                levels = seq(0, 80, 10), 
                labels = c(paste0(seq(0, 79, 10), "-", 
                                  seq(9, 79, 10)), "80+"), 
                ordered = TRUE)
    return(x)
}


categorize_tcode <- function(t_code_column) {
    x <- factor(t_code_column, 
                levels = c("T400", "T401", "T402", "T403", 
                           "T404", "T406", "Other"), 
                labels = c("Opium (T400)", "Heroin (T401)", 
                           "Other Natural (T402)", 
                           "Methadone (T403)", 
                           "Other Synthetic (T404)", 
                           "Unspecified (T406)", 
                           "Other"), 
                ordered = TRUE)
    return(x)
}


convert_age_to_10 <- function(age_5) {
    ## Take as input a vector of 5-year age groups and return 10-year groups
    age_10 <- (findInterval(age_5, seq(0, 90, 10), 
                            rightmost.closed = TRUE) - 1) * 10
    return(age_10)
}


add_tplot_labels <- function(code_column) {
    tcode_dict <- list(
        T374 = "Anthelminthics (T374)",
        T391 = "4-Aminophenol derivatives (T391)",
        T393 = "NSAIDs (T393)",
        T402 = "Other opioids (T402)",
        T403 = "Methadone (T403)",
        T404 = "Synthetic narcotic (T404)",
        T405 = "Cocaine (T405)",
        T406 = "Unspecified narcotics (T406)",
        T407 = "Cannabis (T407)",
        T409 = "Unspecified psychodysleptics (T409)",
        T411 = "Intravenous anesthetics (T411)",
        T412 = "Other general anesthetics (T412)",
        T421 = "Iminostilbenes (T421)",
        T423 = "Barbiturates (T423)",
        T424 = "Benzodiazepines (T424)",
        T426 = "Other antiepileptic (T426)",
        T427 = "Unknown antiepileptic (T427)",
        T428 = "Antiparkinsonism (T428)",
        T430 = "Tri/tetracyclic antidepressants (T430)",
        T432 = "Unspecified antidepressants (T432)",
        T433 = "Phenothiazine antipsychotics (T433)",
        T435 = "Unspecified antipsychotics (T435)",
        T436 = "Psychostimulants (T436)",
        T438 = "Psychotropic (T438)",
        T443 = "Parasympatholytics (T443)",
        T449 = "Unspecific ANS drug (T449)",
        T450 = "Antiallergic or antiemetic (T450)",
        T461 = "Calcium-channel blockers (T461)",
        T483 = "Antitussives(T483)",
        T505 = "Appetite depressants (T505)",
        T509 = "Unspecified medicaments (T509)",
        T510 = "Ethanol (T510)",
        T519 = "Unspecified alcohol (T519)",
        T58  = "Carbon monoxide (T58)",
        T598 = "Gas, fumes, vapors (T598)",
        T659 = "Unspecified substance (T659)",
        T501 = "Loop diuretics (T501)"
    )
    
    x <- factor(code_column, 
                levels = names(tcode_dict), 
                labels = unname(unlist(tcode_dict)), 
                ordered = TRUE)
    return(x)
}


add_vline <- function(grob, year = 2010, ltype = "dotted", alpha = .5) {
    ## Takes a plot and adds a vertical line **underneath** other layers.
    grob$layers <- c(geom_vline(xintercept = year, 
                                linetype = ltype, 
                                alpha = alpha), 
                     grob$layers)
    return(grob)
}


add_ci <- function(grob, alpha = .25, color = NA, p_title = NULL,
                   fill_var = "race_cat", palette = "Set1") {
    ## Plots 95% CI **underneath** other layers. Must have lo_ci and hi_ci
    ## in the data already.
    grob$layers <- c(geom_ribbon(aes_string(ymin = "lo_ci", ymax = "hi_ci", 
                                      fill = fill_var), 
                                 alpha = alpha, color = color), 
                     grob$layers)
    grob <-  grob + 
        scale_fill_brewer(p_title, palette = palette) 
    return(grob)
}


mk_classic <- function(...) {
    ## Just a shortcut for serif fonts and classic theme with legend in upper
    ## left by default. 
    theme_classic() + 
        theme(title = element_text(family = "serif"), 
              legend.key = element_rect(fill = NA, color = NA), 
              legend.position = c(0.01, 1.01), 
              legend.justification = c(0, 1), 
              legend.background = element_rect(fill = alpha("white", .75), 
                                               color = NA))
}


mk_x90 <- function(...) {
    ## Makes x-axis text 90 degrees
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), 
          ...)
}


mk_legend_ur <- function(...) {
    ## Moves legend to upper right
    theme(legend.position = c(0.98, 0.98), 
          legend.justification = c(1, 1))
}


mk_greys <- function(...) {
    ## http://minimaxir.com/2015/02/ggplot-tutorial/
    ## paste0('https://timogrossenbacher.ch/2016/12/', 
    ##        'beautiful-thematic-maps-with-ggplot2-only/')
    ## https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r
    
    ## Colos — stick with the ggplot2() greys
    c_bg    <- "grey95"
    c_grid  <- "grey80"
    c_btext <- "grey5"
    c_mtext <- "grey30"
    
    # Begin construction of chart
    theme_bw(base_size = 12, base_family = "Arial Narrow") +
        
        # Region
        theme(panel.background = element_rect(fill = c_bg, color = c_bg), 
              plot.background  = element_rect(fill = c_bg, color = c_bg), 
              panel.border     = element_rect(color = c_bg)) +
        
        # Grid
        theme(panel.grid.major = element_line(color = c_grid, size = .25), 
              panel.grid.minor = element_blank(), 
              axis.ticks       = element_blank()) +
        
        # Legend
        theme(legend.position = c(0, 1), 
              legend.justification = c(0, 1), 
              legend.direction     = "vertical",
              legend.key           = element_rect(fill = NA, color = NA), 
              legend.background    = element_rect(fill = "transparent", color = NA), 
              legend.text          = element_text(color = c_mtext)) +
        
        # Titles, labels, etc.
        theme(plot.title     = element_text(color = c_btext, vjust = 1.25, 
                                            face = "bold", size = 18), 
              axis.text      = element_text(size = 10, color = c_mtext), 
              axis.title.x   = element_text(size = 12, color = c_mtext,
                                            hjust = 1), 
              axis.title.y   = element_text(size = 12, color = c_mtext, 
                                            hjust = 1)) +
        # Facets
        theme(strip.background = element_rect(fill = c_grid, color = c_btext), 
              strip.text = element_text(size = 10, color = c_btext)) + 
        
        # Plot margins
        theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) + 
        
        # Additionals
        theme(...)
}


mk_nyt <- function(...) {
    ## http://minimaxir.com/2015/02/ggplot-tutorial/
    ## paste0('https://timogrossenbacher.ch/2016/12/', 
    ##        'beautiful-thematic-maps-with-ggplot2-only/')
    ## https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r
    
    ## Colos — stick with the ggplot2() greys
    c_bg    <- "white"
    c_grid  <- "grey80"
    c_btext <- "grey5"
    c_mtext <- "grey30"
    
    # Begin construction of chart
    theme_bw(base_size = 12, base_family = "Arial Narrow") +
        
        # Region
        theme(panel.background = element_rect(fill = c_bg, color = c_bg), 
              plot.background  = element_rect(fill = c_bg, color = c_bg), 
              panel.border     = element_rect(color = c_bg)) +
        
        # Grid
        theme(panel.grid.major = element_line(color = c_grid, size = .25), 
              panel.grid.minor = element_blank(), 
              axis.ticks       = element_blank()) +
        
        # Legend
        theme(legend.position = c(0, 1), 
              legend.justification = c(0, 1), 
              legend.direction     = "vertical",
              legend.key           = element_rect(fill = NA, color = NA), 
              legend.background    = element_rect(fill = "transparent", color = NA), 
              legend.text          = element_text(color = c_mtext)) +
        
        # Titles, labels, etc.
        theme(plot.title     = element_text(color = c_btext, vjust = 1.25, 
                                            face = "bold", size = 18), 
              axis.text      = element_text(size = 10, color = c_mtext), 
              axis.title.x   = element_text(size = 12, color = c_mtext,
                                            hjust = 1), 
              axis.title.y   = element_text(size = 12, color = c_mtext, 
                                            hjust = 1)) +
        # Facets
        theme(strip.background = element_rect(fill = c_grid, color = c_btext), 
              strip.text = element_text(size = 10, color = c_btext)) + 
        
        # Plot margins
        theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) + 
        
        # Additionals
        theme(...)
}



mk_whites <- function(...) {
    ## http://minimaxir.com/2015/02/ggplot-tutorial/
    ## paste0('https://timogrossenbacher.ch/2016/12/', 
    ##        'beautiful-thematic-maps-with-ggplot2-only/')
    ## https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r
    
    ## Colos — stick with the ggplot2() greys
    c_bg    <- "white"
    c_grid  <- "grey50"
    c_btext <- "grey5"
    c_mtext <- "grey30"
    
    # Begin construction of chart
    theme_bw(base_size = 12, base_family = "Arial Narrow") +
        
        # Region
        theme(panel.background = element_rect(fill = c_bg, color = c_bg), 
              plot.background  = element_rect(fill = c_bg, color = c_bg), 
              panel.border     = element_rect(color = NA)) +
        
        # Grid
        theme(panel.grid.major = element_line(color = c_grid, 
                                              size = .25, linetype = "dotted"), 
              panel.grid.minor = element_line(color = c_grid, 
                                              size = .25, linetype = "dotted"), 
              axis.ticks       = element_blank(), 
              axis.line.x      = element_blank(), 
              axis.ticks.x     = element_blank(), 
              axis.ticks.length = unit(0, "cm")) +
        
        # Legend
        theme(legend.position = c(0, 1), 
              legend.justification = c(0, 1), 
              legend.direction     = "vertical",
              legend.key           = element_rect(fill = NA, color = NA), 
              legend.background    = element_rect(fill = "transparent", color = NA), 
              legend.text          = element_text(color = c_mtext)) +
        
        # Titles, labels, etc.
        theme(plot.title     = element_text(color = c_btext, vjust = 1.25, 
                                            face = "bold", size = 18), 
              axis.text      = element_text(size = 10, color = c_mtext), 
              axis.text.x    = element_text(size = 10, color = c_mtext, 
                                            hjust = .5),
              axis.title.x   = element_text(size = 12, color = c_mtext,
                                            hjust = 1), 
              axis.title.y   = element_text(size = 12, color = c_mtext, 
                                            hjust = 1)) +
        # Facets
        theme(strip.background = element_rect(fill = c_bg, color = c_btext), 
              strip.text = element_text(size = 10, color = c_btext)) + 
        
        # Plot margins
        theme(plot.margin = unit(c(0.35, .2, 0.3, 0.35), "cm")) + 
        
        # Additionals
        theme(...)
}


## Postprocessing ----
relativizer <- function(df, group_vars = NULL, 
                        comparison_col = "adj_rate") {
    ## Take a dataframe and turns it into a wide format with relative metrics
    ci_widths <- df %>% 
        select_(.dots = c("race", "year", group_vars, "death_sum")) %>%
        spread_(key_col = "race", value_col = "death_sum") %>% 
        mutate(ci_width = 1.96 * sqrt(1/black + 1/white)) %>% 
        select(year, ci_width)
        
    x <- df %>% 
        group_by_(.dots = group_vars) %>% 
        select_(.dots = c("race", "year", group_vars, comparison_col)) %>%
        spread_(key_col = "race", value_col = comparison_col) %>% 
        mutate(relrisk = white / black, 
               absrisk = white - black) %>% 
        left_join(ci_widths, by = c("year", group_vars)) %>% 
        mutate(lo_ci = exp(log(relrisk) - ci_width), 
               hi_ci = exp(log(relrisk) + ci_width))
    
    return(x)
}


turn_off_clipping <- function(ggplot_grob, draw = FALSE) {
    x <- ggplot_gtable(ggplot_build(ggplot_grob))
    x$layout$clip[x$layout$name == "panel"] <- "off"
    
    if (draw) {
        grid.draw(x)
    }
    
    return(x)
}


## Misc. ----
mkdir_p <- function(dir_name) {
    ## Mimics mkdir -p
    dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
}
