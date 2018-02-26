## Script to generate UDS Enrollment Table
## ... Eventually this should be run through the REDCap API 
## ... with a de-identified data report
## ... and then we can eventually push it to a web server
## ... so that it's easily accessible from any device

library(tidyverse)

# Choose the csv file from the UMMAP Mindset Registry RC report
ms_reg_file <- file.choose()
ms_reg <- readr::read_csv(file = ms_reg_file, trim_ws = TRUE)

names(ms_reg) <- 
  gsub(pattern = "[ [:punct:]]", replacement = "_", names(ms_reg))
names(ms_reg)

# Clean out unneeded columns
ms_reg <- ms_reg %>% 
  select(-Event_Name, -Deceased_, -Exam_Date) 

### TESTING ###
# single_grp_table <- function(x, group_var) {
#   distinct_grp_vals <- distinct(x, !!group_var)
#   x %>% 
#     group_by(!!group_var) %>% 
#     summarize(Count = n()) %>%
#     right_join(distinct_grp_vals) %>% 
#     arrange(!!group_var)
# }
# single_grp_table(ms_reg, group_var = quo(UDS_dx))
# distinct(ms_reg, UDS_dx)
# tibble(UDS_dx = distinct(ms_reg, UDS_dx))
### TESTING ###

# Fxn for outputting 
single_grp_table <- function(x, group_var) {
  # uds_dx_df <- data_frame(UDS_dx = unique(x$UDS_dx))
  distinct_grp_vals <- distinct(x, !!group_var)
  x %>% 
    group_by(!!group_var) %>% 
    summarize(Count = n()) %>%
    # right_join(uds_dx_df, by = "UDS_dx") %>%
    right_join(distinct_grp_vals) %>%
    arrange(!!group_var)
}
# Fxn for outputting tables with one group variable and one filter variable
single_grp_filter_table <- function(x, group_var, filter_var, filter_var_string) {
  # uds_dx_df <- data_frame(UDS_dx = unique(x$UDS_dx))
  distinct_grp_vals <- distinct(x, !!group_var)
  x %>% 
    group_by(!!group_var) %>% 
    filter(!!filter_var == filter_var_string) %>% 
    summarize(Count = n()) %>% 
    # right_join(uds_dx_df, by = "UDS_dx") %>% 
    right_join(distinct_grp_vals) %>% 
    arrange(!!group_var)
}
# Fxn for outputting tables with two group variables
double_grp_table <- function(x, group_var_1, group_var_2) {
  # uds_dx_df <- data_frame(UDS_dx = unique(x$UDS_dx))
  distinct_grp_vals <- distinct(x, !!group_var_1)
  x %>% 
    group_by(!!group_var_1, !!group_var_2) %>% 
    summarize(Count = n()) %>% 
    spread(!!group_var_2, Count) %>% 
    # right_join(uds_dx_df, by = "UDS_dx") %>%
    right_join(distinct_grp_vals) %>%
    arrange(!!group_var_1)
}
# Fxn for outputting tables with three group variables
triple_grp_table <- function(x, group_var_1, group_var_2, group_var_3) {
  # uds_dx_df <- data_frame(UDS_dx = unique(x$UDS_dx))
  distinct_grp_vals <- distinct(x, !!group_var_1)
  x %>% 
    group_by(!!group_var_1, !!group_var_2, !!group_var_3) %>% 
    summarize(Count = n()) %>% 
    unite(col = United, !!group_var_2, !!group_var_3, sep = "_") %>% 
    spread(United, Count) %>% 
    # right_join(uds_dx_df, by = "UDS_dx") %>% 
    right_join(distinct_grp_vals) %>%
    arrange(!!group_var_1)
}


# Total counts
total_cts <- 
  single_grp_table(ms_reg, 
                   group_var = quo(UDS_dx))
# Sex counts
sex_cts <- 
  double_grp_table(ms_reg, 
                   group_var_1 = quo(UDS_dx), 
                   group_var_2 = quo(Sex))
# Race counts
race_cts <- 
  double_grp_table(ms_reg, 
                   group_var_1 = quo(UDS_dx), 
                   group_var_2 = quo(Race))
# Sex + Race counts
sex_race_cts <- 
  triple_grp_table(ms_reg, 
                   group_var_1 = quo(UDS_dx), 
                   group_var_2 = quo(Sex), 
                   group_var_3 = quo(Race))
# Autopsy Consent counts
autopsy_yes_cts <-
  single_grp_filter_table(ms_reg, 
                          group_var = quo(UDS_dx), 
                          filter_var = quo(Did_patient_consent_to_autopsy), 
                          filter_var_string = "Yes")
# Autopsy Consent counts 
autopsy_consid_cts <- 
  single_grp_filter_table(ms_reg,
                          group_var = quo(UDS_dx),
                          filter_var = quo(Did_patient_consent_to_autopsy),
                          filter_var_string = "Considering")
# MRI Yes counts
mri_yes_cts <- 
  single_grp_filter_table(ms_reg,
                          group_var = quo(UDS_dx),
                          filter_var = quo(MRI_Completed_),
                          filter_var_string = "1. Yes")
# Blood Drawn Yes counts
blood_yes_cts <- 
  single_grp_filter_table(ms_reg,
                          group_var = quo(UDS_dx),
                          filter_var = quo(Blood_Drawn_),
                          filter_var_string = "1. Yes")
# Stitch all *_cts dfs together
big_tbl <- 
  bind_cols(total_cts, sex_cts[, -1], race_cts[, -1], sex_race_cts[, -1], 
            autopsy_yes_cts[, -1], autopsy_consid_cts[, -1],
            mri_yes_cts[, -1], blood_yes_cts[, -1])

# Replace "NA" diagnosis row label with "X_Diagnosis_Blank_X"
# big_tbl[!(grepl(pattern = ".*", x = big_tbl$UDS_dx)), "UDS_dx"]
big_tbl[!(grepl(pattern = ".*", x = big_tbl$UDS_dx)), "UDS_dx"] <- "X_Diagnosis_Blank_X"


# Build totals row
totals <- vapply(X = big_tbl[, 2:ncol(big_tbl)], 
                 FUN = sum, na.rm = TRUE, FUN.VALUE = numeric(1))
total_row <- as_data_frame(matrix(c("Totals", totals), nrow = 1, byrow = TRUE))
names(total_row) <- names(big_tbl)
# total_row
# Attach totals row
big_tbl <- rbind(big_tbl, total_row)
# Make integer columns integers
big_tbl[2:ncol(big_tbl)] <- lapply(X = big_tbl[2:ncol(big_tbl)], 
                                   FUN = as.integer)
# big_tbl

# Build proportions row
# big_tbl$UDS_dx == "Totals"
pt_sum <- as.integer(big_tbl[big_tbl$UDS_dx == "Totals", "Count"])
get_proportion <- function(x) {
  round(sum(x, na.rm = TRUE) / pt_sum, 2)
}
proportions <- vapply(X = big_tbl[1:(nrow(big_tbl)-1), 2:ncol(big_tbl)], 
                      FUN = get_proportion, FUN.VALUE = numeric(1))
# proportions
proportion_row <- as_data_frame(matrix(c("Proportions", proportions), nrow = 1, byrow = TRUE))
# proportion_row
names(proportion_row) <- names(big_tbl)
# proportions_row

big_tbl <- rbind(big_tbl, proportion_row)
# big_tbl

names(big_tbl) <- c("UDS dx", "Total Count",
                    "Sex\nFemale", "Sex\nMale", 
                    "Race\nBlack", "Race\nOther", "Race\nWhite", "Race\nNA", 
                    "Female Black", "Female NA", "Female Other", "Female White",
                    "Male Black", "Male White", 
                    "Autopsy Yes", "Autopsy Considering", "MRI Yes", "Blood Drawn Yes")

# Write to a csv
date_time <- substr(Sys.time(), 0, 16) %>% 
  gsub(pattern = " ", replacement = "_", .) %>% 
  gsub(pattern = ":", replacement = "-", .)
export_csv <- file.path("output_csv", paste0("UDS_Enrolled_Table_", date_time, ".csv"))
write_csv(big_tbl, path = export_csv, na = "")













#####################
#### EXTRA SPACE ####
#####################