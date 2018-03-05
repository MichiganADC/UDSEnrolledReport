#!/usr/bin/env RScript

## Script to generate UDS Enrollment plots by Total, Sex, and Race

## To use from *Nix terminal, run this script
## ./UDS_Enrolled_plots.R ./input_csv/[UMMAPMindsetRegistryFile].csv

# Get command line arguments
# args = commandArgs(trailingOnly=TRUE)

# Test if there is at least one argument: if not, return an error
# if (length(args) == 0) {
#   stop("At least one argument must be supplied: [UMMAPMindsetRegistry].csv", 
#        call. = FALSE)
# } 
# Maybe use this later if the user wants to name the output csv file
# else if (length(args) == 1) {
# # default output file
# args[2] = "out.txt"
#}
# # Choose the csv file from the UMMAP Mindset Registry RC report
# # ms_reg_file <- args[1]
# ms_reg_file <- 
#   file.path("input_csv", 
#             "UMMAPMindsetRegistry_DATA_LABELS_2018-03-05_1048.csv")
# ms_reg <- read_csv(file = ms_reg_file, trim_ws = TRUE)

source("config.R") # contains API URL and API token
library(RCurl)
library(jsonlite)
if (!exists("report_df")) {
  # Project report
  report_json <- postForm(
    uri = API_URL,
    token = API_TOKEN,
    content = 'report',
    format = 'json',
    report_id = REPORT_ID,
    rawOrLabel = 'label',
    rawOrLabelHeaders = 'label',
    exportCheckboxLabel = 'false',
    returnFormat = 'json',
    .opts = list(ssl.verifypeer = TRUE, verbose = TRUE)
  )
  report_df <- fromJSON(report_json)
  # print(report_df) # 'report should be the same as 'ms_reg' after the read_csv below
}
ms_reg <- report_df

library(tidyverse)

# names(ms_reg) <- 
#   gsub(pattern = "[ [:punct:]]", replacement = "_", names(ms_reg))
names(ms_reg)

# Coerce 'exam_date' column to Date
ms_reg$exam_date <- as.Date(ms_reg$exam_date, format = "%Y-%m-%d")
# Coerce 'Race' column to factor
ms_reg$race_value <- factor(ms_reg$race_value, levels = c("Black", "White", "Other"))
# Coerce 'Sex' column to factor
ms_reg$sex_value <- factor(ms_reg$sex_value, levels = c("Female", "Male"))
# Clean up 'uds_dx' column (few factors); Coerce 'uds_dx' column to factor
ms_reg <- ms_reg %>% 
  mutate(uds_dx = case_when(
    uds_dx == "Amnestic MCI-memory only" ~ "MCI",
    uds_dx == "Amnestic MCI-memory plus" ~ "MCI",
    uds_dx == "Amnestic MCI, multiple domains" ~ "MCI",
    uds_dx == "Amnestic MCI, single domain" ~ "MCI",
    uds_dx == 
      "Amnestic multidomain dementia syndrome" ~ "Amnestic multidom dem",
    uds_dx == "Dem with Lewy bodies" ~ "LBD",
    # uds_dx == "FTD" ~ "FTD",
    # uds_dx == "Impaired, not MCI" ~ "Impaired, not MCI",
    # uds_dx == "NL" ~ "NL",
    uds_dx == "Non-Amnestic MCI-multiple domains" ~ "MCI",
    uds_dx == "Non-Amnestic MCI-single domain" ~ "MCI",
    uds_dx == "Primary progressive aphasia" ~ "FTD",
    uds_dx == "Probable AD" ~ "AD",
    is.na(uds_dx) & comp_withd == "Y" ~ "Withdrew",
    is.na(uds_dx) & is.na(comp_withd) ~ "Pending consensus dx",
    TRUE ~ uds_dx
  ))
ms_reg$uds_dx <- 
  factor(ms_reg$uds_dx, 
         levels = c("NL", "Impaired, not MCI", "MCI", "AD", 
                    "Amnestic multidom dem", "LBD", "FTD", 
                    "Pending consensus dx", "Withdrew"))
# Coerce 'Deceased_' column to logical
ms_reg$pt_deceased <- as.logical(ms_reg$pt_deceased)

# Check classes of each column in ms_reg
sapply(ms_reg, class) # Looks good

# Sort ms_reg by 'exam_date'
ms_reg <- ms_reg %>%
  arrange(exam_date)
min_date <- as.Date("2017-03-01", format = "%Y-%m-%d")
max_date <- max(ms_reg$exam_date)

# Plot cumulative participants
ggplot(ms_reg, aes(x = exam_date, y = as.numeric(rownames(ms_reg)))) +
  geom_line() +
  geom_vline(xintercept = Sys.Date(), color = "darkgrey", linetype = "longdash") +
  scale_x_date(name = "Visit Date",
               date_labels = "%b %y",
               date_breaks = "1 month",
               date_minor_breaks = "1 month",
               limits = as.Date(c("2017-03-01", Sys.Date()))) +
  scale_y_continuous(name = "Cumulative Participants",
                     breaks = seq(0, nrow(ms_reg) + 10, by = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(label = "Total Participants Over Time")
ggsave(filename = "plots/UDS_Enrolled_plot-Total_participants.png", width = 6, height = 4)


## Count columns by group (Sex, Race, Diagnosis, ...)
ms_reg <- bind_cols(ms_reg, data.frame(units = rep(1, nrow(ms_reg))))
# Sex
ms_reg <- ms_reg %>%
  dplyr::group_by(sex_value) %>% 
  dplyr::mutate(SexCumSum = cumsum(units))
# Race
ms_reg <- ms_reg %>%
  dplyr::group_by(race_value) %>% 
  dplyr::mutate(RaceCumSum = cumsum(units))
# Diagnosis (uds_dx)
ms_reg <- ms_reg %>% 
  dplyr::group_by(uds_dx) %>% 
  dplyr::mutate(DxCumSum = cumsum(units))

# Plot cumulative participants by Sex
ggplot(ms_reg, aes(x = exam_date, y = SexCumSum, group = sex_value, color = sex_value)) + 
  geom_line() +
  geom_vline(xintercept = Sys.Date(), color = "darkgrey", linetype = "longdash") +
  scale_x_date(name = "Visit Date",
               date_labels = "%b %y",
               date_breaks = "1 month",
               date_minor_breaks = "1 month",
               limits = as.Date(c("2017-03-01", Sys.Date()))) +
  scale_y_continuous(name = "Cumulative Participants",
                     breaks = seq(0, nrow(ms_reg) + 10, by = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(label = "Participants Over Time by Sex")
ggsave(filename = "plots/UDS_Enrolled_plot-Participants_by_sex.png", width = 6, height = 4)
# Plot cumulative participants by Race
ggplot(ms_reg, aes(x = exam_date, y = RaceCumSum, group = race_value, color = race_value)) + 
  geom_line() +
  geom_vline(xintercept = Sys.Date(), color = "darkgrey", linetype = "longdash") +
  scale_x_date(name = "Visit Date",
               date_labels = "%b %y",
               date_breaks = "1 month",
               date_minor_breaks = "1 month",
               limits = as.Date(c("2017-03-01", Sys.Date()))) +
  scale_y_continuous(name = "Cumulative Participants",
                     breaks = seq(0, nrow(ms_reg) + 10, by = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(label = "Participants Over Time by Race")
ggsave(filename = "plots/UDS_Enrolled_plot-Participants_by_race.png", width = 6, height = 4)
# Plot cumulative participants by Diagnosis (uds_dx)
ggplot(ms_reg, aes(x = exam_date, y = DxCumSum, group = uds_dx, color = uds_dx)) +
  geom_line() +
  geom_vline(xintercept = Sys.Date(), color = "darkgrey", linetype = "longdash") +
  scale_x_date(name = "Visit Date",
               date_labels = "%b %y",
               date_breaks = "1 month",
               date_minor_breaks = "1 month",
               limits = as.Date(c("2017-03-01", Sys.Date()))) +
  scale_y_continuous(name = "Cumulative Participants",
                     breaks = seq(0, nrow(ms_reg) + 10, by = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(label = "Participants Over Time by Diagnosis")
ggsave(filename = "plots/UDS_Enrolled_plot-Participants_by_diagnosis.png", width = 6, height = 4)


## Diagnosis counts vs. target diagnosis counts
# Target diagnoses plot function
plot_target_dx <- function(ms_df, diagnosis, diagnosis_target, yearly_targets) {
  target_df <- data.frame(matrix(rep(NA, ncol(ms_df) * 6), nrow = 6, byrow = TRUE))
  names(target_df) <- names(ms_df)
  target_df$Subject_Id <- paste0("UM0000XXX", 0:5)
  target_df$exam_date <- as.Date(paste0(2017:2022, "-03-01"))
  target_df$uds_dx <- rep(diagnosis_target, 6)
  target_df$DxCumSum <- yearly_targets
  ms_df %>%
    dplyr::filter(uds_dx == diagnosis) %>%
    dplyr::bind_rows(target_df) %>%
    ggplot(aes(x = exam_date, y = DxCumSum, group = uds_dx, color = uds_dx, linetype = uds_dx)) +
    geom_line() +
    geom_vline(xintercept = Sys.Date(), color = "darkgrey", linetype = "longdash") +
    scale_x_date(name = "Visit Date",
                 date_labels = "%b %y",
                 date_breaks = "6 months",
                 date_minor_breaks = "1 month",
                 limits = as.Date(c("2017-01-01", "2022-03-01"))) +
    scale_y_continuous(name = "Cumulative Participants",
                       breaks = seq(0, nrow(ms_reg) + 10, by = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(label = paste0("Participants Over Time - ", diagnosis, " vs. ", diagnosis_target))
}
# NL targets (normal)
plot_target_dx(ms_reg, "NL", "NL target", c(0, 125, 125, 125, 125, 125))
ggsave(filename = "plots/UDS_Enrolled_plot-NL_targets.png", width = 6, height = 4)
# MCI targets
plot_target_dx(ms_reg, "MCI", "MCI target", c(0, 50, 100, 100, 100, 100))
ggsave(filename = "plots/UDS_Enrolled_plot-MCI_targets.png", width = 6, height = 4)
# AD targets
plot_target_dx(ms_reg, "AD", "AD target", c(0, 18, 23, 36, 47, 58))
ggsave(filename = "plots/UDS_Enrolled_plot-AD_targets.png", width = 6, height = 4)
# LBD targets
plot_target_dx(ms_reg, "LBD", "LBD target", c(0, 10, 19, 38, 40, 37))
ggsave(filename = "plots/UDS_Enrolled_plot-LBD_targets.png", width = 6, height = 4)
# FTD targets
plot_target_dx(ms_reg, "FTD", "FTD target", c(0, 5, 19, 22, 35, 36))
ggsave(filename = "plots/UDS_Enrolled_plot-FTD_targets.png", width = 6, height = 4)










#####################
#####################
#####################
#### EXTRA SPACE ####
#####################
#####################
#####################
