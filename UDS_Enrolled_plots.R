#!/usr/bin/env RScript

## Script to generate UDS Enrollment plots by Total, Sex, and Race

## To use from *Nix terminal, run this script
## ./UDS_Enrolled_plots.R ./input_csv/[UMMAPMindsetRegistryFile].csv

# Get command line arguments
# args = commandArgs(trailingOnly=TRUE)

# Test if there is at least one argument: if not, return an error
# if (length(args) == 0) {
#   stop("At least one argument must be supplied: [UMMAPMindsetRegistryFile].csv", call. = FALSE)
# } 
# Maybe use this later if the user wants to name the output csv file
# else if (length(args) == 1) {
# # default output file
# args[2] = "out.txt"
#}

library(tidyverse)
library(plyr)

# Choose the csv file from the UMMAP Mindset Registry RC report
# ms_reg_file <- args[1]
ms_reg_file <- 
  file.path("input_csv", "UMMAPMindsetRegistry_DATA_LABELS_2018-02-27_1118.csv")
ms_reg <- read_csv(file = ms_reg_file, trim_ws = TRUE)

names(ms_reg) <- 
  gsub(pattern = "[ [:punct:]]", replacement = "_", names(ms_reg))
names(ms_reg)

# Coerce 'Exam_Date' column to Date
ms_reg$Exam_Date <- as.Date(ms_reg$Exam_Date, format = "%m/%d/%y")
# Coerce 'Race' column to factor
ms_reg$Race <- factor(ms_reg$Race, levels = c("Black", "White", "Other"))
# Coerce 'Sex' column to factor
ms_reg$Sex <- factor(ms_reg$Sex, levels = c("Female", "Male"))
# Clean up 'UDS_dx' column (few factors); Coerce 'UDS_dx' column to factor
ms_reg <- ms_reg %>% 
  mutate(UDS_dx = case_when(
    UDS_dx == "Amnestic MCI-memory only" ~ "MCI",
    UDS_dx == "Amnestic MCI-memory plus" ~ "MCI",
    UDS_dx == "Amnestic MCI, multiple domains" ~ "MCI",
    UDS_dx == "Amnestic MCI, single domain" ~ "MCI",
    UDS_dx == "Amnestic multidomain dementia syndrome" ~ "Amnestic multidom dem",
    UDS_dx == "Dem with Lewy bodies" ~ "LBD",
    # UDS_dx == "FTD" ~ "FTD",
    # UDS_dx == "Impaired, not MCI" ~ "Impaired, not MCI",
    # UDS_dx == "NL" ~ "NL",
    UDS_dx == "Non-Amnestic MCI-multiple domains" ~ "MCI",
    UDS_dx == "Non-Amnestic MCI-single domain" ~ "MCI",
    UDS_dx == "Primary progressive aphasia" ~ "FTD",
    # UDS_dx == "Probable AD" ~ "Probable AD",
    is.na(UDS_dx) & Completed_Withdrew_ == "Y" ~ "Withdrew",
    is.na(UDS_dx) & is.na(Completed_Withdrew_) ~ "Pending consensus dx",
    TRUE ~ UDS_dx
  ))
ms_reg$UDS_dx <- factor(ms_reg$UDS_dx, 
                        levels = c("NL", "Impaired, not MCI", "MCI", "Probable AD", "Amnestic multidom dem",
                                   "LBD", "FTD", "Pending consensus dx", "Withdrew"))
# Coerce 'Deceased_' column to logical
ms_reg$Deceased_ <- as.logical(ms_reg$Deceased_)

# Check classes of each column in ms_reg
sapply(ms_reg, class) # Looks good

# Sort ms_reg by 'Exam_Date'
ms_reg <- ms_reg %>%
  arrange(Exam_Date)
min_date <- as.Date("2017-03-01", format = "%Y-%m-%d")
max_date <- max(ms_reg$Exam_Date)

# Plot cumulative participants by 'Exam_Date' -- All participants
ggplot(ms_reg, aes(x = Exam_Date)) +
  geom_line(aes(len = nrow(ms_reg), y = len * ..y..), stat = "ecdf") +
  scale_x_date(name = "Visit Date", 
               date_labels = "%b %y", 
               date_breaks = "1 month",
               limits = c(min_date, max_date)) +
  scale_y_continuous(name = "Cumulative Participants", 
                     breaks = seq(0, nrow(ms_reg) + 10, by = 10)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(label = "Total Participants Over Time")
ggsave("plots/UDS_Enrolled_plot-Total_participants.png", width = 6, height = 4)

# Plot by Sex
sex_ct <- ms_reg %>%
  group_by(Sex) %>% 
  summarize(n = n())
ms_reg <- ms_reg %>%
  mutate(Sex_Count = ifelse(Sex == "Female", sex_ct$n[1],
                            ifelse(Sex == "Male", sex_ct$n[2], NA)))
ggplot(ms_reg, aes(x = Exam_Date, color = Sex)) +
  geom_line(aes(len = Sex_Count, y = ..y.. * len), stat = "ecdf") + 
  scale_x_date(name = "Visit Date", 
               date_labels = "%b %y", 
               date_breaks = "1 month",
               limits = c(min_date, max_date)) +
  scale_y_continuous(name = "Cumulative Participants", 
                     breaks = seq(0, nrow(ms_reg) + 10, by = 10)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(label = "Participants Over Time by Sex")
ggsave("plots/UDS_Enrolled_plot-Participants_by_sex.png", width = 6, height = 4)

# Plot by Race
race_ct <- ms_reg %>% 
  group_by(Race) %>% 
  summarize(n = n())
ms_reg <- ms_reg %>%
  mutate(Race_Count = ifelse(Race == "Black", filter(race_ct, Race == "Black")$n,
                      ifelse(Race == "Other", filter(race_ct, Race == "Other")$n,
                      ifelse(Race == "White", filter(race_ct, Race == "White")$n, NA)))) %>% 
  filter(!is.na(Race))
ggplot(ms_reg, aes(x = Exam_Date, color = Race)) +
  geom_line(aes(len = Race_Count, y = ..y.. * len), stat = "ecdf") + 
  scale_x_date(name = "Visit Date", 
               date_labels = "%b %y", 
               date_breaks = "1 month",
               limits = c(min_date, max_date)) +
  scale_y_continuous(name = "Cumulative Participants", 
                     breaks = seq(0, nrow(ms_reg) + 10, by = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(label = "Participants Over Time by Race")
ggsave("plots/UDS_Enrolled_plot-Participants_by_race.png", width = 6, height = 4)

# Plot by Diagnosis
dx_ct <- ms_reg %>%
  group_by(UDS_dx) %>% 
  summarize(n = n())
ms_reg <- ms_reg %>% 
  mutate(Dx_Count = case_when(
    UDS_dx == "Amnestic multidom dem" ~ 
      filter(dx_ct, UDS_dx == "Amnestic multidom dem")$n,
    UDS_dx == "FTD" ~ filter(dx_ct, UDS_dx == "FTD")$n,
    UDS_dx == "Impaired, not MCI" ~ filter(dx_ct, UDS_dx == "Impaired, not MCI")$n,
    UDS_dx == "LBD" ~ filter(dx_ct, UDS_dx == "LBD")$n,
    UDS_dx == "MCI" ~ filter(dx_ct, UDS_dx == "MCI")$n,
    UDS_dx == "NL" ~ filter(dx_ct, UDS_dx == "NL")$n,
    UDS_dx == "Pending consensus dx" ~ filter(dx_ct, UDS_dx == "Pending consensus dx")$n,
    UDS_dx == "Probable AD" ~ filter(dx_ct, UDS_dx == "Probable AD")$n,
    UDS_dx == "Withdrew" ~ filter(dx_ct, UDS_dx == "Withdrew")$n
  ))
ggplot(ms_reg, aes(x = Exam_Date, color = UDS_dx)) +
  geom_line(aes(len = Dx_Count, y = ..y.. * len), stat = "ecdf", size = 1) +
  scale_x_date(name = "Visit Date",
               date_labels = "%b %y",
               date_breaks = "1 month",
               limits = c(min_date, max_date)) +
  scale_y_continuous(name = "Cumulative Participants",
                     breaks = seq(0, nrow(ms_reg) + 10, by = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(label = "Participants Over Time by Diagnosis")
ggsave("plots/UDS_Enrolled_plot-Participants_by_diagnosis.png", width = 6, height = 4)

# NL diagnosis targets
target_min_date <- as.Date("2017-03-01", format = "%Y-%m-%d")
target_max_date <- as.Date("2022-03-01", format = "%Y-%m-%d")

# MCI diagnosis targets

# AD diagnosis targets

# LBD diagnosis targets

# FTD diagnosis targets

# There has to be a better/scalable way to create these 
# Cumulative Frequency Plots ...
# maybe https://stackoverflow.com/questions/18379933/plotting-cumulative-counts-in-ggplot2
# Empirical Cumulative Distribution Function
df <- data.frame(
  x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
  g = gl(2, 100),
  u = rep(1, 200),
  n = seq(1, 200)
)
cumsum(df$u)
ggplot(df, aes(x = n, y = cumsum(u), color = g)) + geom_line()









#####################
#####################
#####################
#### EXTRA SPACE ####
#####################
#####################
#####################
