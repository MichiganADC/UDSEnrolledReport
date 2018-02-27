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

# Choose the csv file from the UMMAP Mindset Registry RC report
# ms_reg_file <- args[1]
ms_reg_file <- 
  file.path("input_csv", "UMMAPMindsetRegistry_DATA_LABELS_2018-02-23_1042.csv")
ms_reg <- readr::read_csv(file = ms_reg_file, trim_ws = TRUE)

names(ms_reg) <- 
  gsub(pattern = "[ [:punct:]]", replacement = "_", names(ms_reg))
names(ms_reg)

# Coerce 'Exam_Date' column to Date
ms_reg$Exam_Date <- as.Date(ms_reg$Exam_Date, format = "%m/%d/%y")
# Coerce 'Race' column to factor
ms_reg$Race <- factor(ms_reg$Race, levels = c("Black", "White", "Other"))
# Coerce 'Sex' column to factor
ms_reg$Sex <- factor(ms_reg$Sex, levels = c("Female", "Male"))
# Coerce 'UDS_dx' column to factor
ms_reg$UDS_dx <- factor(ms_reg$UDS_dx)

# Check classes of each column in ms_reg
sapply(ms_reg, class) # Looks good

# Sort ms_reg by 'Exam_Date'
ms_reg <- ms_reg %>%
  arrange(Exam_Date)
min_date <- min(ms_reg$Exam_Date)
max_date <- max(ms_reg$Exam_Date)

# Plot cumulative participants by 'Exam_Date' -- All participants
ggplot(ms_reg, aes(x = Exam_Date)) +
  geom_line(aes(len = nrow(ms_reg), y = len * ..y..), stat = "ecdf") +
  scale_x_date(name = "Visit Date", 
               date_labels = "%b %y", 
               date_breaks = "1 month") +
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
               date_breaks = "1 month") +
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
               date_breaks = "1 month") +
  scale_y_continuous(name = "Cumulative Participants", 
                     breaks = seq(0, nrow(ms_reg) + 10, by = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(label = "Participants Over Time by Race")
ggsave("plots/UDS_Enrolled_plot-Participants_by_race.png", width = 6, height = 4)

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
