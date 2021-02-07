# Data pre-processing

# Setup

Sys.setlocale("LC_CTYPE", "finnish")

# Load packages into R session

library("tidyverse")

# Read data

dat = read.csv("../data/webpropol-2020.csv", header = T, sep = ";", encoding = "UTF-8", stringsAsFactors = F) %>% as_tibble()

# Reformat names

names(dat)[1] = "study_group"
names(dat)[2:35] = paste0("q", gsub("\\D", "", names(dat)[2:35]))
names(dat)[36] = "preferred_method"
names(dat)[37] = "challenges"
names(dat)[38] = "positive"
names(dat)[39] = "online"
names(dat)[40] = "improvement"

# Add study subject IDs

dat$student_id = 1:nrow(dat)

# Transform data from wide to long format

likert = gather(dat, key = "question", value = "answer", q1:q34) %>% select(student_id, study_group, question, answer)

likert$question_number = gsub("\\D", "", likert$question)
likert$question_group = NA
likert$question_group[likert$question_number %in% 1:13] = "Ohjaus ja tuki"
likert$question_group[likert$question_number %in% 14:22] = "Sosiaalinen vuorovaikutus"
likert$question_group[likert$question_number %in% 23:34] = "Osaamisen kehittyminen"

# Change answer 6 to NA

likert$answer[likert$answer == 6] = NA

# Extract non-likert answers to a dataframe

nonlikert = dat %>% select(41, 1, 36:40)

