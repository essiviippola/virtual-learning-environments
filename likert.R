# Set working directory

setwd("C:/Users/essiv/Dokumentit/Vaikkari")

# Load packages into R session

library("tidyverse")
library("HH")
library("ggplot2")

# Read data

likert_wide = read.csv("data/likert.csv", sep = ";", header = T, col.names = paste0("Q", 1:34), na.strings = c("NA", 6))

# Data pre-processing

# From wide to long data

likert = gather(likert_wide) %>% as_tibble()
names(likert) = c("question", "answer")
likert$student_id = rep(1:56, 34)

# Groups
# Major groups:  1. Teaching (Q1-Q13), 2. Social (Q14-Q22), 3. Congnitive (Q23-Q34)
# Minor groups: 
# - 1-4: design & organization
# - 5-10: facilitation
# - 11-13: direct instructions
# - 14-16: affective expression
# - 17-19: open communication
# - 20-22: group cohesion
# - 23-25: triggering event
# - 26-28: exploration
# - 29-31: integration
# - 32-34: resolution

likert$major = NA
likert$major[likert$question %in% paste0("Q", 1:13)] = "Teaching Presence"
likert$major[likert$question %in% paste0("Q", 14:22)] = "Social Presence"
likert$major[likert$question %in% paste0("Q", 23:34)] = "Cognitive Presence"

likert$major_f = factor(likert$major, levels = c("Teaching Presence", "Social Presence", "Cognitive Presence"), ordered = T)

likert$minor = NA # initialize minor
likert$minor[likert$question %in% paste0("Q", 1:4)] = "Design & oranization"
likert$minor[likert$question %in% paste0("Q", 5:10)] = "Facilitation"
likert$minor[likert$question %in% paste0("Q", 11:13)] = "Direct instructions"
likert$minor[likert$question %in% paste0("Q", 14:16)] = "Affective expression"
likert$minor[likert$question %in% paste0("Q", 17:19)] = "Open communication"
likert$minor[likert$question %in% paste0("Q", 20:22)] = "Group cohesion"
likert$minor[likert$question %in% paste0("Q", 23:25)] = "Triggering events"
likert$minor[likert$question %in% paste0("Q", 26:28)] = "Exploration"
likert$minor[likert$question %in% paste0("Q", 29:31)] = "Integration"
likert$minor[likert$question %in% paste0("Q", 32:34)] = "Resolution"

likert$question[likert$question %in% paste0("Q", 1:13)] = gsub("Q", "TP", likert$question[likert$question %in% paste0("Q", 1:13)])
likert$question[likert$question %in% paste0("Q", 14:22)] = gsub("Q", "SP", likert$question[likert$question %in% paste0("Q", 14:22)])
likert$question[likert$question %in% paste0("Q", 23:34)] = gsub("Q", "CP", likert$question[likert$question %in% paste0("Q", 23:34)])

likert$question_number = parse_number(likert$question)

likert$question_f = factor(likert$question, levels = unique(likert$question[order(likert$question_number)]), ordered = T)

# Analysis & data visualization

# Question median and IQR by major and minor categories

likert %>%
  group_by(major, minor) %>%
  summarize(
    median = median(answer, na.rm = T),
    IQR = IQR(answer, na.rm = T),
    na_count = sum(is.na(answer))) 

# Exploration has the largest IQR; is the variability subject to a specific question?

likert %>%
  group_by(major, minor, question) %>%
  filter(minor == "Exploration") %>% 
  summarize(
    median = median(answer, na.rm = T),
    IQR = IQR(answer, na.rm = T),
    na_count = sum(is.na(answer)))

# Student-specific IQR by major category

IQR_by_major = likert %>%
  group_by(student_id, major) %>%
  summarize(
    median = median(answer, na.rm = T),
    IQR = IQR(answer, na.rm = T),
    na_count = sum(is.na(answer)))

ggplot(IQR_by_major, aes(x = as.factor(IQR))) + 
  geom_bar() + 
  facet_grid(major~.) + 
  labs(x = "IQR", y = "Count", title = "Student-specific IQR by major category")

# Student's median and IQR 

likert %>%
  group_by(minor) %>%
  summarize(
    median = median(answer, na.rm = T),
    IQR = IQR(answer, na.rm = T),
    na_count = sum(is.na(answer)))

# - student 23: 10 out of 22 missing values were from student 23 - especially in 'Social' category

# Likert graph

temp = likert %>% 
  filter(!is.na(answer)) %>% 
  group_by(major, minor, question, answer, question_number) %>% 
  summarize(n = n()) %>%
  spread(answer, n) %>%
  rename("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5") %>%
  arrange(question_number)
temp[is.na(temp)] = 0

HH::plot.likert(question ~  "Strongly Disagree" + Disagree + "Neutral" + Agree + "Strongly Agree"  | major, 
                data = temp, scales = list(y = list(relation = "free")),
                ylab = NULL, ReferenceZero = 3, rightAxis = F, layout = c(3, 1),
                as.percent = TRUE, main = NULL, col = gray.colors(n = 5))


# Boxplot

temp = likert %>% filter(!is.na(answer))

ggplot(temp, aes(x = question_f, y = answer)) + 
  geom_boxplot() +
  facet_grid(. ~ major_f, scales = "free_x") +
  labs(x  = "", y = "Answer (Likert scale)")

# Barplot - answer by major

ggplot(temp, aes(x = answer)) + geom_bar() + facet_grid(. ~ major_f, scales = "free_x")

# Barplot - answer by question

ggplot(temp, aes(x = answer)) + geom_bar() + facet_wrap(~ question)

# Barplot - answer by student

ggplot(temp, aes(x = answer)) + geom_bar() + facet_wrap(~ student_id)

