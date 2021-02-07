# Vaikkari
# Git commits
# Essi Viippola, 5.7.2020

# Settings

Sys.setlocale("LC_ALL","English")

# Load packages into R session

library("tidyverse")
library("data.table")
library("lubridate")
library("gridExtra")
library("zoo")

# Read data

git_list = list() # intialize a list to store data
git_files = list.files("data/gitlab", ".json") # list all .json files in git folder
for(git_file in git_files){ git_list[[git_file]] = jsonlite::fromJSON(file.path("data", "gitlab", git_file)) %>% as_tibble() }
git = data.table::rbindlist(git_list) %>% as_tibble()
rm(git_list, git_files)

# Pre-process data
# - remove columns that are not needed (id, created_at, parent_ids, authored_date, author_name, author_email, web_url)
# - correct data types, e.g. dates

names(git) # lists all columns

length(unique(git$short_id)) == length(unique(git$id)) # making sure that we can drop 'id' and use 'short_id' instead

all(git$created_at == git$committed_date) # making sure that we can drop 'created_at' and use 'committed_date' instead

all(git$authored_date == git$committed_date) # FALSE -- 'authored_date' and 'committed_date' are not always the same
git %>% filter(authored_date != committed_date) %>% select(authored_date, committed_date) # there is only one row where author date is earlier than the commit date. We are interested in commit dates anyway, so we can discard 'authored_date'

all(git$author_name == git$committer_name) # FALSE -- 'author_name' and 'committer_name' are not always the same
git %>% filter(author_name != committer_name) %>% select(author_name, committer_name) # these seem to be the same person. We can discard 'author_name'

length(unique(git$committer_name)); length(unique(git$committer_email)) # there's 109 committers by name and 71 committers by email

# there's multiple committer names per email and multiple emails per committer
git %>% group_by(committer_name) %>% summarize(n = n_distinct(committer_email), emails = paste(unique(committer_email), collapse = ", ")) %>% arrange(-n)
git %>% group_by(committer_email) %>% summarize(n = n_distinct(committer_name), names = paste(unique(committer_name), collapse = ", ")) %>% arrange(-n)

# Unique messages
git %>% select(title) %>% group_by(title) %>% summarize(n = n()) %>% arrange(-n)

# Remove columns that are not needed
git = git %>% select(short_id, title, message, committer_name, committer_email, committed_date, author_email)

# Date formats
git$datetime = parse_datetime(git$committed_date) %>% lubridate::with_tz("Europe/Helsinki")
git$date = as.Date(git$datetime)
git$hour = hour(git$datetime)
git$weekday = weekdays(git$datetime)
git$weekday_short = ordered(weekdays(git$datetime, abbreviate = T), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# Remove commits after 2019
git = git %>% filter(year(datetime) == 2019)

# data characteristics
# - 1154 commits (in 2019)
# - ? committers (71 distinct emails and 109 distinct names)
# - 16.9.2019 - 13.12.2019

nrow(git) == length(unique(git$short_id)) # 'short_id' is the row index. Each row represents one commit. 
length(unique(git$short_id)) # 1154 commits
summary(git$datetime)

# are there any typos in emails?

unique(git$author_email) #%>% View()

# Data visualization

# Number of commits by date

commits_by_date = git %>% group_by(date) %>% summarize(n = n())
(plot1 = ggplot(commits_by_date) + 
  geom_bar(aes(x = date, y = n), stat = "identity") +
  scale_x_date(breaks = "1 week", labels = scales::date_format("wk %V")) +
  labs(title = "Commits by date", x = "", y = ""))

# Number of commits by week

commits_by_week = git %>% group_by(week = week(date)) %>% summarize(n = n())
ggplot(commits_by_week, aes(x = week, y = n)) + 
  geom_bar(stat = "identity") +
  labs(title = "Commits by week", x = "", y = "")

# Number of commits by weekday

commits_by_weekday = git %>% group_by(weekday_short) %>% summarize(n = n())
(plot2 = ggplot(commits_by_weekday, aes(x = weekday_short, y = n)) + 
  geom_bar(stat = "identity") +
  labs(title = "Commits by weekday", x = "", y = ""))

# Number of commits by hour

commits_by_hour = git %>% group_by(hour) %>% summarize(n = n()) %>% mutate(hour = ifelse(hour == 0, 24, hour)) %>% arrange(hour)
(plot3 = ggplot(commits_by_hour, aes(x = hour, y = n)) + 
  geom_bar(aes(x = hour, y = n), stat = "identity") + 
  labs(title = "Commits by hour", x = "", y = "") +
  scale_x_continuous(breaks = seq(1, 24, 2)))

# All 3 to the same plot
grid.arrange(plot1, plot2, plot3, nrow = 2, ncol = 3, layout_matrix = rbind(c(1, 1), c(2, 3)))

# Cumulative number of commits by hour
M = sum(commits_by_hour$n)
ggplot(commits_by_hour, aes(x = hour, y = cumsum(n)/M*100)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = 0:23) +
  labs(x = "Hour", y = "Percentage of commits")
