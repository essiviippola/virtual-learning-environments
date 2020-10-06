# Vaikkari
# Mattermost data
# Essi Viippola, 12.7.2020

# Settings

Sys.setlocale("LC_ALL","English")

# Load packages into R session

library("tidyverse")
library("data.table")
library("gridExtra")

# Read data

flat = jsonlite::fromJSON(file.path("data/matter", "matter.json"), flatten = T)

matter_list = lapply(flat, function(x){ 
  rem = intersect(names(x), c("metadata", "props", "file_ids", "has_reactions"))
  x = within(x, rm(list = rem)) %>% as_tibble()
  x })

matter = data.table::rbindlist(matter_list) %>% as_tibble()

# Data pre-processing

# Data formats

matter$create_at = as.POSIXct(matter$create_at/1000, origin = "1970-01-01") %>% lubridate::with_tz("Europe/Helsinki")

# Select only messages (exclude e.g. "X has joined the channel")

matter = matter %>% filter(type == "")

# Date variables

matter$date = as.Date(matter$create_at)
matter$hour = hour(matter$create_at)
matter$weekday = weekdays(matter$date)
matter$weekday_short = ordered(weekdays(matter$date, abbreviate = T), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# Only include year 2019

matter = matter %>% filter(year(create_at) == 2019)

# Remove columns that are not needed
# upmattere_at, edit_at, delete_at, is_pinned, channel_id, root_id, parent_id, original_id, type, hashtags, pending_post_id

matter = matter %>% select(id, create_at, user_id, date, hour, weekday, weekday_short)

# matter characteristics

length(unique(matter$id)) # 297 messages
length(unique(matter$user_id)) # 35 users
summary(matter$create_at) # 3.9.2019 - 11.12.2019

# matter visualization

# Messages by weekday

messages_by_weekday = matter %>% group_by(weekday_short) %>% summarize(n = n())
(plot2 = ggplot(messages_by_weekday, aes(x = weekday_short, y = n)) + 
  geom_bar(stat = "identity") +
  labs(title = "Messages by weekday", x = "", y = "")) +
  labs(y = "Messages")

# Messages by date

messages_by_date = matter %>% group_by(date) %>% summarize(n = n())
(plot1 = ggplot(messages_by_date, aes(x = date, y = n)) + 
  geom_bar(stat = "identity") +
  labs(x = "", y = "", title = "Messages by date"))

# Messages by user

messages_by_user = matter %>% group_by(user_id) %>% summarize(n = n())
summary(messages_by_user$n)
ggplot(messages_by_user, aes(x = user_id, y = n)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title = "Messages by user", x = "", y = "")

# Messages by hour

messages_by_hour = matter %>% group_by(hour) %>% summarize(n = n()) %>% mutate(hour = ifelse(hour == 0, 24, hour)) %>% arrange(hour)
(plot3 = ggplot(messages_by_hour, aes(x = hour, y = n)) + 
    geom_bar(aes(x = hour, y = n), stat = "identity") + 
    labs(title = "Commits by hour", x = "", y = "") +
    scale_x_continuous(limits = c(1, 24), breaks = seq(1, 24, 2)))


# All 3 to the same plot
grid.arrange(plot1,plot2,plot3, ncol=3)

# Cumulative number of messages by hour
N = sum(messages_by_hour$n)
ggplot(messages_by_hour, aes(x = hour, y = cumsum(n)/N*100)) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = 0:23) +
  labs(x = "Hour", y = "Percentage of messages")




