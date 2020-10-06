# Virtual Learning Environments

Data analysis for the paper TEAM COLLABORATION PLATFORM SUPPORTING PROJECT-BASED LEARNING

## Research questions

- How much and when are students commiting on GitHub?
- How much and when are students messaging on Mattermost?

## Datasets

### git

Students' commits on GitHub (json)

Variable | Desciption
--- | ---
short_id | Commit ID
title | Commit title
message | Commit message
committer_name | Committer's name, free text
committer_email | Committer's email address, free text
committed_date | Commit date
author_email | Commit author's email address

### matter

Students' messages on Mattermost, message contents excluded (json)

Variable | Desciption
--- | ---
id | Message ID
create_at | Date and time of the message
user_id | User ID
date | Date, extracted from create_at
hour | Hour, extracted from create_at
weekday | Weekday, extracted from create_at
weekday_short | Abbreviated weekday, extracted from create_at
