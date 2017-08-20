# This script builds on the information added from 7_analysis_sessions.R
# to fine tune the start date of questions taking into account the start date
# of sessions. It leaves sessions from which we don't have this information intact.

require(dplyr)
require(jsonlite)

source("addNewData.R")
latestdump <- readChar("latestdumpdate.txt", 10)

# read data from disk, depends on munge/ tasks 1 and 2
answers <- readRDS(paste('cache/answers_with_start_date_', latestdump,'.rds', sep=""))
analysis_sessions <- readRDS(paste('cache/sessions_analysis_', latestdump,'.rds', sep=""))
session_events <- readRDS(paste('cache/sessionevents_', latestdump,'.rds', sep=""))


# find start times for the questions
distinct_questions = answers %>%
  distinct(question) %>%
  select(question)

# find all events for question activated and add a question field 
# with the id of the question

start_times <-session_events %>% 
  filter( type == 'question-activated')  %>% 
  select(id, session, data, time) %>%
  rowwise()  %>%
  mutate(question= fromJSON(data)$question$`$oid`)  %>%
  ungroup() %>% 
  left_join(analysis_sessions %>% select(session=id, sessionStartDate = startDate), by="session") %>%
  filter(time >= sessionStartDate) %>%
  
  # pick only the first activation event for each question
  group_by(question)  %>%
  slice(which.min(time)) %>%
  # filter(time == min(time))%>% this allows ties of min values
  
  left_join(distinct_questions, by='question')  %>%
  select(id, session, question, time)

updated_answers <- answers  %>% 
  ungroup() %>%
  # get only answers from analysis sessions
  filter(session %in% analysis_sessions$id) %>%
  select(- startDate) %>%
  left_join(start_times %>% select(-id), by=c("question", "session")) %>%
  rename(startDate = time) %>% 
  select(id, exercise, question, answeree, session, type, submitDate, submission, confidence, startDate, feedback_time, difficulty_level, relative_position, slide_first, slide_last) %>%
  # merge with unaffected answers
  bind_rows(answers  %>% filter(!(session %in% analysis_sessions$id))) %>%
  arrange(id)


saveRDS(updated_answers, file= paste('cache/answers_with_start_date_', latestdump,'.rds', sep=""))
