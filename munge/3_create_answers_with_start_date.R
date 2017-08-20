require(dplyr)
require(jsonlite)

source("addNewData.R")
latestdump <- readChar("latestdumpdate.txt", 10)

# read data from disk, depends on munge/ tasks 1 and 2
answers <- readRDS(paste('cache/answers_', latestdump,'.rds', sep=""))
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
        
        # pick only the first activation event for each question
        group_by(question)  %>%
        slice(which.min(time)) %>%
        # filter(time == min(time))%>% this allows ties of min values
        
        left_join(distinct_questions, by='question')  %>%
        select(id, session, question, time)

answers <- answers  %>% 
        left_join(start_times, by=c("question", "session")) %>%
        select(-id.y) %>%
        rename(startDate = time, id = id.x)

#Add the feedback time of each question to the answers

allowedVars <- c("feedback_time")
answers <- addNewData("feedback.csv", answers, allowedVars)

#Add the difficulty of each question to the answers

allowedVars_difficulty <- c("difficulty_level")
answers <- addNewData("difficulty.csv", answers, allowedVars_difficulty)

#Add the relative position (distance) of the quizzes to the answers

allowedVars_position <- c("relative_position")
answers <- addNewData("relative_position_quizzes.csv", answers, allowedVars_position)

#Add the first slide id relating to quizzes

allowedVars_slide_first <- c("slide_first")
answers <- addNewData("first_study_slide_for_quiz.csv", answers, allowedVars_slide_first)

#Add the last slide id relating to quizzes

allowedVars_slide_last <- c("slide_last")
answers <- addNewData("last_study_slide_for_quiz.csv", answers, allowedVars_slide_last)

saveRDS(answers, file= paste('cache/answers_with_start_date_', latestdump,'.rds', sep=""))
