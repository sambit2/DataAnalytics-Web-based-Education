require(dplyr)

latestdump <- readChar("latestdumpdate.txt", 10)

# read data from disk, depends on task 1
sessions <- readRDS(paste('cache/sessions_', latestdump,'.rds', sep=""))
answers <- readRDS(paste('cache/answers_', latestdump,'.rds', sep=""))
exercisesubmissions <- readRDS(paste('cache/exercisesubmissions_', latestdump,'.rds', sep=""))
session_events <- readRDS(paste('cache/sessionevents_', latestdump,'.rds', sep=""))
wl <- readRDS(paste('cache/whitelistentries_', latestdump,'.rds', sep=""))
slideshows <- read.csv(paste('csv/slideshows_', latestdump,'.csv', sep=""), stringsAsFactors = FALSE)
questions <- read.csv(paste('csv/questions_', latestdump,'.csv', sep=""), stringsAsFactors = FALSE)
exercises <- read.csv(paste('csv/exercises_', latestdump,'.csv', sep=""), stringsAsFactors = FALSE)

# fix ids and dates
sessions <- sessions %>%
        rename(id=X_id) %>%
        # first parse dates into POSIXct
        mutate(startDate = as.POSIXct(strptime(startDate, "%FT%H:%M:%OS", tz="Europe/London")))  %>%
        mutate(endDate = as.POSIXct(strptime(endDate, "%FT%H:%M:%OS", tz="Europe/London"))) %>%
        # then change timezone to correct one (unfortunately the server had them stored as GMT)
        mutate(startDate = as.POSIXct(format(startDate, tz="Europe/Amsterdam", usetz=TRUE)))  %>%
        mutate(endDate = as.POSIXct(format(endDate, tz="Europe/Amsterdam", usetz=TRUE)))  %>%
        collect()

# make sure that the session ends after 150 minutes
# ifelse will convert the date to numeric, see here for more info: http://stackoverflow.com/questions/31133382/ifelse-stripping-posixct-attribute-from-vector-of-timestamps
# we need to maintain the posixct class and the tzone attributes
sessions$endDate = ifelse(difftime(sessions$endDate, sessions$startDate, units="mins") > 150, sessions$startDate + 8400 , sessions$endDate)
attributes(sessions$endDate) = attributes(sessions$startDate)

exercisesubmissions <- exercisesubmissions %>%
        rename(id=X_id) %>%
        mutate(submitDate = as.POSIXct(strptime(submitDate, "%FT%H:%M:%OS", tz="Europe/London")))  %>%
        mutate(submitDate =  as.POSIXct(format(submitDate, tz="Europe/Amsterdam", usetz=TRUE)))  %>%
        collect()

answers <- answers %>%
        rename(id=X_id) %>%
        mutate(submitDate = as.POSIXct(strptime(submitDate, "%FT%H:%M:%OS", tz="Europe/London")))  %>%
        mutate(submitDate =  as.POSIXct(format(submitDate, tz="Europe/Amsterdam", usetz=TRUE)))  %>%
        collect()

session_events <- session_events %>%
        rename(id=X_id) %>%
        mutate(time = as.POSIXct(strptime(time, "%FT%H:%M:%OS", tz="Europe/London")))  %>%
        mutate(time =  as.POSIXct(format(time, tz="Europe/Amsterdam", usetz=TRUE)))  %>%
        collect()

wl <- wl %>%
        rename(id=X_id) %>%
        collect()

slideshows <- slideshows %>%
        rename(id=X_id) %>%
        collect()

questions <- questions %>%
        rename(id=X_id) %>%
        collect()

exercises <- exercises %>%
        rename(id=X_id) %>%
        collect()


saveRDS(sessions, file = paste('cache/sessions_', latestdump,'.rds', sep=""))
saveRDS(answers, file = paste('cache/answers_', latestdump,'.rds', sep=""))
saveRDS(exercisesubmissions, file = paste('cache/exercisesubmissions_', latestdump,'.rds', sep=""))
saveRDS(session_events, file = paste('cache/sessionevents_', latestdump,'.rds', sep=""))
saveRDS(wl, file = paste('cache/whitelistentries_', latestdump,'.rds', sep=""))
saveRDS(slideshows, file = paste('cache/slideshows_', latestdump,'.rds', sep=""))
saveRDS(questions, file = paste('cache/questions_', latestdump,'.rds', sep=""))
saveRDS(exercises, file = paste('cache/exercises_', latestdump,'.rds', sep=""))
