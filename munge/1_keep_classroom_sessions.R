require(dplyr)

latestdump <- readChar("latestdumpdate.txt", 10)

# read data from disk
class_presentations_with_sessions <- read.csv("class_presentations_with_sessions.csv", stringsAsFactors = FALSE)
sessions <- read.csv(paste('csv/sessions_', latestdump,'.csv', sep=""), stringsAsFactors = FALSE)
answers <- read.csv(paste('csv/answers_', latestdump,'.csv', sep=""), stringsAsFactors = FALSE)
exercisesubmissions <- read.csv(paste('csv/exercisesubmissions_', latestdump,'.csv', sep=""), stringsAsFactors = FALSE)
session_events <- read.csv(paste('csv/sessionevents_', latestdump,'.csv', sep=""), stringsAsFactors = FALSE)
wl <- read.csv(paste('csv/whitelistentries_', latestdump,'.csv', sep=""), stringsAsFactors = FALSE)


# keep only useful sessions
sessionList <- class_presentations_with_sessions$session

sessions <- sessions %>%
        filter(X_id %in% sessionList)  %>%
        collect()

answers <- answers %>%
        filter(session %in% sessionList)  %>%
        collect()

exercisesubmissions <- exercisesubmissions %>%
        filter(session %in% sessionList)  %>%
        collect()

session_events <- session_events %>%
        filter(session %in% sessionList)  %>%
        collect()

wl <- wl %>%
        filter(session %in% sessionList)  %>%
        collect()

dir.create("cache", showWarnings = FALSE)
saveRDS(sessions, file = paste('cache/sessions_', latestdump,'.rds', sep=""))
saveRDS(answers, file = paste('cache/answers_', latestdump,'.rds', sep=""))
saveRDS(exercisesubmissions, file = paste('cache/exercisesubmissions_', latestdump,'.rds', sep=""))
saveRDS(session_events, file = paste('cache/sessionevents_', latestdump,'.rds', sep=""))
saveRDS(wl, file = paste('cache/whitelistentries_', latestdump,'.rds', sep=""))