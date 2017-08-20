require(dplyr)

latestdump <- readChar("latestdumpdate.txt", 10)

# read data from disk, depends on munge/ tasks 1 and 2
sessions <- readRDS(paste('cache/sessions_', latestdump,'.rds', sep=""))
slideshows <- readRDS(paste('cache/slideshows_', latestdump,'.rds', sep=""))
sessions <- sessions %>%
  left_join(slideshows %>% select(id,title) %>% collect(), by = c("slides"="id")) %>%
  collect()


# sessions we care about for this analysis
sids <- c("5829674d759f110025262234", "582eac8fae9a6f0025691ac9", "5832a2f88bd89e023bd91838", "5837e54fe578cc05fd9d5b54", "583be6bc0f6202087e02a4dc", "584125105479920897a5b8ea", "584513365bf5590027bf61fc", "584a5c8082645300263fb70a", "58578d8d6f9a3700272b6701", "585cd4096f9a3700272e42ab", "58733c626f9a370027312cb5", "58787fdc97f6da002561f1d8", "587c766608939c00267a225c", "5881bc1208939c00267cc768")
f_sessions <- sessions %>% filter(id %in% sids)

# patch endDate
f_sessions$startDate <-   as.POSIXct(strptime(c("2016-11-14 09:00:00 CET",
                                             "2016-11-18 08:45:00 CET",
                                             "2016-11-21 08:45:00 CET",
                                             "2016-11-25 08:45:40 CET",
                                             "2016-11-28 09:11:40 CET",
                                             "2016-12-02 08:45:56 CET",
                                             "2016-12-05 08:45:00 CET",
                                             "2016-12-09 08:45:00 CET",
                                             "2016-12-19 08:45:00 CET",
                                             "2016-12-23 08:45:00 CET",
                                             "2017-01-09 08:45:00 CET",
                                             "2017-01-13 08:45:00 CET",
                                             "2017-01-16 08:45:00 CET",
                                             "2017-01-20 08:48:00 CET"),
                                           "%Y-%m-%d %H:%M:%S", tz = "CET"))

f_sessions$endDate <-   as.POSIXct(strptime(c("2016-11-14 10:34:00 CET",
                                           "2016-11-18 10:33:00 CET",
                                           "2016-11-21 10:33:30 CET",
                                           "2016-11-25 10:37:40 CET",
                                           "2016-11-28 10:32:00 CET",
                                           "2016-12-02 10:29:40 CET",
                                           "2016-12-05 10:34:20 CET",
                                           "2016-12-09 10:31:00 CET",
                                           "2016-12-19 10:30:40 CET",
                                           "2016-12-23 10:34:00 CET",
                                           "2017-01-09 10:34:00 CET",
                                           "2017-01-13 10:21:00 CET",
                                           "2017-01-16 10:33:40 CET",
                                           "2017-01-20 10:25:00 CET"),
                                         "%Y-%m-%d %H:%M:%S", tz = "CET"))

f_sessions$breakStartDate <-  as.POSIXct(strptime(c("2016-11-14 09:35:50 CET",
                                                 "2016-11-18 09:30:00 CET",
                                                 "2016-11-21 09:37:40 CET",
                                                 "2016-11-25 09:29:00 CET",
                                                 "2016-11-28 09:32:00 CET",
                                                 "2016-12-02 09:31:50 CET",
                                                 "2016-12-05 09:31:50 CET",
                                                 "2016-12-09 09:29:30 CET",
                                                 "2016-12-19 09:34:20 CET",
                                                 "2016-12-23 09:32:50 CET",
                                                 "2017-01-09 09:33:00 CET",
                                                 "2017-01-13 09:31:10 CET",
                                                 "2017-01-16 09:28:45 CET",
                                                 "2017-01-20 09:31:00 CET"),
                                               "%Y-%m-%d %H:%M:%S", tz = "CET"))

f_sessions$breakEndDate <-  as.POSIXct(strptime(c("2016-11-14 09:50:00 CET",
                                               "2016-11-18 09:46:00 CET",
                                               "2016-11-21 09:50:00 CET",
                                               "2016-11-25 09:41:20 CET",
                                               "2016-11-28 09:46:00 CET",
                                               "2016-12-02 09:45:00 CET",
                                               "2016-12-05 09:46:30 CET",
                                               "2016-12-09 09:41:20 CET",
                                               "2016-12-19 09:49:30 CET",
                                               "2016-12-23 09:45:30 CET",
                                               "2017-01-09 09:46:00 CET",
                                               "2017-01-13 09:45:10 CET",
                                               "2017-01-16 09:39:40 CET",
                                               "2017-01-20 09:44:00 CET"),
                                             "%Y-%m-%d %H:%M:%S", tz = "CET"))

saveRDS(f_sessions, file= paste('cache/sessions_analysis_', latestdump,'.rds', sep=""))