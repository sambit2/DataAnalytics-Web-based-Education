#setwd("C:/Users/Sambit/Desktop/delft-web-and-database-technology-2016-asq-analysis")

require(dplyr)

latestdump <- readChar("latestdumpdate.txt", 10)

total.users.allsessions<- readRDS(paste("cache/whitelistentries_", latestdump, ".rds", sep=""));

#To find the distinct viewers across sessions (only 13th, 14th and 15th lecture)
#Number of distinct students across the 3 sessions is 131

total.users.allsessions.distinct <- total.users.allsessions %>% 
  filter(session %in% c("58787fdc97f6da002561f1d8","587c766608939c00267a225c","5881bc1208939c00267cc768")) %>%
  filter(role == "viewer") %>%
  select(user) %>%
  distinct(user)

#To find the freq of all repeated viewers (i.e. freq > 2) across all sessions
#If it's (freq > 1) then it means they appear across either of the 2 sessions

freq <- total.users.allsessions %>% 
  filter(role == "viewer") %>%
  filter(session %in% c("58787fdc97f6da002561f1d8","587c766608939c00267a225c","5881bc1208939c00267cc768")) %>%
  group_by(user) %>% 
  mutate(replicate=seq(n())) %>%
  filter(replicate > 2)

  