require(dplyr)

latestdump <- readChar("latestdumpdate.txt", 10)

# read data from disk, depends on munge/ tasks 1, 2, 3 and 4
wle <- readRDS(paste('cache/whitelistentries_', latestdump,'.rds', sep=""))
sessionevents <- readRDS(paste('cache/sessionevents_with_user_', latestdump,'.rds', sep=""))

# # rename `ctrl-connected` to `folo-connected` if the user is a viewer
# fixFoloDisconnected <- function (tp, sess, usr){
#         r <- wle %>% filter(user == usr & session==sess) %>% select(role) %>% collect()
#         if (r$role[1] == "viewer") tp <- "folo-disconnected"
#         tp;
# }

wle_v <- wle %>%
  filter(role == "viewer") %>%
  select(user, role, session)

df <- sessionevents %>%
  filter(type == "ctrl-disconnected") %>%
  left_join(wle_v, c("user", "session")) %>%
  mutate(type = "folo-disconnected")  %>%
  select(-role)

# df <- sessionevents %>%
#         filter(type == "ctrl-disconnected") %>%
#         rowwise() %>% 
#         mutate(type = fixFoloDisconnected(type, session,  user))  %>%
#         ungroup () %>%
#         collect()

sessionevents <- sessionevents %>%
        filter(! id  %in% df$id) %>%
        bind_rows(df) %>%
        arrange(id)

saveRDS(sessionevents, file= paste('cache/sessionevents_with_user_', latestdump,'.rds', sep=""))
