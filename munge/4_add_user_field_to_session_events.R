require(dplyr)
require(jsonlite)

latestdump <- readChar("latestdumpdate.txt", 10)

# read data from disk, depends on munge/ tasks 1, 2 and 3
wle <- readRDS(paste('cache/whitelistentries_', latestdump,'.rds', sep=""))
sessionevents <- readRDS(paste('cache/sessionevents_', latestdump,'.rds', sep=""))

# create and populate user field for session events
findUser <- function (data){
        user = ''
        data = fromJSON(data)
        if ("user" %in% names(data) ) {
                user = data$user
                if(typeof(user)=="list"){
                        user = user$`$oid`
                }
        }
        # if it's an answereee it's a whilelist id, so get the corresponding user
        else if ("answeree" %in% names(data) ) {
                user = data$answeree
                if(typeof(user)=="list"){
                        user = as.character(user$`$oid`)
                }
                # cant have the same name as a column
                u <- user
                w <- wle %>% filter (id==u) %>% collect();
                user <- w$user[1]
        }
        user
}

sessionevents <- sessionevents %>%
        rowwise() %>% 
        mutate(user= findUser(data))  %>%
        collect()

saveRDS(sessionevents, file= paste('cache/sessionevents_with_user_', latestdump,'.rds', sep=""))