require(dplyr)
require(jsonlite)

latestdump <- readChar("latestdumpdate.txt", 10)

# read data from disk, depends on munge/ tasks 1, 2, 3, 4 and 5
slideshows <- readRDS(paste('cache/slideshows_', latestdump,'.rds', sep=""))

sessions <- readRDS(paste('cache/sessions_', latestdump,'.rds', sep=""))
# sessions$startDate <- as.POSIXct(sessions$startDate)
# sessions$endDate <- as.POSIXct(sessions$endDate)

sessionevents.with.user <- readRDS(paste('cache/sessionevents_with_user_', latestdump,'.rds', sep=""))
# sessionevents.with.user$time <- as.POSIXct(sessionevents.with.user$time)
sessionevents.with.user$type <- as.factor(sessionevents.with.user$type)

findSlide <- function (data){
        slide = ''
        data = fromJSON(data)
        if ("slide" %in% names(data) ) {
                slide = data$slide
        }
        slide
}

# viewer event types
types.viewer <- c("answer-submitted",
                  "copy",
                  "cut",
                  "exercise-submit",
                  "exerciseblur",
                  "exercisefocus",
                  "focusin",
                  "focusout",
                  "input",
                  "paste",
                  "questioninput",
                  "tabhidden",
                  "tabvisible",
                  "viewer-idle",
                  "folo-connected",
                  "folo-disconnected",
                  "windowblur",
                  "windowfocus",
                  "slideleave",
                  "slideenter")

slide.transisions <- sessionevents.with.user %>%
        filter(type=="ctrl:goto") %>%
        rowwise() %>% 
        mutate(slide = findSlide(data)) %>%
        ungroup() %>%
        group_by(session)  %>%
        # add if the slide has exercise and
        # add time_end for slide. the last slide ends when the session ends
        do({
                df <- .
                
                # get exercises per slide for the slideshow of the specific session
                session <-sessions %>% filter(id==df$session[1]) %>% collect()
                slideshow <-slideshows %>% filter(id==session$slides) %>% collect()
                slideshow_ex <- fromJSON(slideshow$exercisesPerSlide)
                
                # keep only events within the session duration
                df <- df %>% filter(time <= session$endDate)
                
                df <- df %>%
                        rowwise() %>%
                        mutate(hasExercise = slide %in% names(slideshow_ex)) %>%
                        collect()
                
                df$time_end <- c(df$time[-1], sessions %>%
                                         filter(id == df$session[1]) %>%
                                         select(endDate))
                df
        }) %>%
        select(slide, time_start=time, time_end, hasExercise) %>%
        collect()

# when the session starts it takes some time for the first ctrl:goto event
# (depends on the network speed, how heavy is the presentation etc)
# For this reason there is no slide transition at t0 of the presentation
# the following code injects one by taking the first transitions of each 
# session, making their time_end = time_start, and making time_start = session start
init.slide.transisions <- slide.transisions %>%
        arrange(time_start) %>%
        group_by(session) %>%
        filter(row_number() <= 1) %>%
        ungroup() %>%
        left_join(sessions, by = c("session" = "id")) %<>%
        filter(time_start != startDate) %>%
        mutate(time_end = time_start, time_start = startDate) %>%
        select (session, slide, time_start, time_end, hasExercise) %>%
        collect()

slide.transisions %<>%
        bind_rows(init.slide.transisions) %>%
        arrange(time_start) %>%
        collect()


findSlideForEvent <- function (time, sess, slide.transisions){
        g <- which(slide.transisions$time_end >= time & slide.transisions$session == sess)
        # if there's no entry with bigger time put the active slide of the session
        pos <-ifelse(length(g)>0, min(g),  NA)
        if(is.na(pos)){
                slide <- sessions[sessions$id==sess,]$activeSlide
        }else{
                slide <- slide.transisions$slide[pos]  
        }
}

sessionevents.viewers <- sessionevents.with.user %>%
        filter(type %in% types.viewer) %>%
        group_by(session)  %>%
        do({
                df <- .
                st <- slide.transisions %>% filter(session == df$session[1]) %>% collect()
                df <- df %>%
                        rowwise()  %>% 
                        mutate(slide = findSlideForEvent(time, df$session[1], st)) %>%
                        ungroup() %>%
                        collect()
                df
        }) %>%
        ungroup() %>%
        select(id, session, type, data, time, user, slide) %>%
        collect()


sessionevents_viewers <- sessionevents.with.user %>%
  filter(type %in% types.viewer) %>%
  group_by(session)  %>%
  do({
    df <- .
    st <- slide.transisions %>% filter(session == df$session[1]) %>% collect()
    df <- df %>%
      rowwise()  %>% 
      mutate(slide = findSlideForEvent(time, df$session[1], st)) %>%
      ungroup() %>%
      collect()
    df
  }) %>%
  ungroup() %>%
  select(id, session, type, data, time, user, slide) %>%
  collect()

sessionevents_presenters <- sessionevents.with.user %>%
  filter(!(type %in% types.viewer)) %>%
  group_by(session)  %>%
           do({
             df <- .
             st <- slide.transisions %>% filter(session == df$session[1]) %>% collect()
             df <- df %>%
               rowwise()  %>% 
               mutate(slide = findSlideForEvent(time, df$session[1], st)) %>%
               ungroup() %>%
               collect()
             df
           }) %>%
           ungroup() %>%
           select(id, session, type, data, time, user, slide)
         
saveRDS(sessionevents_viewers, file= paste('cache/sessionevents_viewers_', latestdump,'.rds', sep=""))
saveRDS(sessionevents_presenters, file= paste('cache/sessionevents_presenters_', latestdump,'.rds', sep=""))
saveRDS(slide.transisions, file= paste('cache/slide_transisions_', latestdump,'.rds', sep=""))
