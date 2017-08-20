# Find the total number of session events in each lecture

total_session_events_each_session <- sessionevents %>%
  filter(session %in% sessions_with_title$id) %>%
  group_by(session) %>%
  left_join((sessions %>% select(session = id,startDate,endDate)),by="session") %>%
  filter(time > startDate & time < endDate) %>%
  mutate(total_events = n()) %>%
  distinct(total_events) %>%
  left_join(sessions_with_title, by= c("session" = "id"))

# Find the total number of slides in each lecture according to ASQ

total_slides_each_session <- slide_transitions %>%
  filter(session %in% sessions_with_title$id) %>%
  group_by(session) %>%
  left_join((sessions %>% select(session = id,startDate,endDate)),by="session") %>%
  filter((time_start > startDate & time_start < endDate)) %>% 
         #& (time_end > startDate & time_end < endDate)) %>%
  distinct(slide) %>%
  mutate(total_slides_ASQ = n()) %>%
  distinct(total_slides_ASQ) %>%
  left_join(sessions_with_title, by= c("session" = "id"))

# To check how many used copy and paste in each session

#test <- readRDS(paste('cache/user_state_585cd4096f9a3700272e42ab', '.rds', sep=""))

test <- sessionevents %>%
  filter(session %in% sessions_with_title$id) %>%
  filter(type == "copy") %>%
  group_by(session) %>%
  distinct(user) %>%
  summarise(count = n())
# Later we can check how many of these are during the exercises

# Find the duration of each slide in JS and categorize to minutes (more time) 
# and seconds (less time)

seconds_slides_js <- slide_transitions %>%
  filter(session == "5832a2f88bd89e023bd91838") %>%
  filter((time_start > sessions$startDate[[3]] & 
            time_end < sessions$endDate[[3]])) %>%
  mutate(duration = difftime(time_end,time_start)) %>%
  filter(!grepl("asq",slide)) %>%
  filter(duration < 250) %>%
  filter(duration < 20) %>%
  filter(duration > 5) %>%
  arrange(time_start)

minutes_slides_js <- slide_transitions %>%
  filter(session == "5832a2f88bd89e023bd91838") %>%
  filter((time_start > sessions$startDate[[3]] & 
            time_end < sessions$endDate[[3]])) %>%
  mutate(duration = difftime(time_end,time_start)) %>%
  filter(!grepl("asq",slide)) %>%
  filter(duration < 250) %>%
  filter(duration > 60) %>%
  arrange(time_start)



# Generate the stacked-area chart to display the number of users
# who are connected, visible and focus and also highlight slides that took
# minutes and seconds to discuss

draw_connected_focus_area_with_interactive <- function (dt, slide_transitions_with_ex, slide_transitions_without_ex, 
                                                        state_labels, color_scale = get_color_scale()) 
{
  r_labels <- c(normal = "without question", exercise = "with question", 
                `break` = "break", interactive = "interactive")
  r_scale <- c(exercise = "#CD66E0", normal = "#FFFFFF", `break` = "#000000", interactive = "#fffa00")
  stacked_area <- ggplot(dt) + geom_area(aes(x = time, y = count, fill = state)) + 
    geom_segment(aes(x = time, xend = time + 1, y = -0.02 * max(dt$count), yend = 0, colour = hasExercise))
  
  if (nrow(slide_transitions_with_ex) > 0) {
    stacked_area <- stacked_area + geom_vline(data = slide_transitions_with_ex, 
                                              aes(xintercept = as.numeric(time_start)), colour = "lightblue", 
                                              linetype = 1, size = 0.4, alpha = 0.5)
  }
  stacked_area <- stacked_area + geom_vline(data = slide_transitions_without_ex, 
                                            aes(xintercept = as.numeric(time_start)), colour = "#cacaca", 
                                            linetype = 1, size = 0.4, alpha = 0.5) + 
    scale_colour_manual(values = r_scale, name = "slide", labels = r_labels) +
    scale_fill_manual(values = color_scale, name = "state", labels = state_labels) +
    ylim(-2, 122) + 
    scale_x_datetime(labels = date_format("%H:%M", tz = "CET"),
                     breaks = date_breaks(paste(300, "sec"))) + 
    ylab("number of students") + theme_bw() + theme(legend.position = "top", 
                                                    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
                                                    axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), 
                                                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                    legend.direction = "horizontal",
                                                    legend.box = "horizontal",
                                                    legend.key = element_rect(colour = "black"))
  stacked_area
}

js_session <- sessions[3,]

sdata <- filter_data_for_session(js_session, whitelistentries, sessionevents_viewers, slide_transitions)
dt <- readRDS(paste("cache/user_state_", js_session$id, ".rds", sep=""));
dt <- as.data.table(dt)
dt <- dt %>%
  filter(time >= js_session$startDate)

dt<- dt %>%
  filter(time <= js_session$endDate) %>%
  mutate(hasExercise = ifelse( slide %in% interactive_non_question_slides, "interactive", ifelse(hasExercise == TRUE, "exercise", "normal")),
         connected = ifelse(is.na(connected), 0, as.numeric(connected)),
         visible = ifelse(is.na(visible), 0, as.numeric(visible)),
         focus = ifelse(is.na(focus), 0, as.numeric(focus)),
         idle = ifelse(is.na(idle), 0, as.numeric(idle)),
         input = ifelse(is.na(input), 0, as.numeric(input)),
         submitted = ifelse(is.na(submitted), 0, as.numeric(submitted)))

if(! is.na(js_session$breakStartDate)){
  dt<- dt %>%
    mutate(hasExercise = ifelse(time >= js_session$breakStartDate &
                                  time <=js_session$breakEndDate, "break", hasExercise))
}



by_time_aggr_individual_states <- group_by_time(dt) %>%
  select(time, hasExercise,  connected, visible, focus) %>%
  mutate(connected = connected - visible) %>%
  mutate(visible = visible - focus)

by_time_aggr_individual_states_melted <- melt(by_time_aggr_individual_states,
                                              id.vars=c("time", "hasExercise"),
                                              variable.name="state",
                                              value.name="count")

by_time_aggr_individual_states_melted$state <- as.factor(by_time_aggr_individual_states_melted$state)
by_time_aggr_individual_states_melted$state <- factor( by_time_aggr_individual_states_melted$state,c("connected","visible","focus"))
by_time_aggr_individual_states_melted <- by_time_aggr_individual_states_melted %>% arrange(state)

stwe <- sdata[["slide_transitions_with_ex"]]
stwoe <- sdata[["slide_transitions_without_ex"]]

s.labels <- c("visible" = "Visible",
              "focus" = "focus",
              "connected" = "Connected")

c.scales <- c("visible" = "#F6B26B",
              "focus" = "#3cd87f",
              "connected" = "#3C78D8")    


g <- draw_connected_focus_area_with_interactive(by_time_aggr_individual_states_melted, stwe, stwoe, s.labels, c.scales )

# Add rectangle to highlight slides of minute duration and seconds duration
#and also add transparency (alpha) to them for better visibility

d <- g + annotate("rect", xmin = minutes_slides_js$time_start[[1]], xmax = minutes_slides_js$time_end[[1]], ymin = 0, ymax = Inf, color="#ff7700", fill="#ff7700", alpha = 0.6)

for (i in 2:nrow(minutes_slides_js)){

d <- d + annotate("rect", xmin = minutes_slides_js$time_start[[i]], xmax = minutes_slides_js$time_end[[i]], ymin = 0, ymax = Inf, color="#ff7700", fill="#ff7700", alpha = 0.6)

}

for (i in 1:nrow(seconds_slides_js)){
  
  d <- d + annotate("rect", xmin = seconds_slides_js$time_start[[i]], xmax = seconds_slides_js$time_end[[i]], ymin = 0, ymax = Inf, color="#2b5b38", fill="#2b5b38", alpha = 0.6)
  
}
  

ggsave("plots/javascript_visible_focus_highlighted.pdf",  width=7.6, height=3.6,  limitsize=FALSE)

## For HTTP Lecture ###############################################################

exercises_slides_http <- slide_transitions %>%
  filter(session == "5829674d759f110025262234") %>%
  filter((time_start > sessions$startDate[[1]] & 
            time_end < sessions$endDate[[1]])) %>%
  mutate(duration = difftime(time_end,time_start)) %>%
  filter(grepl("asq",slide)) %>%
  arrange(time_start)

 exercises_slides_http[3,3] <- sessions$breakEndDate[[1]]

video_slides_http <- slide_transitions %>%
  filter(session == "5829674d759f110025262234") %>%
  filter((time_start > sessions$startDate[[1]] & 
            time_end < sessions$endDate[[1]])) %>%
  mutate(duration = difftime(time_end,time_start)) %>%
  filter(slide %in% c("pf4","pf46","pf48","pf94")) %>%
  arrange(time_start)

# Generate the stacked-area chart to display the number of users
# who are connected, visible and focus and also highlight slides that
# has exercises and have videos

draw_connected_focus_area_with_interactive <- function (dt, slide_transitions_with_ex, slide_transitions_without_ex, 
                                                        state_labels, color_scale = get_color_scale()) 
{
  r_labels <- c(normal = "without question", exercise = "with question", 
                `break` = "break", interactive = "interactive")
  r_scale <- c(exercise = "#CD66E0", normal = "#FFFFFF", `break` = "#000000", interactive = "#fffa00")
  stacked_area <- ggplot(dt) + geom_area(aes(x = time, y = count, fill = state)) + 
    
    scale_fill_manual(values = color_scale, name = "state", labels = state_labels) +
    ylim(-2, 210) + 
    scale_x_datetime(labels = date_format("%H:%M", tz = "CET"),
                     breaks = date_breaks(paste(300, "sec"))) + 
    ylab("number of students") + theme_bw() + theme(legend.position = "top", 
                                                    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
                                                    axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), 
                                                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                    legend.direction = "horizontal",
                                                    legend.box = "horizontal",
                                                    legend.key = element_rect(colour = "black"))
  stacked_area
}

http_session <- sessions[1,]

sdata <- filter_data_for_session(http_session, whitelistentries, sessionevents_viewers, slide_transitions)
dt <- readRDS(paste("cache/user_state_", http_session$id, ".rds", sep=""));
dt <- as.data.table(dt)
dt <- dt %>%
  filter(time >= http_session$startDate)

dt<- dt %>%
  filter(time <= http_session$endDate) %>%
  mutate(hasExercise = ifelse( slide %in% interactive_non_question_slides, "interactive", ifelse(hasExercise == TRUE, "exercise", "normal")),
         connected = ifelse(is.na(connected), 0, as.numeric(connected)),
         visible = ifelse(is.na(visible), 0, as.numeric(visible)),
         focus = ifelse(is.na(focus), 0, as.numeric(focus)),
         idle = ifelse(is.na(idle), 0, as.numeric(idle)),
         input = ifelse(is.na(input), 0, as.numeric(input)),
         submitted = ifelse(is.na(submitted), 0, as.numeric(submitted)))

if(! is.na(http_session$breakStartDate)){
  dt<- dt %>%
    mutate(hasExercise = ifelse(time >= http_session$breakStartDate &
                                  time <= http_session$breakEndDate, "break", hasExercise))
}



by_time_aggr_individual_states <- group_by_time(dt) %>%
  select(time, hasExercise,  connected, visible, focus) %>%
  mutate(connected = connected - visible) %>%
  mutate(visible = visible - focus)

by_time_aggr_individual_states_melted <- melt(by_time_aggr_individual_states,
                                              id.vars=c("time", "hasExercise"),
                                              variable.name="state",
                                              value.name="count")

by_time_aggr_individual_states_melted$state <- as.factor(by_time_aggr_individual_states_melted$state)
by_time_aggr_individual_states_melted$state <- factor( by_time_aggr_individual_states_melted$state,c("connected","visible","focus"))
by_time_aggr_individual_states_melted <- by_time_aggr_individual_states_melted %>% arrange(state)

stwe <- sdata[["slide_transitions_with_ex"]]
stwoe <- sdata[["slide_transitions_without_ex"]]

s.labels <- c("visible" = "Visible",
              "focus" = "focus",
              "connected" = "Connected")

c.scales <- c("visible" = "#F6B26B",
              "focus" = "#3cd87f",
              "connected" = "#3C78D8")    


g <- draw_connected_focus_area_with_interactive(by_time_aggr_individual_states_melted, stwe, stwoe, s.labels, c.scales )

# Add rectangle to highlight slides of minute duration and seconds duration
#and also add transparency (alpha) to them for better visibility

d <- g + annotate("rect", xmin = exercises_slides_http$time_start[[1]], xmax = exercises_slides_http$time_end[[1]], ymin = 0, ymax = Inf, color="#ff7700", fill="#ff7700", alpha = 0.5)

for (i in 2:nrow(exercises_slides_http)){
  
  d <- d + annotate("rect", xmin = exercises_slides_http$time_start[[i]], xmax = exercises_slides_http$time_end[[i]], ymin = 0, ymax = Inf, color="#ff7700", fill="#ff7700", alpha = 0.5)
  
}

for (i in 1:nrow(video_slides_http)){
  
  d <- d + annotate("rect", xmin = video_slides_http$time_start[[i]], xmax = video_slides_http$time_end[[i]], ymin = 0, ymax = Inf, color="#2b5b38", fill="#2b5b38", alpha = 0.4)
  
}

ggsave("plots/http_exercise_video.pdf",  width=7.6, height=3.6,  limitsize=FALSE)


## HTML ###############################################################

exercises_slides_html <- slide_transitions %>%
  filter(session == "582eac8fae9a6f0025691ac9") %>%
  filter((time_start > sessions$startDate[[2]] & 
            time_end < sessions$endDate[[2]])) %>%
  mutate(duration = difftime(time_end,time_start)) %>%
  filter(grepl("asq",slide)) %>%
  filter(!(duration == 0)) %>%
  arrange(time_start)

exercises_slides_html[4,4] <- sessions$breakStartDate[[2]]

video_slides_html <- slide_transitions %>%
  filter(session == "582eac8fae9a6f0025691ac9") %>%
  filter((time_start > sessions$startDate[[2]] & 
            time_end < sessions$endDate[[2]])) %>%
  mutate(duration = difftime(time_end,time_start)) %>%
  filter(slide %in% c("pf122")) %>%
  arrange(time_start)

# Generate the stacked-area chart to display the number of users
# who are connected, visible and focus and also highlight slides that
# has exercises and have videos

draw_connected_focus_area_with_interactive <- function (dt, slide_transitions_with_ex, slide_transitions_without_ex, 
                                                        state_labels, color_scale = get_color_scale()) 
{
  r_labels <- c(normal = "without question", exercise = "with question", 
                `break` = "break", interactive = "interactive")
  r_scale <- c(exercise = "#CD66E0", normal = "#FFFFFF", `break` = "#000000", interactive = "#fffa00")
  stacked_area <- ggplot(dt) + geom_area(aes(x = time, y = count, fill = state)) + 
    
    scale_fill_manual(values = color_scale, name = "state", labels = state_labels) +
    ylim(-2, 160) + 
    scale_x_datetime(labels = date_format("%H:%M", tz = "CET"),
                     breaks = date_breaks(paste(300, "sec"))) + 
    ylab("number of students") + theme_bw() + theme(legend.position = "top", 
                                                    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
                                                    axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), 
                                                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                    legend.direction = "horizontal",
                                                    legend.box = "horizontal",
                                                    legend.key = element_rect(colour = "black"))
  stacked_area
}

http_session <- sessions[2,]

sdata <- filter_data_for_session(http_session, whitelistentries, sessionevents_viewers, slide_transitions)
dt <- readRDS(paste("cache/user_state_", http_session$id, ".rds", sep=""));
dt <- as.data.table(dt)
dt <- dt %>%
  filter(time >= http_session$startDate)

dt<- dt %>%
  filter(time <= http_session$endDate) %>%
  mutate(hasExercise = ifelse( slide %in% interactive_non_question_slides, "interactive", ifelse(hasExercise == TRUE, "exercise", "normal")),
         connected = ifelse(is.na(connected), 0, as.numeric(connected)),
         visible = ifelse(is.na(visible), 0, as.numeric(visible)),
         focus = ifelse(is.na(focus), 0, as.numeric(focus)),
         idle = ifelse(is.na(idle), 0, as.numeric(idle)),
         input = ifelse(is.na(input), 0, as.numeric(input)),
         submitted = ifelse(is.na(submitted), 0, as.numeric(submitted)))

if(! is.na(http_session$breakStartDate)){
  dt<- dt %>%
    mutate(hasExercise = ifelse(time >= http_session$breakStartDate &
                                  time <= http_session$breakEndDate, "break", hasExercise))
}



by_time_aggr_individual_states <- group_by_time(dt) %>%
  select(time, hasExercise,  connected, visible, focus) %>%
  mutate(connected = connected - visible) %>%
  mutate(visible = visible - focus)

by_time_aggr_individual_states_melted <- melt(by_time_aggr_individual_states,
                                              id.vars=c("time", "hasExercise"),
                                              variable.name="state",
                                              value.name="count")

by_time_aggr_individual_states_melted$state <- as.factor(by_time_aggr_individual_states_melted$state)
by_time_aggr_individual_states_melted$state <- factor( by_time_aggr_individual_states_melted$state,c("connected","visible","focus"))
by_time_aggr_individual_states_melted <- by_time_aggr_individual_states_melted %>% arrange(state)

stwe <- sdata[["slide_transitions_with_ex"]]
stwoe <- sdata[["slide_transitions_without_ex"]]

s.labels <- c("visible" = "Visible",
              "focus" = "focus",
              "connected" = "Connected")

c.scales <- c("visible" = "#F6B26B",
              "focus" = "#3cd87f",
              "connected" = "#3C78D8")    


g <- draw_connected_focus_area_with_interactive(by_time_aggr_individual_states_melted, stwe, stwoe, s.labels, c.scales )

# Add rectangle to highlight slides of minute duration and seconds duration
#and also add transparency (alpha) to them for better visibility

d <- g + annotate("rect", xmin = exercises_slides_html$time_start[[1]], xmax = exercises_slides_html$time_end[[1]], ymin = 0, ymax = Inf, color="#ff7700", fill="#ff7700", alpha = 0.5)

for (i in 2:nrow(exercises_slides_html)){
  
  d <- d + annotate("rect", xmin = exercises_slides_html$time_start[[i]], xmax = exercises_slides_html$time_end[[i]], ymin = 0, ymax = Inf, color="#ff7700", fill="#ff7700", alpha = 0.5)
  
}

for (i in 1:nrow(video_slides_html)){
  
  d <- d + annotate("rect", xmin = video_slides_html$time_start[[i]], xmax = video_slides_html$time_end[[i]], ymin = 0, ymax = Inf, color="#2b5b38", fill="#2b5b38", alpha = 0.4)
  
}

ggsave("plots/html_exercise_video.pdf",  width=7.6, height=3.6,  limitsize=FALSE)


## nodejs ###############################################################

exercises_slides_nodejs <- slide_transitions %>%
  filter(session == "583be6bc0f6202087e02a4dc") %>%
  filter((time_start > sessions$startDate[[5]] & 
            time_end < sessions$endDate[[5]])) %>%
  mutate(duration = difftime(time_end,time_start)) %>%
  filter(grepl("asq",slide)) %>%
  filter(!(duration == 0)) %>%
  arrange(time_start)

video_slides_nodejs <- slide_transitions %>%
  filter(session == "583be6bc0f6202087e02a4dc") %>%
  filter((time_start > sessions$startDate[[5]] & 
            time_end < sessions$endDate[[5]])) %>%
  mutate(duration = difftime(time_end,time_start)) %>%
  filter(slide %in% c("pf57","pf67","pf88","pf137","pf153")) %>%
  arrange(time_start)

# Generate the stacked-area chart to display the number of users
# who are connected, visible and focus and also highlight slides that
# has exercises and have videos

draw_connected_focus_area_with_interactive <- function (dt, slide_transitions_with_ex, slide_transitions_without_ex, 
                                                        state_labels, color_scale = get_color_scale()) 
{
  r_labels <- c(normal = "without question", exercise = "with question", 
                `break` = "break", interactive = "interactive")
  r_scale <- c(exercise = "#CD66E0", normal = "#FFFFFF", `break` = "#000000", interactive = "#fffa00")
  stacked_area <- ggplot(dt) + geom_area(aes(x = time, y = count, fill = state)) + 
    
    scale_fill_manual(values = color_scale, name = "state", labels = state_labels) +
    ylim(-2, 100) + 
    scale_x_datetime(labels = date_format("%H:%M", tz = "CET"),
                     breaks = date_breaks(paste(300, "sec"))) + 
    ylab("number of students") + theme_bw() + theme(legend.position = "top", 
                                                    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
                                                    axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), 
                                                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                    legend.direction = "horizontal",
                                                    legend.box = "horizontal",
                                                    legend.key = element_rect(colour = "black"))
  stacked_area
}

http_session <- sessions[5,]

sdata <- filter_data_for_session(http_session, whitelistentries, sessionevents_viewers, slide_transitions)
dt <- readRDS(paste("cache/user_state_", http_session$id, ".rds", sep=""));
dt <- as.data.table(dt)
dt <- dt %>%
  filter(time >= http_session$startDate)

dt<- dt %>%
  filter(time <= http_session$endDate) %>%
  mutate(hasExercise = ifelse( slide %in% interactive_non_question_slides, "interactive", ifelse(hasExercise == TRUE, "exercise", "normal")),
         connected = ifelse(is.na(connected), 0, as.numeric(connected)),
         visible = ifelse(is.na(visible), 0, as.numeric(visible)),
         focus = ifelse(is.na(focus), 0, as.numeric(focus)),
         idle = ifelse(is.na(idle), 0, as.numeric(idle)),
         input = ifelse(is.na(input), 0, as.numeric(input)),
         submitted = ifelse(is.na(submitted), 0, as.numeric(submitted)))

if(! is.na(http_session$breakStartDate)){
  dt<- dt %>%
    mutate(hasExercise = ifelse(time >= http_session$breakStartDate &
                                  time <= http_session$breakEndDate, "break", hasExercise))
}



by_time_aggr_individual_states <- group_by_time(dt) %>%
  select(time, hasExercise,  connected, visible, focus) %>%
  mutate(connected = connected - visible) %>%
  mutate(visible = visible - focus)

by_time_aggr_individual_states_melted <- melt(by_time_aggr_individual_states,
                                              id.vars=c("time", "hasExercise"),
                                              variable.name="state",
                                              value.name="count")

by_time_aggr_individual_states_melted$state <- as.factor(by_time_aggr_individual_states_melted$state)
by_time_aggr_individual_states_melted$state <- factor( by_time_aggr_individual_states_melted$state,c("connected","visible","focus"))
by_time_aggr_individual_states_melted <- by_time_aggr_individual_states_melted %>% arrange(state)

stwe <- sdata[["slide_transitions_with_ex"]]
stwoe <- sdata[["slide_transitions_without_ex"]]

s.labels <- c("visible" = "Visible",
              "focus" = "focus",
              "connected" = "Connected")

c.scales <- c("visible" = "#F6B26B",
              "focus" = "#3cd87f",
              "connected" = "#3C78D8")    


g <- draw_connected_focus_area_with_interactive(by_time_aggr_individual_states_melted, stwe, stwoe, s.labels, c.scales )

# Add rectangle to highlight slides of minute duration and seconds duration
#and also add transparency (alpha) to them for better visibility

d <- g + annotate("rect", xmin = exercises_slides_nodejs$time_start[[1]], xmax = exercises_slides_nodejs$time_end[[1]], ymin = 0, ymax = Inf, color="#ff7700", fill="#ff7700", alpha = 0.5)

for (i in 2:nrow(exercises_slides_nodejs)){
  
  d <- d + annotate("rect", xmin = exercises_slides_nodejs$time_start[[i]], xmax = exercises_slides_nodejs$time_end[[i]], ymin = 0, ymax = Inf, color="#ff7700", fill="#ff7700", alpha = 0.5)
  
}

for (i in 1:nrow(video_slides_nodejs)){
  
  d <- d + annotate("rect", xmin = video_slides_nodejs$time_start[[i]], xmax = video_slides_nodejs$time_end[[i]], ymin = 0, ymax = Inf, color="#2b5b38", fill="#2b5b38", alpha = 0.4)
  
}

ggsave("plots/nodejs_exercise_video.pdf",  width=7.6, height=3.6,  limitsize=FALSE)



################################################################################
# Find the percentage of students who are in state 14
# (connected, focus and visible) out of total in state 2 (connected)
# then correlate this number with students before and after the slide
# for two conditions where one is the slide that took minutes and second
# condition where the slide took some seconds. Do this for each lecture

get_focus_before_after_per_session <- function(session_i){


sdata <- filter_data_for_session(session_i, whitelistentries, sessionevents_viewers, slide_transitions)
dt <- readRDS(paste("cache/user_state_", session_i$id, ".rds", sep=""));
dt <- as.data.table(dt)
dt <- dt %>%
  filter(time >= session_i$startDate)

dt<- dt %>%
  filter(time <= session_i$endDate) %>%
  mutate(hasExercise = ifelse( slide %in% interactive_non_question_slides, "interactive", ifelse(hasExercise == TRUE, "exercise", "normal")),
         connected = ifelse(is.na(connected), 0, as.numeric(connected)),
         visible = ifelse(is.na(visible), 0, as.numeric(visible)),
         focus = ifelse(is.na(focus), 0, as.numeric(focus)),
         idle = ifelse(is.na(idle), 0, as.numeric(idle)),
         input = ifelse(is.na(input), 0, as.numeric(input)),
         submitted = ifelse(is.na(submitted), 0, as.numeric(submitted)))

if(! is.na(session_i$breakStartDate)){
  dt<- dt %>%
    mutate(hasExercise = ifelse(time >= session_i$breakStartDate &
                                  time <= session_i$breakEndDate, "break", hasExercise))
}

by_time_aggr_individual_states <- group_by_time(dt) %>%
  select(time, hasExercise,  connected, visible, focus)


minutes_slides <- slide_transitions %>%
  filter(session == session_i$id) %>%
  filter((time_start > session_i$startDate & 
            time_end < session_i$endDate)) %>%
  mutate(duration = difftime(time_end,time_start)) %>%
  filter(!grepl("asq",slide)) %>%
  filter(duration > 60) %>%
  arrange(time_start)

seconds_slides <- slide_transitions %>%
  filter(session == session_i$id) %>%
  filter((time_start > session_i$startDate & 
            time_end < session_i$endDate)) %>%
  mutate(duration = difftime(time_end,time_start)) %>%
  filter(!grepl("asq",slide)) %>%
  filter(duration >= 2 & duration <= 30) %>%
  arrange(time_start)

# To find the number of students before the start of the slide and after the end

minutes_slides$time_start <- minutes_slides$time_start - seconds(1) 
minutes_slides$time_end <- minutes_slides$time_end + seconds(1)

seconds_slides$time_start <- seconds_slides$time_start - seconds(1) 
seconds_slides$time_end <- seconds_slides$time_end + seconds(1)

minutes_slides_before <- minutes_slides %>%
  select(time = time_start) %>%
  left_join(by_time_aggr_individual_states,by="time") %>%
  mutate(percentage_before = focus/connected) %>%
  arrange(time) %>%
  select(percentage_before) %>%
  mutate(count = seq(n()))

minutes_slides_after <- minutes_slides %>%
  select(time = time_end) %>%
  left_join(by_time_aggr_individual_states,by="time") %>%
  mutate(percentage_after = focus/connected) %>%
  arrange(time) %>%
  select(percentage_after) %>%
  mutate(count = seq(n())) %>%
  ungroup %>%
  right_join(minutes_slides_before,by=c("session","count")) %>%
  mutate(type = "minutes")

seconds_slides_before <- seconds_slides %>%
  select(time = time_start) %>%
  left_join(by_time_aggr_individual_states,by="time") %>%
  mutate(percentage_before = focus/connected) %>%
  arrange(time) %>%
  select(percentage_before) %>%
  mutate(count = seq(n()))

seconds_slides_after <- seconds_slides %>%
  select(time = time_end) %>%
  left_join(by_time_aggr_individual_states,by="time") %>%
  mutate(percentage_after = focus/connected) %>%
  arrange(time) %>%
  select(percentage_after) %>%
  mutate(count = seq(n())) %>%
  ungroup %>%
  right_join(seconds_slides_before,by=c("session","count")) %>%
  mutate(type = "seconds")

minutes_seconds_slides <- bind_rows(minutes_slides_after,seconds_slides_after)

minutes_seconds_slides

}

all <- list()

for (i in 1:nrow(sessions)){
  session <-  sessions[i, ]
  temp  <- get_focus_before_after_per_session(session)
  all[[i]] <- temp
}

focus_before_after_minutes_seconds <- bind_rows(all)

# Find the difference between percentage of students before and after

focus_before_after_change <- focus_before_after_minutes_seconds %>%
  select(-count) %>%
  mutate(difference = (percentage_after-percentage_before)*100) %>%
  ungroup %>%
  group_by(session) %>%
  left_join(sessions_with_title, by= c("session" = "id"))

# Convert type to factor

focus_before_after_change$type <- as.factor(focus_before_after_change$type)

# Box-plot for type (minutes or seconds) vs change of percentage before and after
# the slide of each type was shown

ggplot(data = focus_before_after_change, 
       aes(x=type, y=difference)) + 
  geom_boxplot() + 
  xlab("type") +
  ylab("difference") +
  facet_wrap(~title,ncol = 4,scales="free_x")


#####################################################################
# Calculate correlation of focus before/ after and the correlation with slide duration

get_focus_duration_before_after_per_session <- function(session_i){
  
  
  sdata <- filter_data_for_session(session_i, whitelistentries, sessionevents_viewers, slide_transitions)
  dt <- readRDS(paste("cache/user_state_", session_i$id, ".rds", sep=""));
  dt <- as.data.table(dt)
  dt <- dt %>%
    filter(time >= session_i$startDate)
  
  dt<- dt %>%
    filter(time <= session_i$endDate) %>%
    mutate(hasExercise = ifelse( slide %in% interactive_non_question_slides, "interactive", ifelse(hasExercise == TRUE, "exercise", "normal")),
           connected = ifelse(is.na(connected), 0, as.numeric(connected)),
           visible = ifelse(is.na(visible), 0, as.numeric(visible)),
           focus = ifelse(is.na(focus), 0, as.numeric(focus)),
           idle = ifelse(is.na(idle), 0, as.numeric(idle)),
           input = ifelse(is.na(input), 0, as.numeric(input)),
           submitted = ifelse(is.na(submitted), 0, as.numeric(submitted)))
  
  if(! is.na(session_i$breakStartDate)){
    dt<- dt %>%
      mutate(hasExercise = ifelse(time >= session_i$breakStartDate &
                                    time <= session_i$breakEndDate, "break", hasExercise))
  }
  
  by_time_aggr_individual_states <- group_by_time(dt) %>%
    select(time, hasExercise,  connected, visible, focus)
  
  
  minutes_slides <- slide_transitions %>%
    filter(session == session_i$id) %>%
    filter((time_start > session_i$startDate & 
              time_end < session_i$endDate)) %>%
    mutate(duration = difftime(time_end,time_start)) %>%
    filter(!grepl("asq",slide)) %>%
    arrange(time_start)
  
  # To find the number of students before the start of the slide and after the end
  
  minutes_slides$time_start <- minutes_slides$time_start - seconds(1) 
  minutes_slides$time_end <- minutes_slides$time_end + seconds(1)
  
  minutes_slides_before <- minutes_slides %>%
    mutate(duration = difftime(time_end,time_start),units="secs") %>%
    select(time = time_start,duration) %>%
    left_join(by_time_aggr_individual_states,by="time") %>%
    mutate(percentage_before = focus/connected) %>%
    arrange(time) %>%
    select(percentage_before,duration) %>%
    mutate(count = seq(n()))
  
  minutes_slides_after <- minutes_slides %>%
    select(time = time_end) %>%
    left_join(by_time_aggr_individual_states,by="time") %>%
    mutate(percentage_after = focus/connected) %>%
    arrange(time) %>%
    select(percentage_after) %>%
    mutate(count = seq(n())) %>%
    ungroup %>%
    right_join(minutes_slides_before,by=c("session","count"))
  
  minutes_slides_after
  
}

all <- list()

for (i in 1:nrow(sessions)){
  session <-  sessions[i, ]
  temp  <- get_focus_duration_before_after_per_session(session)
  all[[i]] <- temp
}

focus_before_after_duration <- bind_rows(all)

# Find the difference between percentage of students before and after

focus_before_after_change <- focus_before_after_duration %>%
  select(-count) %>%
  mutate(difference = (percentage_after-percentage_before)*100) %>%
  ungroup %>%
  group_by(session) %>%
  left_join(sessions_with_title, by= c("session" = "id")) %>%
  filter(duration < 400)

# Make the scatterplot of duration vs difference

ggplot(data = focus_before_after_change, 
       aes(x=duration, y=difference)) + 
  geom_point() + 
  scale_x_continuous(limits=c(0,400)) + 
  xlab("duration") +
  ylab("differnce") +
  facet_wrap(~title,ncol = 4,scales="free_x")

# Find the correlation between duration and difference

focus_before_after_correlation_duration_difference <- focus_before_after_change %>%
group_by(title) %>%
  summarize(spearman_tau = cor.test(duration, difference, method = c("spearman"))$estimate,
            spearman_p.value = cor.test(duration, difference, method = c("spearman"))$p.value) %>%
  distinct(title,spearman_tau,spearman_p.value)


########################################################################################


# Find the number of students taking notes during lectures
# If students copy during non-exercise slides then it means they are taking notes

# The function helps to find the copy activity of students for all of the lectures (sessions)

get_copy_activity <- function(session){
  
  sid <- session$id
  
session_students_info <- readRDS(paste('cache/user_state_', sid, '.rds', sep=""))

copy_activity <- sessionevents %>%
  filter(session %in% sid) %>%
  filter(type == "copy") %>%
  left_join(session_students_info %>% select(user,hasExercise,time), by=c("user","time"))

copy_activity

}

all <- list()

for (i in 1:nrow(sessions)){
  session <-  sessions[i, ]
  temp  <- get_copy_activity(session)
  all[[i]] <- temp
}

copy_activity_all_sessions <- bind_rows(all)

# Check the different operations (to find if someone copied during non-exercise slide)

copy_activity_all_sessions_operations <- copy_activity_all_sessions %>%
  filter(copy_activity_all_sessions$hasExercise == "FALSE") %>%
  group_by(session,user) %>%
  mutate(count_users = n())
  
########################################################################################

#Generate the mean score of each user for each mcq question

#mc_answers_by_user_by_session <- mc_answers %>%
 # group_by(session,user) %>%
  #summarize(percentage_correct = mean(score))

#Generate the percentage of time  users were in state 2 or 18 in the non-question slides
#6 for (connected and visible) and 14 for (connected, visible and focus).

get_percentage_distraction_per_user_per_session <- function(session){
  
  sid <- session$id
  #Check how many users change focus in the questions from the userstates
  
  session_students_info <- readRDS(paste('cache/user_state_', sid, '.rds', sep=""))
  
  #Take the proper start and end time and also remove the break time
  session_students_info <- session_students_info %>%
    filter(time > session$startDate & time < session$endDate) %>%
    filter(time < session$breakStartDate | time > session$breakEndDate )
  
  users_duration_nonexercise <- session_students_info %>%
    group_by(user) %>%
    mutate(duration = n()) %>%
    distinct(duration)
  
  users_distracted_percentage <- session_students_info %>%
    filter(!grepl("asq",slide)) %>%
    group_by(user) %>%
    filter(connected == 'TRUE' & visible == 'TRUE' & focus == 'TRUE')%>%
    mutate(duration_distracted = n()) %>%
    distinct(duration_distracted) %>%
    right_join(users_duration_nonexercise,by=c("user")) %>%
    ungroup %>%
    group_by(user) %>%
    summarise(distracted_percentage = ifelse(!is.na(duration_distracted),
                                             ((duration_distracted/duration)*100), 
                                             0))
  
  #Make new column for the session and attach
  users_distracted_percentage$session <- 
    rep(sid,nrow(users_distracted_percentage))
  
  users_distracted_percentage
}

all <- list()

for (i in 1:nrow(sessions)){
  session <-  sessions[i, ]
  temp  <- get_percentage_distraction_per_user_per_session(session)
  all[[i]] <- temp
}

distraction_percentage_all_sessions <- bind_rows(all)

#Join the distraction percentage and percentage correct of each user in each session

distraction_percentage_mean_score <- distraction_percentage_all_sessions %>%
  right_join(ae_si_data_user,by=c("session","user")) %>%
  ungroup %>%
  group_by(session) %>%
  left_join(sessions_with_title, by= c("session" = "id"))

# Make the scatterplot for each session to show the relation between 
# distraction percentage and percentage of responses correct

ggplot(data = distraction_percentage_mean_score, 
       aes(x=distracted_percentage, y=mean_active_engagement)) + 
  geom_point() + 
  scale_x_continuous(limits=c(0,100)) + 
  xlab("distraction percentage") +
  ylab("percentage of answers submitted") +
  facet_wrap(~title.y,ncol = 4,scales="free_x")

# Find the correlation and p-value of distraction percentage 
# and percentage of responses correct

distraction_percentage_mean_score_correlation <- distraction_percentage_mean_score %>%
  group_by(title.y) %>%
  summarize(kendal_tau = cor.test(distracted_percentage, mean_active_engagement, method = c("kendall"))$estimate,
            kendal_p.value = cor.test(distracted_percentage, mean_active_engagement, method = c("kendall"))$p.value,
            spearman_tau = cor.test(distracted_percentage, mean_active_engagement, method = c("spearman"))$estimate,
            spearman_p.value = cor.test(distracted_percentage, mean_active_engagement, method = c("spearman"))$p.value) %>%
  distinct(title.y,spearman_tau,spearman_p.value,kendal_tau,kendal_p.value)


############################################################################################

#Active Engagement (submit and input)

all <- list()

for( i in 1:nrow(sessions)) {
  session <-  sessions[i, ]
  sid <- session$id
  temp <- calc_mean_active_engagement_per_user(session, use_input_events=F) 
  all[[i]] <- temp
}

ae_si_data_user <- bind_rows(all) %>%
  left_join(sessions_with_title, by = c("session" = "id"))

##########################################################################################

## Generate the events for the three users in JS

library(data.table)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(tidyr)
library(asqr)
library(here)
library(scales)


latestdump <- readChar(here("latestdumpdate.txt"), 10)
sessions <- readRDS(paste(here('cache/sessions_analysis_'), latestdump,'.rds', sep=""))
sessionevents_viewers <- readRDS(paste(here('cache/sessionevents_viewers_'), latestdump,'.rds', sep=""))
sessionevents_presenters <- readRDS(paste(here('cache/sessionevents_presenters_'), latestdump,'.rds', sep=""))

# add slideshow names
slideshows <- readRDS(paste(here('cache/slideshows_'), latestdump,'.rds', sep=""))
questions <- readRDS(paste(here('cache/questions_'), latestdump,'.rds', sep=""))
answers <- readRDS(paste(here('cache/answers_with_start_date_'), latestdump,'.rds', sep=""))
answers <- answers %>% filter(session %in% sessions$id)

# questions with start and end slides and times
questions_study_start_end <- readRDS(here(paste("cache/questions_study_start_end_",latestdump, ".rds", sep="")))

whitelistentries <- readRDS(paste(here('cache/whitelistentries_'), latestdump,'.rds', sep=""))
slide_transitions <- readRDS(paste(here('cache/slide_transisions_'), latestdump,'.rds', sep=""))


# mean of events per user per session
events_per_session <- sessionevents_viewers %>%
  group_by(session, user) %>%
  summarize(total_events=n())

mean (events_per_session$total_events)


# draw three user timeline
# get only the sessionevents for the specific session
session <- sessions[3, ]
sid <- session$id
sessioneventsN <- sessionevents_viewers %>%
  filter(session==sid)

dt<- readRDS(paste("cache/user_state_", session$id, ".rds", sep=""));
dt <- as.data.table(dt)
#  merge states

dt<- dt %>%
  filter(time <= session$endDate) %>%
  mutate(hasExercise = ifelse(hasExercise == TRUE, "exercise", "normal"))

if(! is.na(session$breakStartDate)){
  dt<- dt %>%
    mutate(hasExercise = ifelse(time >= session$breakStartDate &
                                  time <=session$breakEndDate, "break", hasExercise))
}

levels(sessioneventsN$type)[levels(sessioneventsN$type)=="folo-connected"] <- "connected"
levels(sessioneventsN$type)[levels(sessioneventsN$type)=="folo-disconnected"] <- "disconnected"

slide_transitions <-  slide_transitions %>% filter(session==sid)
slideTransisionsWithEx <- slide_transitions[which(slide_transitions$hasExercise == TRUE),]
slideTransisionsWithoutEx <- slide_transitions[which(slide_transitions$hasExercise == FALSE),]

# get users of session
whitelistentriesN <-  whitelistentries %>% filter(session==sid, role=="viewer") %>% collect()


foo <- sessioneventsN %>% 
  group_by(user) %>%
  summarize(total = n())


# 582eb1b8ae9a6f0025693604 33events for html
#u1 <- "582eb1b8ae9a6f0025693604"
#66 events for JS
u1 <- "5832a6c48bd89e023bd9283d"
#u1<-"5832a9098bd89e023bd95238"

# 582eb222ae9a6f00256940da 542 events for html
#u2 <- "582eb222ae9a6f00256940da"
# 820 events for JS
u2 <- "5832a5f18bd89e023bd91dc0"
#u2<-"5832a4058bd89e023bd9193a"

# 582eb0d5ae9a6f00256925b7 1071 events for html
#u3 <- "582eb0d5ae9a6f00256925b7"
#1836 events for JS
u3 <- "5832a6798bd89e023bd92500"
#u3<-"5832a5638bd89e023bd91abd"

levels(sessioneventsN$type)[levels(sessioneventsN$type)=="answer-submitted"] <- "answersubmit"
levels(sessioneventsN$type)[levels(sessioneventsN$type)=="viewer-idle"] <- "idle"
levels(sessioneventsN$type)[levels(sessioneventsN$type)=="exercise-submit"] <- "exercisesubmit"

threeuserdata <- sessioneventsN %>% 
  filter(type %in% c("tabhidden", "tabvisible", "connected", "disconnected", "answersubmit",
                     "focusin","focusout","idle","input","questioninput","windowblur",
                     "windowfocus"))%>%
  filter(user %in% c(u1, u2, u3))%>% 
  mutate(user.name = ifelse(user == u1, "student 1",  ifelse(user == u2, "student 2", "student 3")))

# 
# View(sessioneventsN %>%
#        group_by(user) %>%
#        summarize(count = n()))

my_col_scheme = c("answersubmit" = "#ff9900" ,
                  "copy" = "#1F78B4" ,
                  "cut" = "#B2DF8A" ,
                  "exercisesubmit" = "#990000" ,
                  "exerciseblur" = "#FB9A99" ,
                  "exercisefocus" = "#FFF2CC" ,
                  "focusin" = "#F4CCCC" ,
                  "focusout" = "#E06666" ,
                  "connected" = "#A6CEE3" ,
                  "disconnected" = "#660000",
                  "input" = "#FF7F00" ,
                  "paste" = "#CAB2D6" ,
                  "questioninput" = "#6A3D9A" ,
                  "tabhidden" = "#FDBF6F" ,
                  "tabvisible" = "#33A02C" ,
                  "idle" = "#F6B26B" ,
                  "windowblur" = "#B15928" ,
                  "windowfocus" = "#FFFF99",
                  "slideenter" = "#C0C0C0",
                  "slideleave" = "#A0A0A0")


# Grid of single viewer event plots together with steps
r.labels <- c("normal" = "without question",
              "exercise" = "with question",
              "break" = "break")

r.scale <- c("exercise" = "#CD66E0",
             "normal" = "#FFFFFF",
             "break" = "#000000")   


threeuserdata_f <- threeuserdata %>% 
  filter(time >= as.POSIXct("2016-11-21 10:21:00 CET") & time <= as.POSIXct("2016-11-21 10:33:00 CET")) #%>%
  # remove question input events from non exercise slides
  #filter(type !="questioninput" | (type =="questioninput" & slide=="asq-1"))

dt <- dt %>% filter(time >= as.POSIXct("2016-11-21 10:21:00 CET") & time <= as.POSIXct("2016-11-21 10:33:00 CET"))
p <- ggplot(data = threeuserdata_f) +
  # geom_segment(mapping=aes(x=time,y=0, xend=time, yend=1, colour=type), size=0.5, alpha=0.5) +
  # geom_rect(data=slideTransisionsWithEx, aes(xmin=time_start, xmax=time_end, ymin=-Inf, ymax=Inf), fill="#fef8ff", alpha=1, inherit.aes= FALSE) +
  
  # HACK ALERT `dt` comes from a dataframe like in viewerstate which has the hasExercise vector!!!
  geom_rect(data=dt, aes(xmin=time,xmax=time+1,ymin=-0.05 ,ymax=0, fill=hasExercise)) +
  geom_vline(data=slideTransisionsWithoutEx, aes(xintercept = as.numeric(time_start)),colour="#7c7c7c", linetype=1, size=0.4, alpha=0.5) +
  geom_vline(data=slideTransisionsWithEx, aes(xintercept = as.numeric(time_start)),colour="lightblue", linetype=1, size=0.4, alpha=0.5) +
  geom_point(mapping=aes(x=time, y=0.5, colour=type), shape=16, size=2) +
  # scale_color_manual(values=my_col_scheme, name ="event type") + 
  scale_colour_manual(values=my_col_scheme, name ="event type") + 
  scale_fill_manual(values = r.scale,  name="slide", labels = r.labels) +
  # scale_x_continuous(name="time and slide") +
  # scale_y_continuous(limits = c(-0.05, 2)) +
  scale_x_datetime(labels = date_format("%H:%M:%S", tz = "CET"),
                   breaks = date_breaks(paste(30, "sec")),
                   limits = c(as.POSIXct("2016-11-21 10:21:00 CET"),
                              as.POSIXct("2016-11-21 10:33:00 CET"))) +
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.direction = "horizontal", 
        legend.position = "top",
        legend.box = "horizontal",
        legend.key = element_rect(colour = "black")) +
  ylim(c(-0.05, 1)) +
  coord_fixed(ratio = 100) +
  facet_grid(user.name~., scales="free")

p

ggsave("plots/paper/three_viewer_events_JS_last.pdf", height=3.8, width=10, limitsize=FALSE)

##################

### To perform the Wilcoxon's signed rank test to find the effect of diffculty of questions
### and correctness of response

correctness_difficulty_mcnemar <- mc_answers %>%
  left_join((mc_answers_by_q %>% select(question,difficulty_level)), 
            by = "question") %>%
  ungroup %>%
  group_by(session.x) %>%
  mutate(EC = ifelse((score == 100 & difficulty_level == "Easy"),T,F)) %>%
  mutate(EW = ifelse((score == 0 & difficulty_level == "Easy"),T,F)) %>%
  mutate(HC = ifelse((score == 100 & difficulty_level == "Hard"),T,F)) %>%
  mutate(HW = ifelse((score == 0 & difficulty_level == "Hard"),T,F))

#Count the number of questions in each category satisfying each user

correctness_difficulty_mcnemar_calc_eachuser <- correctness_difficulty_mcnemar %>%
  group_by(session.x,user) %>%
  summarize(count_EC = sum(EC == "TRUE"),
           count_EW = sum(EW == "TRUE"),
           count_HC = sum(HC == "TRUE"),
           count_HW = sum(HW == "TRUE")) %>%
  left_join(sessions_with_title, by = c("session.x" = "id"))

#Perform Wilcoxon's signed rank test on the paired data: count_EC & count_HC

correctness_difficulty_mcnemar_calc_eachuser_temp <- 
  correctness_difficulty_mcnemar_calc_eachuser %>%
  filter(title == "ER logical design") %>%
  group_by(user) %>%
  summarize(count_EC_per = count_EC/4,
            count_HC_per = count_HC/4)

wilcox.test(correctness_difficulty_mcnemar_calc_eachuser_temp$count_EC_per,
            correctness_difficulty_mcnemar_calc_eachuser_temp$count_HC_per,paired = TRUE)


## Do not perform McNemar's test as we have multiple questions
correctness_difficulty_mcnemar_calc <- correctness_difficulty_mcnemar %>%
  group_by(session.x) %>%
  mutate(count_EC = sum(EC == "TRUE")) %>%
  mutate(count_EW = sum(EW == "TRUE")) %>%
  mutate(count_HC = sum(HC == "TRUE")) %>%
  mutate(count_HW = sum(HW == "TRUE")) %>%
  ungroup %>%
  select(session = session.x,count_EC,count_EW,count_HC,count_HW) %>%
  distinct(session,count_EC,count_EW,count_HC,count_HW) %>%
  left_join(sessions_with_title, by = c("session" = "id"))

mcnemar_test_correctness_difficulty <-
  matrix(c(124, 115, 200, 234),
         nrow = 2,
         dimnames = list("Difficulty" = c("Easy", "Hard"),
                         "Correctness" = c("Correct", "Wrong")))
  
mcnemar.test(mcnemar_test_correctness_difficulty)

#########################################################################

# Find the effect of difficulty on the students taking external help 
# and not taking external help



##########################################################################

# Effect of difficulty on the reaction time of students

wilcoxon_difficulty_reaction_easy <- updated_answers_reaction_time_performance %>%
  ungroup %>%
  select(answeree,title,normalized_reaction_time,difficulty_level) %>%
  filter(title == "HTTP") %>%
  filter(difficulty_level == "Easy")

wilcoxon_difficulty_reaction_hard <- updated_answers_reaction_time_performance %>%
  ungroup %>%
  select(answeree,title,normalized_reaction_time,difficulty_level) %>%
  filter(title == "HTTP") %>%
  filter(difficulty_level == "Hard")

mood_test <- updated_answers_reaction_time_performance %>%
  ungroup %>%
  select(title,normalized_reaction_time,difficulty_level) %>%
  filter(title == "ER logical design")

mood_test$difficulty_level <- as.factor(mood_test$difficulty_level)

mood.medtest(mood_test$normalized_reaction_time,mood_test$difficulty_level)
  
leveneTest(mood_test$normalized_reaction_time,mood_test$difficulty_level)
  

wilcoxon_difficulty_reaction$difficulty_level <- 
  as.factor(wilcoxon_difficulty_reaction$difficulty_level)

wilcoxon_difficulty_reaction_wide = as.data.frame(wilcoxon_difficulty_reaction)
wilcoxon_difficulty_reaction_wide$row <- 1:nrow(wilcoxon_difficulty_reaction_wide)

wilcoxon_difficulty_reaction_wide <- wilcoxon_difficulty_reaction_wide %>%
  select(normalized_reaction_time,difficulty_level,row)

wilcoxon_difficulty_reaction_wide <- spread(wilcoxon_difficulty_reaction_wide,
                                       difficulty_level,normalized_reaction_time)
  
wilcox.test(correctness_difficulty_mcnemar_calc_eachuser_temp$count_EC_per,
            correctness_difficulty_mcnemar_calc_eachuser_temp$count_HC_per,paired = TRUE)

#############################################################################################

## Check for the number of students taking external help 

get_focus_percentage_per_question_per_session <- function(session){
  
  sid <- session$id
  #Check how many users change focus in the questions from the userstates
  
  session_students_info <- readRDS(paste('cache/user_state_', sid, '.rds', sep=""))
  
  session_students_info <- session_students_info %>%
    filter(time > session$startDate & time < session$endDate)
  
  #The focus percentage out of the time spent from start to submit 
  #in the exercise for each user
  #As we select the substring containing "asq", users connected to questions
  
  ##Calculate percentage of focus from the total students that submitted answers
  
  users_submit <- session_students_info %>%
    filter(grepl("asq",slide)) %>%
    group_by(user,slide) %>%
    filter(submitted == "TRUE" & state != 1) %>%
    ungroup %>%
    group_by(slide) %>%
    distinct(user)
  
  users_activity_before_submit <- session_students_info %>%
    filter(grepl("asq",slide)) %>%
    group_by(user,slide) %>%
    filter(submitted == "FALSE" & state != 1) %>%
    ungroup %>%
    right_join(users_submit,by=c("slide","user")) %>%
    group_by(user,slide) %>%
    mutate(count =  n()) %>%
    ungroup %>%
    group_by(user) %>%
    distinct(slide,count)
  
  users_focus_activity_before_submit <- session_students_info %>%
    group_by(user,slide) %>%
    filter(focus == "TRUE" & submitted == "FALSE" & state != 1) %>%
    mutate(count_focus = n()) %>%
    ungroup %>%
    group_by(user) %>%
    distinct(slide,count_focus) %>%
    right_join(users_activity_before_submit,by=c("slide","user")) %>%
    #To account for the users who are never focussed before submitting
    mutate(count_focus_percentage_submit_only =
             ifelse(is.na(count_focus), 0, count_focus/count)) %>%
    ungroup %>%
    group_by(slide) %>%
    mutate(count_responses = n())
  
  session_question_focus_percentage <- users_focus_activity_before_submit %>%
    group_by(slide) %>%
    filter(count_focus_percentage_submit_only == 1) %>%
    mutate(count_always_focus=n()) %>%
    ungroup %>%
    distinct(slide,count_always_focus)
  
  session_question_focus_percentage <- users_focus_activity_before_submit %>%
    select(slide, count_responses) %>%
    distinct(count_responses) %>%
    ungroup %>%
    left_join(session_question_focus_percentage, by="slide") %>%
    #To account for the zero focus percentage questions
    mutate(focus_percentage_submit_only = 
             ifelse(is.na(count_always_focus), 0, 
                    count_always_focus/count_responses)) 
  
  
  #Make new column for the session and attach
  session_question_focus_percentage$session <- 
    rep(sid,nrow(session_question_focus_percentage))
  
  session_question_focus_percentage
  
}

all <- list()

for (i in 1:nrow(sessions)){
  session <-  sessions[i, ]
  temp  <- get_focus_percentage_per_question_per_session(session)
  all[[i]] <- temp
}

external_help_all_sessions <- bind_rows(all)

external_help_all_sessions_difficulty <- external_help_all_sessions %>%
  left_join(focus_percentage_all_sessions_difficulty,by = c("session","slide"))

external_help_all_sessions_difficulty_calc_easy <- external_help_all_sessions_difficulty %>%
  ungroup %>%
  group_by(session) %>%
  filter(difficulty == "Easy") %>%
  mutate(count_EN = sum(count_always_focus)) %>%
  mutate(count_EH = sum(count_responses-count_always_focus)) %>%
  distinct(title,count_EN,count_EH)

external_help_all_sessions_difficulty_hard <- external_help_all_sessions_difficulty %>%
  ungroup %>%
  group_by(session) %>%
  filter(difficulty == "Hard") %>%
  mutate(count_HN = sum(count_always_focus)) %>%
  mutate(count_HH = sum(count_responses-count_always_focus)) %>%
  distinct(title,count_HN,count_HH)
  
external_help_all_sessions_difficulty_calc <- 
  left_join(external_help_all_sessions_difficulty_calc_easy, 
            external_help_all_sessions_difficulty_hard, by = c("session","title"))

external_help_all_sessions_difficulty_calc <- external_help_all_sessions_difficulty_calc %>%
  select(-session)

# Now perform McNemar's test

mcnemar_test_help_difficulty <-
  matrix(c(77,
           127,
           254,
           307),
         nrow = 2,
         dimnames = list("Difficulty" = c("Easy", "Hard"),
                         "Help" = c("No", "Yes")))

mcnemar.test(mcnemar_test_help_difficulty)

#######################################################################################

## Slide distance with the performance
## Updated answers reaction time performance contains the performance, reaction time and difficulty

updated_answers_reaction_time_performance_calc <- updated_answers_reaction_time_performance

updated_answers_reaction_time_performance_calc$relative_position <- 
  as.factor(updated_answers_reaction_time_performance_calc$relative_position)

updated_answers_reaction_time_performance_calc_test <- 
  updated_answers_reaction_time_performance_calc %>%
  group_by(title) %>%
  summarize(kruskal_pvalue = kruskal.test(score,relative_position)$p.value)

dunn_test_position_performance <- updated_answers_reaction_time_performance_calc %>%
  filter(title == "NoSQL") %>%
  filter(!is.na(relative_position))

unn_test_position_performance_calc <- dunn_test_position_performance %>%
  group_by(answeree) %>%
  summarize(correct_min = sum(relative_position == 2 & score == 100),
            correct_max = sum(relative_position == 10 & score == 100))
            #correct_3 = sum(relative_position == 3 & score == 100))

dunn_test_position_performance_calc <- unn_test_position_performance_calc %>%
  group_by(answeree) %>%
  summarize(correct_min_per = correct_min/1,
            correct_max_per = correct_max/1)
            #correct_3_per = correct_3/2)

#Perform Wilcoxon's signed rank test for paired data to find differences between the 
#performances of each student in questions with varying distances

wilcox.test(dunn_test_position_performance_calc$correct_min_per,
            dunn_test_position_performance_calc$correct_max_per,
            paired = TRUE, alternative = "greater")

#Friedman's test to detect differences in the max and min distance population

 dunn_test_position_performance_calc <- as.matrix(dunn_test_position_performance_calc)
 friedman.test(dunn_test_position_performance_calc)

# If x,y alternative = "greater" means that x is greater than y if p < 0.01
# http://tutorial.math.trinity.edu/content/wilcoxon-signed-rank-test-r

dunn.test(dunn_test_position_performance$score,
          dunn_test_position_performance$relative_position,method = "Bonferroni")

######################################################################################

# Perform Kruskal test to check differences between 3 group of users i.e. 1) <=20%,
# 2) >20% & <=40% and 3) >40% of the attention percentage with the participation
# in the in-class questions

kruskal_attention_participation <- distraction_percentage_mean_score %>%
  mutate(category = ifelse((distracted_percentage <= 25),"A","B")) %>%
          #(ifelse((distracted_percentage > 20 & distracted_percentage <= 40),"B","C")))) %>%
  filter(!is.na(category))

kruskal_attention_participation$category <- as.factor(kruskal_attention_participation$category)

kruskal_attention_participation_pvalue <- kruskal_attention_participation %>%
  group_by(title.y) %>%
  summarize(kruskal_pvalue = kruskal.test(mean_active_engagement,category)$p.value,
            count_A = sum(category == 'A'),
            count_B = sum(category == 'B'))
            #count_C = sum(category == 'C'))

#To do posthoc tests by using Dunn's test with Bonferroni's correction

kruskal_attention_participation_posthoc <- kruskal_attention_participation %>%
  filter(title.y == "NoSQL")
dunn.test(kruskal_attention_participation_posthoc$mean_active_engagement,
          kruskal_attention_participation_posthoc$category, method="bonferroni")

#Kolmogorov Smirnov test: ks.test()

###########################################################################################

## Check the percentage of responses a student makes i.e. percentage for each student EH, HH
## Number of questions taking external help in easy and hard

get_focus_percentage_per_question_per_user <- function(session){
  
  sid <- session$id
  #Check how many users change focus in the questions from the userstates
  
  session_students_info <- readRDS(paste('cache/user_state_', sid, '.rds', sep=""))
  
  session_students_info <- session_students_info %>%
    filter(time > session$startDate & time < session$endDate)
  
  #The focus percentage out of the time spent from start to submit 
  #in the exercise for each user
  #As we select the substring containing "asq", users connected to questions
  
  ##Calculate percentage of focus from the total students that submitted answers
  
  users_submit <- session_students_info %>%
    filter(grepl("asq",slide)) %>%
    group_by(user,slide) %>%
    filter(submitted == "TRUE" & state != 1) %>%
    ungroup %>%
    group_by(slide) %>%
    distinct(user)
  
  users_activity_before_submit <- session_students_info %>%
    filter(grepl("asq",slide)) %>%
    group_by(user,slide) %>%
    filter(submitted == "FALSE" & state != 1) %>%
    ungroup %>%
    right_join(users_submit,by=c("slide","user")) %>%
    group_by(user,slide) %>%
    mutate(count =  n()) %>%
    ungroup %>%
    group_by(user) %>%
    distinct(slide,count)
  
  users_focus_activity_before_submit <- session_students_info %>%
    group_by(user,slide) %>%
    filter(focus == "TRUE" & submitted == "FALSE" & state != 1) %>%
    mutate(count_focus = n()) %>%
    ungroup %>%
    group_by(user) %>%
    distinct(slide,count_focus) %>%
    right_join(users_activity_before_submit,by=c("slide","user")) %>%
    #To account for the users who are never focussed before submitting
    mutate(count_focus_percentage_submit_only =
             ifelse(is.na(count_focus), 0, count_focus/count)) %>%
    ungroup %>%
    group_by(slide) %>%
    mutate(count_responses = n())
  
  session_question_focus_percentage <- users_focus_activity_before_submit %>%
    group_by(slide,user) %>%
    summarize(count_nohelp = sum(count_focus_percentage_submit_only == 1))
  
  #Make new column for the session and attach
  session_question_focus_percentage$session <- 
    rep(sid,nrow(session_question_focus_percentage))
  
  session_question_focus_percentage
  
}

all <- list()

for (i in 1:nrow(sessions)){
  session <-  sessions[i, ]
  temp  <- get_focus_percentage_per_question_per_user(session)
  all[[i]] <- temp
}

external_help_all_questions_per_user <- bind_rows(all)

external_help_all_sessions_difficulty <- external_help_all_questions_per_user %>%
  left_join(focus_percentage_all_sessions_difficulty,by = c("session","slide"))


external_help_all_sessions_difficulty_calc <- external_help_all_sessions_difficulty %>%
  ungroup %>%
  group_by(session,user) %>%
  summarize(count_EH = sum(count_nohelp == 0 & difficulty == "Easy"),
            count_HH = sum(count_nohelp == 0 & difficulty == "Hard")) %>%
  left_join(sessions_with_title, by = c("session" = "id"))

#Wilcoxon's signed rank test to find significant differences between taking external help
#in easy and difficult questions
 
external_help_all_sessions_difficulty_calc_temp <- 
  external_help_all_sessions_difficulty_calc %>%
  filter(title == "ER logical design") %>%
  group_by(user) %>%
  summarize(count_EH_per = count_EH/4,
            count_HH_per = count_HH/4)

wilcox.test(external_help_all_sessions_difficulty_calc_temp$count_EH_per,
            external_help_all_sessions_difficulty_calc_temp$count_HH_per,paired = TRUE)

#############################################################################################

#Attach question strategy and instructor type to the sessions_with_title dataframe
#formed from a new dataframe

sessions_with_title_strategy_instructor <- sessions_with_title

# "b" for burst, "i" for increasing and "u" for uniform
strategy <- c("b","u","i","i","b","u","b","u","b","u","u","b","u","b")
# c for Claudia and a for Alessandro
instructor <- c("c","c","c","a","c","c","a","a","c","a","c","a","a","a")

strategy_instructor <- c("bc","uc","ic","ia","bc","uc","ba","ua","bc","ua","uc","ba","ua","ba")

total_mcq <- c(10,6,8,5,8,7,7,7,9,3,8,13,8,8)

sessions_with_title_strategy_instructor$total_mcq <- total_mcq
sessions_with_title_strategy_instructor$strategy <- strategy
sessions_with_title_strategy_instructor$instructor <- instructor
sessions_with_title_strategy_instructor$combined <- strategy_instructor

updated_answers_reaction_time_difficulty_strategy_instructor <- 
  updated_answers_reaction_time_performance %>%
  left_join(sessions_with_title_strategy_instructor, by = c("session" = "id")) %>%
  filter(strategy != "i")

updated_answers_reaction_time_difficulty_strategy_instructor$combined <-
  as.factor(updated_answers_reaction_time_difficulty_strategy_instructor$combined)

kruskal.test(updated_answers_reaction_time_difficulty_strategy_instructor$normalized_reaction_time,
             updated_answers_reaction_time_difficulty_strategy_instructor$combined)

#Post-hoc test to check for differences between normalized reaction time between instructors 
#and quiz strategies

dunn.test(updated_answers_reaction_time_difficulty_strategy_instructor$normalized_reaction_time,
          updated_answers_reaction_time_difficulty_strategy_instructor$combined,
          method = "bonferroni")
  

kruskal_test_performance_significance <- 
  updated_answers_reaction_time_difficulty_strategy_instructor %>%
  group_by(title.y,answeree) %>%
  mutate(score_total = sum(score == 100)) %>%
  distinct(score_total,combined,total_mcq)

kruskal_test_performance_significance <- kruskal_test_performance_significance %>%
  mutate(score_percent = score_total/total_mcq)


kruskal.test(kruskal_test_performance_significance$score_percent,
             kruskal_test_performance_significance$combined)

dunn.test(kruskal_test_performance_significance$score_percent,
          kruskal_test_performance_significance$combined,
          method = "bonferroni")


####################################################################################


  










  