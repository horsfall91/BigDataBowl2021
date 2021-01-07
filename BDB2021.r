#Loading pre-installed libraries
setwd("~/Big_Data_Bowl_2021")
library(tidyverse)
library(gifski)
library(gganimate)
library(cowplot)
library(repr)

#turning off warnings
options(warn=-1)

#setting plot width and height
options(repr.plot.width=15, repr.plot.height = 10)

#########################
### NON-TRACKING DATA ###
#########################

#includes schedule info for games
df_games <- read_csv("games.csv", col_types = cols())

#includes play-by-play info on specific plays
df_plays <- read_csv("plays.csv", col_types = cols())

#includes background info for players
df_players <- read_csv("players.csv", col_types = cols())


#####################
### TRACKING DATA ###
#####################

#weeks of NFL season
weeks <- seq(1, 17)

#blank dataframe to store tracking data
df_tracking <- data.frame()

#iterating through all weeks
for(w in weeks){
  
  #temperory dataframe used for reading week for given iteration
  df_tracking_temp <- read_csv(paste0("week",w,".csv"), col_types = cols())
  
  #storing temporary dataframe in full season dataframe
  df_tracking <- bind_rows(df_tracking_temp, df_tracking)                            
  
}

########################
### MUTATE DIRECTION ###
########################
#Standardizing tracking data so its always in direction of offense vs raw on-field coordinates.
df_tracking <- df_tracking %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y))

######################
### PLAY ANIMATION ###
######################

## declaring values for field coordinates
# General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

#picking a random play
#set.seed(1)

example_play <- df_plays %>%
  select(gameId, playId, playDescription) %>% 
  sample_n(1)


#merging games data to play
example_play <- inner_join(example_play,
                           df_games,
                           by = c("gameId" = "gameId"))

#merging tracking data to play
example_play <- inner_join(example_play,
                           df_tracking,
                           by = c("gameId" = "gameId",
                                  "playId" = "playId"))


#colors used for plot - using colors of team
#DEN vs OAK
cols_fill <- c("#FB4F14", "#663300", "#A5ACAF")
cols_col <- c("#000000", "#663300", "#000000")

plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(example_play$playDescription[1])))

# Specific boundaries for a given play
ymin <- max(round(min(example_play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example_play$x, na.rm = TRUE) + 10, -1), 120)

#hash marks
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

# ### COMMENT
# #plotting
# ggplot() +
#   
#   #setting size and color parameters
#   scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
#   scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
#   scale_fill_manual(values = cols_fill, guide = FALSE) + 
#   scale_colour_manual(values = cols_col, guide = FALSE) +
#   
#   #adding hash marks
#   annotate("text", x = df.hash$x[df.hash$x < 55/2], 
#            y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
#   annotate("text", x = df.hash$x[df.hash$x > 55/2], 
#            y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
#   
#   #adding yard lines
#   annotate("segment", x = xmin, 
#            y = seq(max(10, ymin), min(ymax, 110), by = 5), 
#            xend =  xmax, 
#            yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
#   
#   #adding field yardline text
#   annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
#            label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
#            angle = 270, size = 4) + 
#   annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
#            label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
#            angle = 90, size = 4) + 
#   
#   #adding field exterior
#   annotate("segment", x = c(xmin, xmin, xmax, xmax), 
#            y = c(ymin, ymax, ymax, ymin), 
#            xend = c(xmin, xmax, xmax, xmin), 
#            yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
#   
#   #adding players
#   geom_point(data = example_play, aes(x = (xmax-y),
#                                       y = x, 
#                                       shape = team,
#                                       fill = team,
#                                       group = nflId,
#                                       size = team,
#                                       colour = team), 
#              alpha = 0.7) +  
#   
#   #adding jersey numbers
#   geom_text(data = example_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
#             vjust = 0.36, size = 3.5) + 
#   
#   #applying plot limits
#   ylim(ymin, ymax) + 
#   coord_fixed() +
#   
#   #applying theme
#   theme_nothing() + 
#   theme(plot.title = element_text()) +
#   
#   #titling plot with play description
#   labs(title = plot_title) +
#   
#   #setting animation parameters
#   transition_time(frameId)  +
#   ease_aes('linear') + 
#   NULL
# 
# #filtering for specific attributes
# example_play <- df_plays %>%
#   filter(#4th quarter
#     quarter == 4,
#     
#     #shotgun formation
#     offenseFormation == "SHOTGUN",
#     
#     #complete passes
#     passResult == "C",
#     
#     #third down
#     down == 3) %>%
#   
#   select(gameId, playId, playDescription) %>%
#   
#   sample_n(1)
# 
# #merging tracking data to play
# example_play <- inner_join(example_play,
#                            df_tracking,
#                            by = c("gameId" = "gameId",
#                                   "playId" = "playId"))
# 
# #colors used for plot - using colors of team
# #TB vs CIN
# cols_fill <- c("#FB4F14", "#663300", "#A71930")
# cols_col <- c("#000000", "#663300", "#000000")
# 
# plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(example_play$playDescription[1])))
# 
# 
# # Specific boundaries for a given play
# ymin <- max(round(min(example_play$x, na.rm = TRUE) - 10, -1), 0)
# ymax <- min(round(max(example_play$x, na.rm = TRUE) + 10, -1), 120)
# 
# #hash marks
# df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
# df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
# df.hash <- df.hash %>% filter(y < ymax, y > ymin)
# 
# 
# #plotting
# ggplot() +
#   
#   #setting size and color parameters
#   scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
#   scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
#   scale_fill_manual(values = cols_fill, guide = FALSE) + 
#   scale_colour_manual(values = cols_col, guide = FALSE) +
#   
#   #adding hash marks
#   annotate("text", x = df.hash$x[df.hash$x < 55/2], 
#            y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
#   annotate("text", x = df.hash$x[df.hash$x > 55/2], 
#            y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
#   
#   #adding yard lines
#   annotate("segment", x = xmin, 
#            y = seq(max(10, ymin), min(ymax, 110), by = 5), 
#            xend =  xmax, 
#            yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
#   
#   #adding field yardline text
#   annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
#            label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
#            angle = 270, size = 4) + 
#   annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
#            label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
#            angle = 90, size = 4) + 
#   
#   #adding field exterior
#   annotate("segment", x = c(xmin, xmin, xmax, xmax), 
#            y = c(ymin, ymax, ymax, ymin), 
#            xend = c(xmin, xmax, xmax, xmin), 
#            yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
#   
#   #adding players
#   geom_point(data = example_play, aes(x = (xmax-y),
#                                       y = x, 
#                                       shape = team,
#                                       fill = team,
#                                       group = nflId,
#                                       size = team,
#                                       colour = team), 
#              alpha = 0.7) +  
#   
#   #adding jersey numbers
#   geom_text(data = example_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
#             vjust = 0.36, size = 3.5) + 
#   
#   #applying plot limits
#   ylim(ymin, ymax) + 
#   coord_fixed() +
#   
#   #applying theme
#   theme_nothing() + 
#   theme(plot.title = element_text()) +
#   
#   #titling plot with play description
#   labs(title = plot_title) +
#   
#   #setting animation parameters
#   transition_time(frameId)  +
#   ease_aes('linear') + 
#   NULL


#merging plays and tracking data
df_merged <- inner_join(df_games,
                        df_plays,
                        by = c("gameId" = "gameId"))

#merging games data to previously merged frame
df_merged <- inner_join(df_merged,
                        df_tracking,
                        by = c("gameId" = "gameId",
                               "playId" = "playId"))

#defining events that designate pass arrival
passArivalEvents <- c('pass_outcome_caught',
                      'pass_arrived',
                      'pass_outcome_incomplete',
                      'pass_outcome_interception',
                      'pass_outcome_touchdown')


df_distanceToFootball <- df_merged %>%
  
  #determining side of ball
  mutate(sideOfBall = ifelse(#if tracked player is home and home has ball
    ((team == "home") &
       (possessionTeam == homeTeamAbbr)) |
      
      #if tracked player is away and away has ball
      ((team == "away") &
         (possessionTeam == visitorTeamAbbr)),
    
    
    #if either condition is true, offense
    "offense",
    
    #if neither condition is true, defense
    "defense"),
    
    #defining defensive team
    defensiveTeam = ifelse(possessionTeam == homeTeamAbbr,
                           visitorTeamAbbr,
                           homeTeamAbbr)) %>%
  
  
  #using NE on defense only
  #filter(defensiveTeam == "NE", sideOfBall == "defense") %>%
  filter(sideOfBall == "defense") %>%
  
  #grouping by game, play and frame
  group_by(gameId, playId, frameId) %>%
  
  #checking if football reading is in frame
  mutate(footballInPlay = sum(displayName == "Football") > 0) %>%
  
  #using only frames with football marked; some plays its missing
  filter(footballInPlay) %>%
  
  #adding x and y location of football as columns
  mutate(xFootball = x[displayName == "Football"],
         yFootball = y[displayName == "Football"]) %>%
  
  #ungrouping
  ungroup() %>%
  
  #grouping by game and play
  group_by(gameId, playId) %>%
  
  #selecting frames that contain pass arrival events
  filter(event %in% passArivalEvents) %>%
  
  #selecting first frame with in case there are multiple
  filter(frameId == min(frameId)) %>%
  
  #calculating distance to football
  mutate(
    
    distToFootballAtBallArrival = sqrt((x - xFootball) ^ 2 +
                                         (y - yFootball) ^ 2)
    
  )
df_distanceToFootball

#calculating the average distance to the football
averageDistToFootball <- df_distanceToFootball %>%   
  
  #selecting players with valid nfl ID (excluding football)
  filter(!is.na(nflId)) %>%
  
  #grouping by player's id
  group_by(nflId) %>%
  
  #taking mean of distance to football
  summarize(avgDistToFootballAtBallArrival = mean(distToFootballAtBallArrival)) %>%
  
  #joining to players data
  inner_join(df_players,
             by = c("nflId" = "nflId"))

averageDistToFootball

#displaying a few random results
#set.seed(1)

averageDistToFootball  %>%
  #plotting results
  ggplot(aes(reorder(displayName, 
                     -avgDistToFootballAtBallArrival),
             avgDistToFootballAtBallArrival)) +
  
  #using bar chart
  geom_bar(stat = 'identity', color = 'blue', fill = 'lightblue') +
  
  #applying theme
  theme_bw() +
  theme(text = element_text(size=22)) +
  
  #labeling axis
  xlab('') +
  
  ylab("Avg Distance to Football at Pass Arrival") + 
  
  #flipping coordinates
  coord_flip() +
  
  #titling plot
  ggtitle("Avg Distance to Football at Pass Arrival by Player")

averageDistToFootball[order(averageDistToFootball$avgDistToFootballAtBallArrival),]
unique(averageDistToFootball$position)
# we should filter to CB, ILB, OLB, SS, FS, MLB, DB, LB, S

numberOfPlaysClosestDefender <- df_distanceToFootball %>%   
  
  #selecting players with valid nfl ID (excluding football)
  filter(!is.na(nflId)) %>%
  
  #grouping by NFL Id
  group_by(gameId, playId) %>%
  
  #filtering for closest defender to ball
  filter(distToFootballAtBallArrival == min(distToFootballAtBallArrival)) %>%
  
  #ungrouping
  group_by(nflId) %>%
  
  summarize(numberOfPlaysAsClosestDefender = n()) %>%
  
  #joining to players data
  inner_join(df_players,
             by = c("nflId" = "nflId"))

#displaying a few random results
#set.seed(1)

numberOfPlaysClosestDefender  %>%
  #plotting results
  ggplot(aes(reorder(displayName, 
                     numberOfPlaysAsClosestDefender),
             numberOfPlaysAsClosestDefender)) +
  
  #using bar chart
  geom_bar(stat = 'identity', color = 'blue', fill = 'lightblue') +
  
  #applying theme
  theme_bw() +
  theme(text = element_text(size=22)) +
  
  #labeling axis
  xlab('') +
  
  ylab("# of Plays As closest Defender to Football at Pass Arrival") + 
  
  #flipping coordinates
  coord_flip() +
  
  #titling plot
  ggtitle("# of Plays As closest Defender to Ball at Pass Arrival By Player")

numberOfPlaysClosestDefender[order(-numberOfPlaysClosestDefender$numberOfPlaysAsClosestDefender),]

numberOfPlaysClosestDefenderPerPlay <- df_distanceToFootball %>%   
  
  #selecting players with valid nfl ID (excluding football)
  filter(!is.na(nflId)) %>%
  
  #grouping by NFL Id
  group_by(gameId, playId) %>%
  
  #filtering for closest defender to ball
  mutate(isClosestDefender = distToFootballAtBallArrival == min(distToFootballAtBallArrival)) %>%
  
  #ungrouping
  ungroup() %>%
  
  #grouping by defender's id
  group_by(nflId) %>%
  
  #calculatign value of interest
  summarize(numberOfPlaysAsClosestDefenderPerPlay = sum(isClosestDefender) / n()) %>%
  
  #joining to players data
  inner_join(df_players,
             by = c("nflId" = "nflId"))

#displaying a few random results
#set.seed(1)

numberOfPlaysClosestDefenderPerPlay  %>%
  #plotting results
  ggplot(aes(reorder(displayName, 
                     numberOfPlaysAsClosestDefenderPerPlay), 
             numberOfPlaysAsClosestDefenderPerPlay)) +
  
  #using bar chart
  geom_bar(stat = 'identity', color = 'blue', fill = 'lightblue') +
  
  #applying theme
  theme_bw() +
  theme(text = element_text(size=22)) +
  
  #labeling axis
  xlab('') +
  
  ylab("# of Plays As Closest Defender to Football at Pass Arrival  Per Play") + 
  
  #flipping coordinates
  coord_flip() +
  
  #titling plot
  ggtitle("# of Plays As Closest Defender to Ball at Pass Arrival Per Play By Player")
numberOfPlaysClosestDefenderPerPlay[order(numberOfPlaysClosestDefenderPerPlay$numberOfPlaysAsClosestDefenderPerPlay),]

completionPercentageAsClosest <- df_distanceToFootball %>%   
  
  #selecting players with valid nfl ID (excluding football)
  filter(!is.na(nflId),
         
         #removing defensive PI
         !isDefensivePI) %>%
  
  #grouping by NFL Id
  group_by(gameId, playId) %>%
  
  #filtering for closest defender to ball
  filter(distToFootballAtBallArrival == 
           min(distToFootballAtBallArrival)) %>%
  
  #ungrouping
  group_by(nflId) %>%
  
  
  summarize(compPercent = sum(passResult == "C") / n()) %>%
  
  #joining to players data
  inner_join(df_players,
             by = c("nflId" = "nflId"))

#displaying a few random results
#set.seed(1)

completionPercentageAsClosest  %>%
  
  #plotting results
  ggplot(aes(reorder(displayName, 
                     -compPercent), 
             compPercent)) +
  
  #using bar chart
  geom_bar(stat = 'identity', color = 'blue', fill = 'lightblue') +
  
  #applying theme
  theme_bw() +
  theme(text = element_text(size=22)) +
  
  #labeling axis
  xlab('') +
  
  ylab("Allowed Comp % As Closest Defender to Football at Pass Arrival") + 
  
  #changing to percentage scale
  scale_y_continuous(labels = scales::percent) +
  
  #flipping coordinates
  coord_flip() +
  
  
  
  #titling plot
  ggtitle("Allowed Comp % As Closest Defender to Ball at Pass Arrival By Player")

#converting heights to inches
ht_vec <- completionPercentageAsClosest$height
COPY <- as.tibble(data.frame(completionPercentageAsClosest))

height_inches <- list()
for (h in ht_vec) {
  if (str_detect(h,'-')) {
    #first number (feet) plus second number
    inches <- as.integer(strsplit(h,'-')[[1]][1])*12 + as.integer(strsplit(h,'-')[[1]][2])
  }
  else {inches <- as.integer(h)}
  height_inches <- append(height_inches,inches)
}

COPY <- as.tibble(cbind(COPY,unlist(height_inches)))
COPY <- rename(COPY, heightInches = 'unlist(height_inches)')
COPY <- inner_join(COPY, numberOfPlaysClosestDefender[, c("nflId","numberOfPlaysAsClosestDefender")], by = c("nflId" = "nflId"))

#covert age to years
birth_vec <- COPY$birthDate

age_years <- list()
for (b in birth_vec) {
  if (str_detect(b,'/')) {
    #first number (feet) plus second number
    birthday <- as.Date(b, format = "%m/%d/%Y")
  }
  else {birthday <- as.Date(b)}
  age_years <- append(age_years,as.numeric(as.Date("2018-01-01") - birthday)/365)
}

COPY <- as.tibble(cbind(COPY,unlist(age_years)))
COPY <- rename(COPY, ageYears = 'unlist(age_years)')
COPY

speed <- df_tracking %>%
  group_by(nflId) %>%
  filter(s > 0) %>%
  summarise(avg_speed = mean(s)*2.0455)

COPY <- inner_join(COPY,speed, by = c("nflId" = "nflId"))

accel <- df_tracking %>%
  group_by(nflId) %>%
  filter(a > 0) %>%
  summarise(avg_accel = mean(a)*2.0455)

COPY <- inner_join(COPY,accel, by = c("nflId" = "nflId"))

maxspeed <- df_tracking %>%
  group_by(nflId) %>%
  filter(s > 0) %>%
  summarise(max_speed = max(s)*2.0455)

maxaccel <- df_tracking %>%
  group_by(nflId) %>%
  filter(a > 0) %>%
  summarise(max_accel = max(a)*2.0455)

COPY <- inner_join(COPY,maxspeed, by = c("nflId" = "nflId"))
COPY <- inner_join(COPY,maxaccel, by = c("nflId" = "nflId"))
COPY

COPY <- COPY %>% 
  filter(COPY$numberOfPlaysAsClosestDefender > 47)

COPY

######################
conferences = read.csv("collegeDivs.csv", header = TRUE)
COPY <- left_join(COPY,conferences, by = c("collegeName"))
COPY$Conference[is.na(COPY$Conference)] <- "other_conf"
COPY <- 
  COPY %>% 
  mutate(value = 1) %>% 
  spread(Conference, value,  fill = 0) 

COPY$isPowerFive <- ifelse(COPY$other_conf == 1, 0, 1)

Positions = read.csv("positionGroups.csv", header = TRUE)
COPY <- left_join(COPY,Positions, by = c("position"))

COPY$positionGroup[is.na(COPY$positionGroup)] <- "other_pos"
COPY <- 
  COPY %>% 
  mutate(value = 1) %>% 
  spread(positionGroup, value,  fill = 0) 

######################
library(glmnet)
library(qpcR)
library(Metrics)
#num_COPY <- COPY[, c("nflId", "compPercent", "weight", "heightInches", "numberOfPlaysAsClosestDefender", "ageYears", "avg_speed", "avg_accel"
#                     ,"max_speed", "max_accel", "ACC", "Big12", "BigTen", "Pac12", "SEC", "other_conf", "isPowerFive", "Cornerbacks", "Linebackers"
#                     ,"Safeties", "other_pos")]

num_COPY <- COPY[, c("compPercent", "weight", "heightInches", "numberOfPlaysAsClosestDefender", "ageYears", "avg_speed", "avg_accel"
                     ,"max_speed", "max_accel", "ACC", "Big12", "BigTen", "Pac12", "SEC", "other_conf", "isPowerFive", "Cornerbacks", "Linebackers"
                     ,"Safeties")]
num_COPY
dim(num_COPY)

smp_size <- floor(0.7 * nrow(num_COPY))
split_tt <- sample(seq_len(nrow(num_COPY)), size = smp_size)
train <- num_COPY[split_tt, ]
test <- num_COPY[-split_tt, ]

train <- as.data.frame(train)
test <- as.data.frame(test)

stepwise <- step(lm(compPercent ~., data = train), direction = 'both')
step_sum <- summary(stepwise)
step_sum

pred <- predict(stepwise, test[-1])
pred
rmse(test$compPercent,pred)

#######################
library(caret)
library(glmnet)

#COPY_sc <- apply(num_COPY, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
#COPY_sc

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
COPY_sc <- num_COPY 
COPY_sc[,-1] <- apply(num_COPY[-1], MARGIN = 2, FUN = range01)
COPY_sc

smp_size <- floor(0.7 * nrow(COPY_sc))
split_tt <- sample(seq_len(nrow(COPY_sc)), size = smp_size)
train_sc <- COPY_sc[split_tt, ]
test_sc <- COPY_sc[-split_tt, ]

x_tr <- as.matrix(train_sc[,2:19])
y_tr <- as.matrix(train_sc[,1])

x_ts <- as.matrix(test_sc[,2:19])
y_ts <- as.matrix(test_sc[,1])

cowboy <- glmnet(x_tr, y_tr, alpha = 1, family = "mgaussian")
cv.cowboy <- cv.glmnet(x_tr, y_tr, alpha=1)
plot(cv.cowboy)

best.lambda <- cv.cowboy$lambda.min
best.lambda
coef(cv.cowboy, s = "lambda.min")
cv.cowboy

yhat <- predict(cv.cowboy, s=cv.cowboy$lambda.min, newx=x_ts)
mse <- mean((y_ts - yhat)^2)
yhat

###linear model on unscaled data, using LASSO parameters
lasso_lm <- lm(compPercent ~ weight + heightInches + avg_speed + avg_accel + BigTen + SEC + Cornerbacks + Linebackers, data = num_COPY)
lasso_lm <- lm(compPercent ~avg_speed + avg_accel + BigTen + SEC + Linebackers, data = num_COPY)

######################
library(glmnetUtils)
elastic_net <- cva.glmnet(x_tr, y_tr, alpha=seq(0.1, 0.9, len=9))
elastic_net
plot(elastic_net)


coef(elastic_net, s = "lambda.min")

######################

##############################################
##### better way to do it!!! #################
##############################################

bins = read.csv("playsDataWithBinsNum.csv", header = TRUE)
bins <- as.data.frame(bins)

binsTrimmed <- bins[, c("avg_speed", "avg_accel", "BigTen", "ACC", "Linebackers","performanceBin123")]

check_accuracy = function(X){
  predicted <- rep(0,(nrow(binsTrimmed))) # predictions: start with a vector of all zeros
  # for each row, estimate its response based on the other rows
  
  for (i in 1:nrow(binsTrimmed)){
    
    # data[-i] means we remove row i of the data when finding nearest neighbors...
    #...otherwise, it'll be its own nearest neighbor!
    
    model=kknn(performanceBin123~.,binsTrimmed[-i,],binsTrimmed[i,],k=X, scale = TRUE) # use scaled data
    
    # record whether the prediction is at least 0.5 (round to one) or less than 0.5 (round to zero)
    predicted[i] <- as.integer(fitted(model)+0.5) # round off to 0 or 1
  }

  # calculate fraction of correct predictions
  acc = sum(predicted == binsTrimmed[,dim(binsTrimmed)[2]]) / nrow(binsTrimmed)
  return(acc)
}

#
# Now call the function for values of k from 1 to 20 (you could try higher values of k too)
#

accurracy=rep(0,20) # set up a vector of 20 zeros to start
for (X in 1:20){
  accurracy[X] = check_accuracy(X)
}
#
# report accuracies
#

plot(accurracy)
title("K-Nearest-Neighbors")
accurracy


################
new_df <- bins[, c("avg_speed", "avg_accel", "BigTen", "ACC", "Linebackers","performanceBin123")]
s <- sample(156,120)
training <- new_df[s,]
testing <- new_df[-s,]
head(training)

modelk <- train.kknn(performanceBin123 ~ ., data = training, kmax = 20, scale = TRUE)
modelk

prediction <- predict(modelk, testing[,-6])
prediction
prediction.round <- round(prediction)
prediction.round

CM <- table(testing[, 6], prediction.round)
CM

accuracy <- (sum(diag(CM)))/sum(CM)
accuracy
