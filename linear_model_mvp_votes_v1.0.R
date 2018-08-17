#Run a logistic regression to predict/explain NBA MVP
#Libraries and datasets------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(car)

mvp_votes <- read_csv("C:/Users/berghammeraj/Desktop/Projects/NBA/Data/mvp_vote.csv")
team_data <- read_csv("C:/Users/berghammeraj/Desktop/Projects/NBA/Data/nba_team_history_model.csv")

mvp_votes <- mvp_votes[,-1]

team_data <- team_data[,-c(2,3,16,19:29)]

#Make keys on the datasets-----------------

mvp_votes$key <- paste0(mvp_votes$start_year, "_", mvp_votes$Tm)
team_data$key <- paste0(team_data$Start_Year, "_", team_data$Tm_abbrev)

mvp_votes <- mvp_votes %>% 
  left_join(team_data, by=c("key", "key"))

  #Make mvp_binary variable

mvp <- mvp_votes %>% 
  group_by(start_year) %>% 
  summarise(mvp_winner = max(mvp_pts))

mvp_votes <- mvp_votes %>% 
  left_join(mvp, by=c("start_year", "start_year")) %>% 
  mutate(mvp_binary = ifelse(mvp_pts==mvp_winner, 1, 0))

write_csv(mvp_votes, "C:/Users/berghammeraj/Desktop/Projects/NBA/Data/mvp_vote_team.csv")

#Explore Data-------------------------------------

ggplot(mvp_votes, aes(PTS, mvp_pts, color=as.factor(mvp_binary))) +
  geom_point() +
  facet_wrap(~start_year) +
  stat_smooth(method="lm", color='blue')


ggplot(mvp_votes, aes(PTS, mvp_pts, color=as.factor(mvp_binary))) +
  geom_point() +
  stat_smooth(method="lm", color='blue')

  #There are a lot of one vote observations and if we remove them it may change a lot

mvp_votes_filt <- mvp_votes %>% 
  filter(mvp_pts >= 4)


ggplot(mvp_votes_filt, aes(PTS, mvp_pts, color=as.factor(mvp_binary))) +
  geom_point() +
  facet_wrap(~start_year) +
  stat_smooth(method="lm", color='blue')


ggplot(mvp_votes_filt, aes(PTS, mvp_pts, color=as.factor(mvp_binary))) +
  geom_point() +
  stat_smooth(method="lm", color='blue')

  #This helps to reveal some trends.  When included in the regression we should be careful
  #When ommiting observations (we shouldn't)

ggplot(mvp_votes_filt, aes(TRB, mvp_pts, color=as.factor(mvp_binary))) +
  geom_point() +
  facet_wrap(~start_year) +
  stat_smooth(method="lm", color='blue')


ggplot(mvp_votes_filt, aes(TRB, mvp_pts, color=as.factor(mvp_binary))) +
  geom_point() +
  stat_smooth(method="lm", color='blue')

#Make correlation matrix----------------------
temp <- mvp_votes_filt[complete.cases(mvp_votes_filt),-c(21,22,23,24,1,39,27)]

cor <- cor(select_if(temp, is.numeric))
corrplot(cor)

#Run a basic MLR-----------------------

temp_lm <- mvp_votes_filt[complete.cases(mvp_votes_filt),-c(21,32,34,36,22,23,24,1,39,40,27,2,3,4,5,7,8,9,10,25,26,29)]

lm <- lm(mvp_pts~., data=temp_lm)

  #Residual vs. fitted plots show that there is likely a need to transform the dependent
  #There are observations with negative predicted values.  Standardizing may be a solution
  #We could interpret this through standard deviations of relative votes

#Remove more multicollinearity-----------------------
temp_lm$Off_df_rtg <- temp_lm$ORtg/temp_lm$DRtg
temp_lm_filt <- temp_lm[,-c(10,15,19)]

lm <- lm(mvp_pts~., data=temp_lm_filt)
vif(lm)