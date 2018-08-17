#Make the model for NBA MVP points using the caret package
#Libraries & Datasets-----------------
library(dplyr)
library(caret)
library(readr)
library(randomForest)

mvp_votes_read <- read_csv("C:/Users/berghammeraj/Desktop/Projects/NBA/Data/mvp_vote_team.csv")

mvp_votes <- mvp_votes_read[complete.cases(mvp_votes_read), c("PTS", "TRB", "AST", "STL", "BLK",
                                                              "fg_perc", "three_perc", "ft_perc",
                                                              "ws_48", "W_L_perc", "SRS", "Pace",
                                                              "DRtg", "make_playoffs_binary",
                                                              "win_finals_binary", "mvp_pts","Player",
                                                              "start_year")]

write_csv(mvp_votes, "C:/Users/berghammeraj/Desktop/Projects/NBA/Data/mvp_vote_team_2.csv")

train_index <- createDataPartition(mvp_votes$mvp_pts, p=0.8, list=FALSE)

train_mvp <- mvp_votes[train_index,]
test_mvp  <- mvp_votes[-train_index,]

#Start Building this model without normalizing the dependent

#Start with linear regression for interpretability
lm <- lm(mvp_pts~., data=train_mvp)

summary(lm)
confint(lm)
plot(lm)

pred <- predict(lm, newdata= test_mvp)

lm_pred <- cbind(test_mvp, pred)

SSE <- sum((lm_pred$mvp_pts-lm_pred$pred)^2)
MSE <- SSE/(length(pred)-1)
RMSE <- sqrt(MSE)

SST <- sum((lm_pred$mvp_pts - mean(lm_pred$mvp_pts))^2)
SSM <- SST-SSE

Rsq <- SSM/SST
adj_r <- 1-((1-Rsq)*(length(pred)-1)/(length(pred)-(length(lm$coefficients)-1)-1))

#Start building a Caret Model----------------------

ctrl <- trainControl(method="repeatedcv", repeats=3)

mars_fit <- train(mvp_pts~.,
                  data=train_mvp,
                  method="earth",
                  preProc=c("center", "scale"),
                  tuneLength=15,
                  trControl=ctrl)

ggplot(mars_fit)

  #RMSE - 194.33 at 13 terms

svm_fit <- train(mvp_pts~.,
                 data=train_mvp,
                 method="svmLinear",
                 preProc=c("center", "scale"),
                 tuneLength=15,
                 trControl=ctrl)

  #RMSE - 225.2233

rf_fit <- train(mvp_pts~.,
                 data=train_mvp,
                 method="rf",
                 preProc=c("center", "scale"),
                 tuneLength=15,
                 trControl=ctrl)

  #RMSE - 192.2071

#Try Random Forest Again without the Caret package

fit.rf_2 <- randomForest(mvp_pts~.-start_year -Player, data=train_mvp, ntree=1000)

rf.pred <- predict(fit.rf_2, test_mvp)
rf.df   <- cbind(test_mvp, rf.pred)
rf.df$resid <- rf.df$mvp_pts - rf.df$rf.pred
rf.df$resid_sq <- rf.df$resid^2

RMSE.rf <- sqrt(sum(rf.df$resid_sq)/length(rf.df$resid_sq)-1)

  #RMSE of 162... WAYYYY BETTER
  #Sort the new data frame

#So.... This doesn't work to review the correct answers.... We must do the split by year
#Or we we end up with missing winners in the datasets

#Re-split the datasets and re-run the random forest-------

#Need to pick 8 random years to separate the dataset

train_year_index <- data.frame(sample(1:38, 38), c(1979:2016))

names(train_year_index) <- c("rand", "year")

train_mvp_year <- mvp_votes %>% 
  left_join(train_year_index, by=c("start_year" = "year")) %>% 
  filter(rand <= 30)

test_mvp_year <- mvp_votes %>% 
  left_join(train_year_index, by=c("start_year" = "year")) %>% 
  filter(rand > 30)


fit.rf_2_year <- randomForest(mvp_pts~.-start_year -Player, data=train_mvp_year, ntree=1000)

rf.pred.year <- predict(fit.rf_2_year, test_mvp_year)
rf.df.year   <- cbind(test_mvp_year, rf.pred.year)
rf.df.year$resid <- rf.df.year$mvp_pts - rf.df.year$rf.pred
rf.df.year$resid_sq <- rf.df.year$resid^2

RMSE.rf.year <- sqrt(sum(rf.df.year$resid_sq)/length(rf.df.year$resid_sq)-1)

train_rf.pred <- predict(fit.rf_2_year, train_mvp_year)
train_rf.df   <- cbind(train_mvp_year, train_rf.pred)

varImp <- data.frame(fit.rf_2_year$importance)

#What?
