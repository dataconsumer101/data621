library(dplyr)
library(ggplot2)
library(tidyr)

options(scipen = 9)

set.seed(123)

eval <- read.csv('https://raw.githubusercontent.com/dataconsumer101/data621/main/hw1/moneyball-evaluation-data.csv', stringsAsFactors = F)

train <- read.csv('https://raw.githubusercontent.com/dataconsumer101/data621/main/hw1/moneyball-training-data.csv', stringsAsFactors = F)

str(train)

head(train)

train <- select(train, -INDEX)

names(train) <- lapply(names(train), tolower)
names(eval) <- lapply(names(eval), tolower)


summary(train)

### team_batting_so

tbs_avg <- mean(train$team_batting_so, na.rm = T)
tbs_med <- median(train$team_batting_so, na.rm = T)

# check distribution
ggplot(train, aes(x = team_batting_so)) +
  geom_histogram() +
  geom_vline(xintercept = c(tbs_avg, tbs_med), color = c('blue', 'red'))

# use average to fill in NA
train[is.na(train$team_batting_so),]$team_batting_so <- tbs_avg
eval[is.na(eval$team_batting_so),]$team_batting_so <- tbs_avg

summary(train)

### team_baserun_sb

tbsb_avg <- mean(train$team_baserun_sb, na.rm = T)
tbsb_med <- median(train$team_baserun_sb, na.rm = T)

# check distribution
ggplot(train, aes(x = team_baserun_sb)) +
  geom_density() +
  geom_vline(xintercept = c(tbsb_avg, tbsb_med, 70), color = c('blue', 'red', 'green'))

# use most common value to fill in NA
train[is.na(train$team_baserun_sb),]$team_baserun_sb <- 70
eval[is.na(eval$team_baserun_sb),]$team_baserun_sb <- 70


summary(train)

### team_baserun_cs

tbcs_avg <- mean(train$team_baserun_cs, na.rm = T)
tbcs_med <- median(train$team_baserun_cs, na.rm = T)

# check distribution
ggplot(train, aes(x = team_baserun_cs)) +
  geom_density() +
  geom_vline(xintercept = c(tbcs_avg, tbcs_med), color = c('blue', 'red'))

# use most common value to fill in NA
train[is.na(train$team_baserun_cs),]$team_baserun_cs <- tbcs_med
eval[is.na(eval$team_baserun_cs),]$team_baserun_cs <- tbcs_med


summary(train)

### team_pitching_so

tpso_avg <- mean(train$team_pitching_so, na.rm = T)
tpso_med <- median(train$team_pitching_so, na.rm = T)

# check distribution
ggplot(train, aes(x = team_pitching_so)) +
  geom_density() +
  geom_vline(xintercept = c(tpso_avg, tpso_med), color = c('blue', 'red')) +
  scale_x_continuous(limits = c(0,2000))

# use average to fill in NA
train[is.na(train$team_pitching_so),]$team_pitching_so <- tpso_avg
eval[is.na(eval$team_pitching_so),]$team_pitching_so <- tpso_avg


summary(train)

### team_fielding_dp

tfdp_avg <- mean(train$team_fielding_dp, na.rm = T)
tfdp_med <- median(train$team_fielding_dp, na.rm = T)

# check distribution
ggplot(train, aes(x = team_fielding_dp)) +
  geom_density() +
  geom_vline(xintercept = c(tfdp_avg, tfdp_med), color = c('blue', 'red')) 

# use median to fill in NA
train[is.na(train$team_fielding_dp),]$team_fielding_dp <- tfdp_med
eval[is.na(eval$team_fielding_dp),]$team_fielding_dp <- tfdp_med


summary(train)

### team_batting_hbp

tbhbp_avg <- mean(train$team_batting_hbp, na.rm = T)
tbhbp_med <- median(train$team_batting_hbp, na.rm = T)

# check distribution
ggplot(train, aes(x = team_batting_hbp)) +
  geom_density() +
  geom_vline(xintercept = c(tbhbp_avg, tbhbp_med, 54.5), color = c('blue', 'red', 'green')) 

# use median to fill in NA
train[is.na(train$team_batting_hbp),]$team_batting_hbp <- 54.5
eval[is.na(eval$team_batting_hbp),]$team_batting_hbp <- 54.5


summary(eval)




lm1 <- lm(target_wins ~ ., data = train)

summary(lm1)


lm2 <- lm(target_wins ~ team_fielding_e + team_fielding_dp, data = train)

summary(lm2)


lm3 <- lm(target_wins ~ . - team_batting_so - team_pitching_so - team_pitching_hr - team_batting_hr, data = train)

summary(lm3)


lm4 <- lm(target_wins ~ team_batting_h + team_pitching_h + team_pitching_bb + team_fielding_e + team_fielding_dp, data = train)

summary(lm4)


train$pred1 <- predict(lm1, train)
train$pred2 <- predict(lm2, train)
train$pred3 <- predict(lm3, train)
train$pred4 <- predict(lm4, train)



preds <- select(train, target_wins, pred1, pred2, pred3, pred4) %>%
  gather('pred', 'val', -target_wins)

ggplot(preds, aes(x = target_wins, y = val, color = pred)) +
  geom_point() +
  scale_x_continuous(limits = c(0,150)) +
  scale_y_continuous(limits = c(0,150)) +
  facet_wrap(~pred)










