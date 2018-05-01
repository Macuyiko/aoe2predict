library(dplyr)
library(tidyr)
library(dbplyr)
library(rlang)
library(glue)
library(ggplot2)
library(readr)
options(scipen=999)

final_table <- read_csv('prepared_data.csv.gz')
final_table$player_1_wins <- factor(final_table$player_1_wins)

# =========
# Modelling
# =========

# We're now ready to model:
# - player_1_wins is our target
# - match_id and current_time are ignored as features
# - all other columns are diff columns acting as features

library(caret)

# We'll work on a sample to speed the modelling up a bit
# Note that, by nature of this prediction task, we want to have a balanced target outcome

sample <- final_table %>% group_by(player_1_wins) %>% sample_n(size = 30000) %>% as.data.frame
sample$player_1_wins %>% table

partition <- createDataPartition(sample$player_1_wins)

sample.train <- sample[partition$Resample1,]
sample.test <- sample[-partition$Resample1,]
sample.train$player_1_wins %>% table
sample.test$player_1_wins %>% table

# There is another trick we can apply to make the modelling more robust
# Note that every instance can represent two instances by flipping the sign of all diff stats
# And flipping the target outcome

mirror_data <- function(ds, prefix='diff_', target='player_1_wins') {
  ds.m <- ds %>% 
    mutate_at(vars(starts_with(prefix)), (function(x) -x)) %>% 
    mutate_at(vars(target), (function(x) factor(abs( as.numeric(levels(x))[x] - 1))))
  return(bind_rows(ds, ds.m))
}

# Only apply this on the train set

sample.train <- mirror_data(sample.train)
sample.train$player_1_wins %>% table
sample.test$player_1_wins %>% table

# Small function to report results

report_results <- function(data, predfunc, nt=200) {
  data$predictions <- predfunc(data)
  print(confusionMatrix(data$predictions, data$player_1_wins))
  plot <- data %>% 
    mutate(gr_time=ntile(.$current_time, nt)) %>%
    mutate(gr_correct=as.numeric(predictions == player_1_wins)) %>%
    group_by(gr_time) %>%
    summarise(gr_acc=sum(gr_correct)/n()) %>%
    ggplot(aes(x=gr_time,y=gr_acc)) + geom_line() + stat_smooth()
  data$predictions <- NULL
  return(plot)
}

# First try with logistic regression

model.glm <- glm(player_1_wins ~ ., family=binomial(link='logit'), 
                 data=sample.train %>% select(-match_id, -current_time))

report_results(sample.train, function(x) {
  (predict(model.glm, type='response', newdata=x) >= 0.5) %>%
    as.numeric %>% factor
})

report_results(sample.test, function(x) {
  (predict(model.glm, type='response', newdata=x) >= 0.5) %>%
    as.numeric %>% factor
})

# About 71% accuracy on train and test

# Let's see if random forest does better

library(randomForest)

model.rf <- randomForest(player_1_wins ~ .,
                         data=sample.train %>% select(-match_id, -current_time),
                         ntree=300, importance=T)

report_results(sample.train, function(x) {
  (predict(model.rf, type='response', newdata=x)) %>%
    factor
})

report_results(sample.test, function(x) {
  (predict(model.rf, type='response', newdata=x)) %>%
    factor
})

report_results(final_table, function(x) {
  (predict(model.rf, type='response', newdata=x)) %>%
    factor
})

# 83% accuracy on test: let's go with this model (same accuracy on complete set)

# ==========
# Inspection
# ==========

imp <- importance(model.rf, type=1, scale = F)
ggplot(data.frame(imp=imp[,1], var=names(imp[,1])) %>% filter(imp>=0.01), aes(x=var, y=imp)) + 
  geom_bar(stat='identity') +
  coord_flip()

partialPlot(model.rf, sample.train %>% select(-match_id, -current_time, -player_1_wins),
            x.var='diff_score_total')

partialPlot(model.rf, sample.train %>% select(-match_id, -current_time, -player_1_wins),
            x.var='diff_population_total')

# Try some test matches

library(zoo)

show_match <- function(data, match_id, predfunc) {
  # Average: 20 ticks per minute
  match <- data %>% filter(match_id==!!match_id) %>%
    arrange(current_time) %>%
    mutate(prediction=predfunc(.)) %>%
    mutate(correct=ifelse((prediction>=0.5)==(player_1_wins=='1'), 'correct', 'incorrect')) %>%
    mutate(predmavg=rollapply(prediction,40,mean,align='right',fill=0.5))
  clr <- ifelse('0'==match$player_1_wins[1], 'red', 'blue')
  match %>%
    ggplot(aes(x=current_time/60000, y=prediction, color=correct)) + 
    geom_line(aes(group=1), size=1) +
    geom_line(aes(x=current_time/60000, y=predmavg), color='purple', size=1) +
    geom_line(aes(x=current_time/60000, y=((diff_score_total/max(abs(diff_score_total)))+1)/2), color='orange') +
    scale_x_continuous(name="Time") +
    scale_y_continuous(name="Probability", sec.axis=sec_axis(~.*2-1, name = "Score Balance")) +
    geom_hline(yintercept = 0.5) +
    theme_minimal()
}

show_match(final_table, 16720682, function(x) {
  predict(model.rf, type='prob', newdata=x)[,'1']
})

show_match(final_table, 16722679, function(x) {
  predict(model.rf, type='prob', newdata=x)[,'1']
})

db.matches %>% filter(match_id == 16722679) %>% collect %>% select(match_winner, contains("name"))
extra_info %>% filter(match_id == 16722679)
diff_table %>% filter(match_id == 16722679) %>% select(contains("name"))

# Get an idea of accuracy on the last state

last_outcome <- function(data, predfunc) {
  data$predictions <- predfunc(data)
  data <- data %>% group_by(match_id) %>% filter(current_time == max(current_time))
  print(confusionMatrix(data$predictions, data$player_1_wins))
  data <- data %>% filter(predictions != player_1_wins)
  return(data)
}

inc <- last_outcome(final_table, function(x) {
  predict(model.rf, type='response', newdata=x) %>%
    factor
})

# 94% accuracy

inc %>% head

# This seems completely weird. After investigation, seems like the wrong person resigned

show_match(final_table, 16721231, function(x) {
  predict(model.rf, type='prob', newdata=x)[,'1']
})

db.matches %>% filter(match_id == 16721231) %>% collect %>% select(match_winner, contains("name"))
extra_info %>% filter(match_id == 16721231)
diff_table %>% filter(match_id == 16721231) %>% select(contains("name"))

# Of course, everyone can predict the outcome when looking at the score, rating, vil count and total pop
# Let's see how much worse such a model would be

model.rf.simple <- randomForest(player_1_wins ~ .,
                         data=sample.train %>% select(diff_score_total, diff_rating, 
                                                      diff_population_total, diff_population_civilian, 
                                                      player_1_wins),
                         ntree=300, importance=T)

report_results(sample.train, function(x) {
  (predict(model.rf.simple, type='response', newdata=x)) %>%
    factor
})

report_results(sample.test, function(x) {
  (predict(model.rf.simple, type='response', newdata=x)) %>%
    factor
})

report_results(final_table, function(x) {
  (predict(model.rf.simple, type='response', newdata=x)) %>%
    factor
})

# 79% accuracy, so we are doing a bit better

inc.simple <- last_outcome(final_table, function(x) {
  predict(model.rf.simple, type='response', newdata=x) %>%
    factor
})

# And 91% accuracy on the final step

# What we would like to know, however, is how fast we are in terms of predicting the outcome
# For instance as a percentage of the total match running time
# We do this both for the predictions as is and a smoothed variant

better_rate <- function(data, predfunc) {
  data$predictions <- predfunc(data)
  data <- data %>% group_by(match_id) %>% 
    mutate(outcome = predictions >= 0.5) %>%
    mutate(correct = outcome == (player_1_wins == '1')) %>%
    mutate(correct_before = correct & lag(correct)) %>%
    mutate(ratio = current_time/max(current_time)) %>%
    select(match_id, current_time, predictions, correct, correct_before, ratio) %>%
    filter(correct, !correct_before) %>%
    arrange(desc(ratio)) %>% slice(1)
  return(data)
}

show_match(final_table, 16720682, function(x) {
  predict(model.rf, type='prob', newdata=x)[,'1']
})

show_match(final_table, 17582436, function(x) {
  predict(model.rf, type='prob', newdata=x)[,'1']
})

predrate.normal <- better_rate(final_table, function(x) {
  predict(model.rf, type='prob', newdata=x)[,'1']
}) %>% pull(ratio)

predrate.normalsmooth <- better_rate(final_table, function(x) {
  predict(model.rf, type='prob', newdata=x)[,'1'] %>%
    rollapply(40, mean, align='right', fill=0.5)
}) %>% pull(ratio)

predrate.simple <- better_rate(final_table, function(x) {
  predict(model.rf.simple, type='prob', newdata=x)[,'1']
}) %>% pull(ratio)

predrate.simplesmooth <- better_rate(final_table, function(x) {
  predict(model.rf.simple, type='prob', newdata=x)[,'1'] %>%
    rollapply(40, mean, align='right', fill=0.5)
}) %>% pull(ratio)

predrate <- data.frame(type='normal', val=predrate.normal) %>%
  bind_rows(data.frame(type='snormal', val=predrate.normalsmooth)) %>%
  bind_rows(data.frame(type='simple', val=predrate.simple)) %>%
  bind_rows(data.frame(type='ssimple', val=predrate.simplesmooth))


ggplot(predrate, aes(x=val)) + 
  geom_density(aes(fill=type), alpha=0.7) + theme_minimal()

# ==========
# Deployment
# ==========

# Save model

save(model.rf, file="model.rda")

# Ideas for expansion:
# - Age differences and time difference in terms reached
# - Incorporate tick time as feature: there might be interactions
# - Incorporate civs as features
# - Incorporate trend information: might accomodate for smoothing effects
