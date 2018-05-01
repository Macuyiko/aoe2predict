library(dplyr)
library(tidyr)
library(rlang)
library(glue)

load("model.rda")

grepgsub <- function(l, pattern="^data_players_\\d+_(.*)$") {
  grepped <- grep(pattern, l)
  subbed <- gsub(pattern, "\\1", l[grepped])
  result <- as.list(unique(subbed))
  return(result)
}

diff_expression <- function(l, pattern="^data_players_\\d+_(.*)$",
                            expression="(data_players_0_{x} - data_players_1_{x})",
                            prefix="diff") {
  name_list <- grepgsub(l, pattern=pattern)
  expr_list <- name_list %>%
    lapply(function(x) parse_quosure(glue(expression))) %>% 
    setNames(paste(prefix, name_list, sep = "_")) 
  return(expr_list)
}

prepare_instance <- function(instance) {
  diff_table <- instance %>% filter(type=='tick') %>% 
    select(current_time, 
           starts_with("data_players_0_"), 
           starts_with("data_players_1_")) %>% 
    mutate(data_players_0_kills=data_players_0_kills_2,
           data_players_0_razes=data_players_0_razes_2,
           data_players_1_kills=data_players_1_kills_1,
           data_players_1_razes=data_players_1_razes_1) %>%
    select(-contains('kills_'), -contains('razes_')) %>%
    mutate(player_1_name=data_players_0_name, player_2_name=data_players_1_name) %>%
    select(-data_players_0_name, -data_players_1_name) %>%
    mutate_at(vars(starts_with("data_")), funs(as.character)) %>%
    mutate_at(vars(starts_with("data_")), funs(as.numeric)) %>%
    mutate(!!!diff_expression(colnames(.))) %>%
    mutate(current_time=as.numeric(current_time)) %>%
    select(-starts_with('data_players_'), -diff_id)
  return(diff_table)
}

#* @post /predict
prediction <- function(instance){
  instance.df <- data.frame(matrix(unlist(instance), nrow=1, byrow=T))
  colnames(instance.df) <- names(instance)
  instance.df <- prepare_instance(instance.df)
  predict(model.rf, type='prob', newdata=instance.df)[,'1']
}
