library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(dbplyr)
library(rlang)
library(glue)
library(ggplot2)
library(readr)
options(scipen=999)

# ================
# Data preparation
# ================

con <- dbConnect(SQLite(), dbname="voobly.db")
dbListTables(con)

db.matches <- tbl(con, "matches")
db.replay <- tbl(con, "replay")

db.replay %>% colnames

replay_table <- db.replay %>% 
  filter(type=='tick') %>% 
  select(match_id, current_time, 
         starts_with("data_players_0_"), 
         starts_with("data_players_1_"))

replay_table %>% head(100) %>% collect %>% View

replay_table %>% colnames

# Prepare functions to take differences of stats P1 - P2

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

# Calculate diff statistics:
# - First take care of kills and razes (normally pairwise for 8 players)
# - We recode these to binary since we take 1v1 games only
# - Then mutate for difference calculation
# - Then bring in names and make match id and time numeric
# - Then remove original information

diff_table <- replay_table %>% 
  mutate(data_players_0_kills=data_players_0_kills_2,
         data_players_0_razes=data_players_0_razes_2,
         data_players_1_kills=data_players_1_kills_1,
         data_players_1_razes=data_players_1_razes_1) %>%
  select(-contains('kills_'), -contains('razes_')) %>%
  mutate(player_1_name=data_players_0_name, player_2_name=data_players_1_name) %>%
  mutate(!!!diff_expression(colnames(.))) %>%
  mutate(match_id=as.numeric(match_id), current_time=as.numeric(current_time)) %>%
  select(-starts_with('data_players_'), -diff_id, -diff_name)

diff_table %>% head(100) %>% collect %>% View

library(cowplot)

plot_grid(
  diff_table %>% filter(match_id==16720619) %>% 
    select(current_time, diff_population_total) %>% 
    collect %>%
    ggplot(aes(x=current_time, y=diff_population_total)) +
    geom_line() + geom_hline(yintercept=0) + xlab("") + ylab("Pop Diff"),
  replay_table %>% filter(match_id==16720619) %>% 
    select(current_time, data_players_0_population_total, data_players_1_population_total) %>% 
    collect %>% gather("type", "population_total", 2:3) %>%
    ggplot(aes(x=as.numeric(as.character(current_time)), 
               y=as.numeric(as.character(population_total)), 
               colour=type, group=type)) +
    geom_line(show.legend=F) + xlab("Game Time") + ylab("Total Pop"),
  ncol=1
) 


# We need to bring in rating information as well as who won:
# - We now have match_id, current_time, player_1_name and player_2_name
# - Everything else is differences P1 - P2
# - Use the original db.replay here to prevent overloading the SQL stack
# - Note that the matches table starts counting from '1', the replay table from '0'

extra_info <- db.replay %>%
  left_join(db.matches, by='match_id') %>%
  select(match_id, match_winner,
         data_players_0_name, data_players_1_name,
         match_player1_name, match_player2_name,
         match_player1_rating_before, match_player2_rating_before) %>%
  distinct %>% collect %>%
  mutate(winner_name=ifelse(match_winner==1, match_player1_name, match_player2_name)) %>%
  mutate(player_1_wins=as.numeric(endsWith(data_players_0_name, winner_name))) %>%
  mutate(order_normal=endsWith(data_players_0_name, match_player1_name)) %>%
  mutate(diff_rating=ifelse(order_normal, 
                            match_player1_rating_before-match_player2_rating_before,
                            match_player2_rating_before-match_player1_rating_before)) %>%
  select(match_id, winner_name, player_1_wins, diff_rating) %>%
  distinct

# Some spot checks

db.matches %>% filter(match_id == 16720619) %>% collect %>% select(match_winner, contains("name"))
extra_info %>% filter(match_id == 16720619)
diff_table %>% filter(match_id == 16720619) %>% select(contains("name"))
db.matches %>% filter(match_id == 16720615) %>% collect %>% select(match_winner, contains("name"))
extra_info %>% filter(match_id == 16720615)
diff_table %>% filter(match_id == 16720615) %>% select(contains("name"))
extra_info$player_1_wins %>% table

# Okay, that's all, let's now collect and join everything together

final_table <- diff_table %>% collect %>% left_join(extra_info, by='match_id')
final_table <- final_table %>% select(-winner_name, -player_1_name, -player_2_name)
final_table$player_1_wins <- factor(final_table$player_1_wins)
final_table$player_1_wins %>% table
colnames(final_table)

write_csv(final_table, gzfile('prepared_data.csv.gz'))
