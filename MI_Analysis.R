#Analysis of MUMBAI INDIANS
library(dplyr)
summary(mi_matches$winner)

mi_matches_played <- nrow(mi_matches)
mi_win <- nrow(filter(mi_matches, winner == "MI"))
mi_noresult <- nrow(filter(mi_matches, winner == ""))
mi_loss <- ( mi_matches_played - (mi_win) - (mi_noresult))
mi_tie <- nrow(filter(mi_matches, result == "tie"))
mi_win_percent <- round((((mi_win) / mi_matches_played) * 100 ), digits = 2)

mi_analysis <- data.frame(mi_matches_played, mi_win, mi_loss, mi_noresult,
                           mi_tie,mi_win_percent)
colnames(mi_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                            "Win %")
#Toss Analysis
mi_toss_win <- nrow(filter(mi_matches, toss_winner == "MI"))
mi_toss_win_bat <- nrow(filter(mi_matches, toss_winner == "MI" & 
                                  toss_decision == "bat"))
mi_toss_win_bowl <- nrow(filter(mi_matches, toss_winner == "MI" & 
                                   toss_decision == "field")) 

#Biggest Win in Runs
mi_win_run_max <- max(mi_matches$win_by_runs)
mi_high_run_win <- filter(mi_matches, win_by_runs == mi_win_run_max) %>%
  select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
mi_win_wkt_max <- max(mi_matches$win_by_wickets)
mi_high_wkt_win <- filter(mi_matches, win_by_wickets == mi_win_wkt_max) %>%
  select(match_id, team1_id, team2_id, win_by_wickets, venue)
