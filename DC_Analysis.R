#Analysis of DECCAN CHARGERS HYDERABAD
library(dplyr)
summary(dc_matches$winner)

dc_matches_played <- nrow(dc_matches)
dc_win <- nrow(filter(dc_matches, winner == "DC"))
dc_noresult <- nrow(filter(dc_matches, winner == ""))
dc_loss <- ( dc_matches_played - (dc_win) - (dc_noresult))
dc_tie <- nrow(filter(dc_matches, result == "tie"))
dc_win_percent <- round((((dc_win) / dc_matches_played) * 100 ), digits = 2)

dc_analysis <- data.frame(dc_matches_played, dc_win, dc_loss, dc_noresult,
                          dc_tie,dc_win_percent)
colnames(dc_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                           "Win %")
#Toss Analysis
dc_toss_win <- nrow(filter(dc_matches, toss_winner == "DC"))
dc_toss_win_bat <- nrow(filter(dc_matches, toss_winner == "DC" & 
                                 toss_decision == "bat"))
dc_toss_win_bowl <- nrow(filter(dc_matches, toss_winner == "DC" & 
                                  toss_decision == "field")) 

#Biggest Win in Runs
dc_win_run_max <- max(dc_matches$win_by_runs)
dc_high_run_win <- filter(dc_matches, win_by_runs == dc_win_run_max) %>%
  select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
dc_win_wkt_max <- max(dc_matches$win_by_wickets)
dc_high_wkt_win <- filter(dc_matches, win_by_wickets == dc_win_wkt_max) %>%
  select(match_id, team1_id, team2_id, win_by_wickets, venue)
