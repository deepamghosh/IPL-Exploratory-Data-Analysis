#Analysis of RISING PUNE SUPERGIANTS
library(dplyr)
summary(rps_matches$winner)

rps_matches_played <- nrow(rps_matches)
rps_win <- nrow(filter(rps_matches, winner == "RPS"))
rps_noresult <- nrow(filter(rps_matches, winner == ""))
rps_loss <- ( rps_matches_played - (rps_win) - (rps_noresult))
rps_tie <- nrow(filter(rps_matches, result == "tie"))
rps_win_percent <- round((((rps_win) / rps_matches_played) * 100 ), digits = 2)

rps_analysis <- data.frame(rps_matches_played, rps_win, rps_loss, rps_noresult,
                           rps_tie,rps_win_percent)
colnames(rps_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                            "Win %")
#Toss Analysis
rps_toss_win <- nrow(filter(rps_matches, toss_winner == "RPS"))
rps_toss_win_bat <- nrow(filter(rps_matches, toss_winner == "RPS" & 
                                  toss_decision == "bat"))
rps_toss_win_bowl <- nrow(filter(rps_matches, toss_winner == "RPS" & 
                                   toss_decision == "field")) 

#Biggest Win in Runs
rps_win_run_max <- max(rps_matches$win_by_runs)
rps_high_run_win <- filter(rps_matches, win_by_runs == rps_win_run_max) %>%
  select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
rps_win_wkt_max <- max(rps_matches$win_by_wickets)
rps_high_wkt_win <- filter(rps_matches, win_by_wickets == rps_win_wkt_max) %>%
  select(match_id, team1_id, team2_id, win_by_wickets, venue)
