#Analysis of RAJASTHAN ROYALS
library(dplyr)
summary(rr_matches$winner)

rr_matches_played <- nrow(rr_matches)
rr_win <- nrow(filter(rr_matches, winner == "RR"))
rr_noresult <- nrow(filter(rr_matches, winner == ""))
rr_loss <- ( rr_matches_played - (rr_win) - (rr_noresult))
rr_tie <- nrow(filter(rr_matches, result == "tie"))
rr_win_percent <- round((((rr_win) / rr_matches_played) * 100 ), digits = 2)

rr_analysis <- data.frame(rr_matches_played, rr_win, rr_loss, rr_noresult,
                           rr_tie,rr_win_percent)
colnames(rr_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                            "Win %")
#Toss Analysis
rr_toss_win <- nrow(filter(rr_matches, toss_winner == "RR"))
rr_toss_win_bat <- nrow(filter(rr_matches, toss_winner == "RR" & 
                                  toss_decision == "bat"))
rr_toss_win_bowl <- nrow(filter(rr_matches, toss_winner == "RR" & 
                                   toss_decision == "field")) 

#Biggest Win in Runs
rr_win_run_max <- max(rr_matches$win_by_runs)
rr_high_run_win <- filter(rr_matches, win_by_runs == rr_win_run_max) %>%
  select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
rr_win_wkt_max <- max(rr_matches$win_by_wickets)
rr_high_wkt_win <- filter(rr_matches, win_by_wickets == rr_win_wkt_max) %>%
  select(match_id, team1_id, team2_id, win_by_wickets, venue)
