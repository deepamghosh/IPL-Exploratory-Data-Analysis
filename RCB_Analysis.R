#Analysis of RCB
library(dplyr)
summary(rcb_matches$winner)

rcb_matches_played <- nrow(rcb_matches)
rcb_win <- nrow(filter(rcb_matches, winner == "RCB"))
rcb_noresult <- nrow(filter(rcb_matches, winner == ""))
rcb_loss <- ( rcb_matches_played - (rcb_win) - (rcb_noresult))
rcb_tie <- nrow(filter(rcb_matches, result == "tie"))
rcb_win_percent <- round((((rcb_win) / rcb_matches_played) * 100 ), digits = 2)

rcb_analysis <- data.frame(rcb_matches_played, rcb_win, rcb_loss, rcb_noresult,
                           rcb_tie,rcb_win_percent)
colnames(rcb_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                            "Win %")
#Toss Analysis
rcb_toss_win <- nrow(filter(rcb_matches, toss_winner = "rcb"))
rcb_toss_win_bat <- nrow(filter(rcb_matches, toss_winner == "rcb" & 
                                  toss_decision == "bat"))
rcb_toss_win_bowl <- nrow(filter(rcb_matches, toss_winner == "rcb" & 
                                   toss_decision == "field")) 

#Biggest Win in Runs
rcb_win_run_max <- max(rcb_matches$win_by_runs)
rcb_high_run_win <- filter(rcb_matches, win_by_runs == rcb_win_run_max) %>%
  select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
rcb_win_wkt_max <- max(rcb_matches$win_by_wickets)
rcb_high_wkt_win <- filter(rcb_matches, win_by_wickets == rcb_win_wkt_max) %>%
  select(match_id, team1_id, team2_id, win_by_wickets, venue)
