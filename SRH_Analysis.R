#Analysis of SUNRISERS HYDERABAD
library(dplyr)
summary(srh_matches$winner)

srh_matches_played <- nrow(srh_matches)
srh_win <- nrow(filter(srh_matches, winner == "SRH"))
srh_noresult <- nrow(filter(srh_matches, winner == ""))
srh_loss <- ( srh_matches_played - (srh_win) - (srh_noresult))
srh_tie <- nrow(filter(srh_matches, result == "tie"))
srh_win_percent <- round((((srh_win) / srh_matches_played) * 100 ), digits = 2)

srh_analysis <- data.frame(srh_matches_played, srh_win, srh_loss, srh_noresult,
                          srh_tie,srh_win_percent)
colnames(srh_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                           "Win %")
#Toss Analysis
srh_toss_win <- nrow(filter(srh_matches, toss_winner == "SRH"))
srh_toss_win_bat <- nrow(filter(srh_matches, toss_winner == "SRH" & 
                                 toss_decision == "bat"))
srh_toss_win_bowl <- nrow(filter(srh_matches, toss_winner == "SRH" & 
                                  toss_decision == "field")) 

#Biggest Win in Runs
srh_win_run_max <- max(srh_matches$win_by_runs)
srh_high_run_win <- filter(srh_matches, win_by_runs == srh_win_run_max) %>%
  select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
srh_win_wkt_max <- max(srh_matches$win_by_wickets)
srh_high_wkt_win <- filter(srh_matches, win_by_wickets == srh_win_wkt_max) %>%
  select(match_id, team1_id, team2_id, win_by_wickets, venue)
