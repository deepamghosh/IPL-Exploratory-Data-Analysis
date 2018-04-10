#Analysis of DELHI DAREDEVILS
library(dplyr)
summary(dd_matches$winner)

dd_matches_played <- nrow(dd_matches)
dd_win <- nrow(filter(dd_matches, winner == "DD"))
dd_noresult <- nrow(filter(dd_matches, winner == ""))
dd_loss <- ( dd_matches_played - (dd_win) - (dd_noresult))
dd_tie <- nrow(filter(dd_matches, result == "tie"))
dd_win_percent <- round((((dd_win) / dd_matches_played) * 100 ), digits = 2)

dd_analysis <- data.frame(dd_matches_played, dd_win, dd_loss, dd_noresult,
                           dd_tie,dd_win_percent)
colnames(dd_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                            "Win %")
#Toss Analysis
dd_toss_win <- nrow(filter(dd_matches, toss_winner == "DD"))
dd_toss_win_bat <- nrow(filter(dd_matches, toss_winner == "DD" & 
                                  toss_decision == "bat"))
dd_toss_win_bowl <- nrow(filter(dd_matches, toss_winner == "DD" & 
                                   toss_decision == "field")) 

#Biggest Win in Runs
dd_win_run_max <- max(dd_matches$win_by_runs)
dd_high_run_win <- filter(dd_matches, win_by_runs == dd_win_run_max) %>%
  select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
dd_win_wkt_max <- max(dd_matches$win_by_wickets)
dd_high_wkt_win <- filter(dd_matches, win_by_wickets == dd_win_wkt_max) %>%
  select(match_id, team1_id, team2_id, win_by_wickets, venue)
