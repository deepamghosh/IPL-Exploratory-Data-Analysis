#Analysis of PUNE WARRIORS INDIA
library(dplyr)
summary(pw_matches$winner)

pw_matches_played <- nrow(pw_matches)
pw_win <- nrow(filter(pw_matches, winner == "PW"))
pw_noresult <- nrow(filter(pw_matches, winner == ""))
pw_loss <- ( pw_matches_played - (pw_win) - (pw_noresult))
pw_tie <- nrow(filter(pw_matches, result == "tie"))
pw_win_percent <- round((((pw_win) / pw_matches_played) * 100 ), digits = 2)

pw_analysis <- data.frame(pw_matches_played, pw_win, pw_loss, pw_noresult,
                           pw_tie,pw_win_percent)
colnames(pw_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                            "Win %")
#Toss Analysis
pw_toss_win <- nrow(filter(pw_matches, toss_winner == "PW"))
pw_toss_win_bat <- nrow(filter(pw_matches, toss_winner == "PW" & 
                                  toss_decision == "bat"))
pw_toss_win_bowl <- nrow(filter(pw_matches, toss_winner == "PW" & 
                                   toss_decision == "field")) 

#Biggest Win in Runs
pw_win_run_max <- max(pw_matches$win_by_runs)
pw_high_run_win <- filter(pw_matches, win_by_runs == pw_win_run_max) %>%
  select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
pw_win_wkt_max <- max(pw_matches$win_by_wickets)
pw_high_wkt_win <- filter(pw_matches, win_by_wickets == pw_win_wkt_max) %>%
  select(match_id, team1_id, team2_id, win_by_wickets, venue)
