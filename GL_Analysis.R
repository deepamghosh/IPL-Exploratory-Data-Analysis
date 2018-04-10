#Analysis of GUJURAT LIONS
library(dplyr)
summary(gl_matches$winner)

gl_matches_played <- nrow(gl_matches)
gl_win <- nrow(filter(gl_matches, winner == "GL"))
gl_noresult <- nrow(filter(gl_matches, winner == ""))
gl_loss <- ( gl_matches_played - (gl_win) - (gl_noresult))
gl_tie <- nrow(filter(gl_matches, result == "tie"))
gl_win_percent <- round((((gl_win) / gl_matches_played) * 100 ), digits = 2)

gl_analysis <- data.frame(gl_matches_played, gl_win, gl_loss, gl_noresult,
                           gl_tie,gl_win_percent)
colnames(gl_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                            "Win %")
#Toss Analysis
gl_toss_win <- nrow(filter(gl_matches, toss_winner == "GL"))
gl_toss_win_bat <- nrow(filter(gl_matches, toss_winner == "GL" & 
                                  toss_decision == "bat"))
gl_toss_win_bowl <- nrow(filter(gl_matches, toss_winner == "GL" & 
                                   toss_decision == "field")) 

#Biggest Win in Runs
gl_win_run_max <- max(gl_matches$win_by_runs)
gl_high_run_win <- filter(gl_matches, win_by_runs == gl_win_run_max) %>%
  select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
gl_win_wkt_max <- max(gl_matches$win_by_wickets)
gl_high_wkt_win <- filter(gl_matches, win_by_wickets == gl_win_wkt_max) %>%
  select(match_id, team1_id, team2_id, win_by_wickets, venue)
