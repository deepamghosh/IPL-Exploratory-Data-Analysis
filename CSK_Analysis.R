#Analysis of CSK
library(dplyr)
summary(csk_matches$winner)

csk_matches_played <- nrow(csk_matches)
csk_win <- nrow(filter(csk_matches, winner == "CSK"))
csk_noresult <- nrow(filter(csk_matches, winner == ""))
csk_loss <- ( csk_matches_played - (csk_win) - (csk_noresult))
csk_tie <- nrow(filter(csk_matches, result == "tie"))
csk_win_percent <- round((((csk_win) / csk_matches_played) * 100 ), digits = 2)

csk_analysis <- data.frame(csk_matches_played, csk_win, csk_loss, csk_noresult,
                           csk_tie,csk_win_percent)
colnames(csk_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                            "Win %")
#Toss Analysis
csk_toss_win <- nrow(filter(csk_matches, toss_winner == "CSK"))
csk_toss_win_bat <- nrow(filter(csk_matches, toss_winner == "CSK" & 
                                  toss_decision == "bat"))
csk_toss_win_bowl <- nrow(filter(csk_matches, toss_winner == "CSK" & 
                                   toss_decision == "field")) 

#Biggest Win in Runs
csk_win_run_max <- max(csk_matches$win_by_runs)
csk_high_run_win <- filter(csk_matches, win_by_runs == csk_win_run_max) %>%
  select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
csk_win_wkt_max <- max(csk_matches$win_by_wickets)
csk_high_wkt_win <- filter(csk_matches, win_by_wickets == csk_win_wkt_max) %>%
  select(match_id, team1_id, team2_id, win_by_wickets, venue)

#total wins, wins batting 1st and wins batting 2nd 
csk_total_win <- filter(csk_matches, winner == "CSK")
csk_total_win_bat1 <- filter(csk_total_win, ((toss_winner == "CSK" & 
                            toss_decision == "bat") | 
                              (toss_winner != "CSK" & toss_decision == "field")))


csk_total_win_bat2 <- filter(csk_total_win, ((toss_winner == "CSK" & 
                              toss_decision == "field") | 
                              (toss_winner != "CSK" & toss_decision == "bat")))