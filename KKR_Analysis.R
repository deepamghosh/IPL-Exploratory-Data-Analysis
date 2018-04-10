#Analysis of KKR
library(dplyr)
summary(kkr_matches$winner)

kkr_matches_played <- nrow(kkr_matches)
kkr_win <- nrow(filter(kkr_matches, winner == "KKR"))
kkr_noresult <- nrow(filter(kkr_matches, winner == ""))
kkr_loss <- ( kkr_matches_played - (kkr_win) - (kkr_noresult))
kkr_tie <- nrow(filter(kkr_matches, result == "tie"))
kkr_win_percent <- round((((kkr_win) / kkr_matches_played) * 100 ), digits = 2)

kkr_analysis <- data.frame(kkr_matches_played, kkr_win, kkr_loss, kkr_noresult,
                           kkr_tie,kkr_win_percent)
colnames(kkr_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                            "Win %")
#Toss Analysis
kkr_toss_win <- nrow(filter(kkr_matches, toss_winner = "KKR"))
kkr_toss_win_bat <- nrow(filter(kkr_matches, toss_winner == "KKR" & 
                                  toss_decision == "bat"))
kkr_toss_win_bowl <- nrow(filter(kkr_matches, toss_winner == "KKR" & 
                                   toss_decision == "field")) 

#Biggest Win in Runs
kkr_win_run_max <- max(kkr_matches$win_by_runs)
kkr_high_run_win <- filter(kkr_matches, win_by_runs == kkr_win_run_max) %>%
                select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
kkr_win_wkt_max <- max(kkr_matches$win_by_wickets)
kkr_high_wkt_win <- filter(kkr_matches, win_by_wickets == kkr_win_wkt_max) %>%
                select(match_id, team1_id, team2_id, win_by_wickets, venue)
