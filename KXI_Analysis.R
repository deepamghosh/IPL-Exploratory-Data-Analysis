#Analysis of KINGS XI PUNJAB
library(dplyr)
summary(kxi_matches$winner)

kxi_matches_played <- nrow(kxi_matches)
kxi_win <- nrow(filter(kxi_matches, winner == "KXI"))
kxi_noresult <- nrow(filter(kxi_matches, winner == ""))
kxi_loss <- ( kxi_matches_played - (kxi_win) - (kxi_noresult))
kxi_tie <- nrow(filter(kxi_matches, result == "tie"))
kxi_win_percent <- round((((kxi_win) / kxi_matches_played) * 100 ), digits = 2)

kxi_analysis <- data.frame(kxi_matches_played, kxi_win, kxi_loss, kxi_noresult,
                           kxi_tie,kxi_win_percent)
colnames(kxi_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                            "Win %")
#Toss Analysis
kxi_toss_win <- nrow(filter(kxi_matches, toss_winner == "KXI"))
kxi_toss_win_bat <- nrow(filter(kxi_matches, toss_winner == "KXI" & 
                                  toss_decision == "bat"))
kxi_toss_win_bowl <- nrow(filter(kxi_matches, toss_winner == "KXI" & 
                                   toss_decision == "field")) 

#Biggest Win in Runs
kxi_win_run_max <- max(kxi_matches$win_by_runs)
kxi_high_run_win <- filter(kxi_matches, win_by_runs == kxi_win_run_max) %>%
  select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
kxi_win_wkt_max <- max(kxi_matches$win_by_wickets)
kxi_high_wkt_win <- filter(kxi_matches, win_by_wickets == kxi_win_wkt_max) %>%
  select(match_id, team1_id, team2_id, win_by_wickets, venue)
