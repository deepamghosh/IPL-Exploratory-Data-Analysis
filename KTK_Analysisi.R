#Analysis of KOCHI TUSKERS KERLA
library(dplyr)
summary(ktk_matches$winner)

ktk_matches_played <- nrow(ktk_matches)
ktk_win <- nrow(filter(ktk_matches, winner == "KTK"))
ktk_noresult <- nrow(filter(ktk_matches, winner == ""))
ktk_loss <- ( ktk_matches_played - (ktk_win) - (ktk_noresult))
ktk_tie <- nrow(filter(ktk_matches, result == "tie"))
ktk_win_percent <- round((((ktk_win) / ktk_matches_played) * 100 ), digits = 2)

ktk_analysis <- data.frame(ktk_matches_played, ktk_win, ktk_loss, ktk_noresult,
                           ktk_tie,ktk_win_percent)
colnames(ktk_analysis) <- c("Matches Played", "Win", "Loss", "No Result", "Tie", 
                            "Win %")
#Toss Analysis
ktk_toss_win <- nrow(filter(ktk_matches, toss_winner == "KTK"))
ktk_toss_win_bat <- nrow(filter(ktk_matches, toss_winner == "KTK" & 
                                  toss_decision == "bat"))
ktk_toss_win_bowl <- nrow(filter(ktk_matches, toss_winner == "KTK" & 
                                   toss_decision == "field")) 

#Biggest Win in Runs
ktk_win_run_max <- max(ktk_matches$win_by_runs)
ktk_high_run_win <- filter(ktk_matches, win_by_runs == ktk_win_run_max) %>%
  select(match_id, team1_id, team2_id, win_by_runs, venue)

#Biggest Win in Wickets
ktk_win_wkt_max <- max(ktk_matches$win_by_wickets)
ktk_high_wkt_win <- filter(ktk_matches, win_by_wickets == ktk_win_wkt_max) %>%
  select(match_id, team1_id, team2_id, win_by_wickets, venue)
