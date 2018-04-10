#CSK VS MI Analysis

mi_csk <- filter(ball_data, (batting_team == "CSK" & bowling_team == "MI") |
                   (batting_team == "MI" & bowling_team == "CSK"))

#match details between CSK and MI
mi_csk_match <- filter(match_data_new, (team1_id == "CSK" & team2_id == "MI") |
                         (team1_id == "MI" & team2_id == "CSK"))
#head to head matches
mi_csk_total_match <- nrow(mi_csk_match)
#MI WON
mi_csk_mi_win <- sum(str_detect(mi_csk_match$winner, "MI"))                  
#CSK WON
mi_csk_csk_win <- sum(str_detect(mi_csk_match$winner, "CSK"))                  
#NO RESULT
mi_csk_no_result <- mi_csk_total_match - mi_csk_mi_win - mi_csk_csk_win

#biggest win - by runs
mi_csk_big_run_win <- filter(mi_csk_match, win_by_runs == max(win_by_runs)) %>% 
                      select(season, winner,win_by_runs)
#biggest win - by wkts
mi_csk_big_wkt_win <- filter(mi_csk_match, win_by_wickets == max(win_by_wickets)) %>% 
                      select(season, winner, win_by_wickets)

#Leader Run Scorer in between CSK and MI
mi_csk_batsman_id <- sort(unique(mi_csk$batsman_id))
mi_csk_lead_batsman_runs <- 0
for(i in mi_csk_batsman_id) {
  batsman_details <- filter(mi_csk, batsman_id == i) %>% select(batsman_runs, batsman_id, batsman)
  run <- sum(batsman_details$batsman_runs)
  id <- batsman_details$batsman_id[i]
  name <- batsman_details$batsman
  if(run > mi_csk_lead_batsman_runs) {
    mi_csk_lead_batsman_runs <- run
    mi_csk_lead_batsman_id <- id
    mi_csk_lead_batsman_name <- as.character(unique(name))
  }
}

#Leading Wicket Taker in between MI and CSK
mi_csk_bowler_id <- sort(unique(mi_csk$bowler_id))
mi_csk_lead_bowler_wkts <- 0
for(i in mi_csk_bowler_id) {
  bowler_details <- filter(mi_csk, bowler_id == i) %>% select(bowler, bowler_id, player_dismissed_id)
  wkt <-nrow(bowler_details) - sum(is.na(bowler_details$player_dismissed_id))
  id <- bowler_details$bowler_id[i]
  name <- bowler_details$bowler
  if(wkt > mi_csk_lead_bowler_wkts) {
    mi_csk_lead_bowler_wkts <- wkt
    mi_csk_lead_bowler_id <- id
    mi_csk_lead_bowler_name <- as.character(unique(name))
  }
}
mi_csk_analysis <- data.frame(mi_csk_total_match, mi_csk_mi_win, mi_csk_csk_win,
                              mi_csk_no_result, paste(mi_csk_big_run_win$season, mi_csk_big_run_win$winner, mi_csk_big_run_win$win_by_runs, sep = ":"),
                              paste(mi_csk_big_wkt_win$season, mi_csk_big_wkt_win$winner, mi_csk_big_wkt_win$win_by_wickets, sep = ":"),
                              paste(mi_csk_lead_batsman_name, mi_csk_lead_batsman_runs, sep = ":"),
                              paste(mi_csk_lead_bowler_name, mi_csk_lead_bowler_wkts, sep = ":"))
colnames(mi_csk_analysis) <- c("Head To Head", "MI", "CSK", "NR", "Biggest Win: Runs",
                               "Biggest Win: Wickets","Leading Run Scorer", 
                               "Leading Wicket Taker")