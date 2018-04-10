#CSK VS KKR Analysis

kkr_csk <- filter(ball_data, (batting_team == "CSK" & bowling_team == "KKR") |
                    (batting_team == "KKR" & bowling_team == "CSK"))

#match details between csk and kkr
kkr_csk_match <- filter(match_data_new, (team1_id == "CSK" & team2_id == "KKR") |
                          (team1_id == "KKR" & team2_id == "CSK"))
#head to head matches
kkr_csk_total_match <- nrow(kkr_csk_match)
#kkr WON
kkr_csk_kkr_win <- sum(str_detect(kkr_csk_match$winner, "KKR"))                  
#csk WON
kkr_csk_csk_win <- sum(str_detect(kkr_csk_match$winner, "CSK"))                  
#NO RESULT
kkr_csk_no_result <- kkr_csk_total_match - kkr_csk_kkr_win - kkr_csk_csk_win

#biggest win - by runs
kkr_csk_big_run_win <- filter(kkr_csk_match, win_by_runs == max(win_by_runs)) %>% 
  select(season, winner,win_by_runs)
#biggest win - by wkts
kkr_csk_big_wkt_win <- filter(kkr_csk_match, win_by_wickets == max(win_by_wickets)) %>% 
  select(season, winner, win_by_wickets)

#Leader Run Scorer in between csk and kkr
kkr_csk_batsman_id <- sort(unique(kkr_csk$batsman_id))
kkr_csk_lead_batsman_runs <- 0
for(i in kkr_csk_batsman_id) {
  batsman_details <- filter(kkr_csk, batsman_id == i) %>% select(batsman_runs, batsman_id, batsman)
  run <- sum(batsman_details$batsman_runs)
  id <- batsman_details$batsman_id[i]
  name <- batsman_details$batsman
  if(run > kkr_csk_lead_batsman_runs) {
    kkr_csk_lead_batsman_runs <- run
    kkr_csk_lead_batsman_id <- id
    kkr_csk_lead_batsman_name <- as.character(unique(name))
  }
}

#Leading Wicket Taker in between kkr and csk
kkr_csk_bowler_id <- sort(unique(kkr_csk$bowler_id))
kkr_csk_lead_bowler_wkts <- 0
for(i in kkr_csk_bowler_id) {
  bowler_details <- filter(kkr_csk, bowler_id == i) %>% select(bowler, bowler_id, player_dismissed_id)
  wkt <-nrow(bowler_details) - sum(is.na(bowler_details$player_dismissed_id))
  id <- bowler_details$bowler_id[i]
  name <- bowler_details$bowler
  if(wkt > kkr_csk_lead_bowler_wkts) {
    kkr_csk_lead_bowler_wkts <- wkt
    kkr_csk_lead_bowler_id <- id
    kkr_csk_lead_bowler_name <- as.character(unique(name))
  }
}

#Analysis at Kolkata Stadium
kkr_csk_match_kolkata <- filter(match_data_new, (team1_id == "CSK" & team2_id == "KKR") |
                                  (team1_id == "KKR" & team2_id == "CSK")) %>% 
  filter(city == "Kolkata")

kkr_csk_csk_win_kolkata <- sum(str_detect(kkr_csk_match_kolkata$winner, "CSK"))

kkr_csk_kkr_win_kolkata <- sum(str_detect(kkr_csk_match_kolkata$winner, "KKR"))

#Analysis at Chennai Stadium
kkr_csk_match_chennai <- filter(match_data_new, (team1_id == "CSK" & team2_id == "KKR") |
                                    (team1_id == "KKR" & team2_id == "CSK")) %>% 
  filter(city == "Chennai")
kkr_csk_csk_win_chennai <- sum(str_detect(kkr_csk_match_chennai$winner, "CSK"))

kkr_csk_kkr_win_chennai <- sum(str_detect(kkr_csk_match_chennai$winner, "KKR"))

#Final Data Frame
kkr_csk_analysis <- data.frame(kkr_csk_total_match, kkr_csk_kkr_win, kkr_csk_csk_win,
                               kkr_csk_no_result,nrow(kkr_csk_match_kolkata),
                               kkr_csk_kkr_win_kolkata, kkr_csk_csk_win_kolkata,
                               nrow(kkr_csk_match_chennai), kkr_csk_kkr_win_chennai,
                               kkr_csk_csk_win_chennai,paste(kkr_csk_big_run_win$season, 
                                                               kkr_csk_big_run_win$winner, kkr_csk_big_run_win$win_by_runs, sep = ":"),
                               paste(kkr_csk_big_wkt_win$season, kkr_csk_big_wkt_win$winner, kkr_csk_big_wkt_win$win_by_wickets, sep = ":"),
                               paste(kkr_csk_lead_batsman_name, kkr_csk_lead_batsman_runs, sep = ":"),
                               paste(kkr_csk_lead_bowler_name, kkr_csk_lead_bowler_wkts, sep = ":"))
colnames(kkr_csk_analysis) <- c("Head To Head", "KKR", "CSK", "NR", "KKR:CSK(KOLKATA)","KKR(KOLKATA)",
                                "CSK(KOLKATA)","KKR:CSK(CHENNAI)","KKR(CHENNAI)", "CSK(CHENNAI)", 
                                "Biggest Win: Runs","Biggest Win: Wickets","Leading Run Scorer", 
                                "Leading Wicket Taker")
