#RCB VS KKR Analysis

kkr_rcb <- filter(ball_data, (batting_team == "RCB" & bowling_team == "KKR") |
                   (batting_team == "KKR" & bowling_team == "RCB"))

#match details between rcb and kkr
kkr_rcb_match <- filter(match_data_new, (team1_id == "RCB" & team2_id == "KKR") |
                         (team1_id == "KKR" & team2_id == "RCB"))
#head to head matches
kkr_rcb_total_match <- nrow(kkr_rcb_match)
#kkr WON
kkr_rcb_kkr_win <- sum(str_detect(kkr_rcb_match$winner, "KKR"))                  
#rcb WON
kkr_rcb_rcb_win <- sum(str_detect(kkr_rcb_match$winner, "RCB"))                  
#NO RESULT
kkr_rcb_no_result <- kkr_rcb_total_match - kkr_rcb_kkr_win - kkr_rcb_rcb_win

#biggest win - by runs
kkr_rcb_big_run_win <- filter(kkr_rcb_match, win_by_runs == max(win_by_runs)) %>% 
  select(season, winner,win_by_runs)
#biggest win - by wkts
kkr_rcb_big_wkt_win <- filter(kkr_rcb_match, win_by_wickets == max(win_by_wickets)) %>% 
  select(season, winner, win_by_wickets)

#Leader Run Scorer in between rcb and kkr
kkr_rcb_batsman_id <- sort(unique(kkr_rcb$batsman_id))
kkr_rcb_lead_batsman_runs <- 0
for(i in kkr_rcb_batsman_id) {
  batsman_details <- filter(kkr_rcb, batsman_id == i) %>% select(batsman_runs, batsman_id, batsman)
  run <- sum(batsman_details$batsman_runs)
  id <- batsman_details$batsman_id[i]
  name <- batsman_details$batsman
  if(run > kkr_rcb_lead_batsman_runs) {
    kkr_rcb_lead_batsman_runs <- run
    kkr_rcb_lead_batsman_id <- id
    kkr_rcb_lead_batsman_name <- as.character(unique(name))
  }
}

#Leading Wicket Taker in between kkr and rcb
kkr_rcb_bowler_id <- sort(unique(kkr_rcb$bowler_id))
kkr_rcb_lead_bowler_wkts <- 0
for(i in kkr_rcb_bowler_id) {
  bowler_details <- filter(kkr_rcb, bowler_id == i) %>% select(bowler, bowler_id, player_dismissed_id)
  wkt <-nrow(bowler_details) - sum(is.na(bowler_details$player_dismissed_id))
  id <- bowler_details$bowler_id[i]
  name <- bowler_details$bowler
  if(wkt > kkr_rcb_lead_bowler_wkts) {
    kkr_rcb_lead_bowler_wkts <- wkt
    kkr_rcb_lead_bowler_id <- id
    kkr_rcb_lead_bowler_name <- as.character(unique(name))
  }
}
#Analysis at Kolkata Stadium
kkr_rcb_match_kolkata <- filter(match_data_new, (team1_id == "RCB" & team2_id == "KKR") |
                                (team1_id == "KKR" & team2_id == "RCB")) %>% 
  filter(city == "Kolkata")

kkr_rcb_rcb_win_kolkata <- sum(str_detect(kkr_rcb_match_kolkata$winner, "RCB"))

kkr_rcb_kkr_win_kolkata <- sum(str_detect(kkr_rcb_match_kolkata$winner, "KKR"))

#Analysis at Bangalore Stadium
kkr_rcb_match_bangalore <- filter(match_data_new, (team1_id == "RCB" & team2_id == "KKR") |
                               (team1_id == "KKR" & team2_id == "RCB")) %>% 
  filter(city == "Bangalore")
kkr_rcb_rcb_win_bangalore <- sum(str_detect(kkr_rcb_match_bangalore$winner, "RCB"))

kkr_rcb_kkr_win_bangalore <- sum(str_detect(kkr_rcb_match_bangalore$winner, "KKR"))

#Final Data Frame
kkr_rcb_analysis <- data.frame(kkr_rcb_total_match, kkr_rcb_kkr_win, kkr_rcb_rcb_win,
                              kkr_rcb_no_result,nrow(kkr_rcb_match_kolkata),
                              kkr_rcb_kkr_win_kolkata, kkr_rcb_rcb_win_kolkata,
                              nrow(kkr_rcb_match_bangalore), kkr_rcb_kkr_win_bangalore,
                              kkr_rcb_rcb_win_bangalore,paste(kkr_rcb_big_run_win$season, 
                                                        kkr_rcb_big_run_win$winner, kkr_rcb_big_run_win$win_by_runs, sep = ":"),
                              paste(kkr_rcb_big_wkt_win$season, kkr_rcb_big_wkt_win$winner, kkr_rcb_big_wkt_win$win_by_wickets, sep = ":"),
                              paste(kkr_rcb_lead_batsman_name, kkr_rcb_lead_batsman_runs, sep = ":"),
                              paste(kkr_rcb_lead_bowler_name, kkr_rcb_lead_bowler_wkts, sep = ":"))
colnames(kkr_rcb_analysis) <- c("Head To Head", "KKR", "RCB", "NR", "KKR:RCB(KOLKATA)","KKR(KOLKATA)",
                               "RCB(KOLKATA)","KKR:RCB(BANGALORE)","KKR(BANGALORE)", "RCB(BANGALORE)", 
                               "Biggest Win: Runs","Biggest Win: Wickets","Leading Run Scorer", 
                               "Leading Wicket Taker")
