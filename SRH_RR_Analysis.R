#RR VS SRH Analysis

srh_rr <- filter(ball_data, (batting_team == "RR" & bowling_team == "SRH") |
                    (batting_team == "SRH" & bowling_team == "RR"))

#match details between rr and srh
srh_rr_match <- filter(match_data_new, (team1_id == "RR" & team2_id == "SRH") |
                          (team1_id == "SRH" & team2_id == "RR"))
#head to head matches
srh_rr_total_match <- nrow(srh_rr_match)
#srh WON
srh_rr_srh_win <- sum(str_detect(srh_rr_match$winner, "SRH"))                  
#rr WON
srh_rr_rr_win <- sum(str_detect(srh_rr_match$winner, "RR"))                  
#NO RESULT
srh_rr_no_result <- srh_rr_total_match - srh_rr_srh_win - srh_rr_rr_win

#biggest win - by runs
srh_rr_big_run_win <- filter(srh_rr_match, win_by_runs == max(win_by_runs)) %>% 
  select(season, winner,win_by_runs)
#biggest win - by wkts
srh_rr_big_wkt_win <- filter(srh_rr_match, win_by_wickets == max(win_by_wickets)) %>% 
  select(season, winner, win_by_wickets)

#Leader Run Scorer in between rr and srh
srh_rr_batsman_id <- sort(unique(srh_rr$batsman_id))
srh_rr_lead_batsman_runs <- 0
for(i in srh_rr_batsman_id) {
  batsman_details <- filter(srh_rr, batsman_id == i) %>% select(batsman_runs, batsman_id, batsman)
  run <- sum(batsman_details$batsman_runs)
  id <- batsman_details$batsman_id[i]
  name <- batsman_details$batsman
  if(run > srh_rr_lead_batsman_runs) {
    srh_rr_lead_batsman_runs <- run
    srh_rr_lead_batsman_id <- id
    srh_rr_lead_batsman_name <- as.character(unique(name))
  }
}

#Leading Wicket Taker in between srh and rr
srh_rr_bowler_id <- sort(unique(srh_rr$bowler_id))
srh_rr_lead_bowler_wkts <- 0
for(i in srh_rr_bowler_id) {
  bowler_details <- filter(srh_rr, bowler_id == i) %>% select(bowler, bowler_id, player_dismissed_id)
  wkt <-nrow(bowler_details) - sum(is.na(bowler_details$player_dismissed_id))
  id <- bowler_details$bowler_id[i]
  name <- bowler_details$bowler
  if(wkt > srh_rr_lead_bowler_wkts) {
    srh_rr_lead_bowler_wkts <- wkt
    srh_rr_lead_bowler_id <- id
    srh_rr_lead_bowler_name <- as.character(unique(name))
  }
}
#Analysis at Hyderabad Stadium
srh_rr_match_Hyderabad <- filter(match_data_new, (team1_id == "RR" & team2_id == "SRH") |
                                  (team1_id == "SRH" & team2_id == "RR")) %>% 
  filter(city == "Hyderabad")

srh_rr_rr_win_Hyderabad <- sum(str_detect(srh_rr_match_Hyderabad$winner, "RR"))

srh_rr_srh_win_Hyderabad <- sum(str_detect(srh_rr_match_Hyderabad$winner, "SRH"))

#Analysis at Jaipur Stadium
srh_rr_match_Jaipur <- filter(match_data_new, (team1_id == "RR" & team2_id == "SRH") |
                                    (team1_id == "SRH" & team2_id == "RR")) %>% 
  filter(city == "Jaipur")
srh_rr_rr_win_Jaipur <- sum(str_detect(srh_rr_match_Jaipur$winner, "RR"))

srh_rr_srh_win_Jaipur <- sum(str_detect(srh_rr_match_Jaipur$winner, "SRH"))

#Final Data Frame
srh_rr_analysis <- data.frame(srh_rr_total_match, srh_rr_srh_win, srh_rr_rr_win,
                               srh_rr_no_result,nrow(srh_rr_match_Hyderabad),
                               srh_rr_srh_win_Hyderabad, srh_rr_rr_win_Hyderabad,
                               nrow(srh_rr_match_Jaipur), srh_rr_srh_win_Jaipur,
                               srh_rr_rr_win_Jaipur,paste(srh_rr_big_run_win$season, 
                                                               srh_rr_big_run_win$winner, srh_rr_big_run_win$win_by_runs, sep = ":"),
                               paste(srh_rr_big_wkt_win$season, srh_rr_big_wkt_win$winner, srh_rr_big_wkt_win$win_by_wickets, sep = ":"),
                               paste(srh_rr_lead_batsman_name, srh_rr_lead_batsman_runs, sep = ":"),
                               paste(srh_rr_lead_bowler_name, srh_rr_lead_bowler_wkts, sep = ":"))
colnames(srh_rr_analysis) <- c("Head To Head", "SRH", "RR", "NR", "SRH:RR(Hyderabad)","SRH(Hyderabad)",
                                "RR(Hyderabad)","SRH:RR(Jaipur)","SRH(Jaipur)", "RR(Jaipur)", 
                                "Biggest Win: Runs","Biggest Win: Wickets","Leading Run Scorer", 
                                "Leading Wicket Taker")
