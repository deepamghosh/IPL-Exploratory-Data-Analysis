#DD VS KXI Analysis

kxi_dd <- filter(ball_data, (batting_team == "DD" & bowling_team == "KXI") |
                   (batting_team == "KXI" & bowling_team == "DD"))

#match details between dd and kxi
kxi_dd_match <- filter(match_data_new, (team1_id == "DD" & team2_id == "KXI") |
                         (team1_id == "KXI" & team2_id == "DD"))
#head to head matches
kxi_dd_total_match <- nrow(kxi_dd_match)
#kxi WON
kxi_dd_kxi_win <- sum(str_detect(kxi_dd_match$winner, "KXI"))                  
#dd WON
kxi_dd_dd_win <- sum(str_detect(kxi_dd_match$winner, "DD"))                  
#NO RESULT
kxi_dd_no_result <- kxi_dd_total_match - kxi_dd_kxi_win - kxi_dd_dd_win

#biggest win - by runs
kxi_dd_big_run_win <- filter(kxi_dd_match, win_by_runs == max(win_by_runs)) %>% 
  select(season, winner,win_by_runs)
#biggest win - by wkts
kxi_dd_big_wkt_win <- filter(kxi_dd_match, win_by_wickets == max(win_by_wickets)) %>% 
  select(season, winner, win_by_wickets)

#Leader Run Scorer in between dd and kxi
kxi_dd_batsman_id <- sort(unique(kxi_dd$batsman_id))
kxi_dd_lead_batsman_runs <- 0
for(i in kxi_dd_batsman_id) {
  batsman_details <- filter(kxi_dd, batsman_id == i) %>% select(batsman_runs, batsman_id, batsman)
  run <- sum(batsman_details$batsman_runs)
  id <- batsman_details$batsman_id[i]
  name <- batsman_details$batsman
  if(run > kxi_dd_lead_batsman_runs) {
    kxi_dd_lead_batsman_runs <- run
    kxi_dd_lead_batsman_id <- id
    kxi_dd_lead_batsman_name <- as.character(unique(name))
  }
}

#Leading Wicket Taker in between kxi and dd
kxi_dd_bowler_id <- sort(unique(kxi_dd$bowler_id))
kxi_dd_lead_bowler_wkts <- 0
for(i in kxi_dd_bowler_id) {
  bowler_details <- filter(kxi_dd, bowler_id == i) %>% select(bowler, bowler_id, player_dismissed_id)
  wkt <-nrow(bowler_details) - sum(is.na(bowler_details$player_dismissed_id))
  id <- bowler_details$bowler_id[i]
  name <- bowler_details$bowler
  if(wkt > kxi_dd_lead_bowler_wkts) {
    kxi_dd_lead_bowler_wkts <- wkt
    kxi_dd_lead_bowler_id <- id
    kxi_dd_lead_bowler_name <- as.character(unique(name))
  }
}
#Analysis at Mohali Stadium
kxi_dd_match_mohali <- filter(match_data_new, (team1_id == "DD" & team2_id == "KXI") |
                         (team1_id == "KXI" & team2_id == "DD")) %>% 
                        filter(city == "Chandigarh")

kxi_dd_dd_win_mohali <- sum(str_detect(kxi_dd_match_mohali$winner, "DD"))

kxi_dd_kxi_win_mohali <- sum(str_detect(kxi_dd_match_mohali$winner, "KXI"))

#Analysis at Delhi Stadium
kxi_dd_match_delhi <- filter(match_data_new, (team1_id == "DD" & team2_id == "KXI") |
                                (team1_id == "KXI" & team2_id == "DD")) %>% 
  filter(city == "Delhi")
kxi_dd_dd_win_delhi <- sum(str_detect(kxi_dd_match_delhi$winner, "DD"))

kxi_dd_kxi_win_delhi <- sum(str_detect(kxi_dd_match_delhi$winner, "KXI"))

#Final Data Frame
kxi_dd_analysis <- data.frame(kxi_dd_total_match, kxi_dd_kxi_win, kxi_dd_dd_win,
                              kxi_dd_no_result,nrow(kxi_dd_match_mohali),
                              kxi_dd_kxi_win_mohali, kxi_dd_dd_win_mohali,
                              nrow(kxi_dd_match_delhi), kxi_dd_kxi_win_delhi,
                              kxi_dd_dd_win_delhi,paste(kxi_dd_big_run_win$season, 
                              kxi_dd_big_run_win$winner, kxi_dd_big_run_win$win_by_runs, sep = ":"),
                              paste(kxi_dd_big_wkt_win$season, kxi_dd_big_wkt_win$winner, kxi_dd_big_wkt_win$win_by_wickets, sep = ":"),
                              paste(kxi_dd_lead_batsman_name, kxi_dd_lead_batsman_runs, sep = ":"),
                              paste(kxi_dd_lead_bowler_name, kxi_dd_lead_bowler_wkts, sep = ":"))
colnames(kxi_dd_analysis) <- c("Head To Head", "KXI", "DD", "NR", "KXI:DD(MOHALI)","KXI(MOHALI)",
                               "DD(MOHALI)","KXI:DD(DELHI)","KXI(DELHI)", "DD(DELHI)", 
                               "Biggest Win: Runs","Biggest Win: Wickets","Leading Run Scorer", 
                               "Leading Wicket Taker")
