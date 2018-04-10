#Overall Winning Analysis of the teams

win_percent <- data.frame(c("KXI", "DD", "RR", "GL", "KKR", "MI", "PWI", "RPS", "DC",
                            "SRH", "RCB", "CSK", "KTK"), 
                          c("Kings XI Punjab", "Delhi Daredevils", "Rajasthan Royals",
                            "Gujarat Lions", "Kolkata Knight Riders", "Mumbai Indians",
                            "Pune Warriors India", "Rising Pune Supergiant", 
                            "Deccan Chargers Hyderabad", "Sunrisers Hyderabad",
                            "Royal Challengers Bangalore", "Chennai Super Kings",
                            "Kochi Tusker Kerala"),
                          c(kxi_analysis$`Matches Played`, dd_analysis$`Matches Played`,
                            rr_analysis$`Matches Played`, gl_analysis$`Matches Played`, 
                            kkr_analysis$`Matches Played`, mi_analysis$`Matches Played`, 
                            pw_analysis$`Matches Played`, rps_analysis$`Matches Played`,
                            dc_analysis$`Matches Played`, srh_analysis$`Matches Played`,
                            rcb_analysis$`Matches Played`, csk_analysis$`Matches Played`,
                            ktk_analysis$`Matches Played`),
                          c(kxi_analysis$Win, dd_analysis$Win,rr_analysis$Win, 
                            gl_analysis$Win, kkr_analysis$Win,
                              mi_analysis$Win, pw_analysis$Win, rps_analysis$Win,
                              dc_analysis$Win, srh_analysis$Win, rcb_analysis$Win,
                              csk_analysis$Win, ktk_analysis$Win),
                          c(kxi_analysis$Loss, dd_analysis$Loss, 
                                rr_analysis$Loss, gl_analysis$Loss, kkr_analysis$Loss,
                                mi_analysis$Loss, pw_analysis$Loss, rps_analysis$Loss,
                                dc_analysis$Loss, srh_analysis$Loss, rcb_analysis$Loss,
                                csk_analysis$Loss, ktk_analysis$Loss),
                          c(kxi_analysis$`No Result`, dd_analysis$`No Result`, 
                                rr_analysis$`No Result`, gl_analysis$`No Result`, kkr_analysis$`No Result`,
                                mi_analysis$`No Result`, pw_analysis$`No Result`, rps_analysis$`No Result`,
                                dc_analysis$`No Result`, srh_analysis$`No Result`, rcb_analysis$`No Result`,
                                csk_analysis$`No Result`, ktk_analysis$`No Result`),
                          c(kxi_analysis$Tie, dd_analysis$Tie, 
                            rr_analysis$Tie, gl_analysis$Tie, kkr_analysis$Tie,
                            mi_analysis$Tie, pw_analysis$Tie, rps_analysis$Tie,
                            dc_analysis$Tie, srh_analysis$Tie, rcb_analysis$Tie,
                            csk_analysis$Tie, ktk_analysis$Tie ),
                          c(kxi_analysis$`Win %`, dd_analysis$`Win %`, 
                            rr_analysis$`Win %`, gl_analysis$`Win %`, kkr_analysis$`Win %`,
                            mi_analysis$`Win %`, pw_analysis$`Win %`, rps_analysis$`Win %`,
                            dc_analysis$`Win %`, srh_analysis$`Win %`, rcb_analysis$`Win %`,
                            csk_analysis$`Win %`, ktk_analysis$`Win %`)
                          )
colnames(win_percent) <- c("Team ID", "Teams", "Matches Played", "Win", "Loss", "No Result", "Tie", "Win %")

#Graph Plot
##total matches played
ggplot(data = win_percent, aes(x=`Team ID`, y=`Matches Played`)) + 
        geom_bar(stat = "identity", fill = "steelblue", color = "white") +
        geom_text(aes(label=`Matches Played`), vjust=1.6, size=3.5)+
   theme_minimal()

##total wins
ggplot(data = win_percent, aes(x=`Team ID`, y=`Win`)) + 
  geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  geom_text(aes(label=`Win`), vjust=1.6, size=3.5)+
  theme_minimal()

#win %
ggplot(data = win_percent, aes(x=`Team ID`, y=`Win %`)) + 
  geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  geom_text(aes(label=`Win %`), vjust=1.6, size=3.5)+
  theme_minimal()

