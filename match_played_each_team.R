#Code to find out number of matches at each venue
match_data <- read.csv("match_data.csv")
#loading dplyr package
library(dplyr)
glimpse(match_data)
match_data_new <- tbl_df(match_data)

teams <- unique(match_data_new$team1_id)

#matches played by KKR
kkr_matches <- filter(match_data_new, team1_id == "KKR" | team2_id == "KKR")
#kkr_toss_win <- filter(kkr_matches, toss_winner == "KKR" & toss_decision == "bat")

#matches played by CSK
csk_matches <- filter(match_data_new, team1_id == "CSK" | team2_id == "CSK")

#matches played by RR
rr_matches <- filter(match_data_new, team1_id == "RR" | team2_id == "RR")

#matches played by MI
mi_matches <- filter(match_data_new, team1_id == "MI" | team2_id == "MI")

#matches played by DD
dd_matches <- filter(match_data_new, team1_id == "DD" | team2_id == "DD")

#matches played by KXI
kxi_matches <- filter(match_data_new, team1_id == "KXI" | team2_id == "KXI")

#matches played by DC
dc_matches <- filter(match_data_new, team1_id == "DC" | team2_id == "DC")

#matches played by GL
gl_matches <- filter(match_data_new, team1_id == "GL" | team2_id == "GL")

#matches played by KTK
ktk_matches <- filter(match_data_new, team1_id == "KTK" | team2_id == "KTK")

#matches played by PW
pw_matches <- filter(match_data_new, team1_id == "PW" | team2_id == "PW")

#matches played by RCB
rcb_matches <- filter(match_data_new, team1_id == "RCB" | team2_id == "RCB")

#matches played by RPS
rps_matches <- filter(match_data_new, team1_id == "RPS" | team2_id == "RPS")

#matches played by SRH
srh_matches <- filter(match_data_new, team1_id == "SRH" | team2_id == "SRH")


team_names <- as.character(teams)
matches_played <- data.frame(team_names, c(148, 131, 118, 157, 75, 148, 152, 147, 
                                           14, 46, 76, 30, 30))
colnames1 <- c("Team","Matches Played")
colnames(matches_played) <- colnames1




