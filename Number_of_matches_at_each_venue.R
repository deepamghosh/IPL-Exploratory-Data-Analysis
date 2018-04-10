#Code to find out number of matches at each venue
match_data <- read.csv("match_data.csv")
#loading dplyr package
library(dplyr)
glimpse(match_data)
match_data_new <- tbl_df(match_data)
city <- unique(match_data_new$city)

#unique venue
venue <- unique(match_data_new$venue)

#creating a df of venue with no of matches
city <- c("Bengaluru", "Mohali", "Delhi", "Mumbai", "Kolkata", "Jaipur", "Hyderabad",
          "Chennai", "Mumbai-DY Patil", "Capetown", "Port Elizabeth", "Durban", "Centurion",
          "Buffalo Park", "Johannesburg", "Diamond Oval", "OUTsurance Oval", "Mumbai-Braborne",
          "Ahmedabad", "Cuttack","Nagpur", "Dharmasala", "Kochi", "Indore", "Vizag", "Pune", 
          "Raipur", "Ranchi", "Abu Dhabi", "Sharjah", "Dubai", "Rajkot", "Kanpur")
venue_name <- as.character(venue)
venue_count <- data.frame(city, venue_name, c(66, 46, 60, 57, 61, 33, 49, 48, 17, 7, 7, 15
                                        ,12,3,8, 3, 2, 11, 12, 7, 3, 9, 5, 5, 11, 32,
                                        6, 7, 7, 6, 7, 10, 4))
colnames <- c("City", "Venue", "Matches Held")
colnames(venue_count) <- colnames
#loading stringr package
library(stringr)
#since two same stadiums are there with two different names
string <- str_replace(match_data$venue, "Subrata Roy Sahara Stadium", 
                      "Maharashtra Cricket Association Stadium")
match_data_new$venue <- string
venue_fact <- as.factor(match_data_new$venue)
match_data_new$venue <- venue_fact
summary(match_data_new$venue)
View(venue_count)

temp <-  filter(match_data_new, venue == "Maharashtra Cricket Association Stadium")%>% select(venue_id) 
match_data_new$venue_id <-  str_replace(match_data_new$venue_id, "26", "32")

match_data_new$venue_id <- as.integer(match_data_new$venue_id)

ggplot(data=venue_count, aes(x=venue, y=matches)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=len), vjust=-0.3, size=3.5)+
  theme_minimal()

#Graph Plot of different venues
ggplot(data = venue_count, aes(x=City, y=`Matches Held`)) + 
  +      geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  +     scale_x_discrete(limits=c("Bengaluru", "Mohali", "Delhi", "Mumbai","Kolkata",
                                  +                               "Jaipur", "Hyderabad", "Chennai","Indore","Pune")) +
  +   geom_text(aes(label=`Matches Held`), vjust=1.6, size=3.5)+
  +   theme_minimal()
