match_processing <- function(data, avg_days)
{
  
  temp <- copy(data)
  temp <- temp[order(matchId)]
  
  city_distances <- read.csv("city_distances.csv")
  elo_data <- as.data.table(read.csv("elo_data.csv"))
  elo_data[,X:=NULL]
  # Calculating Day Before Match
  x <- temp[,c("matchId","Match_Date","Home","Away","Home_City",
               "Away_City", "Home_Score", "Away_Score", "Half Time Home Team Goals",
               "Half Time Away Team Goals","Result_Home","Result_Away", "Result_Tie",
               "Home Team Shots","Away Team Shots" ,"Home Team Shots on Target","Away Team Shots on Target",
               "Home Team Fouls Committed", "Away Team Fouls Committed", "Home Team Corners", "Away Team Corners",
               "Home Team Yellow Cards",  "Away Team Yellow Cards",   "Home Team Red Cards", "Away Team Red Cards")]
  
  
  x1 <- x[,c("matchId","Match_Date","Home", "Home Team Yellow Cards", "Home Team Red Cards" )]
  x2 <- x[,c("matchId","Match_Date","Away", "Away Team Yellow Cards", "Away Team Red Cards")]
  
  names <- c("matchId", "Match_Date", "Team", "Yellow_Card", "Red_Card" )
  colnames(x1) <- names
  colnames(x2) <- names
  
  x3 <- rbind(x1,x2)
  x3 <- x3[order(Team, Match_Date)]
  
  x4 <- rbind(x3[1,c("Match_Date","Yellow_Card", "Red_Card")], x3[,c("Match_Date","Yellow_Card", "Red_Card")])
  x4 <- x4[1:(nrow(x4)-1),]
  x3$Day_Before_Match <- as.numeric(x3$Match_Date - x4$Match_Date)
  x3$Last_Yellow <- x4$Yellow_Card
  x3$Last_Red <- x4$Red_Card
  
  
  x5 <- merge(x[,c("matchId", "Home")],x3[,c("matchId","Team", "Day_Before_Match", "Last_Yellow", "Last_Red")],by.x = c("matchId","Home"), by.y = c("matchId","Team")  )
  colnames(x5)[c((ncol(x5)-2):ncol(x5))] <- c("Home_Day","Home_Last_Yellow", "Home_Last_Red")
  x6 <- merge(x[,c("matchId", "Away")],x3[,c("matchId","Team", "Day_Before_Match","Last_Yellow", "Last_Red")],by.x = c("matchId","Away"), by.y = c("matchId","Team")  )
  colnames(x6)[c((ncol(x6)-2):ncol(x6))] <- c("Away_Day","Away_Last_Yellow", "Away_Last_Red")
  
  x5 <- x5[order(matchId)]
  x6 <- x6[order(matchId)]
  
  temp$Day_Diff <- x5$Home_Day - x6$Away_Day
  temp[, Day_Diff := (Day_Diff>=14)*14 + (Day_Diff<14)*Day_Diff]
  temp$Home_Last_Yellow <- x5$Home_Last_Yellow
  temp$Home_Last_Red <- x5$Home_Last_Red
  temp$Away_Last_Yellow <- x6$Away_Last_Yellow
  temp$Away_Last_Red <- x6$Away_Last_Red
  
  #Remove unnecessary data
  rm(x1,x2,x3,x4,x5,x6)
  
  
  ############# Average GOALS of the last 5 temp
  names(x)
  
  x1 <- x[,c("matchId", "Match_Date", "Home", "Home_Score", "Away_Score","Half Time Home Team Goals", "Half Time Away Team Goals",
             "Result_Home","Result_Tie","Home Team Shots" , "Away Team Shots","Home Team Shots on Target", "Away Team Shots on Target",
             "Home Team Fouls Committed", "Away Team Fouls Committed", "Home Team Corners", "Away Team Corners")]
  x2 <- x[,c("matchId", "Match_Date", "Away", "Away_Score", "Home_Score","Half Time Away Team Goals", "Half Time Home Team Goals",
             "Result_Away","Result_Tie","Away Team Shots" , "Home Team Shots","Away Team Shots on Target", "Home Team Shots on Target",
             "Away Team Fouls Committed", "Home Team Fouls Committed", "Away Team Corners", "Home Team Corners")]
  
  names <- c("matchId","Date", "Team", "Goals_Scored", "Goals_Received", "HT_Goals_Scored", "HT_Goals_Received", "Win", "Tie", "Shots_Made",
             "Shots_Received", "SoT_Made", "SoT_Received", "Fouls_Committed","Fouls_Received","Corners_Given", "Corners_Taken")
  colnames(x1) <- names
  colnames(x2) <- names
  x3 <- rbind(x1,x2)
  x3 <- x3[order(Team, Date)]
  x3$Goal_Diff <- as.numeric(x3$Goals_Scored - x3$Goals_Received)
  x3$HT_Goal_Diff <- as.numeric(x3$HT_Goals_Scored - x3$HT_Goals_Received)
  x3$Shot_Diff <- as.numeric(x3$Shots_Made - x3$Shots_Received)
  x3$SoT_Diff <- as.numeric(x3$SoT_Made - x3$SoT_Received)
  x3$Foul_Diff <- as.numeric(x3$Fouls_Committed - x3$Fouls_Received)
  x3$Corner_Diff <- as.numeric(x3$Corners_Given - x3$Corners_Taken)
  
  x3$Avg_Goal_Diff <- as.numeric(rep(NA, nrow(x3)))
  x3$Avg_HT_Goal_Diff <- as.numeric(rep(NA, nrow(x3)))
  x3$Avg_Shot_Diff <- as.numeric(rep(NA, nrow(x3)))
  x3$Avg_SoT_Diff <- as.numeric(rep(NA, nrow(x3)))
  x3$Avg_Foul_Diff <- as.numeric(rep(NA, nrow(x3)))
  x3$Avg_Corner_Diff <- as.numeric(rep(NA, nrow(x3)))
  
  days <- (1:avg_days)/sum(1:avg_days)
  
  
  for(i in (avg_days+1):(nrow(x3)))
  {
    if(x3[i-avg_days,]$Team == x3[i,]$Team)
    {
      x3[i,]$Avg_Goal_Diff <- weighted.mean(x3[(i-avg_days):(i-1),]$Goal_Diff,days)
      x3[i,]$Avg_HT_Goal_Diff <- weighted.mean(x3[(i-avg_days):(i-1),]$HT_Goal_Diff,days)
      x3[i,]$Avg_Shot_Diff <- weighted.mean(x3[(i-avg_days):(i-1),]$Shot_Diff,days)
      x3[i,]$Avg_SoT_Diff <- weighted.mean(x3[(i-avg_days):(i-1),]$SoT_Diff,days)
      x3[i,]$Avg_Foul_Diff <- weighted.mean(x3[(i-avg_days):(i-1),]$Foul_Diff,days)
      x3[i,]$Avg_Corner_Diff <- weighted.mean(x3[(i-avg_days):(i-1),]$Corner_Diff,days)
    }
  }
  
  
  x4 <- merge(x[,c("matchId","Home")],x3[,c("matchId","Date", "Team","Avg_Goal_Diff","Avg_HT_Goal_Diff",
                                            "Avg_Shot_Diff", "Avg_SoT_Diff", "Avg_Foul_Diff", "Avg_Corner_Diff")],by.x = c("matchId","Home"), by.y = c("matchId","Team")  )
  colnames(x4)[c((ncol(x4)-5):ncol(x4))] <- c("Home_Avg_Goal_Diff","Home_Avg_HT_Goal_Diff",
                                              "Home_Avg_Shot_Diff", "Home_Avg_SoT_Diff", "Home_Avg_Foul_Diff", "Home_Avg_Corner_Diff")
  x5 <- merge(x[,c("matchId","Away")],x3[,c("matchId","Date", "Team","Avg_Goal_Diff","Avg_HT_Goal_Diff",
                                            "Avg_Shot_Diff", "Avg_SoT_Diff", "Avg_Foul_Diff", "Avg_Corner_Diff")],by.x = c("matchId","Away"), by.y = c("matchId","Team")  )
  colnames(x5)[c((ncol(x5)-5):ncol(x5))] <- c("Away_Avg_Goal_Diff","Away_Avg_HT_Goal_Diff",
                                              "Away_Avg_Shot_Diff", "Away_Avg_SoT_Diff", "Away_Avg_Foul_Diff", "Away_Avg_Corner_Diff")
  
  
  x4 <- x4[order(matchId)]
  x5 <- x5[order(matchId)]
  
  temp <- cbind(temp, x4[,(ncol(x4)-5):ncol(x4)], x5[,(ncol(x5)-5):ncol(x5)])
  
  
  
  #Remove unnecessary data
  rm(x,x1,x2,x3,x4,x5)
  
  ### distances
  #x is a dummy variable to protect the type of matches data 
  x <- temp[,c("matchId","Home_City","Away_City")]
  x$city_combo <- paste(x$Home_City,x$Away_City,sep="-")
  city_distances <- as.data.table(city_distances)
  city_distances <- unique(city_distances)
  city_distances$city_combo <- paste(city_distances$city_1,city_distances$city_2,sep="-")
  x <- merge(x, city_distances[,c("city_combo","distance")], by= "city_combo")
  x <- x[order(matchId)]
  temp$distance <- x$distance
  rm(x)
  
  temp[,c("leagueId", "type", "Home_City", "Away_City", "Home_Score", "Away_Score", "Total_Score", "Result_Home", "Result_Tie", "Result_Away","Full Time Home Team Goals",
          "Full Time Away Team Goals", "Full Time Result", "Half Time Home Team Goals",
          "Half Time Away Team Goals","Half Time Result", "Referee",
          "Home Team Shots","Away Team Shots" ,"Home Team Shots on Target","Away Team Shots on Target",
          "Home Team Fouls Committed", "Away Team Fouls Committed", "Home Team Corners", "Away Team Corners",
          "Home Team Yellow Cards",  "Away Team Yellow Cards",   "Home Team Red Cards", "Away Team Red Cards") := NULL]
  x <- temp[,c("matchId","Home", "Away", "Match_Date")]
  
  x$Match_Date <- as.Date(x$Match_Date)
  elo_data$From <- as.Date(elo_data$From)
  
  x <- merge(x=x, y=elo_data, by.x = c("Home", "Match_Date"), by.y = c("Club", "From"), all.x=TRUE)
  x$Home_ELO = x$Elo
  x[,Elo := NULL]
  
  x <- merge(x=x, y=elo_data, by.x = c("Away", "Match_Date"), by.y = c("Club", "From"), all.x=TRUE)
  x$Away_ELO = x$Elo
  x[,Elo := NULL]
  x <- x[order(matchId)]
  temp$Home_ELO <- x$Home_ELO
  temp$Away_ELO <- x$Away_ELO
  
  temp$Yellow_Diff <- temp$Home_Last_Yellow - temp$Away_Last_Yellow
  temp$Red_Diff <- temp$Home_Last_Red - temp$Away_Last_Red
  temp$Goal_Diff <- temp$Home_Avg_Goal_Diff - temp$Away_Avg_Goal_Diff
  temp$HT_Goal_Diff <- temp$Home_Avg_HT_Goal_Diff - temp$Away_Avg_HT_Goal_Diff
  temp$Corner_Diff <- temp$Home_Avg_Corner_Diff - temp$Away_Avg_Corner_Diff
  temp$Foul_Diff <- temp$Home_Avg_Foul_Diff - temp$Away_Avg_Foul_Diff
  temp$SoT_Diff <- temp$Home_Avg_SoT_Diff - temp$Away_Avg_SoT_Diff
  temp$Shot_Diff <- temp$Home_Avg_Shot_Diff - temp$Away_Avg_Shot_Diff
  temp$ELO_Diff <- temp$Home_ELO - temp$Away_ELO
  
  rm(x)
  gc()
  return(temp)
}