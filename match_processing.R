match_processing <- function(data)
{
  
temp <- copy(data)

# Calculating Day Before Match
x <- temp[,c("matchId","Home","Away","Match_Date")]
x1 <- x[,c(1,2,4)]
x2 <- x[,c(1,3,4)]

names <- c("matchId", "Team", "Match_Date")
colnames(x1) <- names
colnames(x2) <- names

x3 <- rbind(x1,x2)
x3 <- x3[order(Team, -Match_Date)]
x3$Day_Before_Match <- 1:nrow(x2)


#Formatting the information
for(i in 1:(nrow(x3)-1))
{
  x3[i]$Day_Before_Match <- (x3[i]$Match_Date - x3[i+1]$Match_Date)*(x3[i]$Team == x3[i+1]$Team)
}

x$home_day <- 1:nrow(x)
x$away_day <- 1:nrow(x)

x <- merge(x,x3[,c("matchId","Team", "Day_Before_Match")],by.x = c("matchId","Home"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Home_Day"
x <- merge(x,x3[,c("matchId","Team", "Day_Before_Match")],by.x = c("matchId","Away"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Away_Day"

#Adding the information into temp data
temp$Home_Day <- x$Home_Day
temp$Away_Day <- x$Away_Day

#Remove unnecessary data
rm(x,x1,x2,x3)

#Remove days larger than 14
temp[, Home_Day := (Home_Day>=14)*14 + (Home_Day<14)*Home_Day]
temp[, Away_Day := (Away_Day>=14)*14 + (Away_Day<14)*Away_Day]

########## END OF DAY BEFORE MATCH INFORMATION

############# Average GOALS of the last 5 temp

x <- temp[,c("matchId", "Match_Date", "Home", "Away", "Home_Score", "Away_Score")]
x1 <- x[,c(1,2,3,5)]
x2 <- x[,c(1,2,4,6)]
names <- c("matchId","Date", "Team", "Score")
colnames(x1) <- names
colnames(x2) <- names
x3 <- rbind(x1,x2)
x3 <- x3[order(Team, Date)]
x3$avg_score <- as.numeric(x3$Score)

for(i in 5:(nrow(x3)))
{
  x3[i,]$avg_score <- sum(x3[(i-4):i,]$Score)*(x3[i-4,]$Team == x3[i,]$Team)/5
}

x3$Last_Average <- x3$avg_score

for(i in 2:nrow(x3))
{
  x3[i,]$Last_Average <- x3[i-1,]$avg_score 
}



x <- merge(x,x3[,c("matchId","Team", "Last_Average")],by.x = c("matchId","Home"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Home_Goal_Avg"
x <- merge(x,x3[,c("matchId","Team", "Last_Average")],by.x = c("matchId","Away"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Away_Goal_Avg"

temp$Home_Goal_Avg <- x$Home_Goal_Avg
temp$Away_Goal_Avg <- x$Away_Goal_Avg


#Remove unnecessary data
rm(x,x1,x2,x3)

###### END OF AVERAGE GOALS ####

########## RESULT RATIOS

x <- temp[,c("matchId","Home","Away","Match_Date","Result_Home","Result_Away")]
x1 <- x[,c(1,2,4,5)]
x2 <- x[,c(1,3,4,6)]
names <- c("matchId", "Team", "Date", "Win_Lose")
colnames(x1) <- names
colnames(x2) <- names
x3 <- rbind(x1,x2)
x3 <- x3[order(Team, Date)]

x3$avg_winning <- as.numeric(x3$Win_Lose)

for(i in 5:(nrow(x3)))
{
  x3[i,]$avg_winning <- sum(x3[(i-4):i,]$Win_Lose)*(x3[i-4,]$Team == x3[i,]$Team)/5
}

for(i in 1:(nrow(x3)))
{
  if(is.na(x3[i,]$avg_winning) == 1) 
  {
    x3[i,]$avg_winning <- x3[i,]$Win_Lose
  }
}

x3$Last_Winning <- x3$avg_winning

for(i in 2:nrow(x3))
{
  x3[i,]$Last_Winning <- x3[i-1,]$avg_winning 
}


x <- merge(x,x3[,c("matchId","Team", "Last_Winning")],by.x = c("matchId","Home"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Home_Win_Avg"
x <- merge(x,x3[,c("matchId","Team", "Last_Winning")],by.x = c("matchId","Away"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Away_Win_Avg"


temp$Home_Win_Avg <- x$Home_Win_Avg
temp$Away_Win_Avg <- x$Away_Win_Avg

#Remove unnecessary data
rm(x,x1,x2,x3)

#### END OF WINNING RATIO

##### TIE RATIO

x <- temp[,c("matchId","Home","Away","Match_Date","Result_Tie")]
x1 <- x[,c(1,2,4,5)]
x2 <- x[,c(1,3,4,5)]
names <- c("matchId", "Team", "Date", "Tie")
colnames(x1) <- names
colnames(x2) <- names
x3 <- rbind(x1,x2)
x3 <- x3[order(Team, Date)]

x3$avg_tie <- as.numeric(x3$Tie)

for(i in 5:(nrow(x3)))
{
  x3[i,]$avg_tie <- sum(x3[(i-4):i,]$Tie)*(x3[i-4,]$Team == x3[i,]$Team)/5
}

for(i in 1:(nrow(x3)))
{
  if(is.na(x3[i,]$avg_tie) == 1) 
  {
    x3[i,]$avg_tie <- x3[i,]$Tie
  }
}

x3$Last_Tie <- x3$avg_tie

for(i in 2:nrow(x3))
{
  x3[i,]$Last_Tie <- x3[i-1,]$avg_tie 
}



x <- merge(x,x3[,c("matchId","Team", "Last_Tie")],by.x = c("matchId","Home"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Home_Tie_Avg"
x <- merge(x,x3[,c("matchId","Team", "Last_Tie")],by.x = c("matchId","Away"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Away_Tie_Avg"

temp$Home_Tie_Avg <- x$Home_Tie_Avg
temp$Away_Tie_Avg <- x$Away_Tie_Avg


#Remove unnecessary data
rm(x,x1,x2,x3)


#############

x <- temp[,c("matchId", "Match_Date", "Home", "Away", "Half Time Home Team Goals", "Half Time Away Team Goals")]
x1 <- x[,c(1,2,3,5)]
x2 <- x[,c(1,2,4,6)]
names <- c("matchId","Date", "Team", "Score")
colnames(x1) <- names
colnames(x2) <- names
x3 <- rbind(x1,x2)
x3 <- x3[order(Team, Date)]
x3$avg_score <- as.numeric(x3$Score)

for(i in 5:(nrow(x3)))
{
  x3[i,]$avg_score <- sum(x3[(i-4):i,]$Score)*(x3[i-4,]$Team == x3[i,]$Team)/5
}

x3$Last_Average <- x3$avg_score

for(i in 2:nrow(x3))
{
  x3[i,]$Last_Average <- x3[i-1,]$avg_score 
}



x <- merge(x,x3[,c("matchId","Team", "Last_Average")],by.x = c("matchId","Home"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Home_Halftime_Goal_Avg"
x <- merge(x,x3[,c("matchId","Team", "Last_Average")],by.x = c("matchId","Away"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Away_Halftime_Goal_Avg"

temp$Home_Halftime_Goal_Avg <- x$Home_Halftime_Goal_Avg
temp$Away_Halftime_Goal_Avg <- x$Away_Halftime_Goal_Avg


#Remove unnecessary data
rm(x,x1,x2,x3)

##### end of halftime goals

######## shots on target


x <- temp[,c("matchId","Home","Away","Match_Date","Home Team Shots on Target","Away Team Shots on Target")]
x1 <- x[,c(1,2,4,5)]
x2 <- x[,c(1,3,4,6)]
names <- c("matchId", "Team", "Date", "Win_Lose")
colnames(x1) <- names
colnames(x2) <- names
x3 <- rbind(x1,x2)
x3 <- x3[order(Team, Date)]

x3$avg_winning <- as.numeric(x3$Win_Lose)

for(i in 5:(nrow(x3)))
{
  x3[i,]$avg_winning <- sum(x3[(i-4):i,]$Win_Lose)*(x3[i-4,]$Team == x3[i,]$Team)/5
}

for(i in 1:(nrow(x3)))
{
  if(is.na(x3[i,]$avg_winning) == 1) 
  {
    x3[i,]$avg_winning <- x3[i,]$Win_Lose
  }
}

x3$Last_Winning <- x3$avg_winning

for(i in 2:nrow(x3))
{
  x3[i,]$Last_Winning <- x3[i-1,]$avg_winning 
}


x <- merge(x,x3[,c("matchId","Team", "Last_Winning")],by.x = c("matchId","Home"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Home_Shot_on_Target_Avg"
x <- merge(x,x3[,c("matchId","Team", "Last_Winning")],by.x = c("matchId","Away"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Away_Shot_on_Target_Avg"


temp$Home_Shot_on_Target_Avg <- x$Home_Shot_on_Target_Avg
temp$Away_Shot_on_Target_Avg <- x$Away_Shot_on_Target_Avg

#Remove unnecessary data
rm(x,x1,x2,x3)

################# end of shots on target ####

############## shotssss

x <- temp[,c("matchId","Home","Away","Match_Date","Home Team Shots","Away Team Shots")]
x1 <- x[,c(1,2,4,5)]
x2 <- x[,c(1,3,4,6)]
names <- c("matchId", "Team", "Date", "Win_Lose")
colnames(x1) <- names
colnames(x2) <- names
x3 <- rbind(x1,x2)
x3 <- x3[order(Team, Date)]

x3$avg_winning <- as.numeric(x3$Win_Lose)

for(i in 5:(nrow(x3)))
{
  x3[i,]$avg_winning <- sum(x3[(i-4):i,]$Win_Lose)*(x3[i-4,]$Team == x3[i,]$Team)/5
}

for(i in 1:(nrow(x3)))
{
  if(is.na(x3[i,]$avg_winning) == 1) 
  {
    x3[i,]$avg_winning <- x3[i,]$Win_Lose
  }
}

x3$Last_Winning <- x3$avg_winning

for(i in 2:nrow(x3))
{
  x3[i,]$Last_Winning <- x3[i-1,]$avg_winning 
}


x <- merge(x,x3[,c("matchId","Team", "Last_Winning")],by.x = c("matchId","Home"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Home_Shot_Avg"
x <- merge(x,x3[,c("matchId","Team", "Last_Winning")],by.x = c("matchId","Away"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Away_Shot_Avg"


temp$Home_Shot_Avg <- x$Home_Shot_Avg
temp$Away_Shot_Avg <- x$Away_Shot_Avg

#Remove unnecessary data
rm(x,x1,x2,x3)


################ end of shots

####### fouls

x <- temp[,c("matchId","Home","Away","Match_Date","Home Team Fouls Committed","Away Team Fouls Committed")]
x1 <- x[,c(1,2,4,5)]
x2 <- x[,c(1,3,4,6)]
names <- c("matchId", "Team", "Date", "Win_Lose")
colnames(x1) <- names
colnames(x2) <- names
x3 <- rbind(x1,x2)
x3 <- x3[order(Team, Date)]

x3$avg_winning <- as.numeric(x3$Win_Lose)

for(i in 5:(nrow(x3)))
{
  x3[i,]$avg_winning <- sum(x3[(i-4):i,]$Win_Lose)*(x3[i-4,]$Team == x3[i,]$Team)/5
}

for(i in 1:(nrow(x3)))
{
  if(is.na(x3[i,]$avg_winning) == 1) 
  {
    x3[i,]$avg_winning <- x3[i,]$Win_Lose
  }
}

x3$Last_Winning <- x3$avg_winning

for(i in 2:nrow(x3))
{
  x3[i,]$Last_Winning <- x3[i-1,]$avg_winning 
}


x <- merge(x,x3[,c("matchId","Team", "Last_Winning")],by.x = c("matchId","Home"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Home_Fouls_Avg"
x <- merge(x,x3[,c("matchId","Team", "Last_Winning")],by.x = c("matchId","Away"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Away_Fouls_Avg"


temp$Home_Fouls_Avg <- x$Home_Fouls_Avg
temp$Away_Fouls_Avg <- x$Away_Fouls_Avg

#Remove unnecessary data
rm(x,x1,x2,x3)

############ end of fouls

############ Corners


x <- temp[,c("matchId","Home","Away","Match_Date","Home Team Corners","Away Team Corners")]
x1 <- x[,c(1,2,4,5)]
x2 <- x[,c(1,3,4,6)]
names <- c("matchId", "Team", "Date", "Win_Lose")
colnames(x1) <- names
colnames(x2) <- names
x3 <- rbind(x1,x2)
x3 <- x3[order(Team, Date)]

x3$avg_winning <- as.numeric(x3$Win_Lose)

for(i in 5:(nrow(x3)))
{
  x3[i,]$avg_winning <- sum(x3[(i-4):i,]$Win_Lose)*(x3[i-4,]$Team == x3[i,]$Team)/5
}

for(i in 1:(nrow(x3)))
{
  if(is.na(x3[i,]$avg_winning) == 1) 
  {
    x3[i,]$avg_winning <- x3[i,]$Win_Lose
  }
}

x3$Last_Winning <- x3$avg_winning

for(i in 2:nrow(x3))
{
  x3[i,]$Last_Winning <- x3[i-1,]$avg_winning 
}


x <- merge(x,x3[,c("matchId","Team", "Last_Winning")],by.x = c("matchId","Home"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Home_Corners_Avg"
x <- merge(x,x3[,c("matchId","Team", "Last_Winning")],by.x = c("matchId","Away"), by.y = c("matchId","Team")  )
colnames(x)[ncol(x)] <- "Away_Corners_Avg"


temp$Home_Corners_Avg <- x$Home_Corners_Avg
temp$Away_Corners_Avg <- x$Away_Corners_Avg

#Remove unnecessary data
rm(x,x1,x2,x3)

####### cornerss endddd

############## yellowwww carddd


x <- temp[,c("matchId","Home","Away","Match_Date","Home Team Yellow Cards","Away Team Yellow Cards","Home Team Red Cards","Away Team Red Cards" )]
x1 <- x[,c(1,2,4,5,7)]
x2 <- x[,c(1,3,4,6,8)]
names <- c("matchId", "Team", "Date", "yellow", "red")
colnames(x1) <- names
colnames(x2) <- names
x3 <- rbind(x1,x2)
x3 <- x3[order(Team, Date)]
x3$last_yellow <- x3$yellow
x3$last_red <- x3$red

for(i in 2:nrow(x3))
{
  x3[i,]$last_yellow <- x3[i-1,]$yellow
  x3[i,]$last_red <- x3[i-1,]$red
}







x <- merge(x,x3[,c("matchId","Team", "last_yellow", "last_red")],by.x = c("matchId","Home"), by.y = c("matchId","Team")  )
colnames(x)[c(ncol(x)-1,ncol(x))] <- c("Home_Last_Yellow", "Home_Last_Red")
x <- merge(x,x3[,c("matchId","Team", "last_yellow", "last_red")],by.x = c("matchId","Away"), by.y = c("matchId","Team")  )
colnames(x)[c(ncol(x)-1,ncol(x))] <- c("Away_Last_Yellow", "Away_Last_Red")


temp$Home_Last_Yellow <- x$Home_Last_Yellow
temp$Home_Last_Red <- x$Home_Last_Red
temp$Away_Last_Yellow <- x$Away_Last_Yellow
temp$Away_Last_Red <- x$Away_Last_Red
#Remove unnecessary data
rm(x,x1,x2,x3)


gc()
return(temp)
}