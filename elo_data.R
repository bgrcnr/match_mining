library(data.table)
library("httr")

clubnames <- c("Tottenham", "AstonVilla", "Wolves","Bolton","Wigan","Sunderland", "Blackburn","Chelsea", "Liverpool",
               "ManUnited", "WestBrom","Birmingham","Everton","Stoke","WestHam", "Arsenal", "Newcastle","Fulham",
               "ManCity","Blackpool","QPR", "Swansea","Norwich","Reading" ,"Southampton","CrystalPalace","Hull",
              "Cardiff", "Leicester","Burnley","Bournemouth","Watford","Middlesbrough","Brighton","Huddersfield" )
elo_data <- as.data.table(NULL)

for (names in clubnames){
  query <- paste("api.clubelo.com/", names, sep="")
  out <- GET(url=query)
  data <- as.data.table(content(out))
  data <- data[From >= "2010-08-14"]
  data <- data[,-c("Level", "Country","Rank")]
  elo_data <- rbind(elo_data,data)
  rm(data,names,out,query)
}

elo_data <- as.data.table(lapply(elo_data, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

elo_data[Club=="man united", Club:= "manchester united"]
elo_data[Club=="man city", Club:= "manchester city"]


elo_data[,Diff:=To-From]
n.times <- elo_data$Diff + 1
elo_data <- elo_data[rep(seq_len(nrow(elo_data)),n.times)]



for(i in 1:28337)
{
  dates = NULL
  if(min(elo_data[index==i]$From) != unique(elo_data[index==i]$To))
  {
  dates <- seq(as.Date(min(elo_data[index==i]$From)),as.Date(unique(elo_data[index==i]$To)), by="days")
  elo_data[index==i]$From <- dates
  }
}

elo_data <- elo_data[,-c("To", "Diff")]
elo_data <- elo_data[order(Club,From)]

write.csv(elo_data, "C:/Users/Bugra/Documents/GitHub/match_mining/elo_data.csv" )

