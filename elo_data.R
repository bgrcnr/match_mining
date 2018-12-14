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
elo_data[order(Club,From)]
elo_data <- elo_data[,-c("To", "Diff")]



for (i in 2:(nrow(elo_data))) {
  if (elo_data$Elo[i] == elo_data$Elo[i-1]){
    elo_data$From[i] = (elo_data$From[i-1] + 1)
  }
}


source('data_preprocessing.r')
matches_data_path = "Files/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds"
matches_raw=readRDS(matches_data_path)
matches <- matches_data_preprocessing(matches_raw)

setkey(matches, c("Home", "Match_Date"))
setkey(elo_data, c("Club", "From"))

matches <- merge(x=matches, y=elo_data, by.x = c("Home", "Match_Date"), by.y = c("Club", "From"), all.x=TRUE)
matches$Home_ELO = matches$Elo
matches[,Elo := NULL]

setkeyv(matches, c("Away", "Match_Date"))
setkeyv(elo_data, c("Club", "From"))

matches <- merge(x=matches, y=elo_data, by.x = c("Away", "Match_Date"), by.y = c("Club", "From"), all.x=TRUE)
matches$Away_ELO = matches$Elo
matches[,Elo := NULL]
