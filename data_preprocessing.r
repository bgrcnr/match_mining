

#' Data Preprocessing for Matches Data
#'
#' Makes preprocessing on raw matches data so that it can be used by other functions.
#'
#' @param data A data.table containing raw match data
#' @export
#' @examples
#' matches_data_preprocessing(matches_raw)
#'

matches_data_preprocessing <- function(data){

  temp = copy(data)
  temp = unique(temp,by="matchId") 
  setnames(temp,c("home","away","score","date"),c("Home","Away","Score","Match_Date"))
  ##in order to eliminate repeated team names
  temp[Home %in% c("manchester-utd", "manchester-united"), Home:= "manchester united"]
  temp[Home == "manchester-city", Home:= "manchester city"]
  temp[Home == "crystal-palace", Home:= "crystal palace"]
  temp[Home %in% c("newcastle utd","newcastle united") , Home:= "newcastle"]
  temp[Home == "stoke city", Home:= "stoke"]
  temp[Home == "west-ham", Home:= "west ham"]
  temp[Away %in% c("manchester-utd", "manchester-united"), Away:= "manchester united"]
  temp[Away == "manchester-city", Away:= "manchester city"]
  temp[Away == "crystal-palace", Away:= "crystal palace"]
  temp[Away %in% c("newcastle utd","newcastle united") , Away:= "newcastle"]
  temp[Away == "stoke city", Away:= "stoke"]
  temp[Away == "west-ham", Away:= "west ham"]
  #####################################################################################
  ##city info added
  cities = c("blackburn","blackpool", "bolton", "bournemouth","brighton","burnley","cardiff","huddersfield","hull city",
             "leicester","manchester","middlesbrough","newcastle","norwich","portsmouth","reading","southampton",
             "stoke","sunderland","swansea","watford","wigan","wolverhampton")
  temp[Home %in% cities, Home_City := Home]
  temp[Away %in% cities, Away_City := Away]
  temp[Home %in% c("arsenal", "chelsea","crystal palace","fulham","qpr","tottenham","west ham"), Home_City:= "london"]
  temp[Away %in% c("arsenal", "chelsea","crystal palace","fulham","qpr","tottenham","west ham"), Away_City:= "london"]
  temp[Home %in% c("aston villa", "birmingham", "west brom"), Home_City:= "birmingham"]
  temp[Away %in% c("aston villa", "birmingham", "west brom"), Away_City:= "birmingham"]
  temp[Home %in% c("manchester united", "manchester city"), Home_City:="manchester"] 
  temp[Away %in% c("manchester united", "manchester city"), Away_City:="manchester"] 
  temp[Home %in% c("everton", "liverpool"), Home_City:= "liverpool"]
  temp[Away %in% c("everton", "liverpool"), Away_City:= "liverpool"]
  temp[Home == "wolves", Home_City:= "wolverhampton"]
  temp[Away == "wolves", Away_City:= "wolverhampton"]
  #####################################################################################  
  
  
  temp[,Home:=tolower(Home)]
  temp[,Away:=tolower(Away)]
  ### Unix Date is added by Bugra
  temp[,Unix_Date := Match_Date]
  temp[,Match_DateTime:=as.POSIXct(Match_Date,tz="UTC",origin = as.POSIXct("1970-01-01",tz="UTC"))]
  temp[,Match_Hour := format(strptime(Match_DateTime,"%Y-%m-%d %H:%M:%OS"),'%H')]
  temp[,Match_Hour := as.numeric(Match_Hour)]
  temp[,Match_Date := as.Date(Match_DateTime,format="%Y-%m-%d")]
  ### Match_Day is added by bugra, works with lubridate package
  temp[,Match_Day := wday(Match_Date)]
  temp[,AWA_FLAG:=0]
  temp[(Score %like% "AWA."),`:=`(Score=gsub("AWA.","",Score),AWA_FLAG=1)]
  temp[,POSTP_FLAG:=0]
  temp[(Score == "POSTP."),`:=`(Score=gsub("POSTP.","",Score),POSTP_FLAG=1)]
  temp[,CAN_FLAG:=0]
  temp[(Score == "CAN."),`:=`(Score=gsub("CAN.","",Score),CAN_FLAG=1)]
  latestDateTimeofKnownScore = max(temp[Score!=""]$Match_DateTime)
  POSTP_toberemoved = temp[which(temp$POSTP_FLAG==1 & temp$Match_DateTime <= latestDateTimeofKnownScore)]$matchId
  if(length(POSTP_toberemoved)!=0){
    cat("Following postponed matches are REMOVED during data_preprocessing:\n")
    print(data[matchId%in%POSTP_toberemoved,])
    temp=temp[!matchId%in%POSTP_toberemoved,]
  }
  POSTP_tobekept = temp[which(temp$POSTP_FLAG==1 & temp$Match_DateTime > latestDateTimeofKnownScore)]$matchId
  if(length(POSTP_tobekept)!=0){
    cat("Following postponed matches are KEPT during data_preprocessing:\n")
    print(data[matchId%in%POSTP_tobekept,])
  }

  temp[,c("Home_Score","Away_Score") := lapply(tstrsplit(Score,":",fixed=T),as.integer)]
  temp[,`:=`(Match_Result = ifelse(Home_Score == Away_Score, "Tie" , ifelse(Home_Score > Away_Score, 'Home' , 'Away'))
             ,Total_Score = Home_Score + Away_Score)]
  temp[,`:=`(Result_Home = ifelse(Match_Result=="Home",1,0)
             ,Result_Tie = ifelse(Match_Result=="Tie",1,0)
             ,Result_Away = ifelse(Match_Result=="Away",1,0))]

  temp[,c('Score','Match_DateTime','AWA_FLAG','POSTP_FLAG','CAN_FLAG'):=NULL]
  gc()
  return(temp)
}

#' Data Preprocessing for Odd Details Data
#'
#' Makes preprocessing on raw odd details data so that it can be used by other functions.
#' only 1x2 bets are considered, if you are interested in other type of odds manipulate ''which_bets''
#' @param data A data.table containing raw odd details data
#' @param remove_bookmaker A character vector containing the bookmakers to be removed
#' @export
#' @examples

details_data_preprocessing <- function(data,matches,which_bets=c('1x2'),remove_bookmaker=c('BetfairExchange','PaddyPower'),removeOlderThan=30){
  # data manipulation for historical odd data
  details = copy(data)
  
  #remove duplicate entries
  details = unique(details)

  details = details[betType %in% which_bets]
  details[,totalhandicap:=NULL]

  details = merge(details,matches[,list(matchId,Match_Date)],by="matchId",all.x=T )
  setnames(details,"date","OddChangeDateTime")
  details[,OddChangeDateTime:=as.POSIXct(OddChangeDateTime,tz="UTC",origin = as.POSIXct("1970-01-01",tz="UTC"))]
  details = details[difftime(Match_Date,OddChangeDateTime, units = "days") <= removeOlderThan] #remove odds seen earlier than 10 days from the match date
  details[, odd := as.numeric(odd)]

  details[,bookmaker:=gsub(" |-","",bookmaker)]
  if(!is.null(remove_bookmaker)){
    details = details[!(bookmaker %in% remove_bookmaker)]
  }

  gc()
  return(details)
}

