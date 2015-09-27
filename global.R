#setwd('/Users/acarlini/Documents/R Working Directory/DataProductsAssignment1')
#cat("\014")
#rm(list=ls()) 

#Load librarys and data from Lahman
library(Lahman)
library(dplyr)
library(ggplot2)
library(shiny)

data(Master)
data(HallOfFame)
data(Fielding)
data(Batting)

#Trim imported data to just variables of interest
Master <- mutate(Master,nameFull = paste(nameFirst,nameLast))
Master <- select(Master,playerID,nameFull,weight,height,bats,throws,debut,finalGame)
HallOfFame <- select(HallOfFame,playerID,yearID,inducted,category)
Fielding <- select(Fielding,playerID,POS,G)
Batting <- select(Batting,playerID,yearID,teamID,AB,R,H,X2B,X3B,HR,BB,SO,HBP,SH,SF)

#Correct sacrifice fly data for entries prior to 1954 when it officially started being tracked
Batting$SF <- ifelse(Batting$yearID<1954 & is.na(Batting$SF),0,Batting$SF)

#Generate list of players inducted into the hall of fame
hof <- HallOfFame[HallOfFame$inducted=="Y" &
                        HallOfFame$category=="Player",]

#Career fielding statistics for players
#---fielding stats only needed to choose players by position
#---players will be assigned to the position they played the most games at
#---designated hitters (POS="DH") will not be used by this application
Fielding2 <- Fielding %>%
      group_by(playerID,POS) %>%
      summarize(gamesPOS=sum(G)) %>%
      select(playerID,POS,gamesPOS)
Fielding2 <- group_by(Fielding2,playerID)
Fielding3 <- filter(Fielding2,gamesPOS==max(gamesPOS) & POS!="DH")

histBattingSeason <- filter(Batting,
                            (yearID==1927 & teamID=="NYA") |
                                  (yearID==1939 & teamID=="NYA") |
                                  (yearID==1975 & teamID=="CIN") |
                                  (yearID==1998 & teamID=="NYA") |
                                  (yearID==1929 & teamID=="PHA") |
                                  (yearID==1961 & teamID=="NYA") |
                                  (yearID==1902 & teamID=="PIT") |
                                  (yearID==1970 & teamID=="BAL") |
                                  (yearID==1984 & teamID=="DET") |
                                  (yearID==1907 & teamID=="CHN"))
histBatting <- semi_join(Batting,histBattingSeason,by = "playerID")
hofBatting <- semi_join(Batting,hof,by = "playerID")

#Create career stat data sets
#---Career batting statistics for players
hofBatting <- group_by(hofBatting,playerID)
hofBattingCareer <- summarize(hofBatting,
                              AB=sum(AB,na.rm = TRUE),
                              R=sum(R,na.rm = TRUE),
                              H=sum(H,na.rm = TRUE),
                              X2B=sum(X2B,na.rm = TRUE),
                              X3B=sum(X3B,na.rm = TRUE),
                              HR=sum(HR,na.rm = TRUE),
                              BB=sum(BB,na.rm = TRUE),
                              SO=sum(SO,na.rm = TRUE),
                              HBP=sum(HBP,na.rm = TRUE),
                              SH=sum(SH,na.rm = TRUE),
                              SF=sum(SF,na.rm = TRUE))
hofBattingCareer <- mutate(hofBattingCareer,
                           X1B=H-X2B-X3B-HR,
                           AVG=format(round(H/AB, digits = 3), nsmall = 3),
                           SLG=format(round((X1B+2*X2B+3*X3B+4*HR)/AB, digits = 3), nsmall = 3),
                           OBP=format(round((H+BB+HBP)/(AB+BB+HBP+SF), digits = 3), nsmall = 3))

histBatting <- group_by(histBatting,playerID)
histBattingCareer <- summarize(histBatting,
                               AB=sum(AB,na.rm = TRUE),
                               R=sum(R,na.rm = TRUE),
                               H=sum(H,na.rm = TRUE),
                               X2B=sum(X2B,na.rm = TRUE),
                               X3B=sum(X3B,na.rm = TRUE),
                               HR=sum(HR,na.rm = TRUE),
                               BB=sum(BB,na.rm = TRUE),
                               SO=sum(SO,na.rm = TRUE),
                               HBP=sum(HBP,na.rm = TRUE),
                               SH=sum(SH,na.rm = TRUE),
                               SF=sum(SF,na.rm = TRUE))
histBattingCareer <- mutate(histBattingCareer,
                            X1B=H-X2B-X3B-HR,
                            AVG=format(round(H/AB, digits = 3), nsmall = 3),
                            SLG=format(round((X1B+2*X2B+3*X3B+4*HR)/AB, digits = 3), nsmall = 3),
                            OBP=format(round((H+BB+HBP)/(AB+BB+HBP+SF), digits = 3), nsmall = 3))

#Calculate statistics for single season historical team entries (already at the player level)
histBattingSeason <- mutate(histBattingSeason,
                            X1B=H-X2B-X3B-HR,
                            AVG=format(round(H/AB, digits = 3), nsmall = 3),
                            SLG=format(round((X1B+2*X2B+3*X3B+4*HR)/AB, digits = 3), nsmall = 3),
                            OBP=format(round((H+BB+HBP)/(AB+BB+HBP+SF), digits = 3), nsmall = 3))

Master2 <- left_join(Master,hof,by="playerID")
Master2 <- mutate(Master2,nameFull=ifelse(is.na(yearID),
                                          nameFull,
                                          paste0(nameFull," (",yearID,")")))
Master3 <- left_join(Master2[,1:8],Fielding3,by="playerID")

histBattingSeasonMaster <- left_join(histBattingSeason,Master3)
histBattingCareerMaster <- left_join(histBattingCareer,Master3)
hofBattingCareerMaster <- left_join(hofBattingCareer,Master3)

#Choose one of the "top 10" of all time
#---team will be used for comparison to the one the user builds
getTeam<-function(y,t){
      histBattingSeasonMaster[histBattingSeasonMaster$yearID==y & histBattingSeasonMaster$teamID==t, ]
}

sumStats <- function(x) {
      theTeam <- summarize(x,
                           AB=sum(AB,na.rm = TRUE),
                           R=sum(R,na.rm = TRUE),
                           H=sum(H,na.rm = TRUE),
                           X2B=sum(X2B,na.rm = TRUE),
                           X3B=sum(X3B,na.rm = TRUE),
                           HR=sum(HR,na.rm = TRUE),
                           BB=sum(BB,na.rm = TRUE),
                           SO=sum(SO,na.rm = TRUE),
                           HBP=sum(HBP,na.rm = TRUE),
                           SH=sum(SH,na.rm = TRUE),
                           SF=sum(SF,na.rm = TRUE))
      theTeam <- mutate(theTeam,
                        X1B=H-X2B-X3B-HR,
                        AVG=format(round(H/AB, digits = 3), nsmall = 3),
                        SLG=format(round((X1B+2*X2B+3*X3B+4*HR)/AB, digits = 3), nsmall = 3),
                        OBP=format(round((H+BB+HBP)/(AB+BB+HBP+SF), digits = 3), nsmall = 3))
      return(theTeam)
}
getLabel <- function(statChoice) {
      if (statChoice=="AB") {statLabel <- "At Bats (AB)"}
      else if (statChoice=="R") {statLabel <- "Runs (R)"}
      else if (statChoice=="H") {statLabel <- "Hits (H)"}
      else if (statChoice=="HR") {statLabel <- "Home Runs (HR)"}
      else if (statChoice=="BB") {statLabel <- "Walks (BB)"}
      else if (statChoice=="SO") {statLabel <- "Strikouts (SO)"}
      else if (statChoice=="AVG") {statLabel <- "Batting Average (AVG)" = "AVG"}
      else if (statChoice=="SLG") {statLabel <- "Slugging Percentage (SLG)"}
      else if (statChoice=="OBP") {statLabel <- "On Base Percentage (OBP)"}
      return(statLabel)
}

abb<-c("AB","AVG","BB","H","HOF","HR","OBP","POS","R","SLG","SO")
meaning<-c("At Bats","Batting Average","Walks","Hits",
           "Hall of Fame (year of induction on Rosters)",
           "Home Runs","On Base Percentage","Position Played","Runs",
           "Slugging Percentage","Strikouts")
abbTable <- data.frame(cbind(abb,meaning))
