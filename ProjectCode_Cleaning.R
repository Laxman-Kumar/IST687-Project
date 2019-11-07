install.packages("RCurl")
install.packages("bitops")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("tidyverse")
library("RCurl")
library("bitops")
library("jsonlite")
library("dplyr")
library("tidyverse")
dataset <- read_json("fall2019-survey-M03.json")
fall <- jsonlite::fromJSON("fall2019-survey-M03.json")
View(fall)


####### - Laxman Kumar - ########

install.packages("hash")
library(hash)
library(sqldf)
library(ggplot2)
#Hash mapping of partner code with the partner names
partnerCodeToName <- hash(unique(fall$Partner.Code),unique(fall$Partner.Name))

#Creating new dataframe with origin, destination, partnercode, RecommendScore and freetext for faster accessing
textMiningDF <- data.frame(fall$Origin.City,fall$Destination.City,fall$Partner.Code,fall$Likelihood.to.recommend,fall$freeText)

#changin names of the columns since . creates problem while fetching
colnames(textMiningDF) <- c("OriginCity","DestinationCity","PartnerCode","RecommendScore","TextReview")

#changin the data in recommend score column to numeric
textMiningDF$RecommendScore <- as.numeric(textMiningDF$RecommendScore)

#Plotting average score of partner airlines on a bargraph
#Average score of Partner airlines groub by partner code 
averageScoreByPartner <- textMiningDF %>%
  group_by(PartnerCode) %>%
  summarise(mean(RecommendScore,na.rm = TRUE))
colnames(averageScoreByPartner)[2] <- "AverageScore"

averageScorePlot <- ggplot(data=averageScoreByPartner)+
              geom_col(aes(PartnerCode,AverageScore))
  
averageScorePlot


#### Joey - finding where there are no matches in Flight.time.in.minutes

#creating dataframe to isolate origin city, destination city, flight time
flights <- fall %>%
  group_by(Origin.City, Destination.City, Partner.Code)%>%
  summarise(mean(Flight.time.in.minutes, na.rm=TRUE))
View(flights)            

#isolating NAs in flights dataframe
flightsNA <- flights[flights$`mean(Flight.time.in.minutes, na.rm = TRUE)`== "NaN",]
flightsNA
#There are 11 flights without matching flight times, meaning they are the only case of these flights.