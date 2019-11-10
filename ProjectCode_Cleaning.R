install.packages("RCurl")
install.packages("bitops")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("hash")
install.packages("tidyverse")
library("RCurl")
library("bitops")
library("jsonlite")
library("dplyr")
library("tidyverse")
library("hash")
dataset <- read_json("fall2019-survey-M03.json")
fall <- jsonlite::fromJSON("fall2019-survey-M03.json")
remove(dataset)
View(fall)

####### - Cleaning Data Starts here  - ######
####### - BY - Laxman Kumar, Bhavish Kumar and Vidushi Mishra - ####

    ## DepartureDelay, ArrivalDelay, FlightTimesInMinutes(There are 33 rows with no combination of values)

    columnNames <- c("DestinationCity","OriginCity","AirlineStatus","Age","Gender","PriceSensitivity",
                     "YearOfFirstFlight","FLightsPerYear","Loyalty","TypeOfTravel","TotalFreqFlyAccount",
                     "ShoppingAmount","FoodExpenses","Class","FlightDate","PartnerCode",
                     "ScheduleDepHour","DepartureDelayInMin","ArrivalDelayInMin",
                     "FlightCancelledByUser","FlightDuration","Distance","LikelihoodRecommendScore",
                     "OriginLong","OriginLat","DestLong","DestLat","DestinationState","OriginState")
  
    #Hash mapping of partner code with the partner names
    partnerCodeToName <- hash(unique(fall$Partner.Code),unique(fall$Partner.Name))
    
    b <- data.frame(t(data.frame(strsplit(fall$Destination.City,","))))
    rownames(b) <- NULL
    a <- data.frame(t(data.frame(strsplit(fall$Origin.City,","))))
    rownames(b) <- NULL
      
    fall$DestinationStateAbbr <- trimws(b$X2)
    fall$OriginStateAbbr <- trimws(a$X2)
    
    StateMap <- hash(fall$OriginStateAbbr,fall$Origin.State)
    
    df <- fall
    
    df$Destination.City <- b$X1
    df$Origin.City <- a$X1
    
    remove(a)
    remove(b)
    
    df$Day.of.Month <- NULL
    df$freeText <- NULL
    df$Origin.State <- NULL
    df$Destination.State <- NULL
    df$Partner.Name <- NULL
    
    #Replacing NA in likelihood to mean of route of partner airline
    averageLikelihood <- df %>%
      group_by(Origin.City, Destination.City, Partner.Code)%>%
      summarise(mean(Likelihood.to.recommend, na.rm=TRUE))
    
    indexOfNA <- which(is.na(df$Likelihood.to.recommend))
    df$Likelihood.to.recommend[indexOfNA] <-
      as.integer(averageLikelihood[averageLikelihood$Origin.City==df[indexOfNA,2] & 
                          averageLikelihood$Destination.City==df[indexOfNA,1] & 
                          averageLikelihood$Partner.Code==df[indexOfNA,"Partner.Code"],4])
    remove(indexOfNA)
    remove(averageLikelihood)
    
    #Replacing NA in Flight time in minutes to mean of route of partner airline
    #Assuming flight cancelled is customer data
    averageFlightTime <- df %>%
      group_by(Origin.City, Destination.City, Partner.Code)%>%
      summarise(mean(Flight.time.in.minutes, na.rm=TRUE))
    
    indexOfNAList <- which(is.na(df$Flight.time.in.minutes))
    for(indexOfNA in indexOfNAList){
      df$Flight.time.in.minutes[indexOfNA] <-
        as.integer(averageFlightTime[averageFlightTime$Origin.City==df[indexOfNA,2] & 
                                       averageFlightTime$Destination.City==df[indexOfNA,1] & 
                                       averageFlightTime$Partner.Code==df[indexOfNA,"Partner.Code"],4])
      
    }
  
    remove(indexOfNA)
    remove(averageFlightTime)
    indexOfNAList <- which(is.na(df$Flight.time.in.minutes))
    
    #Replacing Arrival and Departure delay
    
    averageArrivalDelay <- df %>%
      group_by(Origin.City, Destination.City, Partner.Code)%>%
      summarise(mean(Arrival.Delay.in.Minutes, na.rm=TRUE))
    
    averageDepartureDelay <- df %>%
      group_by(Origin.City, Destination.City, Partner.Code)%>%
      summarise(mean(Departure.Delay.in.Minutes, na.rm=TRUE))
    
    indexArrivalDelay <- which(is.na(df$Arrival.Delay.in.Minutes))
    indexDepartureDelay <- which(is.na(df$Departure.Delay.in.Minutes))
    
    for(i in indexArrivalDelay){
      df$Arrival.Delay.in.Minutes[i] <-
        as.integer(averageArrivalDelay[averageArrivalDelay$Origin.City==df[i,2] & 
                                         averageArrivalDelay$Destination.City==df[i,1] & 
                                         averageArrivalDelay$Partner.Code==df[i,"Partner.Code"],4])
      if(is.na(df$Arrival.Delay.in.Minutes[i])){
        df$Arrival.Delay.in.Minutes[i] <-0
      }
      
    } 
    
    
    for(i in indexDepartureDelay){
      df$Departure.Delay.in.Minutes[i] <-
        as.integer(averageDepartureDelay[averageDepartureDelay$Origin.City==df[i,2] & 
                                           averageDepartureDelay$Destination.City==df[i,1] & 
                                           averageDepartureDelay$Partner.Code==df[i,"Partner.Code"],4])
      if(is.na(df$Departure.Delay.in.Minutes[i])){
        df$Departure.Delay.in.Minutes[i] <-0
      }
      
    }
    
    df2 <- df
    colnames(df) <- columnNames
    
    remove(i)
    
    remove(indexArrivalDelay)
    remove(averageArrivalDelay)
    
    remove(indexDepartureDelay)
    remove(averageDepartureDelay)
    
    remove(columnNames)
    remove(fall)
    
    save(df,file="CleanedData.Rda")
    
     
####### - Laxman Kumar - ########

library(sqldf)
library(ggplot2)



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

flights <- fall %>%
  group_by(Origin.City, Destination.City, Partner.Code)%>%
  summarise(mean(Likelihood.to.recommend, na.rm=TRUE))
View(flights)            



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