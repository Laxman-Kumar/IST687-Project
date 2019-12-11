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

    #Field with NAs are DepartureDelay, ArrivalDelay, FlightTimeinMin
    
    ## All the NAs in Departure Delay column are due to flight cancellation.
    ## There are 19 Nas in Arrival Delay and Flight Time in Minutes even for flights which are not cancelled and those have to be treated.

    
  
    #Hash mapping of partner code with the partner names
    partnerCodeToName <- hash(unique(fall$Partner.Code),unique(fall$Partner.Name))     ## Creating a hash map between partner code and partner name
    
    b <- data.frame(t(data.frame(strsplit(fall$Destination.City,","))))             ## Removing the comma and state code from destination and origin city
    rownames(b) <- NULL
    a <- data.frame(t(data.frame(strsplit(fall$Origin.City,","))))
    rownames(a) <- NULL
    
  
    fall$DestinationStateAbbr <- trimws(b$X2)              ## Creating new Destination and origin state abbreviation columns
    fall$OriginStateAbbr <- trimws(a$X2)
    
    StateMap <- hash(fall$OriginStateAbbr,fall$Origin.State)    ## Creating hash map between state abbreviation and state name
    
    df <- fall
    
    df$Destination.City <- b$X1                   ## Reassigning cleaned values (after removing comma and state code) to dest & orig cities
    df$Origin.City <- a$X1
    
    remove(a)
    remove(b)
                                                 ## Removing the columns that are not necessary      
    df$Day.of.Month <- NULL
    df$freeText <- NULL              
    df$Origin.State <- NULL
    df$Destination.State <- NULL
    df$Partner.Name <- NULL
    
    #Replacing 1 NA in likelihood with mean at route & partner level
    averageLikelihood <- df %>%
      group_by(Origin.City, Destination.City, Partner.Code)%>%
      summarise(mean(Likelihood.to.recommend, na.rm=TRUE))
    
    indexOfNA <- which(is.na(df$Likelihood.to.recommend))
    df$Likelihood.to.recommend[indexOfNA] <-
      as.integer(averageLikelihood[averageLikelihood$Origin.City==df[indexOfNA,"Origin.City"] & 
                          averageLikelihood$Destination.City==df[indexOfNA,"Destination.City"] & 
                          averageLikelihood$Partner.Code==df[indexOfNA,"Partner.Code"],4])
    remove(indexOfNA)
    remove(averageLikelihood)
    
    #Code for filtering out the Na when flight cancelled is No
    
    df %>%
      select(Origin.City,Destination.City,Flight.cancelled,Flight.time.in.minutes) %>%
      filter(is.na(Flight.time.in.minutes) & Flight.cancelled=="No")
    
    #Replacing NA in Flight time in minutes when the flight was not cancelled, to mean at route & partner airline level
    
    averageFlightTime <- df %>%                                  ## Creating dataframe with average flight time in minutes at route and partner level
      group_by(Origin.City, Destination.City, Partner.Code)%>%
      summarise(mean(Flight.time.in.minutes, na.rm=TRUE))
    
  
    indexOfNAList <- which(is.na(df$Flight.time.in.minutes) & df$Flight.cancelled=="No")
    
    for(indexOfNA in indexOfNAList){
      df$Flight.time.in.minutes[indexOfNA] <-
        as.integer(averageFlightTime[averageFlightTime$Origin.City==df[indexOfNA,"Origin.City"] & 
                                       averageFlightTime$Destination.City==df[indexOfNA,"Destination.City"] & 
                                       averageFlightTime$Partner.Code==df[indexOfNA,"Partner.Code"],4])
    }
    
    ## Setting Flight Time in minutes to 0 for all cancelled flights.
    indexOfNAList <- which(is.na(df$Flight.time.in.minutes) & df$Flight.cancelled=="Yes")
    for(indexOfNA in indexOfNAList){df$Flight.time.in.minutes[indexOfNA] <- 0}
    
    
    ### For 3 route airline combinations, the average flight time in minutes was not obtained for which we manually found their flight time and assigned it
    indexOfNAList <- which(is.na(df$Flight.time.in.minutes) & df$Flight.cancelled=="No")
    df$Flight.time.in.minutes[indexOfNAList[1]] <- 90
    df$Flight.time.in.minutes[indexOfNAList[2]] <- 100
    df$Flight.time.in.minutes[indexOfNAList[3]] <- 135
    
    
    remove(indexOfNA)
    remove(averageFlightTime)
    indexOfNAList <- which(is.na(df$Flight.time.in.minutes))
    
    
    #Replacing Arrival and Departure delay Nas. Only Arrival Delay column has 19 NAs wherever flight was not cancelled, 
    ## which is treated in the same way as flight time in minutes
    
    averageArrivalDelay <- df %>%
      group_by(Origin.City, Destination.City, Partner.Code)%>%
      summarise(mean(Arrival.Delay.in.Minutes, na.rm=TRUE))
    
    ## Departure Delaay has Nas only when flight was cancelled
    
    indexArrivalDelay <- which(is.na(df$Arrival.Delay.in.Minutes) & df$Flight.cancelled=="No")     ## Obtaining indexes where arrival delay is NA & flight was not cancelled
    
    ### Treating Nas for arrival delay column where the flight was not cancelled
    for(i in indexArrivalDelay)
    {
      df$Arrival.Delay.in.Minutes[i] <-
        as.integer(averageArrivalDelay[averageArrivalDelay$Origin.City==df[i,"Origin.City"] & 
                                         averageArrivalDelay$Destination.City==df[i,"Destination.City"] & 
                                         averageArrivalDelay$Partner.Code==df[i,"Partner.Code"],4])
      if(is.na(df$Arrival.Delay.in.Minutes[i]))
        {
        df$Arrival.Delay.in.Minutes[i] <-df$Departure.Delay.in.Minutes[i]
        }
      
    } 
    
    ### Replacing Nas where flight was cancelled with NaN (Not a number) for both Arrival Delay and Departure Delay columns.
    
    df$Arrival.Delay.in.Minutes[which(is.na(df$Arrival.Delay.in.Minutes) & df$Flight.cancelled=="Yes")] <- NaN
    df$Departure.Delay.in.Minutes[which(is.na(df$Departure.Delay.in.Minutes) & df$Flight.cancelled=="Yes")] <- NaN
    
    df2 <- df
    
    ## Creating new column names to be renamed
    
    columnNames <- c("DestinationCity","OriginCity","AirlineStatus","Age","Gender","PriceSensitivity",
                     "YearOfFirstFlight","FLightsPerYear","Loyalty","TypeOfTravel","TotalFreqFlyAccount",
                     "ShoppingAmount","FoodExpenses","Class","FlightDate","PartnerCode",
                     "ScheduleDepHour","DepartureDelayInMin","ArrivalDelayInMin",
                     "FlightCancelled","FlightDuration","Distance","LikelihoodRecommendScore",
                     "OriginLong","OriginLat","DestLong","DestLat","DestinationState","OriginState")     
    
    colnames(df) <- columnNames
    
    df$AgeGroup <- cut(df$Age, breaks = c(0,18,36,54, Inf), labels = c('0-18','18-36','36-54','>54'), right = FALSE)
    
    
    remove(i)
    
    remove(indexArrivalDelay)
    remove(averageArrivalDelay)
    
    remove(indexDepartureDelay)
    remove(averageDepartureDelay)
    
    remove(columnNames)
    remove(fall)
    remove(df2)
    remove(indexOfNAList)
    saveRDS(df,file="CleanedData.Rda")
    saveRDS(partnerCodeToName,file="PartnerName.Rda")
    saveRDS(StateMap,file="StateName.Rda")
    
    testdf <- readRDS(file = "CleanedData.Rda")
    write.csv(df, file = "cleandata.csv")
    
    #### DATA CLEANING ENDS HERE ####
         
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