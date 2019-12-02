data <- read.csv("cleandata.csv")
data <- data.frame(data)
View(data)

###Making some changes to data
data$TotalDelays <- data$DepartureDelayInMin+data$ArrivalDelayInMin

data$FlightCancelledNumeric <- as.numeric(data$FlightCancelled)
#no is 1, yes is 2

data$AirportSpending <- data$ShoppingAmount + data$FoodExpenses

###Linear Models
#First Test Regression - takes out Origin and Destination
regression1 <- lm(LikelihoodRecommendScore ~ AirlineStatus + Age + AgeGroup + Gender + PriceSensitivity + Loyalty + TypeOfTravel
                  + TotalFreqFlyAccount + AirportSpending + Class + ScheduleDepHour + TotalDelays + FlightCancelledNumeric + FlightDuration
                  + Distance + PartnerCode, data=data)
#error with singularities for FlightCancelledNumeric
summary(regression1)
#Adj R-Squared = .4224

#Second Regression - Takes out Age (only age group) and FlightCancelledNumeric (since data didn't work)
regression2 <- lm(LikelihoodRecommendScore ~ AirlineStatus + AgeGroup + Gender + PriceSensitivity + Loyalty + TypeOfTravel
                  + TotalFreqFlyAccount + AirportSpending + Class + ScheduleDepHour + TotalDelays + FlightDuration
                  + Distance + PartnerCode, data=data)
summary(regression2)
#Adj R-Squared = .4218


#Third Regression - Takes out Age (only age group) and FlightCancelledNumeric (since data didn't work) and Loyalty and PriceSensitivity
regression3 <- lm(LikelihoodRecommendScore ~ AirlineStatus + AgeGroup + Gender + TypeOfTravel
                  + TotalFreqFlyAccount + AirportSpending + Class + ScheduleDepHour + TotalDelays + FlightDuration
                  + Distance + PartnerCode, data=data)
summary(regression3)
#Adj R-Squared = .4213


table(data$PartnerCode)

