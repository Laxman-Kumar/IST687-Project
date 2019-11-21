data <- load("CleanedData.Rda")
View(df)
df$AgeRange <- cut(df$Age, breaks=c(15,19,49,85), labels=c("Teenager","Adult","Elderly Adult"))

#filter and remove HA

regression1 <- lm(LikelihoodRecommendScore ~ . -DestinationCity -OriginCity -FlightDate -OriginState -DestinationState -DestLong -DestLat
                  -OriginLong -OriginLat -Age, data=df)
summary(regression1)

table(df$PartnerCode)
table(df$TypeOfTravel)
table(df$Class) 
