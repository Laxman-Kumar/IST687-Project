df<- readRDS(file = "CleanedData.Rda")
df_arm <- df
df_arm$recommandornot <- cut(df$LikelihoodRecommendScore, breaks = c(0,7,9, Inf), labels = c('Detractors','Passive','Promoters'), right = FALSE)   ## Creating Recommender Type categorical variable
library(arules)
library(arulesViz)
df_arm<- data.frame(df_arm$Gender,df_arm$Class,df_arm$AgeGroup,df_arm$AirlineStatus,df_arm$TypeOfTravel,df_arm$FlightCancelled,df_arm$recommandornot)
df_arm$Class<- as.factor(df_arm$Class)
df_arm$Gender<- as.factor(df_arm$Gender)
df_arm$AgeGroup<- as.factor(df_arm$AgeGroup)
df_arm$AirlineStatus<- as.factor(df_arm$AirlineStatus)
df_arm$TypeOfTravel<- as.factor(df_arm$TypeOfTravel)
df_arm$FlightCancelled<- as.factor(df_arm$FlightCancelled)
df_arm$recommandornot<- as.factor(df_arm$recommandornot)
dfX <- as(df_arm,"transactions")
ruleset <- apriori(dfX, 
                   parameter=list(support=0.005,confidence=0.5),
                   appearance = list(default="lhs", rhs=("dfX.recommandornot=Yes")))
inspect(ruleset)
inspectDT(ruleset)

View(df_arm)

