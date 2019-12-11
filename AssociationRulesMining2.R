
df <- read.csv(file = "cleandata.csv", header = header, sep = sep)
getwd()
setwd("C:\\Users\\vidus\\OneDrive\\Desktop\\Documents")

df <- readRDS(file = "CleanedData.Rda")
library(arules)
library(arulesViz)
library(tidyverse)
View(df)
table(df$recommender_type)
df$recommender_type <- cut(df$LikelihoodRecommendScore, breaks = c(0,7,9, Inf),
                           labels = c('Detractors','Passive','Promoters'), right = FALSE)   ## Creating Recommender Type categorical variable

Associative_Df <- df %>% select (AirlineStatus,Gender,
                                 TypeOfTravel,Class,AgeGroup,
                                 recommender_type)

Associative_Df$AirlineStatus <- as.factor(Associative_Df$AirlineStatus)
Associative_Df$AgeGroup <- as.factor(Associative_Df$AgeGroup)
Associative_Df$Gender <- as.factor(Associative_Df$Gender)
Associative_Df$TypeOfTravel <- as.factor(Associative_Df$TypeOfTravel)
Associative_Df$Class <- as.factor(Associative_Df$Class)
Associative_Df$recommender_type <- as.factor(Associative_Df$recommender_type)

Associative_DfX <- as(Associative_Df,"transactions")
#Associative_DfX
#inspect(Associative_DfX)

ruleset <- apriori(Associative_DfX,     ### RULES FOR DETRACTORS
                   parameter=list(support=0.005,confidence=0.5), # Setting support as 0.5% and confidence as 50%       
                   appearance = list(default="lhs", rhs=("recommender_type=Detractors")))

inspect(ruleset)
inspectDT(ruleset)


ruleset_p <- apriori(Associative_DfX,     ### RULES FOR PROMOTERS
                   parameter=list(support=0.005,confidence=0.5), # Setting support as 0.5% and confidence as 50%       
                   appearance = list(default="lhs", rhs=("recommender_type=Promoters")))

inspect(ruleset_p)
inspectDT(ruleset_p)
