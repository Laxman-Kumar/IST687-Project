install.packages("dplyr",dependencies = TRUE)
install.packages("hash",dependencies = TRUE)
install.packages("tidyverse",dependencies = TRUE)
install.packages("ggiraph",dependencies = TRUE)
install.packages("plotly",dependencies = TRUE)
install.packages("ggplot2",dependencies=TRUE)
install.packages("RColorBrewer",dependencies=TRUE)
install.packages("colorRamps",dependencies=TRUE)
install.packages("gapminder",dependencies=TRUE)
install.packages("gganimate",dependencies=TRUE)

library(dplyr)
library(hash)
library(tidyverse)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(colorRamps)
library(gapminder)
library(gganimate)
library(ggthemes)

df <- readRDS('CleanedData.Rda')
partnerNameHash <- readRDS("PartnerName.Rda")
stateNameHash <- readRDS("StateName.Rda")

partnerNameHash


plotTheme <- theme_classic()+theme(axis.title.x = element_text(size = 12),
                                   axis.title.y = element_text(size = 10),plot.title = element_text(size = 15, hjust = 0.5),
                                   panel.grid.major = element_line(color="#e6e6e6",linetype=1))

### *** BOX PLOTS *** ###

classPlot <- ggplot(df,aes(Class,LikelihoodRecommendScore))+geom_boxplot(aes(fill=Class))+
  xlab("Traveller Class")+ ylab("Likelihood to recommend Score")+plotTheme                     ### Boxplot for each traveller classes
classPlot

genderPlot <- ggplot(df,aes(Gender,LikelihoodRecommendScore))+geom_boxplot(aes(fill=Gender))+
  xlab("Gender")+ ylab("Likelihood to recommend Score")+plotTheme                                ## Boxplot for the genders
genderPlot

traveltypePlot <-ggplot(df,aes(TypeOfTravel,LikelihoodRecommendScore))+geom_boxplot(aes(fill=TypeOfTravel))+
  xlab("Traveller Type")+ ylab("Likelihood to recommend Score")+plotTheme                             ## Boxplot for traveller types
traveltypePlot

airlineStatusPlot <- ggplot(df,aes(AirlineStatus,LikelihoodRecommendScore))+geom_boxplot(aes(fill=AirlineStatus))+
  xlab("Airline Status")+ ylab("Likelihood to recommend Score")+plotTheme           ## Boxplot for airline status
airlineStatusPlot

p1 <- genderPlot+theme(legend.position = "none")
p2 <- traveltypePlot+theme(legend.position = "none")
p3 <- airlineStatusPlot+theme(legend.position = "none")
p4 <- classPlot+theme(legend.position = "none")+ggtitle("Box plot of Likelihood score w.r.t gender, class, travel type and airline status")

subplot(p1,p2,p3,p4,nrows=2,margin = 0.05)

### Boxplot for Age Groups

ageGroupPlot <-ggplot(df,aes(AgeGroup,LikelihoodRecommendScore))+
  xlab("Age Groups")+ ylab("Likelihood to recommend Score")+
  ggtitle("Likelihood Recommendation Score by age group")+plotTheme

p1 <- ageGroupPlot+geom_boxplot(aes(fill=AgeGroup))            ## BOXPLOT for Age Groups
p2 <- ageGroupPlot+geom_violin(aes(fill=AgeGroup))
#table(df$TypeOfTravel)
subplot(p1,p2)


scoreByPartner <- ggplot(df,aes(PartnerCode,LikelihoodRecommendScore,partnerNameHash))+
  xlab("Partner Airlines")+ ylab("Likelihood to recommend Score")+
  ggtitle("Likelihood Recommendation Score by Partner Airlines")+
  plotTheme+theme(legend.position = "none")

p1 <- scoreByPartner+geom_violin(aes(fill=PartnerCode))
p2 <- scoreByPartner+geom_boxplot(aes(fill=PartnerCode))
#scoreByPartner
subplot(p2,p1)
#ggplotly(scoreByPartner)

### NUMBER OF REVIEWS (customers) BY PARTNER & GENDER

dfT <- data.frame(table(df$PartnerCode,df$Gender))
colnames(dfT) <- c("PartnerCode","Gender","Count")
genderPlot <- ggplot(dfT,aes(x=PartnerCode,y=Count,group=Gender))+
  geom_col(aes(fill=Gender),show.legend=FALSE,position = "dodge")+
  xlab("Partner Airlines")+ ylab("Number of customer surveys")+
  ggtitle("Number of customer surveys by gender & partner airlines")+
  plotTheme+theme(plot.title = element_text(size = 12, hjust = 0.5))

ggplotly(genderPlot,tooltip=c("text","x","y"),dynamicTicks = TRUE)

#ggplotly(genderPlot,tooltip=c("text","x","y"),dynamicTicks = TRUE)


## NUMBER OF CUSTOMER SURVEYS BY CLass, Airline Status, Gender, Agegroups

dfClass <- data.frame(table(df$Class,df$Gender))
colnames(dfClass) <- c("Class","Gender","Count")
#dfClass
dfStatus <- data.frame(table(df$AirlineStatus,df$Gender))
colnames(dfStatus) <- c("AirlineStatus","Gender","Count")
#dfStatus
dfAge <- data.frame(table(df$AgeGroup,df$Gender))
colnames(dfAge) <- c("AgeGroup","Gender","Count")
#dfAge
dfType <- data.frame(table(df$TypeOfTravel,df$Gender))
colnames(dfType) <- c("dfType","Gender","Count")

p1 <- ggplot(dfClass,aes(x=Class,y=sort(Count),group=Gender))+
  geom_col(aes(fill=Gender),show.legend=FALSE,position = "dodge")+
  xlab("Class")+ ylab("Gender")+plotTheme+theme(legend.position = "none")

p2 <- ggplot(dfStatus,aes(x=AirlineStatus,y=sort(Count),group=Gender))+
  geom_col(aes(fill=Gender),show.legend=FALSE,position = "dodge")+
  xlab("Partner Airlines")+ ylab("Gender")+plotTheme+theme(legend.position = "none")

p3 <- ggplot(dfAge,aes(x=AgeGroup,y=sort(Count),group=Gender))+
  geom_col(aes(fill=Gender),show.legend=FALSE,position = "dodge")+
  xlab("Partner Airlines")+ ylab("Gender")+plotTheme+theme(legend.position = "none")

p4 <- ggplot(dfType,aes(x=dfType,y=sort(Count),group=Gender))+
  geom_col(aes(fill=Gender),show.legend=FALSE,position = "dodge")+
  xlab("Partner Airlines")+ ylab("Gender")+plotTheme+theme(legend.position = "none")


ggplotly(genderPlot)
subplot(p1,p2,p3,p4,nrows=2,margin=0.05)

### NO of PROMOTERS, DETRACTORS and PASSIVE for southeast airines, across cancelled and non cancelled flights

df$recommender_type <- cut(df$LikelihoodRecommendScore, breaks = c(0,7,9, Inf), labels = c('Detractors','Passive','Promoters'), right = FALSE)   ## Creating Recommender Type categorical variable

partner_cancellation_nps <- data.frame(table(df$FlightCancelled,df$recommender_type))    ## Number of Detractors, Promoters & Passives for each partner & cancellation status

colnames(partner_cancellation_nps) <- c('FlightCancellationStatus','RecommenderType','Number')

partner_flight_status_no <- ggplot(partner_cancellation_nps[partner_cancellation_nps$FlightCancellationStatus == 'No',],aes(x=FlightCancellationStatus,y=Number,group=RecommenderType))+
  geom_col(aes(fill=RecommenderType))+
  xlab("Flight Cancellation Status")+ ylab("Number")+plotTheme

partner_flight_status_yes <- ggplot(partner_cancellation_nps[partner_cancellation_nps$FlightCancellationStatus == 'Yes',],aes(x=FlightCancellationStatus,y=Number,group=RecommenderType))+
  geom_col(aes(fill=RecommenderType))+
  xlab("Flight Cancellation Status")+ ylab("Number")+plotTheme


ggplotly(partner_flight_status,tooltip=c("text","x","y"))


### DAY OF WEEK PLOT
install.packages("lubridate")
library(lubridate)
install.packages("ggthemr")
library(ggthemr)

df$day <- weekdays(as.Date(mdy(df$FlightDate)))
days <- data.frame(table(df$PartnerCode,df$day))
p1 <- ggplot(days,aes(x=Var2,y=Freq,group=Var2,fill=Var2))+
  geom_col(show.legend=FALSE,position = "dodge")+
  plotTheme+scale_fill_brewer(type="qual")
ggplotly(p1)
