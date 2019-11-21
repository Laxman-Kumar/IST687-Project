library(ggplot2)
install.packages("ggiraph",dependencies = TRUE)
library(ggiraph)
install.packages("plotly")
library(plotly)

generateScoreGraph <- function(X1,xlabel,ylabel,title,yRange,colorLength){
  generatedPlot <-ggplot(df,aes_string(x=X1,y="LikelihoodRecommendScore"))+
    stat_summary(fun.y = "mean",geom="bar",colour=colorLength,fill=colorLength) + 
    xlab(xlabel)+ ylab(ylabel)+
    ggtitle(title)+
    coord_cartesian(ylim = yRange) + 
    theme_classic()
  return (generatedPlot)
}

classPlot <- generateScoreGraph("Class","Traveller Class","Likelihood to recommend Score","Average recommendation score by traveller class",c(0,8),c("#2196f3","#2e7d32","#ff8f00"))
ggplotly(classPlot)

genderPlot <- generateScoreGraph("Gender","Gender of Traveller","Likelihood to recommend Score","Average recommendation score by Gender",c(0,8),c("#2196f3","#e91e63"))
ggplotly(genderPlot)

traveltypePlot <- generateScoreGraph("TypeOfTravel","Traveller Type","Likelihood to recommend Score","Average recommendation score by Traveller type",c(0,8),c("#ff5722","#3f51b5","#76ff03"))
ggplotly(traveltypePlot)

airlineStatusPlot <- generateScoreGraph("AirlineStatus","Traveller Type","Likelihood to recommend Score","Average recommendation score by Traveller type",c(0,8),c("#42a5f5","#ffab00","#cfd8dc","#455a64"))
ggplotly(airlineStatusPlot)


scoreByPartner <-ggplot(df,aes(x=PartnerCode,y=LikelihoodRecommendScore,fill=cond))+scale_fill_brewer(palette = "BrBG")+
  stat_summary(fun.y = "mean",geom="bar") + 
  xlab("Partner Airlines")+ ylab("Likelihood to recommend Score")+
  ggtitle("Average recommendation score by Partner airlines")+
  coord_cartesian(ylim = c(0,8)) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1) )

ggplotly(scoreByPartner)


genderByPartner<- ggplot(df)+geom_bar(aes(x=PartnerCode,fill=Gender))+
  xlab("Partner Airlines")+ ylab("Count")+
  ggtitle("Gender count by Airline Partners")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1) )
ggplotly(genderByPartner)

plotByGender <- function(X1,xlabel,ylabel,title){
  generatedPlot <- ggplot(df)+geom_bar(aes_string(x=X1,fill="Gender"))+
    xlab(xlabel)+ ylab(ylabel)+
    ggtitle(title)+
    theme_classic() 
  return (generatedPlot)
}

ggplotly(plotByGender("Class","Traveller Class","Count","Gender count by Traveller Class"))
ggplotly(plotByGender("TypeOfTravel","Traveller Type","Class","Gender count by Traveller Type"))
ggplotly(plotByGender("AirlineStatus","Airline Status","Count","Gender count by Airline Status"))


ggplot(df,aes(x=Class,y=LikelihoodRecommendScore))+geom_col(stat="Identity",aes(fill=Class),position="dodge")+theme_bw()+scale_fill_brewer(palette="Spectral")


