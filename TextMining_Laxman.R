
library(sqldf)

textDF <- data.frame(fall$Partner.Code,fall$Partner.Name,fall$Likelihood.to.recommend,fall$freeText)
textDF$fall.Likelihood.to.recommend <- as.numeric(textDF$fall.Likelihood.to.recommend)

df <- data.frame(table(fall$Partner.Code,fall$Destination.City,fall$Origin.City,fall$Likelihood.to.recommend))

df <- df[df$Freq>0,]
rownames(df) <- NULL

agency <- textDF %>%
  pull(textDF$fall.Likelihood.to.recommend) %>%
  group_by(textDF$fall.Partner.Code) %>%
  mean(textDF$fall.Likelihood.to.recommend,na.rm = TRUE)

df2 <- data.frame(table(fall$Partner.Code,fall$Likelihood.to.recommend))
colnames(textDF$fall.Likelihood.to.recommend) <- "Score"

sqldf("select Partner.Code,avg(Likelihood.to.recommend) from fall groupby Partner.Code")
