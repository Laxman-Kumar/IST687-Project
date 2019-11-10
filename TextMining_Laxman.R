
library(sqldf)





df <- data.frame(table(fall$Partner.Code,fall$Destination.City,fall$Origin.City,fall$Likelihood.to.recommend))

df <- df[df$Freq>0,]
rownames(df) <- NULL

agency <- textDF %>%
  pull(textDF$fall.Likelihood.to.recommend) %>%
  group_by(textDF$fall.Partner.Code) %>%
  mean(textDF$fall.Likelihood.to.recommend,na.rm = TRUE)

df2 <- data.frame(table(fall$Partner.Code,fall$Likelihood.to.recommend))

colnames(textDF)[3] <- "point"
colnames(textDF)[1] <- "agency"


