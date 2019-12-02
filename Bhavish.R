###
test <- c(1,2,3)
lm_df <- readRDS(file = "CleanedData.Rda")
View(lm_df)
lm_df$total_delay <- lm_df$ArrivalDelayInMin + lm_df$DepartureDelayInMin

length(which(is.na(lm_df$total_delay)))

unique(lm_df$PartnerCode)

## 1.  CREATING DF for AA Partner Code

lm_df_aa <- lm_df[lm_df$PartnerCode == 'AA',]

##View(lm_df_aa)
##str(lm_df_aa)

linearmodel_aa <- lm(LikelihoodRecommendScore ~ AirlineStatus+Age+Gender+PriceSensitivity + Loyalty + TypeOfTravel + TotalFreqFlyAccount +
                       FoodExpenses + ShoppingAmount + Class + ScheduleDepHour + total_delay + FlightDuration + Distance + DestinationState + OriginState,
                     data = lm_df_aa)
summary(linearmodel_aa)


## 2. CREATING DF for EV Partner Code

lm_df_ev <- lm_df[lm_df$PartnerCode == 'EV',]

##View(lm_df_aa)
##str(lm_df_aa)

linearmodel_ev <- lm(LikelihoodRecommendScore ~ AirlineStatus+Age+Gender+PriceSensitivity + Loyalty + TypeOfTravel + TotalFreqFlyAccount +
                       FoodExpenses + ShoppingAmount + Class + ScheduleDepHour + total_delay + FlightDuration + Distance + DestinationState + OriginState,
                     data = lm_df_ev)
summary(linearmodel_ev)

## 3. CREATING DF for WN Partner Code

lm_df_wn <- lm_df[lm_df$PartnerCode == 'WN',]

##View(lm_df_aa)
##str(lm_df_aa)

linearmodel_wn <- lm(LikelihoodRecommendScore ~ AirlineStatus+Age+Gender+PriceSensitivity + Loyalty + TypeOfTravel + TotalFreqFlyAccount +
                       FoodExpenses + ShoppingAmount + Class + ScheduleDepHour + total_delay + FlightDuration + Distance + DestinationState + OriginState,
                     data = lm_df_wn)
summary(linearmodel_wn)

## 4. CREATING DF for MQ Partner Code

lm_df_mq <- lm_df[lm_df$PartnerCode == 'MQ',]

##View(lm_df_aa)
##str(lm_df_aa)

linearmodel_mq <- lm(LikelihoodRecommendScore ~ AirlineStatus+Age+Gender+PriceSensitivity + Loyalty + TypeOfTravel + TotalFreqFlyAccount +
                       FoodExpenses + ShoppingAmount + Class + ScheduleDepHour + total_delay + FlightDuration + Distance + DestinationState + OriginState,
                     data = lm_df_mq)
summary(linearmodel_mq)

## 5. CREATING DF for MQ Partner Code

lm_df_mq <- lm_df[lm_df$PartnerCode == 'MQ',]

##View(lm_df_aa)
##str(lm_df_aa)

linearmodel_mq <- lm(LikelihoodRecommendScore ~ AirlineStatus+Age+Gender+PriceSensitivity + Loyalty + TypeOfTravel + TotalFreqFlyAccount +
                       FoodExpenses + ShoppingAmount + Class + ScheduleDepHour + total_delay + FlightDuration + Distance + DestinationState + OriginState,
                     data = lm_df_mq)
summary(linearmodel_mq)
