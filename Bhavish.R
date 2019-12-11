###
## TEST COMMIT
library(tidyverse)

lm_df <- readRDS(file = "CleanedData.Rda")
View(lm_df)
lm_df$total_delay <- lm_df$ArrivalDelayInMin + lm_df$DepartureDelayInMin

### Checking NPS score for each partner

lm_df$recommender_type <- cut(lm_df$LikelihoodRecommendScore, breaks = c(0,7,9, Inf), labels = c('Detractors','Passive','Promoters'), right = FALSE)   ## Creating Recommender Type categorical variable

partner_detractors <- lm_df[lm_df$recommender_type == 'Detractors',] %>%
                      group_by(PartnerCode) %>% 
                      count(recommender_type)
colnames(partner_detractors) <- c('PartnerCode','recommender_type','number_detractors')        ## Getting number of detractors at partner level


partner_passive <- lm_df[lm_df$recommender_type == 'Passive',] %>%
  group_by(PartnerCode) %>% 
  count(recommender_type)
colnames(partner_passive) <- c('PartnerCode','recommender_type','number_passive')       ## Getting number of passive at partner level



partner_promoters <- lm_df[lm_df$recommender_type == 'Promoters',] %>%
  group_by(PartnerCode) %>% 
  count(recommender_type)
colnames(partner_promoters) <- c('PartnerCode','recommender_type','number_promoters')   ## Getting number of promoters at partner level


partner_np <- merge(partner_detractors,partner_passive, by = 'PartnerCode')
partner_nps <- merge(partner_np,partner_promoters, by = 'PartnerCode')           ## Obtaining number of promoters, detractors and passive at partner level

View(partner_nps)
remove(partner_np)
 
partner_nps$total <- partner_nps$number_detractors+partner_nps$number_passive+partner_nps$number_promoters
partner_nps$nps <- ((partner_nps$number_promoters - partner_nps$number_detractors)/partner_nps$total)*100   ## Obtaining NPS score for each partner


library(ggplot2)
plotTheme <- theme_classic()+theme(axis.title.x = element_text(size = 12),
                                   axis.title.y = element_text(size = 10),plot.title = element_text(size = 15, hjust = 0.5),
                                   panel.grid.major = element_line(color="#e6e6e6",linetype=1))
partner_nps_plot <- ggplot(data = partner_nps, aes(x = reorder(partner_nps$PartnerCode,partner_nps$nps), y = partner_nps$nps, fill = partner_nps$nps))    ## Plotting NPS score for each partner
partner_nps_plot <- partner_nps_plot + geom_col(aes(fill = PartnerCode)) + plotTheme+theme(legend.position = "none") + xlab("Partners")+ ylab("NPS")
partner_nps_plot

## CREATING DF by removing partner HA that has very less observations and other partners with high NPS scores
    ## HA has >50%, AS, OO & DL have around 19%. Hence these 4 partners are removed from our linear model. Also VX is removed as it has very few rows.

lm_df_partner_rem <- lm_df[(lm_df$PartnerCode != 'HA') & (lm_df$PartnerCode != 'AS') & (lm_df$PartnerCode !='OO') & (lm_df$PartnerCode !='DL'),]


##Linear Model for non cancelled flights after excluding high performing partners and partners with low number of observations. ADJ R square = 49.3 %

linearmodel_non_cancel <- lm(LikelihoodRecommendScore ~ AirlineStatus + Age + Gender + PriceSensitivity + Loyalty + TypeOfTravel + 
                       FoodExpenses + FLightsPerYear +ShoppingAmount + Class + ScheduleDepHour+ total_delay + PartnerCode + FlightDuration + Distance + OriginState + DestinationState,
                     data = lm_df_partner_rem[lm_df_partner_rem$FlightCancelled == 'No',])
summary(linearmodel_non_cancel)      


## Generate Training and Test Data for the abve built model to measure accuracy.

#install.packages('splitstackshape')
library(splitstackshape)
randindex1 <- sample(1:dim(lm_df_partner_rem))
cut_point2_3 <- floor(2*dim(lm_df_partner_rem)[1]/3)
train_non_cancel_rem <- stratified(lm_df_partner_rem, c("OriginState","DestinationState"), 0.66)      ### 66.6% of data is used as Training Data


test_non_cancel_rem <- lm_df_partner_rem[randindex[(cut_point2_3+1):dim(lm_df_partner_rem)[1]],]

linearmodel_train_non_cancel_rem <- lm(LikelihoodRecommendScore ~ AirlineStatus + Age + Gender + PriceSensitivity + Loyalty + TypeOfTravel +
                                FoodExpenses + FLightsPerYear + ShoppingAmount + Class + total_delay + PartnerCode + FlightDuration + DestinationState + OriginState,
                                 data = train_non_cancel_rem[train_non_cancel_rem$FlightCancelled == 'No',])
summary(linearmodel_train_non_cancel_rem)

test_predictions <-predict(linearmodel_train_non_cancel_rem,test_non_cancel_rem)
comparison_table <- data.frame(test_non_cancel_rem$LikelihoodRecommendScore,test_non_cancel_rem$recommender_type,round(test_predictions,0))
colnames(comparison_table) <- c('actual','actual_recommender_type','predicted')
comparison_table$predicted_recommender_type <- cut(comparison_table$predicted, breaks = c(0,7,9, Inf), labels = c('Detractors','Passive','Promoters'), right = FALSE)


length(which(comparison_table$actual==comparison_table$predicted))/length(comparison_table$actual)   ## 22.1% Accuracy based on actual Likelihood to recommend score predicted values.

length(which(comparison_table$actual_recommender_type==comparison_table$predicted_recommender_type))/length(comparison_table$actual)   ## 54% Accuracy based on classifying predicted likelihood to recommend score



## Linear model for cancelled flights after excluding high performing partners and partners with low number of observations. ADJ R square = 38.07 %

linearmodel_cancel <- lm(LikelihoodRecommendScore ~ AirlineStatus + Age + Gender + PriceSensitivity + Loyalty + TypeOfTravel + 
                       FoodExpenses + FLightsPerYear +ShoppingAmount + Class + ScheduleDepHour+ PartnerCode + FlightDuration + Distance + DestinationState + OriginState,
                     data = lm_df_partner_rem[lm_df_partner_rem$FlightCancelled == 'Yes',])
summary(linearmodel_cancel)

lm_df_partner_rem_cancel <- lm_df_partner_rem[lm_df_partner_rem$FlightCancelled == 'Yes',]

randindex2 <- sample(1:dim(lm_df_partner_rem_cancel))
cut_point2_3 <- floor(2*dim(lm_df_partner_rem_cancel)[1]/3)
train_cancel_rem <- stratified(lm_df_partner_rem_cancel, c("OriginState","DestinationState"), 0.66)      ### 66.6% of data is used as Training Data


test_cancel_rem <- lm_df_partner_rem_cancel[randindex2[(cut_point2_3+1):dim(lm_df_partner_rem_cancel)[1]],]

linearmodel_train_cancel_rem <- lm(LikelihoodRecommendScore ~ AirlineStatus + Age + Gender + PriceSensitivity + Loyalty + TypeOfTravel +
                                         FoodExpenses + FLightsPerYear + ShoppingAmount + Class + PartnerCode + FlightDuration + DestinationState + OriginState,
                                       data = lm_df_partner_rem_cancel)
summary(linearmodel_train_cancel_rem)

test_predictions2 <-predict(linearmodel_train_cancel_rem,test_cancel_rem)
comparison_table2 <- data.frame(test_cancel_rem$LikelihoodRecommendScore,test_cancel_rem$recommender_type,round(test_predictions2,0))
colnames(comparison_table2) <- c('actual','actual_recommender_type','predicted')
comparison_table2$predicted_recommender_type <- cut(comparison_table2$predicted, breaks = c(0,7,9, Inf), labels = c('Detractors','Passive','Promoters'), right = FALSE)


length(which(comparison_table2$actual==comparison_table2$predicted))/length(comparison_table2$actual)   ## 35% Accuracy based on actual Likelihood to recommend score predicted values.

length(which(comparison_table2$actual_recommender_type==comparison_table2$predicted_recommender_type))/length(comparison_table2$actual)   ## 73% Accuracy based on classifying predicted likelihood to recommend score

## Including High performing partners and building a model for the df

lm_df_partner_incl <- lm_df[(lm_df$PartnerCode == 'HA') | (lm_df$PartnerCode == 'AS') | (lm_df$PartnerCode == 'OO') | (lm_df$PartnerCode == 'DL'),]


##Linear Model for non cancelled flights after including high performing partners and partners with low number of observations. ADJ R square = 39.43 %

linearmodel_non_cancel_highpf <- lm(LikelihoodRecommendScore ~ AirlineStatus + Age + Gender + PriceSensitivity + Loyalty + TypeOfTravel + 
                               FoodExpenses + FLightsPerYear +ShoppingAmount + Class + ScheduleDepHour+ total_delay + PartnerCode + FlightDuration + Distance + OriginState + DestinationState,
                             data = lm_df_partner_incl[lm_df_partner_incl$FlightCancelled == 'No',])
summary(linearmodel_non_cancel_highpf)      


## Linear model for cancelled flights after including high performing partners and partners with low number of observations. ADJ R square = 86.43 %

linearmodel_cancel_highpf <- lm(LikelihoodRecommendScore ~ AirlineStatus + Age + Gender + PriceSensitivity + Loyalty + TypeOfTravel + 
                           FoodExpenses + FLightsPerYear +ShoppingAmount + Class + ScheduleDepHour+ PartnerCode + FlightDuration + Distance + DestinationState + OriginState,
                         data = lm_df_partner_incl[lm_df_partner_incl$FlightCancelled == 'Yes',])
summary(linearmodel_cancel_highpf)

    ### *** END OF LINEAR MODEL CODE *** ###
  
## APPENDIX/POTENTIAL ENHANCEMENTS

## Generate Training and Test Data for the abve built model to measure accuracy.
## NOT POSSIBLE TO BUILD STRATIFIED SAMPLE OF TRAINING & TEST DATASET because Origin & Destination state columns have been included.

#lm_df_partner_rem_cancel <- lm_df_partner_rem[lm_df_partner_rem$FlightCancelled == 'Yes',]
#View(lm_df_partner_rem_cancel)

#randindex1 <- sample(1:dim(lm_df_partner_rem_cancel))
#cut_point2_3 <- floor(2*dim(lm_df_partner_rem_cancel)[1]/3)
#train_cancel <- lm_df_partner_rem[randindex1[1:cut_point2_3],]    ### 66.6% of data is used as Training Data

#test_cancel <- lm_df_partner_rem[randindex[(cut_point2_3+1):dim(lm_df_partner_rem)[1]],]

#linearmodel_train_cancel <- lm(LikelihoodRecommendScore ~ AirlineStatus + Age + Gender + PriceSensitivity + Loyalty + TypeOfTravel +
 #                                    FoodExpenses + FLightsPerYear + ShoppingAmount + Class + total_delay + PartnerCode + FlightDuration + DestinationState + OriginState,
  #                                 data = train_cancel)
#summary(linearmodel_train_cancel)

#test_predictions <-predict(linearmodel_train_cancel,test_cancel)
#comparison_table <- data.frame(test_non_cancel$LikelihoodRecommendScore, round(test_predictions,0))
#colnames(comparison_table) <- c('actual','predicted')
#length(which(comparison_table$actual==comparison_table$predicted))/length(comparison_table$actual)

## Generate Training and Test Data for the abve built model to measure accuracy.

##library(tidyverse)
#no_orig_dest_states <- lm_df_partner_rem %>%
# group_by(OriginState,DestinationState) %>% 
#count(recommender_type)
#View(no_orig_dest_states)
#colnames(no_orig_dest_states) <- c('OriginState','DestinationState','recommender_type','number_passengers')


#randindex <- sample(1:dim(lm_df_partner_rem))
#cut_point2_3 <- floor(2*dim(lm_df_partner_rem)[1]/3)
#train_non_cancel <- lm_df_partner_rem[randindex[1:cut_point2_3],]    ### 66.6% of data is used as Training Data

#test_non_cancel <- lm_df_partner_rem[randindex[(cut_point2_3+1):dim(lm_df_partner_rem)[1]],]

#linearmodel_train_non_cancel <- lm(LikelihoodRecommendScore ~ AirlineStatus + Age + Gender + PriceSensitivity + Loyalty + TypeOfTravel +
 #                                    FoodExpenses + FLightsPerYear + ShoppingAmount + Class + total_delay + PartnerCode + FlightDuration,
  #                                 data = train_non_cancel)
#summary(linearmodel_train_non_cancel)

#test_predictions <-predict(linearmodel_train_non_cancel,test_non_cancel)
#comparison_table <- data.frame(test_non_cancel$LikelihoodRecommendScore, round(test_predictions,0))
#colnames(comparison_table) <- c('actual','predicted')
#length(which(comparison_table$actual==comparison_table$predicted))/length(comparison_table$actual)

