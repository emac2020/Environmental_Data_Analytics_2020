almonds2 <- read.csv("Survey_numeric_answers_CLEANED_Feb5.csv")

# Concerns for Cover Crop 
# I want to do this with other concerns. Should I just use qualtrics to identify which concerns were the highest?
# How do I make a table of all the concerns for CC that appear (all Q12 variables)
# table: categorize all the concerns (water, time, labor, interference, etc.). table with different categorize
# treat concern as a factor and it will give a number to each concern. 
# What were the factors that made someone think labor was the most important (use: multinomial)
# Write down questions, and how I'm gonna deal with it (bar, glm, multinomial)

# Water makes a difference in concerns people have in growing CC. 

# Does increased water price make it more likely that certain concerns are present?

table(almonds2$ConcernCC_WaterAvailability)
# 87 people selected water availability  

almonds2$ConcernCC_WaterAvailability [is.na(almonds2$ConcernCC_WaterAvailability)] <- 0

table(almonds2$ConcernCC_WaterAvailability)

# DO this for all the relevant variables (i.e. not text entries)


# Water Price Variable

str(almonds2$HighestWaterPrice)
summary(almonds2$HighestWaterPrice)

# Remove water price outliers (over 1000)

almonds2$HighestWaterPrice <- ifelse(almonds2$HighestWaterPrice > 1200, NA, almonds2$HighestWaterPrice)

summary(almonds2$HighestWaterPrice)
# Mean water price of $128.2

# Table for water price to water concern relationship

table(almonds2$ConcernCC_WaterAvailability, almonds2$HighestWaterPrice)

water.model <- glm(ConcernCC_WaterAvailability~HighestWaterPrice, almonds2, family = binomial)
summary(water.model)

exp(coef(water.model)[2])
# for every $ change in price, 1/10 cent increase in concern
# Multiply water price by 100: 17% increase in odds for every $100 increase in price that they'd be concern
# Multiple entire column by 100

#p-value is over .05 (0.06), so it is significant. if under, then as price of water is going up, probability of selecting water as a concern goes up. Know this b/c estimate is
# .0004 which is the prob that you will select water as a concern for every $1 increase in cost of water (i.e. for each $100 it goes up its .6 likelihood that they'd select water as a concern (this is a lot))




# Table of mean price of each status of whether they have water as a concern or not

library(tidyverse)

waterprice.naomit <- almonds2[!is.na(almonds2$HighestWaterPrice),]

water.summary <- summarize(group_by(waterprice.naomit, ConcernCC_WaterAvailability), mean(HighestWaterPrice)) 

water.summary
# For people who list water as a concern, the avg price is $170.4, people who dont = $108.5, clearly significant
# Is this significant?

# Barplot of water availability and price
library(xtable)

watertable <- as.table(t(water.summary))
watertable

waterconcern.barplot <- barplot(watertable)
# Does this show that those who said water was a concern paid $170 and those who said it wasn't paid $109?
# Is this helpful to use?


 

# table for incentives

almonds2[is.na(almonds2)] <- 0

CC.summary <- summarize(group_by(almonds2, GrownCC), sum(FutureIncentivesCC_AssociatedNonPollination), sum(FutureCC_DecreasedRentalFee), 
                                 sum(FutureCC_FedCostShare),
                                 sum(FutureCC_PrivateCostShare), sum(FutureCC_Equipment), sum(FutureCC_BeeStrength),
                                 sum(FutureCC_None), sum(FutureCC_PreferNoAnswer))
CC.summary

# Can use above structure to make tables for others. Variable inside group_by is the dependent variable
# In this case, can the dependent variable be whether they've grown CC, or should it be operation role etc.?

