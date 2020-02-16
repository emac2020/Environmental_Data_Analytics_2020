almonds2 <- read.csv("Survey_numeric_answers_CLEANED_Feb5.csv")

# Concerns for Cover Crop

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

water.model <- lm(ConcernCC_WaterAvailability~HighestWaterPrice, almonds2)
summary(water.model)

#p-value is over .05 (0.08), SO NOT SIGNIFICANT BUT NEED TO CHECK THIS. if under, then as price of water is going up, probability of selecting water as a concern goes up. Know this b/c estimate is
# .0004 which is the prob that you will select water as a concern for every $1 increase in cost of water (i.e. for each $100 it goes up its .6 likelihood that they'd select water as a concern (this is a lot))

# Table of mean price of each status of whether they have water as a concern or not

library(tidyverse)

waterprice.naomit <- almonds2[!is.na(almonds2$HighestWaterPrice),]

water.summary <- summarize(group_by(waterprice.naomit, ConcernCC_WaterAvailability), mean(HighestWaterPrice)) 
# For people who list water as a concern, the avg price is $170.4, people who dont = $108.5, clearly significant

# Barplot of water availability and price
library(xtable)

watertable <- as.table(t(water.summary))
watertable

waterconcern.barplot <- barplot(watertable)

# table of all the concerns for CC that appear (all Q12 variables)

# table for incentives

almonds2[is.na(almonds2)] <- 0

CC.summary <- summarize(group_by(almonds2, Q11), sum(Q13_1), sum(Q13_2), sum(Q13_3)) 

# Can use above structure to make tables for others. Variable inside group_by is the dependent variable