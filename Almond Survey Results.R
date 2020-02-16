almonds <- read.csv("Almond_Survey_Cleaned_NoColumnTitles.csv")
almonds2 <- read.csv("Survey_numeric_answers_CLEANED_Feb5.csv")

CC.county.interest <- almonds[,c("Q2","Q6", "Q8", "Q9")]


#North: Butte, Colusa, Glenn, Tehama
#Delta: Sac, Solano, Yolo, Yuba
#Central: San Joaquin, Stan, Merced, Madera
#Socal: Tulare, Kings, Kern, Fresno


# create new variable, attach to DF
# excluding mult. counties. Use package "stringR" to search through column and any row in column that contains text, it will ID and categorize 

CC.county.interest$North <- ifelse( CC.county.interest$Q2 == "Butte" | CC.county.interest$Q2 == "Colusa"
                                   | CC.county.interest$Q2 == "Glenn" | CC.county.interest$Q2 == "Tehama", 1, 0)


CC.county.interest$Delta <- ifelse(CC.county.interest$Q2 == "Sacramento" | CC.county.interest$Q2 == "Solano" | 
                                     CC.county.interest$Q2 == "Yolo" | CC.county.interest$Q2 == "Yuba" |
                                     CC.county.interest$Q2 == "Sutter", 1, 0)                                   

CC.county.interest$Central <- ifelse( CC.county.interest$Q2 == "San Joaquin" |
                                       CC.county.interest$Q2 == "Stanislaus" | CC.county.interest$Q2 == "Merced" | 
                                       CC.county.interest$Q2 == "Madera", 1, 0)

CC.county.interest$South <- ifelse(CC.county.interest$Q2 == "Tulare" | CC.county.interest$Q2 == "Kings" |
                                     CC.county.interest$Q2 == "Kern" | CC.county.interest$Q2 == "Fresno" | 
                                     CC.county.interest$Q2 == "Kerman",  1, 0)


CC.county.interest$Region <- ifelse(CC.county.interest$North == 1, "North", 
                                    ifelse(CC.county.interest$South == 1, "South", 
                                           ifelse(CC.county.interest$Central== 1, "Central", "Delta" )))


CC.county.interest$Cover <- ifelse(CC.county.interest$Q6 == "Yes" | CC.county.interest$Q9 == "Yes", 1, 0)

CC.table <- prop.table(table(CC.county.interest$Region, CC.county.interest$Cover), 1)

CC.table


# Central: 47.5% have or are interested in CC
# Delta: 72.5%
# North: 72.2% 
# South: 34.7% 


# Interest in Permanent Pollinator Habitat by Region

PPH.county.interest <- almonds[,c("Q2","Q12", "Q14", "Q15")]

PPH.county.interest$North <- ifelse( CC.county.interest$Q2 == "Butte" | CC.county.interest$Q2 == "Colusa"
                                    | CC.county.interest$Q2 == "Glenn" | CC.county.interest$Q2 == "Tehama", 1, 0)


PPH.county.interest$Delta <- ifelse(CC.county.interest$Q2 == "Sacramento" | CC.county.interest$Q2 == "Solano" | 
                                     CC.county.interest$Q2 == "Yolo" | CC.county.interest$Q2 == "Yuba" |
                                     CC.county.interest$Q2 == "Sutter", 1, 0)                                   

PPH.county.interest$Central <- ifelse( CC.county.interest$Q2 == "San Joaquin" |
                                        CC.county.interest$Q2 == "Stanislaus" | CC.county.interest$Q2 == "Merced" | 
                                        CC.county.interest$Q2 == "Madera", 1, 0)

PPH.county.interest$South <- ifelse(CC.county.interest$Q2 == "Tulare" | CC.county.interest$Q2 == "Kings" |
                                     CC.county.interest$Q2 == "Kern" | CC.county.interest$Q2 == "Fresno" | 
                                     CC.county.interest$Q2 == "Kerman",  1, 0)


PPH.county.interest$Region <- ifelse(CC.county.interest$North == 1, "North", 
                                    ifelse(CC.county.interest$South == 1, "South", 
                                           ifelse(CC.county.interest$Central== 1, "Central", "Delta" )))


PPH.county.interest$Covercrop <- ifelse(PPH.county.interest$Q12 == "Yes" | PPH.county.interest$Q15 == "Yes", 1, 0)

PPH.table <- prop.table(table(PPH.county.interest$Region, PPH.county.interest$Covercrop), 1)

PPH.table

# Central: 38.3% have or are interested in CC
# Delta: 47.1%
# North: 38.9% 
# South: 34.7% 

# Farm Size

almonds2$TotalYieldBearing [is.na(almonds2$TotalYieldBearing)] <- 0

mean(almonds2$TotalYieldBearing)
# Mean total bearing acreage size: 726.8795

summary(almonds2$TotalYieldBearing)

# How does size of operation affect the growers interest in Cover Crop?

table(almonds2$GrownCC)
# 101 people have grown CC, 200 people have not

table(almonds2$CC_Interest)
# Out of the 200 people who have not grown cover crop in the last 5 years, 52 people are interested, 63 people are not, 85 unsure

CC.table <- as.table()


# Respondent Location
library(ggplot2)
library(tidyverse)
countylocation <- filter(almonds, almonds$Q2 %in% c("Butte", "Colusa", "Glenn", "Tehama", "Sacramento", "Solano", "Yolo"
                                            , "Yuba", "San Joaquin", "Stanislaus", "Merced", "Madera", "Tulare"
                                            , "Kings", "Kern", "Fresno", "Stanislaus,Merced", "Fresno,Kings", 
                                            "San Joaquin,Madera,Fresno,Kings,Tulare", "Fresno,Tulare",  "Stanislaus,Madera",
                                            "Solano,Merced", "San Joaquin,Stanislaus", "Stanislaus,Fresno", "Yolo,Stanislaus,Fresno",
                                            "Tehama,Butte,Glenn,Colusa", "San Joaquin, Kerman", "Madera,Fresno,Kings,Tulare,Kern",
                                            "San Joaquin,Stanislaus,Merced", "San Joaquin,Madera,Fresno", "Madera,Fresno", "Yolo,Solano",
                                            "Glenn,Stanislaus"))


countylocation2 <- select(almonds2, Tehama:Sacramento) 

almonds2$EndDate <- as.Date(almonds2$EndDate, format= "%m/%d/%y" )
class(almonds2$EndDate)



locoplot<- ggplot(almonds, aes(x = Q2, color = Q2)) +
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "County", y = "Count") +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
  
print(locoplot)


 




