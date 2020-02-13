almonds <- read.csv("Almond_Survey_CLEANED.csv")

CC.county.interest <- almonds[,c("Q5","Q8", "Q11", "Q12")]


#Socal: Tulare, Kings, Kern, Fresno
#Central: San Joaquin, Stan, Merced, Madera
#Delta: Sac, Solano, Yolo, Yuba
#North: Sutter, Butte, Colusa, Glenn, Tehama
## More dependent on watershed and rainfall. Could go by Basins: Sac, Delta & Eastside streams, SJ Basin, Tulare Basin 

# create new variable, attach to DF
# excluding mult. counties. Use package "stringR" to search through column and any row in column that contains text, it will ID and categorize 

CC.county.interest$North <- ifelse(CC.county.interest$Q5 == "Solano" | CC.county.interest$Q5 == "Yolo" |
                                     CC.county.interest$Q5 == "Yuba" | CC.county.interest$Q5 == "Butte" | CC.county.interest$Q5 == "Colusa"
                                   | CC.county.interest$Q5 == "Glenn" | CC.county.interest$Q5 == "Tehama", 1, 0)
head(CC.county.interest$North)                                     

CC.county.interest$Central <- ifelse(CC.county.interest$Q5 == "Sacramento" | CC.county.interest$Q5 == "San Joaquin" |
                                       CC.county.interest$Q5 == "Stanislaus" | CC.county.interest$Q5 == "Merced" | 
                                       CC.county.interest$Q5 == "Madera", 1, 0)

CC.county.interest$South <- ifelse(CC.county.interest$Q5 == "Tulare" | CC.county.interest$Q5 == "Kings" |
                                     CC.county.interest$Q5 == "Kern" | CC.county.interest$Q5 == "Fresno", 1, 0)


CC.county.interest$Region <- ifelse(CC.county.interest$North == 1, "North", ifelse(CC.county.interest$South == 1, "South", "Central" ))


CC.county.interest$Cover <- ifelse(CC.county.interest$Q8 == "Yes" | CC.county.interest$Q11 == "Yes", 1, 0)

CC.table <- prop.table(table(CC.county.interest$Region, CC.county.interest$Cover), 1)

CC.table

# In the north: 79.4% have/want cover, in South: 34.2% have planted or are interested in planting cover cover

# Interest in CC depending on who oversees pollination mgmt

Pollinator.mgmt.CC <- almonds[, c("Q6", "Q8", "Q11", "Q12")]

Poll.mgmt <- ifelse(Pollinator.mgmt.CC$Q6 == "Owner of the almond orchard(s)" | Pollinator.mgmt.CC$Q6 ==  "Farm manager" | 
                      Pollinator.mgmt.CC$Q6 == "Independent PCA (not affiliated with an agricultural retailer)" |
                      Pollinator.mgmt.CC$Q6 == "Affiliated PCA (with an agricultural retailer" |
                      Pollinator.mgmt.CC$Q6 ==  "Pesticide applicator" | Pollinator.mgmt.CC$Q6 == "Beekeeper" |
                      Pollinator.mgmt.CC$Q6 == "Bee broker", 1, 0)
head(Poll.mgmt)


poll.mgmt.Independent <- ifelse(Pollinator.mgmt.CC$Q6 == "Independent PCA (not affiliated with an agricultural retailer)", 1, 0)

poll.mgmt.CC.interest <- ifelse(Pollinator.mgmt.CC$Q8 == "Yes" | Pollinator.mgmt.CC$Q11 == "Yes", 1, 0)

Pollinator.mgmt.CC.table <- prop.table(table(poll.mgmt.Independent, poll.mgmt.CC.interest), 1) 

Pollinator.mgmt.CC.table

# 60% of people who said an independent PCA was one of the most influential people in pollinator mgmt were 
