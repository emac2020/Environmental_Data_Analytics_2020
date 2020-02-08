almonds <- read.csv("MAIN SURVEY_Feb_5.csv")

CC.county.interest <- almonds[,c("Q5","Q8", "Q11", "Q12")]


#,2 = proportions based on column

CC.table

Socal: Tulare, Kings, Kern, Fresno
Central: Sac, San Joaquin, Stan, Merced, Madera
North:Solano, Yolo, Sutter, Yuba, Butte, Colusa, Glenn, Tehama
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

# In the north: 81% have/want cover, in South: 33% have/want cover


