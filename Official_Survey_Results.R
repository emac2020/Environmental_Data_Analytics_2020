library(lme4)
library(ggplot2)
library(cowplot)
library(corrplot)
library(tidyverse)
library(viridis)
library(nlme)
library(nnet)
library(MASS) 
library(boot)

almonds <- read.csv("Almond_Survey_Cleaned_Official.csv")

almonds2 <- read.csv("Survey_numeric_answers_CLEANED_Feb5.csv")

summary(almonds$Q1)

summary(almonds$Q31)
summary(almonds$Q30_1)
summary(almonds$Q3_1)



# 1. Plot: Respondent Location

locoplot<- ggplot(almonds, aes(x = Counties)) +
  geom_bar(fill = "blue") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "County", y = "Count") +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 10), 
        legend.title = element_text( face = "bold",size = 10))


print(locoplot)



# What affects whether or not theyve grown CC?

GrownCC <- glm(Q6 ~ as.factor(Regions) + as.factor(Q1) + as.factor(Q31)
               + as.factor(Acre.Ranges), almonds, family = binomial )

summary(GrownCC)


GrownCC.2 <- glm(GrownCC ~  as.factor(Regions) + as.factor(RoleOperation) + as.factor(Q50) +
                as.factor(TotalYieldBearing), almonds2, family = binomial )

summary(GrownCC.2)

# What affects whether or not they are interested in growing CC?

InterestCC <- glm(Q9 ~ as.factor(Regions) + as.factor(Q1) + as.factor(Q31)
                  + as.factor(Acre.Ranges), almonds, family = binomial )

summary(InterestCC)

# What affects whether or not they've grown PPH?

GrownPPH <- glm(Q12 ~ as.factor(Regions) + as.factor(Q1) + as.factor(Q31)
                + as.factor(Acre.Ranges), almonds, family = binomial )

summary(GrownPPH)

# What affects whether or not they are interested in growing PPH?

InterestPPH <- glm(Q15 ~ as.factor(Regions) + as.factor(Q1) + as.factor(Q31)
                   + as.factor(Acre.Ranges), almonds, family = binomial )

summary(InterestPPH)

# 2. How does Role in Operation affect whether or not a person has GROWN cover crop?

Role.Operation.CCgrown <- glm(Q6~ Q1, almonds, family = binomial)

summary(Role.Operation.CCgrown)


exp(coef(Role.Operation.CCgrown)[2])

# Chi-square

almonds2$RoleOperation <- as.factor(almonds2$RoleOperation)
almonds2$GrownCC <- as.factor(almonds2$GrownCC)



Role.GrownCC.tbl = table(almonds2$RoleOperation, almonds2$GrownCC) 
Role.GrownCC.tbl

chisq.test(Role.GrownCC.tbl)



# 2.plot Role in operation and whether or not a person has GROWN cover crop 

alm= almonds[almonds$Q1 != " " ,]

role.count <- data.frame(table(data.frame(alm$Q1, alm$Q6)))

role.count

Role.CCgrown.plot <- ggplot(role.count, aes(x = alm.Q1, y = Freq, fill = alm.Q6)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12)) +
  #scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = c("darkblue", "#009E73")) +
  labs(x = "Role in Operation", y = "Count", fill = "Grown Cover Crop") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 12), legend.title = element_text(size = 12))
print(Role.CCgrown.plot)

# 3. How does a person's role in the operation affect whether they are INTERESTED in growing cover crop?

Role.Operation.CCinterest <- glm(Q9 ~ Q1, almonds, family = binomial)

summary(Role.Operation.CCinterest)

exp(coef(Role.Operation.CCinterest)[2])

# Chi-square: Role in Operation vs. Interest in CC

Role.InterestCC.tbl = table(almonds2$RoleOperation, almonds2$CC_Interest) 
Role.InterestCC.tbl

chisq.test(Role.InterestCC.tbl)

# 3.plot: Role in Operation and Interest in Growing CC 
alm= almonds[almonds$Q1 != " " ,]

role.interest.count <- data.frame(table(data.frame(alm$Q1, alm$Q9)))

role.interest.count

Role.CCinterest.plot <- 
  ggplot(role.interest.count, aes(x = alm.Q1, y = Freq, fill = alm.Q9)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "Role in Operation", y = "Count", fill = "Interest in Cover Crop") +
  scale_fill_manual(values = c("darkblue", "#E69F00", "#009E73", "#CC79A7"))  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12)) +
  theme(legend.position = "right", 
        legend.text = element_text(size = 12), legend.title = element_text(size = 12))
print(Role.CCinterest.plot)

#4.Cowplot: Role in Operation and Grown CC vs. Interested in Growing CC

CC.Role.plots <- plot_grid(Role.CCgrown.plot, Role.CCinterest.plot, 
                          align = "h", ncol = 1)

print(CC.Role.plots)



# 5. How does location affect whether or not a person has GROWN cover crop? 


# Chi-square: Location vs. Grown CC

almonds$Regions <- as.factor(almonds$Regions)

Location.GrownCC.tbl = table(almonds$Regions, almonds$Q6) 
Location.GrownCC.tbl

chisq.test(Location.GrownCC.tbl)



regions.CCgrown.glm <- glm(Q6 ~  Region.North + Region.Delta + Region.Central, 
                           data = almonds, family = binomial )

summary(regions.CCgrown.glm)

exp(coef(regions.CCgrown.glm)[2])



# just says which region has sig effect on cover

exp(1.6854)

exp(1.9136)

exp( 0.2083 )

#inv.logit in boot package***

# Region North
inv.logit(1.6854) # 0.84***What does this mean...

# Glm with just "Regions"
regions.CCgrown.glm2 <- glm(Q6 ~ Regions, data=almonds, family = binomial)

summary(regions.CCgrown.glm2) # comparing everything to Central. D and N have sig higher log odds to CC than Cental and South has sig lower



# 5.plot: Location and Grown CC

alm.loco= almonds[almonds$Counties != " ." ,]

Location.CC <- data.frame(table(data.frame(alm.loco$Counties, alm.loco$Q6)))

Location.CC

Location.CCgrown.plot <- ggplot(Location.CC, aes(x = alm.loco.Counties, y = Freq, fill = alm.loco.Q6)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("darkblue", "#E69F00")) +
labs(x = "Location", y = "Count", fill = "Grown CC") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(Location.CCgrown.plot)



# 6: How does location affect whether or not a person is INTERESTED in growing cover crop?

Location.InterestCC <- glm(Q9 ~ Counties.with..multiple., almonds, family = binomial)
summary(Location.InterestCC)

exp(coef(Location.InterestCC)[2]) ## Nothing sig

Regions.InterestCC.glm <- glm(Q9 ~ Region.North + Region.South + Region.Delta, 
                              almonds, family = binomial)

summary(Regions.InterestCC.glm) # Significant?

exp(-0.2120 ) # delta has 0.81 odds over central?


# CC Interest with "Regions"
Regions.InterestCC.glm2 <- glm(Q9 ~ Regions, 
                              almonds, family = binomial)

summary(Regions.InterestCC.glm2)



# Chi-Square

almonds$Q9 <- as.factor(almonds$Q9)

Location.InterestCC.tbl = table(almonds$Regions, almonds$Q9) 

Location.InterestCC.tbl

chisq.test(Location.InterestCC.tbl) #####LOOK INTO THIS


# 6.plot: Location and Interest in Growing CC 

alm.loco= almonds[almonds$Counties != " ." ,]

Location.Interest.CC <- data.frame(table(data.frame(alm.loco$Counties, alm.loco$Q9)))

Location.Interest.CC

Location.InterestCC.plot <- ggplot(Location.Interest.CC, aes(x = alm.loco.Counties, y = Freq, fill = alm.loco.Q9)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Location", y = "Count", fill = "Interest in Growing CC") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(Location.InterestCC.plot)


# 7.Cowplot: Location and Grown CC vs. Interested in Growing CC

CC.Location.plots <- plot_grid(Location.CCgrown.plot, Location.InterestCC.plot, 
                           align = "h", ncol = 1)

print(CC.Location.plots)



# 8: How does age affect whether or not a person has GROWN CC? 

Age.CCgrown <- glm(Q6~Q31, almonds, family = binomial)

summary(Age.CCgrown)

exp(coef(Age.CCgrown)[2])

almonds2$Q50 <- as.factor(almonds2$Q50)

# Chi-Square


Age.GrownCC.tbl = table(almonds2$Q50, almonds2$GrownCC) #almonds 2
Age.GrownCC.tbl

chisq.test(Age.GrownCC.tbl)


Age.GrownCC.tbl2 = table(almonds$Q31, almonds$Q6) #almonds
Age.GrownCC.tbl2

chisq.test(Age.GrownCC.tbl2)

# 8.plot: Age and CC Grown *****ARE these plots skewed because 25-34 was most prevalent age group?

alm.Age= almonds[almonds$Q31 != " " ,]

age.CCgrown <- data.frame(table(data.frame(alm.Age$Q31, alm.Age$Q6)))

age.CCgrown

Age.grown.plot <- ggplot(age.CCgrown, aes(x = alm.Age.Q31, y = Freq, fill = alm.Age.Q6)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 60) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Age", y = "Count", fill = "Grown CC") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(Age.grown.plot)



# 9: How does age affect whether or not a person is INTERESTED in growing CC?

Age.CCinterested <- glm(Q9~Q31, almonds, family = binomial)

summary(Age.CCinterested)

exp(coef(Age.CCinterested)[2])

# Chi-square

Age.InterestCC.tbl = table(almonds2$Q50, almonds2$CC_Interest) #almonds 2
Age.InterestCC.tbl

chisq.test(Age.InterestCC.tbl)

Age.InterestCC.tbl2 = table(almonds$Q31, almonds$Q9) #almonds
Age.InterestCC.tbl2

chisq.test(Age.InterestCC.tbl2)

# 9.plot: Age and Interest in Growing CC 

alm.Age= almonds[almonds$Q31 != " " ,]

age.CCinterest <- data.frame(table(data.frame(alm.Age$Q31, alm.Age$Q9)))

age.CCinterest

Age.interest.plot <- ggplot(age.CCinterest, aes(x = alm.Age.Q31, y = Freq, fill = alm.Age.Q9)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,23) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Age", y = "Count", fill = "Interest in Growing CC") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(Age.interest.plot)

# 10.Cowplot: Age and Grown CC vs. Interested in CC

CC.age.plots <- plot_grid(Age.grown.plot, Age.interest.plot, 
                          align = "h", ncol = 1)

print(CC.age.plots)


# 11: How does size of operation affect whether or not people have grown cover crop?

farmsize.GrownCC <- glm(Q6 ~ Q3_1, almonds, family = binomial) # numeric entries
summary(farmsize.GrownCC)

exp(coef(farmsize.GrownCC)[2])


farmsize.GrownCC2 <- glm(Q6~Acre.Ranges , almonds, family = binomial) # acre ranges
summary(farmsize.GrownCC2)

exp(coef(farmsize.GrownCC2)[2])

# Chi-square

almonds$Acre.Ranges <- as.factor(almonds$Acre.Ranges)
almonds$Q6 <- as.factor(almonds$Q6)

Size.GrownCC.tbl = table(almonds$Acre.Ranges, almonds$Q6) 
Size.GrownCC.tbl

chisq.test(Size.GrownCC.tbl)

summary(almonds$Acre.Ranges)

# 11.Plot: Operation size and Grown CC (HOW DO I PLOT THIS??)

alm.Size= almonds[almonds$Acre.Ranges != "NA" ,]

Size.CCgrown <- data.frame(table(data.frame(alm.Size$Acre.Ranges, alm.Size$Q6)))

Size.CCgrown

FarmSize.GrownCC.plot <- 
  ggplot(Size.CCgrown, aes(x = alm.Size.Acre.Ranges, y = Freq, fill = alm.Size.Q6)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,23) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Farm Size", y = "Count", fill = "alm.Size.Q6") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(FarmSize.GrownCC.plot)


# 12: How does size of operation affect whether or not people are interested in growing cover crop?

farmsize.InterestCC <- glm(Q9~Acre.Ranges , almonds, family = binomial)
summary(farmsize.InterestCC)

exp(coef(farmsize.InterestCC)[2])


farmsize.InterestCC2 <- glm(Q9 ~ Q3_1, almonds, family = binomial) # numeric entries
summary(farmsize.InterestCC2)

exp(coef(farmsize.InterestCC2)[2])


# Chi-square 
almonds$Q9 <- as.factor(almonds$Q9)

Size.InterestCC.tbl = table(almonds$Acre.Ranges, almonds$Q9) 
Size.InterestCC.tbl

chisq.test(Size.InterestCC.tbl)

# 12.Plot: Operation Size and Interested in Growing CC

Size.InterestCC.plot <-
  ggplot(almonds, aes(x = Q9, y = Q3_1, color = Q9)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_color_viridis_d()
print(Size.InterestCC.plot)


# 13.Cowplot: Operation Size and Grown CC vs. Interested in Growing CC



# COncerns:
# column with concerns numbered 1 - 12 as response variable. 


# 14: How do concerns about cover crop differ by location? Region? 

almonds$Q10 <- as.factor(almonds$Q10)

CCconcerns.Region.tbl = table(almonds$Regions, almonds$Q10)

CCconcerns.Region.tbl

chisq.test(CCconcerns.Region.tbl)



ConcernsCC.Regions.glm <- glm(Q10 ~ Region.South + Region.Delta + Region.Central, 
                              almonds, family = binomial)

summary(ConcernsCC.Regions.glm)

# Concerns for Cover Crop 
# How do I make a table of all the concerns for CC that appear (all Q12 variables)
# table: categorize all the concerns (water, time, labor, interference, etc.). table with different categorize
# treat concern as a factor and it will give a number to each concern. 
# What were the factors that made someone think labor was the most important (use: multinomial)

almonds$Q10 <- as.factor(almonds$Q10)

CCconcernTable <- table(almonds$Q10)

CCconcernTable # This just shows all the combos of concerns

# 14.Plot: Concerns by location

# What factors influenced people's decision to chose "Availability of water" as a factor in growing CC?
# Use multinomial 
# Make columns with different concerns. Set NA to zero in almonds2 and set the column to as.factor
#water concern ~ region + operation size 


alm.H20.Avail= almonds[almonds$Q10 == "Availability of water" ,]

H20.Avail.CCconcern <- data.frame(table(data.frame(alm.H20.Avail$Q10, alm.H20.Avail$Q6)))

H20.Avail.CCconcern

Availability.water.CCconcern <- multinom(alm.H20.Avail.Q6 ~ alm.H20.Avail.Q10 + alm.H20.Avail, 
                                         data = H20.Avail.CCconcern)


### Permanent Pollinator Habitat Section

# 1. How does Role in Operation affect whether or not a person has planted PPH?

Role.Operation.PPHplanted <- glm(Q12~ Q1, almonds, family = binomial)

summary(Role.Operation.PPHplanted)


exp(coef(Role.Operation.PPHplanted)[2])


# Chi-Square

almonds2$RoleOperation <- as.factor(almonds2$RoleOperation)
almonds2$GrownPPH <- as.factor(almonds2$GrownPPH)



Role.GrownPPH.tbl = table(almonds2$RoleOperation, almonds2$GrownPPH) 
Role.GrownPPH.tbl

chisq.test(Role.GrownPPH.tbl)


# Plot: Role in Operation and PPH GROWN
alm= almonds[almonds$Q1 != " " ,]

role.grownPPH.count <- data.frame(table(data.frame(alm$Q1, alm$Q12)))

role.grownPPH.count

Role.PPHgrown.plot <- 
  ggplot(role.grownPPH.count, aes(x = alm.Q1, y = Freq, fill = alm.Q12)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "Role in Operation", y = "Count", fill = "Grown Permenant Habitat") +
  scale_fill_manual(values = c("darkblue", "#E69F00", "#009E73", "#CC79A7"))  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12)) +
  theme(legend.position = "right")
print(Role.PPHgrown.plot)


# 2. How does Role in Operation affect whether or not a person is interested in planting PPH?

Role.Operation.PPHinterest <- glm(Q15~ Q1, almonds, family = binomial)

summary(Role.Operation.PPHinterest)


exp(coef(Role.Operation.PPHinterest)[2])

# Chi-Square

almonds$PPHInterest <- as.factor(almonds2$PPHInterest)



Role.InterestPPH.tbl = table(almonds2$RoleOperation, almonds2$PPHInterest) 
Role.InterestPPH.tbl

chisq.test(Role.InterestPPH.tbl)


# Plot: Role in Operation and INTERESTED in planting PPH
alm= almonds[almonds$Q1 != " " ,]

role.interestPPH.count <- data.frame(table(data.frame(alm$Q1, alm$Q15)))

role.interestPPH.count

Role.PPHinterest.plot <- 
  ggplot(role.interestPPH.count, aes(x = alm.Q1, y = Freq, fill = alm.Q15)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "Role in Operation", y = "Count", fill = "Interest in Growing PPH") +
  scale_fill_manual(values = c("darkblue", "#E69F00", "#009E73", "#CC79A7"))  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12)) +
  theme(legend.position = "right")
print(Role.PPHinterest.plot)

# 3. How does location affect whether or not a person has planted PPH? 

almonds$Q12 <- as.factor(almonds$Q12)

Region.GrownPPH.tbl = table(almonds$Regions, almonds$Q12) 
Region.GrownPPH.tbl

chisq.test(Region.GrownPPH.tbl)

# Region glm: PPH GROWN

Region.GrownPPH.glm <- glm(Q12 ~ Region.North + Region.Delta + Region.Central,
                           almonds, family = binomial)
summary(Region.GrownPPH.glm)

# 4. How does location affect whether or not a person is interested in planting PPH? 

almonds$Q15 <- as.factor(almonds$Q15)
almonds$Regions <- as.factor(almonds$Regions)

Region.InterestPPH.tbl = table(almonds$Regions, almonds$Q15) 
Region.InterestPPH.tbl

chisq.test(Region.InterestPPH.tbl)

# Region glm: PPH INTEREST 

Region.InterestPPH.glm <- glm(Q15 ~ Region.North + Region.Delta + Region.Central,
                           almonds, family = binomial)
summary(Region.InterestPPH.glm)



# 5: How does age affect whether or not a person has planted PPH? 

Age.PPHgrown <- glm(Q12~Q31, almonds, family = binomial)

summary(Age.PPHgrown)

exp(coef(Age.PPHgrown)[2])


# 6: How does age affect whether or not a person is interested in planting PPH? 

Age.PPHinterest <- glm(Q15~Q31, almonds, family = binomial)

summary(Age.PPHinterest)

exp(coef(Age.PPHinterest)[2])


# 7: How does size of operation affect whether or not people have planted PPH?

farmsize.PPHgrown <- glm(Q12 ~ Q3_1, almonds, family = binomial)
summary(farmsize.PPHgrown)

exp(coef(farmsize.PPHgrown)[2])


# Chi-square


Size.PPHgrown.tbl = table(almonds$Regions, almonds$Q12)
Size.PPHgrown.tbl

chisq.test(Size.PPHgrown.tbl)

# 8: How does size of operation affect whether or not people are interested in planting PPH?


farmsize.PPHinterest <- glm(Q15 ~ Q3_1, almonds, family = binomial)
summary(farmsize.PPHinterest)

exp(coef(farmsize.PPHinterest)[2])


# 9: How do concerns about PPH differ by location? Region? 









### MORE PLOTS BELOW



# Plot PPHgrown_Role: How does Role in Operation affect whether or not a person has grown PPH?

alm= almonds[almonds$Q1 != " " ,]

role.PPH <- data.frame(table(data.frame(alm$Q1, alm$Q12)))

role.PPH

RolePPH.grown.plot <- ggplot(role.PPH, aes(x = alm.Q1, y = Freq, fill = alm.Q12)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Role in Operation", y = "Count", fill = "Grown PPH") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(RolePPH.grown.plot)


# Plot Location_BMPs: How does location affect whether or not respondent has adopted AB Bee BMPs?
alm= almonds[almonds$Q1 != " " ,]

BMPs <- almonds %>%
  select(Q18_1:Q18_6)

role.BMPs <- data.frame(table(data.frame(alm$Q1, BMPs)))

role.BMPs

Location.BMPs.plot <- ggplot(role.BMPs, aes(x = alm.Q1, y = Freq, fill = BMPs)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
labs(x = "Role in Operation", y = "Count") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(Location.BMPs.plot)




# How do I make it so it includes all the times "Availability of water" was mentioned?

almonds$Q10 <- as.factor(almonds$Q10)


CC.water.county.plot <- ggplot(subset(almonds, Q10 == "Availability of water"),
                        aes(x = Counties, fill = Q9)) +
                         geom_bar() 
      print(CC.water.county.plot)


