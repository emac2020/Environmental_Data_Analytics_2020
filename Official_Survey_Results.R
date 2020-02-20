library(lme4)
library(ggplot2)
library(cowplot)
library(corrplot)
library(tidyverse)
library(viridis)

almonds <- read.csv("Almond_Survey_Cleaned_Official.csv")

# 1. Plot: Respondent Location (how do i change the color?)

locoplot<- ggplot(almonds, aes(x = Counties, fill = Counties)) +
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "County", y = "Count") +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))

print(locoplot)


# 2. How does Role in Operation affect whether or not a person has grown cover crop?

Role.Operation.CCgrown <- glm(Q1 ~ Q6, almonds, family = binomial)

summary(Role.Operation.CCgrown)

exp(coef(Role.Operation.CCgrown)[2])


# 2.plot of Role in operation and whether or not a person has grown cover crop 

alm= almonds[almonds$Q1 != " " ,]

role.count <- data.frame(table(data.frame(alm$Q1, alm$Q6)))

role.count

Role.CCgrown.plot <- ggplot(role.count, aes(x = alm.Q1, y = Freq, fill = alm.Q6)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Role in Operation", y = "Count", fill = "Grown Cover Crop") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(Role.CCgrown.plot)

# 3. How does the respondent's role in the operation affect interest in growing cover crop?

Role.Operation.CCinterest <- glm(Q1 ~ Q9, almonds, family = binomial)

summary(Role.Operation.CCinterest)

exp(coef(Role.Operation.CCinterest)[2])

# 3.plot: Role in Operation and Interest in Growing CC
alm= almonds[almonds$Q1 != " " ,]

role.interest.count <- data.frame(table(data.frame(alm$Q1, alm$Q9)))

role.interest.count

Role.CCinterest.plot <- 
  ggplot(role.interest.count, aes(x = alm.Q1, y = Freq, fill = alm.Q9)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  #scale_color_manual(values = c("#FF1234", "#41b6c4", "#1d91c0")) +
  labs(x = "Role in Operation", y = "Interest in Growing Cover Crop", fill = "Interest in Cover Crop") +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "right")
print(Role.CCinterest.plot)

#4.Cowplot: Role in Operation and Grown CC vs. Interested in Growing CC

CC.Role.plots <- plot_grid(Role.CCgrown.plot, Role.Operation.CCinterest.plot, 
                          align = "h", ncol = 1)

print(CC.Role.plots)



# 5. How does location affect whether or not a person has grown cover crop?

Location.GrownCC <- glm(Counties ~ Q6, almonds, family = binomial)
summary(Location.GrownCC)


#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    5.293      1.003   5.280 1.29e-07 ***
#Q6Yes         17.273   4795.696   0.004    0.997 


multiple = almonds[almonds$Counties == "Multiple",]

region.GrownCC.multiple <- glmer(data = almonds, Counties ~ Q6 + (1|multiple), family = binomial)

summary(region.GrownCC.multiple)

exp(coef(region.GrownCC.multiple)[2])


# 5.plot: Location and Grown CC

alm.loco= almonds[almonds$Counties != " ." ,]

Location.CC <- data.frame(table(data.frame(alm.loco$Counties, alm.loco$Q6)))

Location.CC

Location.CCgrown.plot <- ggplot(Location.CC, aes(x = alm.loco.Counties, y = Freq, fill = alm.loco.Q6)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
labs(x = "Location", y = "Count", fill = "Grown CC") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(Location.CCgrown.plot)



# 6: How does location affect whether or not a person is Interested in growing cover crop?

Location.InterestCC <- glm(Counties ~ Q9, almonds, family = binomial)
summary(Location.InterestCC)

exp(coef(Location.InterestCC)[2])


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

# 8: How does age affect whether or not a person has grown CC? 


# 8.plot: Age and CC Grown


# 9: How does age affect whether or not a person is interested in growing CC?


# 8.plot: Age and Interest in Growing CC



# How does size of operation affect whether or not people have grown cover crop?

farmsize.GrownCC <- glm(Q3_1 ~ Q6, almonds, family = binomial)
summary(farmsize.GrownCC)

# (Intercept)   4.1846     0.5817   7.193 6.32e-13 ***
# Q6Yes        -0.9962     0.7738  -1.287    0.198 



# Concerns for Cover Crop 
# How do I make a table of all the concerns for CC that appear (all Q12 variables)
# table: categorize all the concerns (water, time, labor, interference, etc.). table with different categorize
# treat concern as a factor and it will give a number to each concern. 
# What were the factors that made someone think labor was the most important (use: multinomial)

almonds$Q10 <- as.factor(almonds$Q10)

CCconcernTable <- table(almonds$Q10)

view(CCconcernTable) # This just shows all the combos of concerns


# How does Role of Respondent (Owner, Owner/Operator, Farm Manager) affect the concerns they have for cover crop?
CC.concern <- glm(as.factor(Q10) ~ Q1, almonds, family = binomial)
summary(CC.concern)



# What factors influenced people's decision to chose "Availability of water" as a factor in growing CC?

Availability.water.CCconcern <- lm(subset(almonds, Q10 = "Availability of water"),
                                   Q3_1~Q1)


# How does respondent's age affect whether or not they have grown cover crop?
CCgrown.age <- glm(Q6~Q31, almonds, family = binomial)

summary(CCgrown.age)



# Plot 1: How does location affect whether or not respondent has grown CC?




# Plot 2: How does location affect whether or not respondent has grown PPH?

alm= almonds[almonds$Q1 != " " ,]

role.PPH <- data.frame(table(data.frame(alm$Q1, alm$Q12)))

role.PPH

RolePPH.grown.plot <- ggplot(role.PPH, aes(x = alm.Q1, y = Freq, fill = alm.Q12)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")
  #scale_color_manual(values = c("#FF1234", "#41b6c4", "#1d91c0")) +
  labs(x = "Role in Operation", y = "Count") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(RolePPH.grown.plot)

# Plot 3: How does location affect whether or not respondent has adopted AB Bee BMPs?
alm= almonds[almonds$Q1 != " " ,]

role.BMPs <- data.frame(table(data.frame(alm$Q1, alm$Q18_1:Q18_6)))

role.BMPs

RolePPH.grown.plot <- ggplot(role.PPH, aes(x = alm.Q1, y = Freq, fill = alm.Q12)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")
#scale_color_manual(values = c("#FF1234", "#41b6c4", "#1d91c0")) +
labs(x = "Role in Operation", y = "Count") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(RolePPH.grown.plot)

#Plot 4: How does age affect whether or not respondent has adopted cover crop?
alm= almonds[almonds$Q31 != " " ,]

age.CC <- data.frame(table(data.frame(alm$Q31, alm$Q6)))

age.CC

Age.grown.plot <- ggplot(age.CC, aes(x = alm.Q31, y = Freq, fill = alm.Q6)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 60) +
  scale_fill_brewer(palette = "Set1") +
labs(x = "Age", y = "Count") +
  theme(legend.position = "right")
print(Age.grown.plot)
#scale_color_manual(values = c("#FF1234", "#41b6c4", "#1d91c0")) +
#+ theme(legend.position = "right", 
        #legend.text = element_text(size = 7), legend.title = element_text(size = 8))


# Age and INTEREST in growing cover crop (Shows that people ages 25-34 are most interested in growing cc?)

alm= almonds[almonds$Q31 != " " ,]

alm.age= alm[alm$Q9 != "." ,]

age.interestCC <- data.frame(table(data.frame(alm.age$Q31, alm.age$Q9)))

age.interestCC

Age.interest.plot <- ggplot(age.interestCC, aes(x = alm.age.Q31, y = Freq, fill = alm.age.Q9)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,23) +
  scale_fill_brewer(palette = "Set1") +
#scale_color_manual(values = c("#FF1234", "#41b6c4", "#1d91c0")) +
labs(x = "Age", y = "Count") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(Age.interest.plot)


# Plot: Age: have grown and are interested in growing CC

CC.age.plots <- plot_grid(Age.grown.plot, Age.interest.plot, 
                        align = "h", ncol = 1)

print(CC.age.plots)


# How do I make it so it includes all the times "Availability of water" was mentioned?

almonds$Q10 <- as.factor(almonds$Q10)


CC.water.county.plot <- ggplot(subset(almonds, Q10 == "Availability of water"),
                        aes(x = Counties, color = Q9)) +
                         geom_bar() 
      print(CC.water.county.plot)


