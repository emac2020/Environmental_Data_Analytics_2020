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


# 2. How does Role in Operation affect whether or not a person has GROWN cover crop?

Role.Operation.CCgrown <- glm(Q1 ~ Q6, almonds, family = binomial)

summary(Role.Operation.CCgrown)

exp(coef(Role.Operation.CCgrown)[2])


# 2.plot Role in operation and whether or not a person has GROWN cover crop 

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

# 3. How does a person's role in the operation affect whether they are INTERESTED in growing cover crop?

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



# 5. How does location affect whether or not a person has GROWN cover crop?

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



# 6: How does location affect whether or not a person is INTERESTED in growing cover crop?

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



# 8: How does age affect whether or not a person has GROWN CC? 

Age.CCgrown <- glm(Q6~Q31, almonds, family = binomial)

summary(Age.CCgrown)

exp(coef(Age.CCgrown)[2])




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

Age.CCinterested <- glm(Q15~Q31, almonds, family = binomial)

summary(Age.CCinterested)

exp(coef(Age.CCinterested)[2])


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

farmsize.GrownCC <- glm(Q3_1 ~ Q6, almonds, family = binomial)
summary(farmsize.GrownCC)

exp(coef(farmsize.GrownCC)[2])


# 11.Plot: Operation size and Grown CC


# 12: How does size of operation affect whether or not people are interested in growing cover crop?



# 12.Plot: Operation Size and Interested in Growing CC



# 13.Cowplot: Operation Size and Grown CC vs. Interested in Growing CC



# 14: How do concerns about cover crop differ by location? Region? 

# Concerns for Cover Crop 
# How do I make a table of all the concerns for CC that appear (all Q12 variables)
# table: categorize all the concerns (water, time, labor, interference, etc.). table with different categorize
# treat concern as a factor and it will give a number to each concern. 
# What were the factors that made someone think labor was the most important (use: multinomial)

almonds$Q10 <- as.factor(almonds$Q10)

CCconcernTable <- table(almonds$Q10)

view(CCconcernTable) # This just shows all the combos of concerns



# What factors influenced people's decision to chose "Availability of water" as a factor in growing CC?
# Use multinomial 

Availability.water.CCconcern <- lm(subset(almonds, Q10 = "Availability of water"),
                                   Q3_1~Q1)






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


