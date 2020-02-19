library(lme4)
library(ggplot2)
library(cowplot)

almonds <- read.csv("Almond_Survey_Cleaned_Official.csv")


# How does location affect whether or not people have grown cover crop? 
## How much variation in cover crop adoption is there among people from different counties? 

region.GrownCC <- glm(Counties ~ Q6, almonds, family = binomial)
summary(region.GrownCC)

#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    5.293      1.003   5.280 1.29e-07 ***
#Q6Yes         17.273   4795.696   0.004    0.997 

multiple = almonds[almonds$Counties == "Multiple",]

region.GrownCC.multiple <- glmer(data = almonds, Counties ~ Q6 + (1|multiple), family = binomial)

summary(region.GrownCC.multiple)

exp(coef(region.GrownCC.multiple)[2])


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



# What factors influenced people's decision to chose "physical interference w/ operations" as a factor in growing CC?

Physical.operations.CC <- lm(subset(almonds, Q10 = "Physical interference with operations"),
                                   Q1 ~ Q3_1 + Q5)


# How does respondent's age affect whether or not they have grown cover crop?
CCgrown.age <- glm(Q6~Q31, almonds, family = binomial)

summary(CCgrown.age)



# Plot 1: How does location affect whether or not respondent has grown CC?

# table with x = each of roles and y = 
# Put "." in the cells without info 

alm= almonds[almonds$Q1 != " " ,]

role.count <- data.frame(table(data.frame(alm$Q1, alm$Q6)))

role.count

RoleCCgrown.plot <- ggplot(role.count, aes(x = alm.Q1, y = Freq, fill = alm.Q6)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Role in Operation", y = "Count") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 7), legend.title = element_text(size = 8))
print(RoleCCgrown.plot)


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


# Age and INTEREST in growing cover crop (Shows that perople ages 25-34 are most interested in growing cc?)

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


