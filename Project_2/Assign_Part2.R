### Assignment 2 ###

# 6272 Swedish men 
# Is there any association between the amount of fish in their diet and prostrate cancer?

fishdata <- read.csv("fish-diet.csv")
View(fishdata)
str(fishdata)
summary(fishdata)

# Fish in diet - The amount of fish in diet - Categorical
ggplot(data = fishdata,aes(x=fishdata$fish_in_diet))+
  geom_bar()

# Cancer - Categorical
ggplot(data = fishdata,aes(x=fishdata$cancer))+
  geom_bar()

# Cancer and Fish_in_diet
ggplot(data = fishdata,aes(x=fishdata$fish_in_diet, fill = cancer))+
  geom_bar()
#Contingency Table
table(fishdata$fish_in_diet,fishdata$cancer)
prop.table(table(fishdata$fish_in_diet,fishdata$cancer))*100


# Performing Chi-Squared Test

# H0: Cancer is independent of the amount of fish eaten by Swedish men. 
# H1: Cancer is dependent on the amount of fish eaten. 

chisq.test(fishdata$fish_in_diet, fishdata$cancer)

# p-value > 0.05, we fail to reject null hypothesis. Cancer is independent of the amount of fish eaten. 




