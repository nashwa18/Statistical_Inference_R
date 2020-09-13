### Assignment Part - 1 ###

library(ggplot2)

# Load Dataset
data("ToothGrowth")

# View Data Set
View(ToothGrowth)

# Structure of Data Set
str(ToothGrowth)
help("ToothGrowth")


### Basic Summary of Data ###

# The Effect of Vitamin C on Tooth Growth in Guinea Pigs
# 60 guinea pigs - rows
## There are 3 Variables:
 # 1. Tooth Length - Numeric
 # 2. Dose in miligrams per day (3 Levels) - 0.5, 1 & 2
 # 3. Supplement Type: Vitamin C (VC) or Orange Juice (OJ)

 # 1. Tooth Length - Numeric Continuous Variable
summary(ToothGrowth$len)
boxplot(ToothGrowth$len)

ggplot(ToothGrowth,aes(x = ToothGrowth$len))+
  geom_histogram()

ggplot(ToothGrowth,aes(x = ToothGrowth$len))+
  geom_histogram(binwidth = 5)

 # 2. Supplement Type - Categorical Varibale
summary(ToothGrowth$supp)

ggplot(ToothGrowth,aes(x = supp))+
  geom_bar()

 # 3. Dose - Numerical Discrete Variable
summary(ToothGrowth$dose)

ggplot(ToothGrowth,aes(x = dose))+
  geom_bar()

  # Dose and Supp 
ggplot(ToothGrowth, aes(x=dose,fill=supp))+
  geom_bar()
  # There are 60 GPs in the data set.
  # The 3 doses are given to 20 pigs each respectively. 
  # Each dose is given either through Orange Juice or Vitamin C.

 # Length and Dose
boxplot(len ~ dose, data = ToothGrowth)



### Tooth Growth by Supp ### 
ggplot(ToothGrowth,aes(y=len, color = supp)) +
  geom_boxplot()
 # Supp received through OJ provides a higher median length of tooth than VC.

# Hypothesis Test based on difference in means
  # H0 - There's no difference in average tooth growth of GPs who received supplements through OJ and VC. (mu_OJ - mu_VC = 0)  
  # H1 - There's some difference in average tooth growth of GPs who received supplements through OJ and VC. (mu_OJ - mu_VC =! 0)
# Perform t-test
t.test(len ~ supp, data = ToothGrowth)
# Results
 # As p-value > 0.05, therefore we fail to reject null hypothesis. There is no
 # difference in average tooth growth of GPs. 




### What is the effect of dosage on the length of tooth growth when the supp is received either through VC or OJ?
boxplot(len ~ dose, data = ToothGrowth, subset = supp == "VC")
boxplot(len ~ dose, data = ToothGrowth, subset = supp == "OJ")
    # As dosage increases, the the median length also increases.
boxplot(len ~ dose:supp, data = ToothGrowth)
# From the graph we can see that there's some difference in the median length 
# of the tooth growth with respect to different dosage received through either VC or OJ. 

# The median length of tooth growth is higher when 1 mg of dose is received through OJ compared
# to 1 mg of dosage received through VC. 
 



### The effect of supp on tooth growth with a dosage of only 0.5 mg
df_d1 <- subset(ToothGrowth, dose == 0.5)
#Hypothesis Testing
  # H0: There is no difference in average tooth growth. 
  # H1: There is a difference in average tooth growth.
# Performing t-test. Difference of mean
t.test(len ~ supp,data = df_d1)
# p-value < 0.05, therefore we reject the Null Hypothesis. There is a difference in the average tooth growth
# when the supp is received with a dosage of 0.5 mg. 




### The effect of supp on tooth growth with a dosage of only 1 mg
df_d2 <- subset(ToothGrowth, dose == 1)
#Hypothesis Testing
 # H0: There is no difference in average tooth growth. 
 # H1: There is a difference in average tooth growth.
# Performing t-test. Difference of mean
t.test(len ~ supp,data = df_d2)
# p-value < 0.05, therefore we reject the Null Hypothesis. There is a difference in the average tooth growth
# when the supp is received with a dosage of 1 mg.



### The effect of supp on tooth growth with a dosage of only 2 mg
df_d3 <- subset(ToothGrowth, dose == 2)
#Hypothesis Testing
# H0: There is no difference in average tooth growth. 
# H1: There is a difference in average tooth growth.
# Performing t-test. Difference of mean
t.test(len ~ supp,data = df_d3)
# p-value > 0.05, therefore we fail to reject the Null Hypothesis. There is no difference in the average 
# tooth growth when the supp is received with a dosage of 2 mg.





