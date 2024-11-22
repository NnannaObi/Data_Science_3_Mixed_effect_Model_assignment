# Name: Obiabuchi_Martin_Nnanna_Mix_efferct_assignement
#Loading the libraries
install.packages(lme4)
library(lme4,quietly = TRUE)
import(tidyverse)
import(broom)
library('tidyverse')
library('broom')
library(ggplot2)

# Reaction times in a sleep deprivation study
# Description
# The average reaction time per day (in milliseconds) for subjects in a sleep deprivation study.


# DESCRIPTION OF THE DATASET
" These data are from the study described in Belenky et al. (2003),
for the most sleep-deprived group (3 hours time-in-bed) and for the first 10 days of the study,
up to the recovery period. The original study analyzed speed (1/(reaction time)) 
and treated day as a categorical rather than a continuous predicton "

#01 DATA EXPLORATION


# a. LOADING THE DATA
data(sleepstudy)
?sleepstudy
data <- sleepstudy
head(data, 4)  #visualizng the dataset
 

#b. exploring the structure of the data
str(sleepstudy)
#The sleepstudy contains 180 observations and 3 variables
# Both the Reaction and Days are numerical
# The Subject column is a factor with 18 levels.
# Loading the dataset 'sleepstudy'

# Extra
# Check for missing values
any(is.na(data))

# check for duplicate values by columnwise
Subj_duplicates <- sum(duplicated(data$Subject))
Subj_duplicates
# For the Subject column, I obtained 162 duplicates which is indication non-independence for the variable

Days<-sum(duplicated(data$Day))
Days
#And the Days column, I obtained 170 duplicates which is indication non-independence for the variable

RT_duplicates <- sum(duplicated(data$Reaction))
RT_duplicates
# No duplicate for the reaction time



# c. VISUALIZATION OF  THE DATA USING APPROPRIATE PLOTS
# Visualizing the data to understand the distribution and relation

# Correct histogram for Reaction variable
ggplot(data, aes(x = Reaction)) + 
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Reaction", x = "Reaction", y = "Frequency") +
  theme_minimal()
#Reaction is little right skewed and not normally distributed

# Visualizing Effect of  Subjet on Reaction Time
ggplot(data, aes(x = factor(Subject), y = Reaction, fill = factor(Subject))) +
  geom_boxplot() +
  labs(title = "Boxplot of Reaction Time by Subject",
       x = "Subject",
       y = "Reaction Time") +
  theme_minimal()

# 02  DESCRIPTIVE STATISTICS
data %>% select(c(Subject, Reaction)) %>% summarise()

sleepstudy %>% select(c(Subject,Reaction)) %>%
 group_by((Subject)) %>%
  summarise(Mean_RT= mean(Reaction),
    Median_RT = median(Reaction),
    Std_Dev_RT= sd(Reaction),
    Max_Days = max(Days))

#SUBSETTING THE DATASET
#VISUALIZING EFFECT OF DAYS ON REACTION TIME

#Adaptation
data_1 <- filter(data ,Days < 2) 
data_1 
summary(data_1)

boxplot(Reaction ~ Subject, col=c("white","lightgray"),data_2)


#Baseline
data_2 <- filter(data,Days == 2) 
data_2
summary(data_2)

boxplot(Reaction ~ Days, col=c("white","lightgray"),data_2)


#Sleep deprived
data_3 <- filter(data,Days > 2) 
summary(data_3)

boxplot(Reaction ~ Days, col=c("white","lightgray"),data_3)



#PLOTTING THE DENSITY DISTRIBUTION OF THE RESPONSE VARIABLE REACTION TIME

ggplot(data, aes(x = Reaction)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "PuOr")
  labs(title = "Densityplot of Reaction Time",
       y = "Reaction Time") +
  theme_minimal()

# COMPARING MEANS BETWEEN THE SUBSETTED GROUPS: based on Adaptive Response, Baseline and Sleep Deprivation
  
# Extracting the Baseline, Adaption stage, and  Sleep deprived from the Reaction column.

#Adaptation
RT_1 <- data_1$Reaction 

#Baseline
RT_2 <- data_2$Reaction 

#Sleep deprived
RT_3 <- data_3$Reaction 
  
# Two-sample t-test for Adaption stage and Baseline
 t.test(RT_1, RT_2)
  
# VISUALIZING RESULT FROM T TEST
combined_data_1 <- rbind(
  data.frame(Group = "Adaptation", Reaction_Time = RT_1),
  data.frame(Group = "Baseline", Reaction_Time = RT_2))

  
ggplot(combined_data_1, aes(x = Group, y = Reaction_Time, fill = Group)) +
  geom_boxplot() +
  labs(title = "Box Plot of Reaction Time Between Adaption stage and Baseline",
        x = "Group",
        y = "Reaction Time") +
  theme_minimal()
  
  
## Two-sample t-test for Baseline and Sleep deprived
t.test(RT_2, RT_3)
  
## VISUALIZING RESULT FROM T TEST
combined_data_2 <- rbind(
  data.frame(Group = "Baseline", Reaction_Time = RT_2),
  data.frame(Group = "Sleep Deprived", Reaction_Time = RT_3)
)
  
ggplot(combined_data_2, aes(x = Group, y = Reaction_Time, fill = Group)) +
  geom_boxplot() +
  labs(title = "Box Plot of Reaction Time Between Baseline and Sleep deprived",
        x = "Group",
        y = "Reaction Time") +
  theme_minimal()



# Creation of vizualization plot to understand the distribution of reaction times over day

# Exploring the relationship between Reaction and Days
ggplot(data, aes(x = factor(Days), y = Reaction)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(
    title = "Reaction vs. Days",
    x = "Days",
    y = "Reaction"
  ) +
  theme_minimal()


#There is an increasing trend in the reaction time  over the 10-days,
# the number reaction time increase by one day, 
#with first and last day having the lowest and highest reaction time respecivety.


# Summarize the data
mean_reaction <- data %>%
  group_by(Days) %>%
  summarise(Mean_Reaction = mean(Reaction))
mean_reaction
# Bar plot
ggplot(mean_reaction, aes(x = factor(Days), y = Mean_Reaction)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "darkblue") +
  labs(
    title = "Mean Reaction Time by Day",
    x = "Days",
    y = "Mean Reaction"
  ) +
  theme_minimal()



### CHECK FOR NORMALITY OF THE RESPONSE VARIABLE

shapiro_test_result <- shapiro.test(data$Reaction)
print(shapiro_test_result)
p_value <- shapiro_test_result$p.value
print(p_value)
#p_value <0.005
#We reject the null hypothesis,because the Reaction data is not normally distributed.


## DATA TRANSFORMATION

data$LogReaction <- log(data$Reaction)

# Visualizing the normality assumption using the  the Histogram with density
ggplot(data, aes(x = Reaction)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Reaction Times", x = "Reaction", y = "Density")
#And th Q-Q Plot
qqnorm(data$Reaction)
qqline(data$Reaction, col = "red")

#03 FIT AN ADEQUATE MODEL(S)
## MODEL IMPLEMENTATION
model_1 <- lmer(Reaction~ Days + (1|Subject), data)
summary(model_1)

model_2<- lmer(LogReaction ~ Days + (1|Subject), data)
summary(model_2)

Intercept2 <- exp(5.530065)
Intercept2


model_3 <- lmer(log(data_3$Reaction) ~ Days + (1|Subject), data_3)
summary(model_3)
Intercept3 <- exp(5.530238)
Intercept3


## MODEL INTERPRETATION
#Model 1:
#The intercept indicates the average reaction time without sleep deprivation, the baseline reaction time is 251.41 ms.
#coefficent entails that With each additional day of sleep deprivation, the reaction time increases by 10.47 ms.
#The standard deviation of reaction time for a subject is 37.12 ms, reflecting variability between subjects' reaction times.

  
#Model 2 (after log-transforming reaction time):

#The exponential of the intercept shows the reaction time without sleep deprivation,the baseline reaction time is 5.53 ms.
#The t value of 13.36 indicates the significance of day on reaction time, and it is highly significant.
#There is significant variability between subjects.


#Model 3 (log-transformed reaction time for sleep deprivation data_3):

#The exponential of the intercept shows the reaction time without sleep deprivation, with a value of 252.20 ms.
#The t value of 8.41 indicates the significance of day on reaction time, and it is highly significant.
#There is significant variability between subjects.

# RESIDUAL ANALYSIS
# For mixed-effects model

residuals_mixed_model_2 <- resid(model_2)
residuals_mixed_model_3 <- resid(model_3)


# For mixed-effects model residuals


qqnorm(residuals_mixed_model_2)
qqline(residuals_mixed_model_2, col = "red")

qqnorm(residuals_mixed_model_3)
qqline(residuals_mixed_model_3, col = "red")

# Shapiro-Wilk normality test for mixed-effects model residuals
shapiro.test(residuals_mixed_model_1)
shapiro.test(residuals_mixed_model_2)
shapiro.test(residuals_mixed_model_3)


# RESIDUAL ANALYSIS FOR MODEL 1

residuals_mixed_model_1 <- resid(model_1)

par(mfrow = c(1,3))

hist(residuals_mixed_model_1)

qqnorm(residuals_mixed_model_1)
qqline(residuals_mixed_model_1, col = "red")

plot(fitted(model_1),residuals_mixed_model_1)

shapiro.test(residuals_mixed_model_1)

# RESIDUAL ANALYSIS FOR MODEL 2
residuals_mixed_model_2 <- resid(model_2)

par(mfrow = c(1,3))

hist(residuals_mixed_model_2)

qqnorm(residuals_mixed_model_2)
qqline(residuals_mixed_model_2, col = "red")

plot(fitted(model_1),residuals_mixed_model_2)

shapiro.test(residuals_mixed_model_2)


# RESIDUAL ANALYSIS FOR MODEL 3
residuals_mixed_model_3 <- resid(model_3)

par(mfrow = c(1,3))

hist(residuals_mixed_model_3)

qqnorm(residuals_mixed_model_3)
qqline(residuals_mixed_model_3, col = "red")

plot(fitted(model_3),residuals_mixed_model_3)

shapiro.test(residuals_mixed_model_3)

# The residual show that there is no constant variance across fitted values.This indicates heteroscedasticity