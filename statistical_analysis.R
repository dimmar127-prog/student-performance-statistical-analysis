
library(tidyverse)
library(car)
library(ggpubr)
library(dplyr)
library(readxl)
library(rstatix)
library(randtests)
library(jtools)
library(gtsummary)
library(GGally)



data <- read_excel("Students_Data.xlsx")
glimpse(data)

#Μετατροπή σε factor των charackter μεταβλητών
data <- mutate(data,
               Gender = factor(Gender),
               LearningStyle = factor(LearningStyle),
               Class = factor(Class),
               DeviceUsed = factor(DeviceUsed)
              )

glimpse(data)

data <- data |>
  mutate(Motivation_Cat = factor(ifelse(MotivationLevel <= 3, "Low", "High")))

glimpse(data)

#summary of quantitative data
quant_summary <- describe(select(data,where(is.numeric)))
print(quant_summary)

#summary of qualitative data
cat_data <- select(data,where(is.factor))
cat_summary <- summary(cat_data)
print(cat_summary)

#Σύνοψη Μεταβλητών σε πίνακα
data |>
  select(PreLecture, PostLecture, StudyHours, Age, AttendanceRate, FinalScore, LearningStyle, Gender, DeviceUsed, Motivation_Cat) |>
  tbl_summary(statistic = list(PreLecture ~ "{mean} ({sd})",
                               PostLecture ~ "{mean} ({sd})",
                               Age ~ "{mean} ({sd})",
                               AttendanceRate ~ "{median} ({sd})",
                               FinalScore ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) |> 
  add_stat_label()

#################################### Β1 ########################################

#Histogram
data |> 
  ggplot(aes(x = PreLecture)) + 
  geom_histogram(fill='purple', binwidth = 3, alpha  = 0.5) +
  facet_wrap(~DeviceUsed)

#Shapiro-Wilk test for normality
data |>
  group_by(DeviceUsed) |>
  shapiro_test(PreLecture)

#Levene's test for equality of variances 
leveneTest(PreLecture ~ DeviceUsed, data = data)

anova_result <- aov(PreLecture ~ DeviceUsed, data = data)
summary(anova_result)

#################################### Β2 ########################################

#Histogram
data |> 
  ggplot(aes(x = FinalScore)) + 
  geom_histogram(fill='blue', binwidth = 3, alpha  = 0.5) +
  facet_wrap(~Motivation_Cat)

#Shapiro-Wilk test for normality
data |>
  group_by(Motivation_Cat) |>
  shapiro_test(FinalScore)

#Levene's test for equality of variances 
leveneTest(FinalScore ~ Motivation_Cat, data = data)

t_test_result <- t.test(FinalScore ~ Motivation_Cat , data = data , var.equal = TRUE)
print(t_test_result)

#################################### Β3 ########################################
#Histogram
data |> 
  ggplot(aes(x = AttendanceRate)) + 
  geom_histogram(fill='green', binwidth = 3, alpha  = 0.5) +
  facet_wrap(~LearningStyle)

#Shapiro-Wilk test for normality
data |>
  group_by(LearningStyle) |>
  shapiro_test(AttendanceRate)

ggboxplot(data, x = "LearningStyle", y = "AttendanceRate", color = "LearningStyle")

kruskal.test(AttendanceRate ~ LearningStyle, data = data)

#################################### Β4 ########################################

#X^2 test between 2 categorical variables
chisq.test(table(data$DeviceUsed, data$Motivation_Cat))

#################################### Β5 ########################################

#Normality test of the difference 
shapiro.test(data$PostLecture - data$PreLecture)

t.test(data$PostLecture , data$PreLecture , paired = TRUE)

#################################### B-Task ####################################

#correlation matrix
data |> 
  select(FinalScore, Age, StudyHours, AttendanceRate) |> 
  ggpairs()

#Normality test of the variances 
shapiro.test(data$FinalScore)
shapiro.test(data$Age)
shapiro.test(data$StudyHours)
shapiro.test(data$AttendanceRate)

#Use spearman rank correlation due to non normality of the variables
cor.test(data$FinalScore , data$Age , method = 'spearman')
cor.test(data$FinalScore , data$StudyHours , method = 'spearman')
cor.test(data$FinalScore , data$AttendanceRate , method = 'spearman')

#################################### C-Task ####################################

model <- lm(FinalScore ~ StudyHours + AttendanceRate + Age + Gender , data = data)
summ(model , confint = TRUE , vifs = TRUE , digits = 3)

plot(model)



#################################### Task 2 ####################################

library(randtests)

data_4 <- read_excel("Data_4.xlsx")

str(data_4)

#Έλεγχος τυχαιότητας για τη μεταβλητη Var1
runs_test <- runs.test(data_4$Var_1)
print(runs_test)

shapiro_test <- shapiro.test(data_4$Var_2)
print(shapiro_test)
mean(data_4$Var_2)
sd(data_4$Var_2)

data_4 |> 
  ggplot(aes(x = Var_2)) + 
  geom_histogram(fill='red', binwidth = 0.05, alpha  = 0.5) 
