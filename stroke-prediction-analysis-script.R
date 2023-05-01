#Remove any variable before start new script
rm(list = ls())

#Import data
strokedata <- read.csv("healthcare-dataset-stroke-data.csv")

#Import package
library(Hmisc)

#Hmisc command to take a brief look on the data set
describe(strokedata)

#Hypothesis: people who had stroke is higher in bmi than people who had no stroke.
#Create two table: stroke people, normal people
strokepeople = subset(strokedata, strokedata$stroke == 1)
normalpeople = subset(strokedata, strokedata$stroke == 0)

#Turn bmi character into numeric data type
strokepeople$bmi <- as.numeric(strokepeople$bmi)
normalpeople$bmi <- as.numeric(normalpeople$bmi)

#calculate mean BMI of people has stroke & mean BMI of people has no stroke
mean(strokepeople$bmi, na.rm = TRUE)
mean(normalpeople$bmi, na.rm = TRUE)

#Do the statistical difference to check the hypothesis
t.test(strokepeople$bmi, normalpeople$bmi, alternative = "two.sided", conf.level = 0.99)

#The p-value = 0.00033 < 0.05 (threshold) --> The test is statistical difference
#At 99% CI, the stroke people bmi is higher than normal people bmi at 0.47 - 2.82 bmi
#Conclusion: Reject the null hypothesis, finding that higher bmi level is likely cause stroke

#Plot the stroke people & normal people relevant charts to compare 
#Import ggplot2 package
library(ggplot2)

#First charts: age and BMI of stroke people; age and BMI of normal people
ggplot(data = strokepeople, mapping = aes(x = age, y = bmi)) +
  geom_point() +
  geom_smooth()

ggplot(data = normalpeople, mapping = aes(x = age, y = bmi)) +
  geom_point() +
  geom_smooth()

ggplot(data = strokepeople) +
  geom_point(mapping = aes(x = age, y = bmi), colour = "blue", pch = "+") +
  labs(title = "The BMI pattern of stroke people", x = "Age of stroke people",
       y = "BMI of stroke people")

ggplot(data = normalpeople) +
  geom_point(mapping = aes(x = age, y = bmi), colour = "blue", pch = "+") +
  labs(title = "The BMI pattern of normal people", x = "Age of normal people",
       y = "BMI of normal people")

#Conclusion from chart: 
#number of normal people is greater than stroke people
#the average bmi of stroke people (30) is higher than average bmi of normal people (28)
#people older than 40 years old + BMI level higher than average of 25 is likely to encounter stroke

#Create pie chart table
#a) Gender Distribution in Stroke Dataset

#Calculate frequency of each gender in the stroke dataset
gender_counts <- table(strokepeople$gender)

#Convert gender_counts table to a dataframe
gender_df <- as.data.frame(gender_counts)

#Rename columns within the dataframe
colnames(gender_df) <- c("gender", "count")

#Create pie chart
ggplot(gender_df, aes(x="", y=count, fill=gender)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar(theta="y") +
  labs(title="Gender Distribution in Stroke Dataset") +
  scale_fill_manual(values=c("pink", "blue")) +
  theme_void()

