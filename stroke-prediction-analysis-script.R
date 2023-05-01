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

