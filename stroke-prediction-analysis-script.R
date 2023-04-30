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