#Remove any variable before start new script
rm(list = ls())

#Import data
strokedata <- read.csv("healthcare-dataset-stroke-data.csv")

#Import package
library(Hmisc)

#Hmisc command to take a brief look on the data set
describe(strokedata)

#Task 1: Check Hypothesis and do the Statistical Difference
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

#Task 2: Plot point chart
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

#Another chart with same meaning 

ggplot(data = strokepeople) +
  geom_point(mapping = aes(x = age, y = bmi), colour = "blue", pch = "+") +
  labs(title = "The BMI pattern of stroke people", x = "Age of stroke people",
       y = "BMI of stroke people")

ggplot(data = normalpeople) +
  geom_point(mapping = aes(x = age, y = bmi), colour = "blue", pch = "+") +
  labs(title = "The BMI pattern of normal people", x = "Age of normal people",
       y = "BMI of normal people")

#Conclusion from charts: 
#number of normal people is greater than stroke people
#the average bmi of stroke people (30) is higher than average bmi of normal people (28)
#people older than 40 years old + BMI level higher than average of 25 is likely to encounter stroke

#Task 3: Plot pie chart
#Create pie chart table
#a) Gender Distribution in Stroke Dataset

#Calculate frequency of each gender in the stroke dataset
gender_counts <- table(strokepeople$gender)

#Convert gender_counts table to a dataframe
gender_df <- as.data.frame(gender_counts)

#Rename columns within the dataframe
colnames(gender_df) <- c("gender", "count")

#Create pie chart
gender_percentage <- round (100*gender_df$count/sum(gender_df$count),1)

pie(gender_df$count, labels = gender_percentage, radius = 1, main = "Gender Distribution in Stroke Dataset", col = rainbow(length(gender_df$count)))
legend("topright", legend = gender_df$gender, cex = 0.8, fill = rainbow(length(gender_df$count)))
#Conclusion: Female have higher stroke risk than male however it is not significant.

#b) Hypertension Distribution in Stroke Dataset
#Rename the categories in the dummy variable "hypertension" of the dataset
strokepeople$hypertension <- ifelse(strokepeople$hypertension == 0, "no hypertension", "hypertension")

#Calculate the frequency of the hypertension in the stroke dataset

hypertension_count <- table(strokepeople$hypertension)

#Create hypertension dataframe
hypertension_df <- as.data.frame(hypertension_count)

#Rename the column of the dataframe
colnames(hypertension_df) <- c("medical_background", "count")

#Create pie chart
hypertension_percent<- round(100*hypertension_df$count/sum(hypertension_df$count), 1)

pie(hypertension_df$count, labels = hypertension_percent, radius = 1, main = "Hypertension Distribution in Stroke Dataset", col = rainbow(2))
legend("topright", legend = hypertension_df$medical_background, cex = 0.8, fill = rainbow(2))

#Conclusion: Having hypertension may not necessarily be a strong risk factor for stroke in this population.

#c) Heart Disease Distribution in Stroke Dataset
#Rename the categories in the dummy variable "heart_disease" of the dataset
strokepeople$heart_disease <- ifelse(strokepeople$heart_disease == 0, "No heart disease", "Heart disease")

#Calculate the frequency of the heart disease in the stroke dataset

heartdisease_count <- table(strokepeople$heart_disease)

#Create heart disease dataframe
heartdisease_df <- as.data.frame(heartdisease_count)

#Rename the column of the dataframe
colnames(heartdisease_df) <- c("Heart_Background", "count")

#Create pie chart
piepercent<- round(100*heartdisease_df$count/sum(heartdisease_df$count), 1)


pie(heartdisease_df$count, labels = piepercent, radius = 1, main = "Heart Disease Distribution in Stroke Dataset", col = rainbow(2))
legend("topright", legend = heartdisease_df$Heart_Background, cex = 0.8, fill = rainbow(2))

#d) Smoking Status in the Stroke Dataset [3D Pie Chart]
#Count the variable in the smoking status

smoking_count <- table(strokepeople$smoking_status)

#Create smoking status dataframe
smoking_df <- as.data.frame(smoking_count)

#Rename the column names of the smoking status dataframe
colnames(smoking_df) <- c("Smoking status", "count")
print(smoking_df)

#Import the 3D pie chart package
library(plotrix)

#Create pie chart

smoking_percentage <- paste0(round(smoking_df$count /sum(smoking_df$count) * 100, 1), "%")

par(mar = c(5, 4, 4, 8),                                  # Specify par parameters
    xpd = TRUE)

png(file = "smokingpercentagepiechart.png") 

pie3D(smoking_df$count,labels = smoking_percentage ,explode = 0.1, main = "Smoking Distribution in Stroke Dataset")
legend("topright",legend = smoking_df$`Smoking status`, cex = 0.8, fill = rainbow(length(smoking_df$count)))

dev.off()
#Task 4: Plot bar chart
#import the tidyverse
library(tidyverse)

#a) Glucose level status of normal people and stroke people

#The normal glucose level is below 100mg/dL. Plot the bar chart that distinct the normal glucose level and high glucose level in the normal people and stroke people
#Step by step: need 4 info: stroke (number of =<100, number of > 100), normal people (number of =<100, number of > 100)

#Gather required information for the bar chart

#The glucose level of stroke
stroke_glucose <- ifelse(strokepeople$avg_glucose_level > 100, "higher than normal (> 100 mg/dL)", "normal (<= 100 mg/dL)")
stroke_glucose_count <- table(stroke_glucose)
#Create stroke matrix 
stroke_glucose_mt <- as.matrix(stroke_glucose_count)

#The glucose level of normal
normal_glucose <- ifelse(normalpeople$avg_glucose_level > 100, "higher than normal (> 100 mg/dL)", "normal (<= 100 mg/dL)")
normal_glucose_count <- table(normal_glucose)
#Create normal matrix
normal_glucose_mt <- as.matrix(normal_glucose_count)

#Combine two matrix together by cbind function
finalglucosedata_mt <- cbind(stroke_glucose_mt, normal_glucose_mt)

#Change the column name of the matrix
colnames(finalglucosedata_mt) = c("Stroke sample", "Normal sample")

jpeg(filename = "glucosebarplot.jpg", width = 800, height = 800, pointsize = 16) 

#Draw a barplot
bp <- barplot(finalglucosedata_mt,
        main = "The Glucose Level Distribution in stroke and normal sample",
        xlab ="Glucose status",
        ylab ="Number of people",
        ylim = c(0, max(finalglucosedata_mt) + 500),
        col = c("#bc4749","#6a994e"),
        beside = TRUE,
        cex.axis = 1.3,
        cex.names = 1.3
)
legend("topleft",
       legend = c("higher than normal (>100 mg/dL)","normal (<= 100mg/dL)"),
       fill = c("#bc4749","#6a994e"),
       cex = 1.3
)
text(bp, finalglucosedata_mt + 100, labels = finalglucosedata_mt, cex = 1.3)

dev.off()