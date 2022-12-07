Fat <- read_excel("OneDrive - University of Missouri/Senior Fall/Data Analysis/Fat.xlsx")

summary(model <- lm(BMI ~ Density + Age + 
                    Weight.lbs + Height.in + BodyFatSiri + FatFreeWeight + Neck + Chest + Abdomen + Hip + Thigh + Knee +
                    Ankle + Bicep + Forearm + Wrist, data = Fat))
                    
summary(model2 <- lm(BodyFatSiri ~ Density + Age + 
                      Weight.lbs + Height.in + BMI + FatFreeWeight + Neck + Chest + Abdomen + Hip + Thigh + Knee + 
                      Ankle + Bicep + Forearm + Wrist, data = Fat))


plot(Fat$BMI, Fat$Height.in, main = "BMI and Height", xlab = "BMI", ylab= "Height")

plot(Fat$BMI, Fat$Weight.lbs, main = "BMI and Weight", xlab = "BMI", ylab = " Weight") 

plot(Fat$BMI, Fat$FatFreeWeight, main = "BMI and Fat Free Weight", xlab = "BMI", ylab = "Fat Free Weight")


plot(Fat$BodyFatSiri, Fat$FatFreeWeight, main = "Body Fat(Siri) and Fat Free Weight", xlab = "Body Fat", ylab= "Fat Free Weight")

plot(Fat$BodyFatSiri, Fat$Height.in, main = "Body Fat(Siri) and Height", xlab = "Body Fat", ylab= "Height")

plot(Fat$BodyFatSiri, Fat$Weight.lbs, main = "Body Fat(Siri) and Weight", xlab = "Body Fat", ylab= "Weight") 

plot(Fat$BodyFatSiri, Fat$Density, main = "Body Fat(Siri) and Density", xlab = "Body Fat", ylab= "Density")



attach(Fat)

Fat$HealthGroup[BMI < 18.5] <- 1

Fat$HealthGroup[BMI >= 18.5 & BMI <25] <- 2

Fat$HealthGroup[BMI >= 25 & BMI < 30] <- 3

Fat$HealthGroup[BMI >= 30 ] <- 4

detach(Fat)

WeightType <- c("Underweight", "Healthy", "Overweight", "Obese")

Fat$HealthGroup <-factor(Fat$HealthGroup, labels = WeightType)


attach(Fat)

Fat$AgeRange[Age > 18] <- 1

Fat$AgeRange[Age >= 25 & BMI <40] <- 2

Fat$AgeRange[Age >= 40 & BMI < 60] <- 3

Fat$AgeRange[Age >= 60 ] <- 4

detach(Fat)

AgeRange1 <- c("18-25", "25-40", "40-60", "Above 60")

Fat$AgeRange <- factor(Fat$AgeRange, labels = AgeRange1)


ggplot(Fat, aes(AgeRange, BMI)) + geom_point(aes(color=HealthGroup))


ggplot(Fat, aes(AgeRange, BodyFatSiri)) + geom_point(aes(color=HealthGroup))
