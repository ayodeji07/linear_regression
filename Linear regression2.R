# Importing dataset


library(readr)
copd <- read_csv("COPD_Student_Dataset.csv")
View(copd)

# Plot a histogram to check distribution of 'MWT1Best' column

hist(copd$MWT1Best, main = "Histogram of MWT1BEST", xlab = "MWT1BEST", ylab = "Frequency", breaks = 12)


# Subset

subset(copd, MWT1Best>650)

subset(copd, MWT1Best>650|MWT1Best<150)


# Plotting a histigram to check distribution of 'FEV1' column

hist(copd$FEV1, main = "Histogram of FEV1", xlab = "FEV1", ylab = "Frequency")


# Taking the descriptive statistics of 'MWT1Best' column

list("Summary" = summary(copd$MWT1Best), "Mean" = mean(copd$MWT1Best, na.rm = TRUE),
     "Standard Deviation" = sd(copd$MWT1Best, na.rm = TRUE), "Range" = range(copd$MWT1Best, na.rm = TRUE),
     "Inter-Quartile Range" = IQR(copd$MWT1Best, na.rm = TRUE))


# Taking the descriptive statistics of 'FEV1' column

list("Summary" = summary(copd$FEV1), "Mean" = mean(copd$FEV1, na.rm = TRUE),
     "Standard Deviation" = sd(copd$FEV1, na.rm = TRUE), "Range" = range(copd$FEV1, na.rm = TRUE),
     "Inter-Quartile Range" = IQR(copd$FEV1, na.rm = TRUE))


# Plotting a scatter plot of 'MWT1Best' and 'FEV1'

plot(copd$FEV1, copd$MWT1Best, xlab = "FEV1", ylab = "MWT1Best")



#Performing Pearson's correlation test of 'FEV1' and 'MWT1Best'

cor.test(copd$FEV1, copd$MWT1Best, use = "complete.obs", method = "pearson")


#Performing Spearman's correlation test of 'FEV1' and 'MWT1Best'

cor.test(copd$FEV1, copd$MWT1Best, method = 'spearman', use = 'complete.obs')



# Plotting a histigram to check distribution of 'AGE' column

hist(copd$AGE, main = "Histogram of Age", xlab = "Age", ylab = "Frequency")


# Taking the descriptive statistics of 'AGE' column

list("Summary" = summary(copd$AGE), "Mean" = mean(copd$AGE, na.rm = TRUE),
     "Standard Deviation" = sd(copd$AGE, na.rm = TRUE), "Range" = range(copd$AGE, na.rm = TRUE),
     "Inter-Quartile Range" = IQR(copd$AGE, na.rm = TRUE))



# Plotting a scatter plot of 'MWT1Best' and 'Age'

plot(copd$AGE, copd$MWT1Best, xlab = "AGE", ylab = "MWT1Best")


#Performing Pearson's correlation test of 'Age' and 'MWT1Best'

cor.test(copd$AGE, copd$MWT1Best, use = "complete.obs", method = "pearson")


#Performing Spearman's correlation test of 'Age' and 'MWT1Best'

cor.test(copd$AGE, copd$MWT1Best, method = 'spearman', use = 'complete.obs')


# Linear regression model for 'MWT1Best' and 'FEV1'

MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data=copd)

summary(MWT1Best_FEV1)

# Confidence interval
confint(MWT1Best_FEV1)


# Plot
plot(MWT1Best_FEV1)
par(mfrow=c(2,2))


# Linear regression model for 'MWT1Best' and 'Age'

MWT1Best_Age <- lm(MWT1Best~AGE, data=copd)

summary(MWT1Best_Age)

# Confidence interval
confint(MWT1Best_FEV1)


# Plot
plot(MWT1Best_Age)
par(mfrow=c(2,2))

predictedVals <- predict(MWT1Best_Age)
residualVals <- residuals(MWT1Best_Age)
par(mfrow=c(2,2))
plot(MWT1Best_Age)

hist(residualVals)


# Fitting Multiple regression model using lung function (FEV1), Age, walking distance (MWT1Best)

MWT1Best_FEV1_AGE <- lm(MWT1Best~FEV1+AGE, data = copd)

summary(MWT1Best_FEV1_AGE)

confint(MWT1Best_FEV1_AGE)


# Linear regression model for 'MWT1Best' and 'FVC'

MWT1Best_FVC <- lm(MWT1Best~FVC, data=copd)

summary(MWT1Best_FVC)

# Confidence interval
confint(MWT1Best_FVC)


# Plot
par(mfrow=c(1,1))
plot(MWT1Best_FVC)
#par(mfrow=c(2,2))

#predictedVals <- predict(MWT1Best_Age)
#residualVals <- residuals(MWT1Best_Age)
#par(mfrow=c(2,2))
#plot(MWT1Best_Age)


# Fitting Multiple regression model using lung function (FVC), Age, walking distance (MWT1Best)

MWT1Best_FVC_AGE <- lm(MWT1Best~FVC+AGE, data = copd)

summary(MWT1Best_FVC_AGE)

confint(MWT1Best_FVC_AGE)


