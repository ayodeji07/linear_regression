# Importing dataset


library(readr)
copd <- read_csv("COPD_Student_Dataset.csv")
View(copd)


# GETTING TO KNOW THE DATASET

# checking the dimension
dim(copd)


# checking first few rows
head(copd)


# checking the variable type of AGE column
class(copd$AGE)

# Summary statistics of AGE column
summary(copd$AGE)


# checking duistribution of AGE column
hist(copd$AGE)


# checking the variable type of CAT column
class(copd$CAT)

# Summary statistics of CAT column
summary(copd$CAT)


# checking duistribution of CAT column
hist(copd$CAT)

# copd$CAT[copd$CAT > 40] <- NA

# checking the variable type of COPDSEVERITY column
class(copd$COPDSEVERITY)


# change the variable type of COPDSEVERITY to factor
copd$COPDSEVERITY <- as.factor(copd$COPDSEVERITY)
class(copd$COPDSEVERITY)

# check the distribution of entries in COPDSEVERITY column
table(copd$COPDSEVERITY, exclude = NULL)

# check the variable type of gender column
class(copd$gender)


# change the variable type of gender to factor
copd$gender <- as.factor(copd$gender)
class(copd$gender)


# check the distribution of entries in gender column
table(copd$gender)


# Inspecting the dataset for missing values and outliers

#installed.packages()
#library(dplyr)
#library(janitor)

install.packages('Hmisc')
library(Hmisc)

describe(copd)

# summary statistics and tabulations of the dataset

install.packages('gmodels')
library(gmodels)

CrossTable(copd$copd) # categorical variables

sum(is.na(copd$copd))

summary(copd$MWT1Best) # continous variables

hist(copd$AGE) # to check for outliers in continous variables


# Examine relationship between predictor variables

# Pairwise correlations and scatter plot matrices for continous variables
# cross tabulations for categorical variables

# created a new vector including the variables to be analysed
my_data <- copd[,c("AGE", "PackHistory", "FEV1", "FEV1PRED", "FVC", "CAT", "HAD", "SGRQ")]
cor_matrix <- cor(my_data) # create a correlation matrix
cor_matrix # view the correlation matrix

# correlation plot
pairs(~AGE+PackHistory+FEV1+FEV1PRED+FVC+CAT+HAD+SGRQ, data=copd)


#crosstable for categorical variables

CrossTable(copd$hypertension, copd$IHD)


# Fit a simple linear regression model


multi_var <- lm(MWT1Best~FEV1+AGE+gender+COPDSEVERITY+Diabetes, data = copd)

summary(multi_var)

confint(multi_var)


# Interaction between two binary predictor variables

copd$Diabetes <- c(0,1)[as.integer(copd$Diabetes)]
copd$AtrialFib <- c(0,1)[as.integer(copd$AtrialFib)]

r <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(Diabetes*AtrialFib), data = copd)
summary(r)

confint(r)


install.packages('prediction')
library(prediction)

list("Diabetes" = prediction(r, at = list(Diabetes = c(0,1))),
     "AtrialFib" = prediction(r, at = list(AtrialFib = c(0,1))),
     "Diabetes*AtrialFib" = prediction(r, at = list(Diabetes = c(0,1), AtrialFib = c(0,1))))
