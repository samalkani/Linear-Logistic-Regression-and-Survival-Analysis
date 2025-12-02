# Install packages
install.packages("psych")

# Load Libraries
library(psych)

# Load Data
read.csv("addicts.csv")

# Check the addicts dataset

addicts[1:5,]
addicts[1:5,1:6]

str(addicts)

# Summary of survival time data (univariate analysis)

summary(addicts[c("clinic","status","survt","prison","dose")])
summary(addicts$survt)
hist(addicts$survt)
summary(addicts$clinic)
hist(addicts$clinic)
summary(addicts$prison)
hist(addicts$prison)
table(addicts$clinic)

# Enhanced Scatterplot matrix (bivarate analysis)

library(psych)
pairs.panels(addicts[c("clinic","status","survt","prison","dose")])

# Multiple linear regression model

linearregr_model <- lm(status ~ clinic + survt + prison + dose, data = addicts)

linearregr_model

# Detailed output

summary(linearregr_model)

# Logistic regression model

logistic_model <- glm(formula=status ~ clinic + survt + prison + dose, data = addicts, family ="binomial")

logistic_model

# Detailed output

summary(logistic_model)

# Prediction for logistic regression model

newdata <-data.frame(clinic=2, survt=402.6, prison=0, dose=60.4)
predict(logistic_model,newdata, type = "response")






