str(Biospy1)
str(vetcancer)
summary(vetcancer)
hist(vetcancer$time)


# Enhanced Scatterplot matrix (bivarate analysis)

library(psych)
pairs.panels(vetcancer[c("trt","celltype","time","status","karno","diagtime","age","prior")])

# Multiple linear regression model

linearregr_model <- lm(status ~ trt + celltype_large + celltype_smallcell + celltype_squamous + time + karno + diagtime + age + prior, data = vetcancer)

linearregr_model

# Detailed output

summary(linearregr_model)

# Logistic regression model

logistic_model <- glm(formula=status ~ trt + celltype_large + celltype_smallcell +celltype_squamous + time + karno + diagtime + age + prior, data = vetcancer2, family ="binomial")

logistic_model

# Detailed output

summary(logistic_model)

# Prediction for logistic regression model

newdata <-data.frame(trt=1.496, celltype_large=0.1971, celltype_smallcell=0.3504, celltype_squamous=0.2555, time=121.6, status=0.9343, karno=58.57, diagtime=8.774, age=58.31, prior=2.92)
predict(logistic_model,newdata, type = "response")






