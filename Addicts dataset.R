# 1. Install packages
install.packages("psych")

# 2. Load Libraries
library(psych)

# 3. Load Data
addicts = read.csv("addicts.csv")
head(addicts)
# addicts[1:5,]
# addicts[1:5,1:6]

# 4. Summary of survival time data (univariate analysis)

summary(addicts[c("clinic","status","survt","prison","dose")])
summary(addicts$survt)
hist(addicts$survt)
summary(addicts$clinic)
hist(addicts$clinic)
summary(addicts$prison)
hist(addicts$prison)
table(addicts$clinic)

# 5. Enhanced Scatterplot matrix (bivarate analysis)
pairs.panels(addicts[c("clinic","status","survt","prison","dose")])

# 6. Multiple linear regression model

# 6. A. Creating Multiple linear regression model
linearregr_model <- lm(status ~ clinic + survt + prison + dose, data = addicts)

linearregr_model

# 6. B. Multiple linear regression model - Detailed output
summary(linearregr_model)

# 7. Logistic regression model

# 7. A. Creating Logistic regression model
logistic_model <- glm(formula=status ~ clinic + survt + prison + dose, data = addicts, family ="binomial")

logistic_model

# 7. B. Logistic regression model - Detailed output
summary(logistic_model)

# 7. C. Prediction for logistic regression model
newdata <-data.frame(clinic=2, survt=402.6, prison=0, dose=60.4)
predict(logistic_model,newdata, type = "response")

# 7. D. Predicting the Test set results
prob_pred = predict(logistic_model, type = 'response', newdata = addicts[3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# 7. E. Making the Confusion Matrix
cm = table(addicts[,3], y_pred > 0.5)
cm

# 7. F. Accuracy Measures
accuracy = sum(cm[1], cm[4]) / sum(cm[1:4])
print(paste(c("accuracy:", accuracy)))

precision = cm[4] / sum(cm[4], cm[2])
print(paste(c("precision:", precision)))

sensitivity <- cm[4] / sum(cm[4], cm[3])
print(paste(c("sensitivity:", sensitivity)))

specificity = cm[1] / sum(cm[1], cm[2])
print(paste(c("specificity:", specificity)))

fscore = (2 * (sensitivity * precision))/(sensitivity + precision)
print(paste(c("fscore:", fscore)))

# 4. Creating a survival object
library(survival)
Surv(addicts$survt,addicts$status==1)

# 5. Estimating survival functions (unadjusted) and comparing them across strata

# 5. A. Survival table
summary(survfit(Surv(addicts$survt,addicts$status==1)~1))

# 5. B. Median Survival time in days in the addicts data set
Y=Surv(addicts$survt,addicts$status==1)
Y~1
Y~addicts$clinic
kmfit1=survfit(Y~1)
kmfit1

# 5. C. 1 year Survival rate
summary(kmfit1)
summary(kmfit1,times=365)

# 5. D. Survival rates incremented in 100 day increments
kmfit2=survfit(Y~addicts$clinic)
summary(kmfit2,times=c(0,100,200,300,400,500,600,700,800,900,1000))
summary(kmfit2,times=100*(0:10))

# 5. E. Plot Non-Parametric (KM) survival curves
plot(kmfit2)

# 5. F. Plot Non-Parametric (KM) survival curves in distinguishing colours
plot(kmfit2, lty=c("solid", "dashed"), col=c("black","grey"),
     xlab="survival time in days", ylab="survival probabilities")

# 5. G. Plot Non-Parametric (KM) survival curves in distinguishing colours
plot(kmfit2, lty = c("solid", "dashed"), col=c("red","green"),
     xlab="survival time in days", ylab="survival probabilities")

# 5. H. Detecting significant difference between the two clinic groups
survdiff(Surv(survt,status)~clinic, data=addicts)

# 5. I. Attaching and detaching survival object to the data set addicts
attach(addicts)
survdiff(Surv(survt,status)~clinic)

detach(addicts)

# 5. J. Detecting significant difference between the two clinic groups
survdiff(Surv(survt,status)~clinic,data=addicts,rho = 1)

# 5. K. Detecting significant difference between the two clinic groups with prison as stratification
survdiff(Surv(survt,status)~clinic + strata(prison),data=addicts)

# 6. Assessing the PH assumption using graphical approaches

# 6. A. Plot of Log-log curvers vs log time for the two clinics
plot(kmfit2,fun="cloglog",xlab = "time in days using logarithmic scale",
     ylab = "log-logsurvival",main="log-log curves by clinic")

# 6. B. Construct a data frame of clinic, time and survival probability
kmfit3=summary(kmfit2)
names(kmfit3)
kmfit4=data.frame(kmfit3$strata,kmfit3$time,kmfit3$surv)
names(kmfit4)=c("clinic","time","survival")

kmfit4[1:5,]

# 6. C. Split the dataframe into separate dataframes for the two clinics
clinic1=kmfit4[kmfit4$clinic=="addicts$clinic=1",]
clinic2=kmfit4[kmfit4$clinic=="addicts$clinic=2",]

clinic1[1:5,]
clinic2[1:5,]

# 6. D. Overlay plot log-log survival time vs survival time for the two clinics
plot(clinic1$time,log(-log(clinic1$survival)),
     xlab = "survival time in days",ylab = "log-log survival",
     xlim=c(0,800),col="black",type='l',lty="solid",main="log-log curves by clinic")

par(new=T)
plot(clinic2$time,log(-log(clinic2$survival)),axes=F,xlab = "survival time in days",
     ylab="log-log survival",col="grey50",type='l',lty="dashed")

legend("bottomright",c("Clinic 1","Clinic 2"),
       lty = c("solid","dashed"),col = c("black","grey50"))

# 7. Running a COX PH model

# 7. A. Creating a COX PH model
Y=Surv(addicts$survt,addicts$status==1)

coxph(Y~prison + dose + clinic,data=addicts)

# 7. B.  Detailed COX PH model

summary(coxph(Y~prison+dose+clinic,data=addicts))

# 7. C. Handling ties (i.e. more than one event in a defined time interval) - several methods
coxph(Y~prison + dose + clinic,data=addicts, method="efron")
coxph(Y~prison + dose + clinic,data=addicts, method="breslow")
coxph(Y~prison + dose + clinic,data=addicts, method="exact")

# 7. D. Testing the significance of interactions - mod 1 - the reduced model 
mod1=coxph(Y~prison + dose + clinic,data=addicts)
mod1

# 7. E. Testing the significance of interactions - mod 2 - the full model
mod2=coxph(Y~prison + dose + clinic + clinic*prison + clinic*dose, data=addicts)
mod2

# 7. F. Utilising information from mod 1 and mod 2
names(mod2)

# 7. G. Call up the log-likelihood function

# first term = model with no predictors, second term model with predictors
mod2$loglik

# or
mod2[[3]]

# 7. H. Calculate the difference in -2 Log-likelihood (deviance) of the two models
(-2)*(mod1$loglik[2]-mod2$loglik[2])

# 7. I. Determine the significance of the difference between the models using Chi-square statistic
LRT=(-2)*(mod1$loglik[2]-mod2$loglik[2])
Pvalue = 1 - pchisq(LRT,2)
Pvalue

# 7. K. Determine the significance of the difference between models using a self-defined function.
lrt.surv=function(mod.full,mod.reduced,df){
  lrts=(-2)*(mod.full$loglik[2]-mod.reduced$loglik[2])
  pvalue=1-pchisq(lrts,df)
  return(pvalue)
}

lrt.surv(mod1,mod2, 2)

# 8. Running a stratified Cox model

# 8. A. Creating a stratified Cox model
Y=Surv(addicts$survt,addicts$status==1)
coxph(Y~ prison + dose + strata(clinic),data = addicts)

# 8. B. Including interaction term in a stratified Cox model
coxph(Y~prison + dose + clinic:prison + clinic:dose + strata(clinic),data=addicts)

# 8. C.  Estimating hazard ratio for PRISON=1 vs PRISON=0 for CLINIC=2
addicts$clinic2=addicts$clinic-2
summary(coxph(Y~prison + dose + clinic2:prison +
                clinic2:dose+strata(clinic2),data=addicts))

# 9. Assessing the PH assumption with a statistical test

# 9. A. Creating the model
Y=Surv(addicts$survt,addicts$status==1)
mod1=coxph(Y~prison + dose + clinic,data=addicts)

cox.zph(mod1,transform=rank)

# 9. B. Plot of Schoenfeld residuals against each individual's failure time.
plot(cox.zph(mod1,transform=rank),se=F,var='clinic')

# 9. C. Obtaining Cox-adjusted survival curves - Run Cox unadjusted model
Y=Surv(addicts$survt,addicts$status==1)
mod1=coxph(Y~prison + dose + clinic, data=addicts)

# 9. D. Obtaining Cox-adjusted survival curves - Adjusted Cox model with the adjustments as follows
# PRISON = 0, DOSE=70 AND CLINIC=2
# CREATE DATA FRAME FIRST

pattern1=data.frame(prison=0,dose=70,clinic=2)

pattern1

# 9. E. Use summary function to get the Cox adjusted survival estimates
summary(survfit(mod1,newdata=pattern1))

# 9. F. Plot of the adjusted survival curve
plot(survfit(mod1, newdata=pattern1),
     conf.int=F,main="Adjusted survival for prison=0, dose=70, clinic=2")

# 9. G. Stratified Cox adjusted survival curves - stratified cox model
mod3=coxph(Y~prison + dose + strata(clinic),data=addicts)

# 9. H. stratified cox model controlling for PRISON and DOSE 
# Creation of one observation data frame, with the mean for prison 
# and dose.
pattern2=data.frame(prison=0.46,dose=60.40)

# 9. I. Plotting adjusted survival curve with clinic as the strata
plot(survfit(mod3,newdata=pattern2), conf.int=F, lty=c("solid","dashed"), 
     col=c("red","green"), main="Survival curves for clinic, adjusted for prison and dose")
legend("topright",c("Clinic 1","Clinic 2"),lty=c("solid","dashed"), col=c("red","green"))

# 9. J. Plotting adjusted log-log survival curves for clinic
plot(survfit(mod3,newdata = pattern2),fun="cloglog",
     main="Log-Log curves for clinic, adjusted for prison and dose")

# 9. K. Plotting adjusted log-log survival curves for clinic with the time scale not logged
sum.mod3=summary(survfit(mod3,newdata=pattern2))

 # similar code to section 3

sum.mod4=data.frame(sum.mod3$strata,sum.mod3$time,sum.mod3$surv)
colnames(sum.mod4)=c("clinic","time","survival")
clinic1=sum.mod4[sum.mod4$clinic=="clinic=1",]
clinic2=sum.mod4[sum.mod4$clinic=="clinic=2",]

# 9. L. Plot

plot(clinic1$time,log(-log(clinic1$survival)),xlab="survival time in days",ylab="log-log survival",xlim=c(0,800),col=
       "red",type='l',lty="solid", main="log-log curves stratified by 
     clinic, adjusted for prison, dose")

par(new=T)

plot(clinic2$time,log(-log(clinic2$survival)),axes=F,xlab=
       "survival time in days",ylab="log-log survival",col="green",
     type='l',lty="dashed")

legend("bottomright",c("Clinic 1","Clinic 2"), lty=c("solid","dashed"),col=c("red","green"))

par(new=F)

# 10. - RUNNING AN EXTENDED COX MODEL

# 10. A. Transforming the dataset into counting process (start, stop) format to run an extended Cox model
addicts.cp=survSplit(addicts,cut=addicts$survt[addicts$status==1], 
                     end="survt", event="status",start="start")

nrow(addicts.cp)

# 10. B. Creation of a new variable based on multiplying the variable dose with the log of the survival
 # time. This variable is created because we suspect that the variable dose has failed the proportional
 # hazards assumption.

addicts.cp$logtdose=addicts.cp$dose*log(addicts.cp$survt)

# 10. C. For the new variable DOSE=ln(DOSE)*T, which varies over time we print observation 106 
 # who had an event at time = 35 days for a selected group of variables
addicts.cp[addicts.cp$id==106,c('id','start','survt','status','dose','logtdose')]

# 10. D. Run the extended Cox model with the inclusion of predictors;
 # PRISON, DOSE, and CLINIC, and  time dependent variable LOGTDOSE
coxph(Surv(addicts.cp$start,addicts.cp$survt,addicts.cp$status) ~
        prison + dose + clinic + logtdose + cluster(id),data=addicts.cp)

# 10. E. Running the extended Cox model with a time cutpoint of 365 days
 # Essentially splitting the time of the study into observations 
 # below 365 days and observations above 365 days
addicts.cp365=survSplit(addicts,cut=365,end="survt", event="status",start="start")

# 10. F. Defining the timepoint 365 days and the two time intervals (heaviside functions) above and below 365 days
addicts.cp365$hv1=addicts.cp365$clinic*(addicts.cp365$start<365)
addicts.cp365$hv2=addicts.cp365$clinic*(addicts.cp365$start>=365)

# 10. G. Sort the dataset by variables ID and START
addicts.cp365=addicts.cp365[order(addicts.cp365$id,addicts.cp365$start),]

# 10. H. Printout of the first 10 observations for selected variables
addicts.cp365[1:10,c('id','start', 'survt','status','clinic','hv1','hv2')]

# 10. I. Running an extended Cox model with heaviside functions (time intervals) - 
# Define the object for the response variable
Y365=Surv(addicts.cp365$start,addicts.cp365$survt,addicts.cp365$status)

# 10. J. Run the Cox extended model with the two heaviside functions
coxph(Y365 ~ prison + dose + hv1 + hv2 + cluster(id), data=addicts.cp365)

# 10. K. Handling ties in the two different time intervals (heaviside functions)
coxph(Y365 ~ prison + dose +hv1 + hv2,data=addicts.cp365,method="breslow")

# 10. L. Run Cox extended model with one heaviside function and the variable CLINIC
coxph(Y365 ~ prison + dose + clinic + hv2 + cluster(id),data=addicts.cp365)

# 11. RUNNING PARAMETRIC MODELS

# 11. A. log-log survival vs time in days using log-time plot
plot(survfit(Y~addicts$clinic),fun="cloglog",xlab="time in days using log-
     arithmic scale",ylab="log-log survival", main="log-log curves by clinic")

# 11. B. Exponential AFT model
modpar1=survreg(Surv(addicts$survt,addicts$status) ~ prison + dose +
                  clinic,data=addicts,dist="exponential")

summary(modpar1)

# 11. C. Weibull AFT model
modpar2=survreg(Surv(addicts$survt,addicts$status)
                ~ prison + dose + clinic,data=addicts,dist="weibull")

summary(modpar2)

# 11. D. Use of predict function to estimate then median or any other quantile time to event
 # for any pattern of co-variates
 # Co-variate pattern; PRISON=1, DOSE=50 AND CLINIC=1
pattern1=data.frame(prison=1,dose=50,clinic=1)
pct=c(0.25,0.50,0.75)
days=predict(modpar2,newdata=pattern1,type="quantile",p=pct)
cbind(pct,days) # adds vectors pct and days to create a matrix containing both vectors

# 11. E. Creating a plot for individual with covariate pattern;  PRISON=1, DOSE=50 AND CLINIC=1
pct2=0:100/100
days2=predict(modpar2,newdata=pattern1,type="quantile",p=pct2)
survival=1-pct2

plot(days2,survival,xlab="survival time in days",ylab="survival 
     probabilities",main="Weibull survival estimates for prison=0, 
     dose=50, clinic=1",xlim=c(0,800))

# 11. F. log-logistic AFT model
modpar3=survreg(Surv(addicts$survt,addicts$status)~
                  prison + dose + clinic,data=addicts,dist="loglogistic")

summary(modpar3)

# 11. G. Kaplan Meier (KM) estimates object
kmfit2=survfit(Surv(addicts$survt,addicts$status)~addicts$clinic)

plot(log(kmfit2$time),log(kmfit2$surv/(1-kmfit2$surv)))

# 12 - RUNNING FRAILTY MODELS

# 12. A. Re-run the stratified Cox model without the Frailty (random) component.
Y=Surv(addicts$survt,addicts$status==1)
coxph(Y~ prison + dose + strata(clinic),data=addicts)

# 12. B. Run the stratified Cox model with Frailty (random) component.
coxph(Y~ prison + dose + strata(clinic) + frailty(clinic,distribution="gamma"),data=addicts)

# 12. C. Run the Cox model without the CLINIC variable and without FRAILTY.
coxph(Y~ prison + dose,data=addicts)

# 12. D. Run the Cox model without the CLINIC variable and with FRAILTY
coxph(Y~ prison + dose + frailty(clinic,distribution="gamma"),data=addicts)

# 12. E. Detailed output for the Cox model without CLINIC variable and with FRAILTY
summary(coxph(Y~ prison + dose + frailty(clinic,distribution="gamma"), data=addicts))

# 13. Read Data
bladder1 = read.csv("bladder1.csv")
bladder1[12:20,]
library(survival)

# 14. Modelling Recurrent events

# 14. A. Define the response variable
Y=Surv(bladder1$start,bladder1$stop,bladder1$event==1)

# 14. B. Recurrent events Cox model with predictors;

# Treatment status (tx)
# Initial number of tumours (NUM)
# Initial size of tumours (SIZE)
coxph(Y~tx + num + size + cluster(id),data=bladder1)

# 14. C. More detailed output
summary(coxph(Y~tx + num + size + cluster(id),data=bladder1))

# 14. D. A Cox stratified Counting process (CP) model
coxph(Y~tx + num + size + strata(interval) + cluster(id),data=bladder1)

# 14. E. A cox stratified Counting process (CP) model + interactions
coxph(Y~tx + num + size + tx:interval + num:interval + size:interval + strata(interval) + cluster(id),data=bladder1)

# 14. F. Detailed stratified counting process (CP) model + interactions
summary(coxph(Y~tx + num + size + tx:interval + num:interval + size:interval + strata(interval) + cluster(id),data=bladder1))

# 14. G. Gap Time approach, the starting time for patients at risk is set to zero 
# at the beginning of each time interval
bladder1$start2=0
bladder1$stop2=bladder1$stop-bladder1$start

attach(bladder1)
data.frame(id,event,start,stop,start2,stop2)[12:20,]

# 14. H. Reset the response variable with start2 and stop2 variables
Y2=Surv(bladder1$start2,bladder1$stop2,bladder1$event)

# 14. I. A Cox stratified Gap time model
coxph(Y2 ~ tx + num + size + strata(interval) + cluster(id),data=bladder)


         