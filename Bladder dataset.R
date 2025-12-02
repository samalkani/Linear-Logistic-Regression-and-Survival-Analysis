# Install packages
install.packages("psych")

# Load Libraries
library(psych)

# Read Data
read.csv("bladder.csv")
bladder1[12:20,]
library(survival)

 # 11 - Modelling Recurrent events

 # Define the response variable

Y=Surv(bladder1$start,bladder1$stop,bladder1$event==1)

 # Recurrent events Cox model with predictors;
 # Treatment status (tx)
 # Initial number of tumours (NUM)
 # Initial size of tumours (SIZE)

coxph(Y~tx + num + size + cluster(id),data=bladder1)

 # More detailed output

summary(coxph(Y~tx + num + size + cluster(id),data=bladder1))

 # A Cox stratified Counting process (CP) model

coxph(Y~tx + num + size + strata(interval) + cluster(id),data=bladder1)

 # A cox stratified Counting process (CP) model + interactions

coxph(Y~tx + num + size + tx:interval + num:interval + size:interval + strata(interval) + cluster(id),data=bladder1)

 # Detailed stratified counting process (CP) model + interactions

summary(coxph(Y~tx + num + size + tx:interval + num:interval + size:interval + strata(interval) + cluster(id),data=bladder1))

 # Gap Time approach, the starting time for patients at risk is set to zero 
 # at the beginning of each time interval

bladder1$start2=0
bladder1$stop2=bladder1$stop-bladder1$start

attach(bladder1)
data.frame(id,event,start,stop,start2,stop2)[12:20,]

 # Reset the response variable with start2 and stop2 variables

Y2=Surv(bladder1$start2,bladder1$stop2,bladder1$event)

 # A Cox stratified Gap time model

coxph(Y2 ~ tx + num + size + strata(interval) + cluster(id),data=bladder)




