# Linear-Logistic-Regression-and-Survival-Analysis

Analysis of the Addicts and Bladder cancer data sets, initially with an exploratory data analysis (EDA) then with linear and logistic regression and then with Survival Analysis

## Description of the data sets

### Addicts Data Set

A data.frame with 238 rows and the following variables:

id - ID of subject
clinic - Clinic (1 or 2)
status - status (0=censored, 1=endpoint)
survt - survival time (days)
prison - prison record?
dose - methodone dose (mg/day)

### Baldder Cancer Data set

A data.frame with 191 rows and the following variables:

id
event
interval
inttime
start
stop
tx
num
size

## Analysis Performed

The Analysis was performed in;

1. R (https://github.com/samalkani/Linear-Logistic-Regression-and-Survival-Analysis/blob/main/Linear%2C%20Logistic%20regression%20and%20Survival%20Analysis.R),
2. R Markdown (https://github.com/samalkani/Linear-Logistic-Regression-and-Survival-Analysis/blob/main/Linear%2C%20Logistic%20regression%20and%20Survival%20Analysis%20of%20the%20Addicts%20and%20Bladder%20data%20sets.Rmd),
3. and a downloadable R Markdown HTML document (https://github.com/samalkani/Linear-Logistic-Regression-and-Survival-Analysis/blob/main/Linear%2C-Logistic-regression-and-Survival-Analysis-of-the-Addicts-and-Bladder-data-sets.html).

## Details of Analysis

1. __EDA - Univariate Analysis__ - Descriptive Statistics
2. __EDA - Bivariate Analysis__ - Scatterplot Matrix
3. __Multiple Linear Regression Model__
4. __Logistic Regression Model__ - Prediction of a single observation and whole dataset, Confusion Matrix, Accuracy metrics, Model performance evaluation, Best Cut-off Values, ROC curve, AUROC curve, Optimal Logistic regression model based on best cut-off value.
5. __Survival Analysis__ - Creating survival object, estimating survival function, Median survival time of the data, 1 year estimated median survival time.
6. __Non - Parametric Analysis__ - KM survival curves, detecting significant difference for the group variables clinic and prison.
7. __Semi - Parametric Analysis__ - Assessing the proportional hazards (PH) assumption using the graphical method plot of log-log survival vs log time in days, plot of log-log survival vs time in days.
8. __Semi - Parametric Analysis - Cox PH model__ - Handling ties in the same time interval using Efron, Breslow and exact methods, Comparing the reduced model (without interactions) to the full model (with interactions).
9. __Semi - Parametric Analysis - Stratified Cox PH model__ - using the clinic variable, with interaction term, Estimating Hazard ratio for model
10. __Semi - Parametric Analysis - Testing PH assumption with statistical test__ - Schoenfeld residuals, Comparing unadjusted Cox model with adjusted Cox model and adjusted stratified Cox model with clinic as strata, plotting log-log survival vs survival time for adjusted Stratified Cox model.
11. __Semi - Parametric Analysis - Extended Cox Model__ - dataset transformed into counting process, extended Cox model + predictors, extended Cox model with cut-points of 365 days, Extended Cox model + heaviside functions, handling ties for the model
12. __Parametric Models__ - Log-log survival vs time, Accelerated Failure time models (AFT's) - Exponential AFT model, Weibull AFT model, Log-logistic AFT model, using predict function to estimate median survival time + other quantiles, Kaplan Meier (KM) estimates.
13. __Semi - Parametric Analysis - Frailty Models__ - Stratified Cox model with and without frailty
14. __Semi - Parametric Analysis - Modelling Recurrent Events__ - (using bladder data) - Cox PH model with predictors, Cox PH stratified counting process (CP) model, Cox PH stratified counting process (CP) model + interactions, Gap Time Approach

