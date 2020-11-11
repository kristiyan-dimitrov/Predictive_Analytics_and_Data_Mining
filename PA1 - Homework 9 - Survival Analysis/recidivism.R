# Recidivism Example
library(survival)
recid=read.csv("recid.csv")
summary(recid)
names(recid)
# Week: Week of first arrest after release or censoring time.
# Arrest: The event indicator(1 if arrested, 0 if not arrested).
# Aid: Financial aid (yes = 1 or no = 0) 
# Age: In years at the time of release.
# Race: Black = 1 or other = 0.
# Work: Yes = 1 if the individual had full-time work experience prior to incarceration, no = 0 if he did not.
# Married: Married = 1 or not married = 0.
# Parole: Yes = 1 if the individual was released on parole and no = 0 if he was not.
# Prior: number of prior convictions.
# Education: A categorical variable with codes 2 (grade 6 or less), 3 (grades 6 through 9), 4 (grades 10 and 11), 5 (grade 12), or 6 (some post-secondary).
head(recid)

# Plotting KM Curve
fit0 <- survfit(Surv(Week, Arrest) ~ Aid, data=recid)
summary(fit0)
plot(fit0, col=1:2, xlab="Weeks",ylab="Proportion Not Arrested")
legend("bottomleft", paste("Aid=",c("No","Yes")), col=1:2, lty=c(1 ,2),inset=0.02)

# Fitting CoxPH Model just on Aid
fit1 <- coxph(Surv(Week, Arrest) ~ Aid, data=recid)
summary(fit1) # Almost significant at 5%

# Fitting fukk CoxPH Model with all variables
fit2 <- coxph(Surv(Week, Arrest) ~ Aid + Age + Race + Work + Married + Parole + Prior + Education, data=recid)
summary(fit2)

# Keeping only the significant variables and refitting CoxPH Model
fit3 <- coxph(Surv(Week, Arrest) ~ Aid + Age + Prior, data=recid)
summary(fit3)



plot(survfit(fit2), ylim=c(0.7, 1), xlab="Weeks", ylab="Proportion Not Rearrested")
recid.fin <- with(recid, data.frame(Aid=c(0, 1), Age=rep(mean(Age), 2), Race=rep(mean(Race == "other"), 2), Work=rep(mean(Work == "yes"), 2), Married=rep(mean(Married == "not married"), 2), Parole=rep(mean(Parole == "yes"), 2), Prior=rep(mean(Prior), 2)))

plot(survfit(fit3, newdata=recid.fin), conf.int=TRUE, lty=c(1, 2), ylim=c(0.6, 1), xlab="Weeks", ylab="Proportion Not Rearrested")
legend("bottomleft", legend=c("Aid = no", "Aid = yes"), lty=c(1 ,2), inset=0.02)

