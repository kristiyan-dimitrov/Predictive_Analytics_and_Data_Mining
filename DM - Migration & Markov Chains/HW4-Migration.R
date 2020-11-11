# MSIA 421: Data Mining
# HW4 - Migration
library(data.table)
library(dplyr)
library(car)
library(glmnet)
library(expm)

# Problem 1
np = fread("~/Documents/Google Drive/Northwestern University/Winter 2020/MSIA 421/Homework/np.csv", sep = " ", na.strings = ".")
nrow(distinct(np, SubscriptionId))  ## 2,064 subs
## a)
np = np %>%
  group_by(SubscriptionId) %>%
  mutate(nextchurn = lead(churn), nextprice = lead(currprice)) %>%
  ungroup()

## b)
np = np %>% mutate(t = factor(t))
table(np$t)

## c)
fit1 = glm(nextchurn ~ t+trial+nextprice+regularity+intensity, family = binomial, data = np)
fit2 = glm(nextchurn ~ t+trial+nextprice+regularity, family = binomial, data = np)
fit3 = glm(nextchurn ~ t+trial+nextprice+intensity, family = binomial, data = np)
summary(fit1) 
summary(fit2) 
summary(fit3)
# Model 1:
## in trial period, more likely to churn; higher price, more likely to churn;
## more regularity, less likely to churn; intensity not significant;
# Model 2:
## same conclusion without adding intensity
# Model 3:
## in trial period, more likely to churn; higher price, more likely to churn;
## without regularity, intensity becomes significant, more intensity, less likely to churn;

round(cor(na.omit(np[, c("nextchurn", "trial", "nextprice", "regularity", "intensity")])), 4)
## high correlation between regularity and intensity
vif(fit1); vif(fit2); vif(fit3); ## trial term has large VIF


## d)
fit4 = glm(nextchurn~t+trial+nextprice+sports1+news1+crime1+life1+obits1+business1+opinion1, family = binomial, data = np)
summary(fit4)
## neg sig: sports, news

fit4.2 = glm(nextchurn~t+trial+nextprice+regularity+sports1+news1+crime1+life1+obits1+business1+opinion1, family = binomial, data = np)
summary(fit4.2)
## none of contents significant

## e)
setnames(np, old = c("Loc2", "Loc3", "Loc4"), new = c("loc2", "loc3", "loc4"))
fit5 = glm(nextchurn~t+trial+nextprice+loc1+loc2+loc3+loc4, family = binomial, data = np)
summary(fit5) ## loc1 neg sig
fit6 = glm(nextchurn~t+trial+nextprice+regularity+loc1+loc2+loc3+loc4, family = binomial, data = np)
summary(fit6)  ## none location sig

## f)
fit7 = glm(nextchurn~., family = binomial, data = np %>% select(nextchurn, t, trial, nextprice, SrcGoogle:SrcGoogleAd))
summary(fit7)
## neg sig:ScrGoogle, ScrGoogleNews

## g)
fit8 = glm(nextchurn~t+trial+nextprice+mobile+tablet+desktop, family = binomial, data = np)
summary(fit8)
## neg sig: tablet, desktop

## h)
fit9 = glm(nextchurn~., family = binomial, data = np %>% select(-c(SubscriptionId, churn, currprice)))
summary(fit9)  ## none of non-controls are sig

### lasso
x.model = model.matrix(nextchurn ~., data = np %>% select(-c(SubscriptionId, churn, currprice)))
set.seed(421)
fit10 = cv.glmnet(x = x.model, y = na.omit(np)$nextchurn, alpha = 1, family = "binomial", standardize = T, nfold = 10)
## trace plot
plot(fit10, xvar = "lambda", main = "Lasso", type = "l", ylab = "Coeff")
lambda = fit10$lambda.min
lambda
coeff = coef(fit10$glmnet.fit)[, which(fit10$lambda == lambda)]
coeff 
## negative:  regularity, intensity, desktop, SrcNewsletter
## positive: trial, nextprice, SrcGoogleAd


## i)
### No association with churn: crime1, life1, obits1, business1, opinion1, mobile, loc2, loc3, loc4, SrcDirect, SrcElm, SrcSocial, SrcBingYahooAol, SrcLegacy
### Strong drivers of churn: trial, nextprice
### Strong drivers of retention: regularity, intensity, desktop
### Questionable drivers of churn: SrcGoogleAd
### Questionable drivers of retention: tablet, SrcNewsletter


# Problem 3
npmarkov = read.csv("~/Documents/Google Drive/Northwestern University/Winter 2020/MSIA 421/Homework/npmarkov.csv")
colnames(npmarkov) = c(names(npmarkov)[-1], "PV")
## a)
P = prop.table(x = as.matrix(npmarkov[, -ncol(npmarkov)]), margin = 1)
P

## b)
r = c(0, 0, 1, 1, 10, 10, 0, 0)
v = 0.002*npmarkov$PV + r
v

## c)
d = 0.01
n0 = c(5000, 5000, 1000, 1000, 3000, 3000, 4000, 4000)  ## initial n
CE1 = CLVcalculatorSF(t = Inf, P = P, v = v, n0 = n0, d = d)$CE
CE1  ## 10902522

## d)
v2 = 0.001*npmarkov$PV + r
CE2 = CLVcalculatorSF(t = Inf, P = P, v = v2, n0 = n0, d = d)$CE
CE2  ## 10730531
CE2-CE1  ## -171991.8; decreases

## e)
CE3 = CLVcalculatorSF(t = 36, P = P, v = v2, n0 = n0, d = d)$summary$CE
CE3  ## 3810786; Note that 3745127 is also right, in this case, students are not adding CE(t=0) = t(n0) %*% v

## f)
a = c(100, 100, 0, 0, 0, 0, 0, 0)
temp = CLVcalculatorSF(t = 36, P = P, v = v2, n0 = n0, d = d, a = a)
CE4 = temp$summary$CE
CE4  ## 4382980; Note that 4317321 is also right;
totalsubs4 = temp$summary$totalSubs
floor(totalsubs4)  ## 14708
CE4-CE3 ## 572193.9

## g)
PP = P
for(i in 1:2){
  for(j in 1:2)
    PP[i, j] = P[i, j]-0.01
}
for(i in 3:6){
  for(j in 7:8)
    PP[i, j] = P[i, j]-0.01
}
for(i in 1:2){
  for(j in 3:4)
    PP[i, j] = P[i, j]+0.01
}
for(i in 3:6){
  for(j in 5:6)
    PP[i, j] = P[i, j]+0.01
}


temp = CLVcalculatorSF(t = 36, P = PP, v = v2, n0 = n0, d = d, a = a)
CE5 = temp$summary$CE
CE5  ## 5369459; Note that 5303800 is also right
totalsubs5 = temp$summary$totalSubs
floor(totalsubs5)  ## 19949
CE5-CE4 ## 986479.1


############################################################################################
# a function to compute the expected profit, CLV and CE at time t = k; also, the long term CLV and CE
CLVcalculatorSF = function(t = Inf, P, v, n0, d, a = rep(0, times = nrow(P))){
  ## k = k step transition
  ## data = model data, each row is a subscriber with state in period t
  ## P = transition matrix
  ## v = vector value
  ## n = initial # of subs
  ## d = discount rate
  ## a = # of acquired people
  I = diag(nrow(P))
  
  # compute expected profit, CLV and CE
  if(t == Inf){   ## if in infinity
    CLV = solve(I-P/(1+d)) %*% v  ## CLV
    CE = t(n0) %*% solve(I-P/(1+d)) %*% v  ## CE
    result = rbind(n0, t(CLV))
    rownames(result) = c("n0", "CLV")
    return(list(summary = result, CE = as.numeric(CE)))
  }else{  ## in finite time
    nt = t(n0)
    countTable = matrix(nrow = t, ncol = ncol(P)+1)
    CE = c()
    for(i in 1:t){  ## better to do it recursively
      # nt = t(n0) %*% (P%^%i) + t(a) %*% (I - (P%^%i)) %*% solve(I - P, tol = 1e-20)  ## might not be correct as conditional number is too small
      nt = nt %*% P + t(a)
      countTable[i, ] = c(t = i, nt)
      CE[i] = nt %*% v/((1+d)^i)
    }
    
    colnames(countTable) = c("t", colnames(P))
    CE = as.numeric(sum(CE) + t(n0) %*% v)
    result = data.frame(T = t, ProsSubs = sum(countTable[t, 2:3]), TrialSubs = sum(countTable[t, 4:5]), FullSubs = sum(countTable[t, 6:7]), ChurnSubs = sum(countTable[t, 8:9])) %>%
      mutate(totalSubs = TrialSubs + FullSubs,
             CE = CE)
    CLV = (I-(P/(1+d))%^%t) %*% solve(I-P/(1+d)) %*% v ## CLV(T)
    return(list(summary = result, migrationTable = countTable, CLV = as.numeric(CLV)))
  }
}

