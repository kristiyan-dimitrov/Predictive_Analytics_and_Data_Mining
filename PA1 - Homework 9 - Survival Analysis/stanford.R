library(survival)
head(jasa)
tdata=read.csv("c:/data/tdata.csv")
tdata[1:10,]
sdata=read.csv("c:/data/sdata.csv")
sdata[1:10,]
trtfit1<- survfit(Surv(survtime,death)~trt,data=tdata)
summary(trtfit1)
plot(trtfit1,col=1:2,xlab="Days", ylab="Proportion Surviving")
legend("topright", paste(" ",c("No Transplant","Transplant")), col=1:2,lty=c(1 ,2))
trtfit2<- coxph(Surv(survtime,death)~trt,data=tdata)
summary(trtfit2)

tfit<- coxph(Surv(survtime,death)~trt+year+age+surgery,data=tdata)
tfit
sfit<-coxph(Surv(tstart, tstop, death) ~ trt+year+age+surgery,data=sdata)
sfit

