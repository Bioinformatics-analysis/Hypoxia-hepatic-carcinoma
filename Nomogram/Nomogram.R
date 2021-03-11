#Nomogram
pbc<-read.table("Nomogram.txt",head=T,sep='\t',check.names = F,row.names = 1)
pbc$futime=pbc$futime/365
pbc$died <- pbc$fustat==1
head(pbc)
library(rms)
dd<-datadist(pbc)
options(datadist="dd")
options(na.action="na.delete")
summary(pbc$futime)
coxpbc<-cph(formula = Surv(futime,died) ~  Stage + riskScore  ,data=pbc,x=T,y=T,surv = T,na.action=na.delete)  #,time.inc =2920

print(coxpbc)
surv<-Survival(coxpbc) 
surv3<-function(x) surv(3,x)
surv1<-function(x) surv(1,x)
surv5<-function(x) surv(5,x)
x<-nomogram(coxpbc,fun = list(surv1,surv3,surv5),lp=T,
            funlabel = c('1-year survival Probability','3-year survival Probability','5-year survival Probability'),
            maxscale = 100,fun.at = c(0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1))

pdf("nomogram_classical.pdf",width = 12,height = 10)
plot(x, lplabel="Linear Predictor",
     xfrac=.35,varname.label=TRUE, varname.label.sep="=", ia.space=.2, 
     tck=NA, tcl=-0.20, lmgp=0.3,
     points.label='Points', total.points.label='Total Points',
     total.sep.page=FALSE, 
     cap.labels=FALSE,cex.var = 1.6,cex.axis = 1.05,lwd=5,
     label.every = 1,col.grid = gray(c(0.8, 0.95)))
dev.off()

#1-year
cox1 <- cph(Surv(futime,fustat) ~ Stage + riskScore ,surv=T,x=T, y=T,time.inc = 1,data=pbc)
cal1<- calibrate(cox1, cmethod="KM", method="boot", u=1, m= 80, B=1000)

pdf("calibrate1.pdf")
plot(cal1,lwd=2,lty=1,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram-Predicted Probability of 1-Year Survival",ylab="Actual 1-Year Survival",col="blue",sub=F)
mtext("")
box(lwd = 0.5)
abline(0,1,lty = 3,lwd = 2,col = "black")
dev.off()


#3-year
cox3 <- cph(Surv(futime,fustat) ~ Stage + riskScore ,surv=T,x=T, y=T,time.inc = 3,data=pbc)
cal3 <- calibrate(cox3, cmethod="KM", method="boot", u=3, m= 80, B=1000)

pdf("calibrate3.pdf")
plot(cal3,lwd=2,lty=1,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram-Predicted Probability of 3-Year Survival",ylab="Actual 3-Year Survival",col="blue",sub=F)
mtext("")
box(lwd = 0.5)
abline(0,1,lty = 3,lwd = 2,col = "black")
dev.off()      

#5-year
cox5 <- cph(Surv(futime,fustat) ~ Stage + riskScore ,surv=T,x=T, y=T,time.inc = 5,data=pbc)
cal5 <- calibrate(cox5, cmethod="KM", method="boot", u=5, m= 80, B=1000)

pdf("calibrate5.pdf")
plot(cal5,lwd=2,lty=1,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram-Predicted Probability of 5-Year Survival",ylab="Actual 5-Year Survival",col="blue",sub=F)
mtext("")
box(lwd = 0.5)
abline(0,1,lty = 3,lwd = 2,col = "black")
dev.off() 
#ROC analysis
library(survival)                                     
rt=read.table("Nomogram.txt",header=T,sep="\t",check.names=F,row.names=1)  
rt$futime=rt$futime/365
multiCox=coxph(Surv(futime, fustat) ~ ., data = rt)
multiCoxSum=summary(multiCox)
outTab=data.frame()
outTab=cbind(
             coef=multiCoxSum$coefficients[,"coef"],
             HR=multiCoxSum$conf.int[,"exp(coef)"],
             HR.95L=multiCoxSum$conf.int[,"lower .95"],
             HR.95H=multiCoxSum$conf.int[,"upper .95"],
             pvalue=multiCoxSum$coefficients[,"Pr(>|z|)"])
outTab=cbind(id=row.names(outTab),outTab)
outTab=gsub("`","",outTab)
Nomogram_riskScore=predict(multiCox,type="risk",newdata=rt)
coxGene=rownames(multiCoxSum$coefficients)
coxGene=gsub("`","",coxGene)
outCol=c("futime","fustat",coxGene)
risk=as.vector(ifelse(Nomogram_riskScore>median(Nomogram_riskScore),"high","low"))
write.table(cbind(id=rownames(cbind(rt[,outCol],Nomogram_riskScore,risk)),cbind(rt[,outCol],Nomogram_riskScore,risk)),
    file="Nmogram_risk.txt",
    sep="\t",
    quote=F,
    row.names=F)

library(survivalROC)
library(RColorBrewer)
Nomogram_risk<-read.table("Nmogram_risk.txt",head=T,sep='\t',check.names=F,row.names=1)
rocCol=brewer.pal("Dark2",n=3)
aucText=c()
pdf(file="Nomogram_risk_ROC.pdf",width=6,height=6)
par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=survivalROC(Stime=Nomogram_risk$futime, status=Nomogram_risk$fustat, marker = Nomogram_risk$Nomogram_riskScore, predict.time =5, method="KM")
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col=rocCol[1], 
  xlab="False positive rate", ylab="True positive rate",
  lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
aucText=c(aucText,paste0("five year"," (AUC=",sprintf("%.3f",roc$AUC),")"))
abline(0,1)

roc=survivalROC(Stime=Nomogram_risk$futime, status=Nomogram_risk$fustat, marker = Nomogram_risk$Nomogram_riskScore, predict.time =3, method="KM")
aucText=c(aucText,paste0("three year"," (AUC=",sprintf("%.3f",roc$AUC),")"))
lines(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col=rocCol[2],lwd = 2)


roc=survivalROC(Stime=Nomogram_risk$futime, status=Nomogram_risk$fustat, marker = Nomogram_risk$Nomogram_riskScore, predict.time =1, method="KM")
aucText=c(aucText,paste0("one year"," (AUC=",sprintf("%.3f",roc$AUC),")"))
lines(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col=rocCol[3],lwd = 2)

legend("bottomright", aucText,lwd=2,bty="n",col=rocCol)
dev.off()