library(survival)
library("survminer")
#Train survival curve and ROC analysis
Train=read.table("Trainrisk.txt",header=T,sep="\t")
diff=survdiff(Surv(futime, fustat) ~risk,data = Train)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)
pValue=format(pValue, scientific = TRUE)

fit <- survfit(Surv(futime, fustat) ~ risk, data = Train)

pdf(file="Train_survival.pdf",onefile = FALSE,
       width = 6,            
       height =5)             
ggsurvplot(fit, 
           data=Train,
           conf.int=TRUE,
           pval=paste0("p=",pValue),
           pval.size=4,
           risk.table=TRUE,
           legend.labs=c("High risk", "Low risk"),
           legend.title="Risk",
           xlab="Time(years)",
           break.time.by = 1,
           risk.table.title="",
           palette="Set1",
           risk.table.height=.25)
dev.off()

summary(fit)

library(survivalROC)
library(RColorBrewer)

rocCol=brewer.pal("Dark2",n=3)
aucText=c()

pdf(file="Train_ROC.pdf",width=6,height=6)
par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=survivalROC(Stime=Train$futime, status=Train$fustat, marker = Train$riskScore, predict.time =5, method="KM")
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col=rocCol[1], 
  xlab="False positive rate", ylab="True positive rate",
  lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
aucText=c(aucText,paste0("five year"," (AUC=",sprintf("%.3f",roc$AUC),")"))
abline(0,1)

roc=survivalROC(Stime=Train$futime, status=Train$fustat, marker = Train$riskScore, predict.time =3, method="KM")
aucText=c(aucText,paste0("three year"," (AUC=",sprintf("%.3f",roc$AUC),")"))
lines(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col=rocCol[2],lwd = 2)


roc=survivalROC(Stime=Train$futime, status=Train$fustat, marker = Train$riskScore, predict.time =1, method="KM")
aucText=c(aucText,paste0("one year"," (AUC=",sprintf("%.3f",roc$AUC),")"))
lines(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col=rocCol[3],lwd = 2)

legend("bottomright", aucText,lwd=2,bty="n",col=rocCol)
dev.off()

#Test survival curve and ROC analysis
Test=read.table("Testrisk.txt",header=T,sep="\t")
diff=survdiff(Surv(futime, fustat) ~risk,data = Test)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)
pValue=format(pValue, scientific = TRUE)

fit <- survfit(Surv(futime, fustat) ~ risk, data = Test)

pdf(file="Test_survival.pdf",onefile = FALSE,
       width = 6,             
       height =5)             
ggsurvplot(fit, 
           data=Test,
           conf.int=TRUE,
           pval=paste0("p=",pValue),
           pval.size=4,
           risk.table=TRUE,
           legend.labs=c("High risk", "Low risk"),
           legend.title="Risk",
           xlab="Time(years)",
           break.time.by = 1,
           risk.table.title="",
           palette="Set1",
           risk.table.height=.25)
dev.off()

summary(fit)

library(survivalROC)
library(RColorBrewer)

rocCol=brewer.pal("Dark2",n=3)
aucText=c()

pdf(file="Test_ROC.pdf",width=6,height=6)
par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=survivalROC(Stime=Test$futime, status=Test$fustat, marker = Test$riskScore, predict.time =5, method="KM")
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col=rocCol[1], 
  xlab="False positive rate", ylab="True positive rate",
  lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
aucText=c(aucText,paste0("five year"," (AUC=",sprintf("%.3f",roc$AUC),")"))
abline(0,1)

roc=survivalROC(Stime=Test$futime, status=Test$fustat, marker = Test$riskScore, predict.time =3, method="KM")
aucText=c(aucText,paste0("three year"," (AUC=",sprintf("%.3f",roc$AUC),")"))
lines(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col=rocCol[2],lwd = 2)


roc=survivalROC(Stime=Test$futime, status=Test$fustat, marker = Test$riskScore, predict.time =1, method="KM")
aucText=c(aucText,paste0("one year"," (AUC=",sprintf("%.3f",roc$AUC),")"))
lines(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col=rocCol[3],lwd = 2)

legend("bottomright", aucText,lwd=2,bty="n",col=rocCol)
dev.off()