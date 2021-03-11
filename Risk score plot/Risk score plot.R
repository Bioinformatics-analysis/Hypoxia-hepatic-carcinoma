library(pheatmap)           
#Train risk score plot
Train=read.table("Trainrisk.txt",sep="\t",header=T,row.names=1,check.names=F)      
Train=Train[order(Train$riskScore),]                                  
riskClass=Train[,"risk"]
lowLength=length(riskClass[riskClass=="low"])
highLength=length(riskClass[riskClass=="high"])
line=Train[,"riskScore"]
pdf(file="Train_riskScore.pdf",width = 10,height = 4)
plot(line,
     type="p",
     pch=20,
     xlab="Patients (increasing risk socre)",
     ylab="Risk score",
     col=c(rep("green",lowLength),
     rep("red",highLength)))
abline(h=median(Train$riskScore),v=lowLength,lty=2)
legend("topleft", c("High risk", "low Risk"),bty="n",pch=19,col=c("red","green"),cex=1.2)
dev.off()


color=as.vector(Train$fustat)
color[color==1]="red"
color[color==0]="green"
pdf(file="Train_survStat.pdf",width = 10,height = 4)
plot(Train$futime,
     pch=19,
     xlab="Patients (increasing risk socre)",
     ylab="Survival time (years)",
     col=color)
legend("topleft", c("Dead", "Alive"),bty="n",pch=19,col=c("red","green"),cex=1.2)
abline(v=lowLength,lty=2)
dev.off()

Train1=Train[c(3:(ncol(Train)-2))]
Train1=log2(t(Train1)+0.001)
annotation=data.frame(type=Train[,ncol(Train)])
rownames(annotation)=rownames(Train)
pdf(file="Train_heatmap.pdf",width = 10,height = 4)
pheatmap(Train1, 
         annotation=annotation, 
         cluster_cols = FALSE,
         fontsize_row=11,
         show_colnames = F,
         fontsize_col=3,
         color = colorRampPalette(c("green", "black", "red"))(50) )
dev.off()
#Test risk score plot          
Test=read.table("Testrisk.txt",sep="\t",header=T,row.names=1,check.names=F)      
Test=Test[order(Test$riskScore),]                                  
riskClass=Test[,"risk"]
lowLength=length(riskClass[riskClass=="low"])
highLength=length(riskClass[riskClass=="high"])
line=Test[,"riskScore"]
pdf(file="Test_riskScore.pdf",width = 10,height = 4)
plot(line,
     type="p",
     pch=20,
     xlab="Patients (increasing risk socre)",
     ylab="Risk score",
     col=c(rep("green",lowLength),
     rep("red",highLength)))
abline(h=median(Test$riskScore),v=lowLength,lty=2)
legend("topleft", c("High risk", "low Risk"),bty="n",pch=19,col=c("red","green"),cex=1.2)
dev.off()


color=as.vector(Test$fustat)
color[color==1]="red"
color[color==0]="green"
pdf(file="Test_survStat.pdf",width = 10,height = 4)
plot(Test$futime,
     pch=19,
     xlab="Patients (increasing risk socre)",
     ylab="Survival time (years)",
     col=color)
legend("topleft", c("Dead", "Alive"),bty="n",pch=19,col=c("red","green"),cex=1.2)
abline(v=lowLength,lty=2)
dev.off()

Test1=Test[c(3:(ncol(Test)-2))]
Test1=log2(t(Test1)+0.001)
annotation=data.frame(type=Test[,ncol(Test)])
rownames(annotation)=rownames(Test)
pdf(file="Test_heatmap.pdf",width = 10,height = 4)
pheatmap(Test1, 
         annotation=annotation, 
         cluster_cols = FALSE,
         fontsize_row=11,
         show_colnames = F,
         fontsize_col=3,
         color = colorRampPalette(c("green", "black", "red"))(50) )
dev.off()