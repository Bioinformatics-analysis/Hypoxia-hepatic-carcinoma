library(survival)
library(randomForestSRC)
library(survival)
pFilter=0.05                                                     
rt=read.table("expTime.txt",header=T,sep="\t",check.names=F,row.names=1) 
outTab=data.frame()
sigGenes=c("futime","fustat")
for(i in colnames(rt[,3:ncol(rt)])){
 cox <- coxph(Surv(futime, fustat) ~ rt[,i], data = rt)
 coxSummary = summary(cox)
 coxP=coxSummary$coefficients[,"Pr(>|z|)"]
 if(coxP<pFilter){
     sigGenes=c(sigGenes,i)
		 outTab=rbind(outTab,
		              cbind(id=i,
		              HR=coxSummary$conf.int[,"exp(coef)"],
		              HR.95L=coxSummary$conf.int[,"lower .95"],
		              HR.95H=coxSummary$conf.int[,"upper .95"],
		              pvalue=coxSummary$coefficients[,"Pr(>|z|)"])
		              )
  }
}
write.table(outTab,file="uniCox.txt",sep="\t",row.names=F,quote=F)
uniSigExp=rt[,sigGenes]
uniSigExp=cbind(id=row.names(uniSigExp),uniSigExp)
write.table(uniSigExp,file="uniSigExp.txt",sep="\t",row.names=F,quote=F)
dt.rf<-read.table("uniSigExp.txt",sep='\t',check.names=F,row.names=1)
ntree <- 1000
surv.rf <- randomSurvivalForest::rsf(Surv(futime, fustat) ~ ., 
                                     data = dt.rf, 
                                     ntree = ntree,
                                     seed = 12345678)
err.rate <- surv.rf$err.rate
pdf("error rate.pdf",width = 5,height = 5)
par(bty = "o", mgp = c(1.5,.33,0), mar = c(3,4,1,2),las = 1, tcl = -.25)
plot(1:ntree,err.rate,
     xlab = "Number of Trees",
     ylab = "",
     type = "l",
     las = 1,
     cex = 1.5)
mtext("Error Rate",side = 2,line = 2.5,las = 3)
invisible(dev.off())

raw.imp <- surv.rf$importance; names(raw.imp) <- gsub("_","-",names(raw.imp))
rel.imp <- raw.imp/max(abs(raw.imp)) # calculate relative importance

imp.res <- data.frame(gene = names(raw.imp),
                      raw.importance = raw.imp,
                      rel.importance = rel.imp,
                      stringsAsFactors = F)
write.csv(imp.res[order(imp.res$rel.importance,decreasing = T),],"importance result.csv",row.names = F,quote = F)

imp.cutoff <- 0.4 
rel.imp.sel <- rel.imp[rel.imp > imp.cutoff] 
rel.imp.sel <- sort(rel.imp.sel) 
xrange <- range(pretty(range(rel.imp.sel)))
yrange <- c(1,length(rel.imp.sel))

pdf("relative importance.pdf",width = 5,height = 5)
par(bty = "o", mgp = c(1.5,.33,0), mar = c(3,6,1,2),las = 1, tcl = -.25)
plot(NULL,NULL,
     xlim = xrange,
     ylim = yrange,
     xlab = "Variable Relative Importance",
     ylab = "",
     yaxt = "n",
     las = 1)
axis(side = 2,at = 1:length(rel.imp.sel),names(rel.imp.sel)) 
for (i in 1:length(rel.imp.sel)) { 
  lines(c(xrange[1],rel.imp.sel[i]),
        c(i,i),
        lwd = 2.5,
        col = "steelblue")
}
invisible(dev.off())