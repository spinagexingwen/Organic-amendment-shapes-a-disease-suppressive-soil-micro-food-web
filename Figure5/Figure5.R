
setwd(readClipboard())
getwd()
library(corrplot)
library(psych)
library(readxl)
library(pdftools)

x<- read.csv("HN.csv", row.names=1,header=T)
y<- read.csv("coretaxa.csv", row.names=1 ,header=T)
x
y
corr_matrix<- corr.test(x[,], y[,], method = "pearson",adjust = "bonferroni")
corr_matrix$r
corr_matrix$p

write.csv(corr_matrix$r,"corr_matrix$r.csv")
write.csv(corr_matrix$p,"corr_matrix$p.csv")

col1 <- colorRampPalette(c("#EF3B2C","#e6cfe6","#4292C6" ))
corrplot(corr_matrix$r, p.mat = corr_matrix$p, insig = 'label_sig', sig.level = c(.001, .01, .05),
         pch.cex = 1,pch.col = "grey26",
         method = 'circle',outline=FALSE, addCoef.col = NULL, col = col1(5),
         number.cex = 2, number.font=1, tl.cex =1,tl.col="black", tl.srt = 60, cl.cex  = 1)# 导出为20 20

##pdf export: A4 size##