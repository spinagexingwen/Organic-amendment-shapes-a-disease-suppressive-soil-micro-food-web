library(ggsci)
library(ggplot2)
library(gridExtra)
library(grid)
library(Rmisc)
library(multcompView)
library(lsmeans)
library(agricolae)
library(scales)
library(readxl)
library(ggpubr)

setwd(readClipboard())
getwd()

######p1-Bacteria######
fit<- read_excel("Figure2A.xlsx",1)
head(fit)
######T test######
Result1<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result1
fit$Time<-factor(fit$Time,levels=c("May","July"))
p1<- ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),linewidth=1,outlier.shape =1,outlier.size= 1)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab(expression("Bacteria (nmol/g)"))+ 
  theme_bw() + 
  theme(panel.grid =element_blank(),axis.text.x = element_text(hjust = 1))+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"),
        axis.title.y = element_text(size = 16), 
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"))+ 
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(10,110),breaks = c(10,60,110))
p1
dat <- ggplot_build(p1)$data[[1]]
p1a<- p1+geom_segment(data=dat,aes(x=xmin,xend=xmax,y=middle, yend=middle),colour="white", size=0.5)
p1a
ggsave("p1-bacteria.pdf", dpi=1000, width=62,height =65, units="mm")


######p2-fungi######
fit<- read_excel("Figure2A.xlsx",2)
head(fit)
######T test######
Result1<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result1
fit$Time<-factor(fit$Time,levels=c("May","July"))
p2<- ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),linewidth=1,outlier.shape =1,outlier.size= 1)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab(expression("Fungi (nmol/g)"))+ 
  theme_bw() + 
  theme(panel.grid =element_blank(),axis.text.x = element_text(hjust = 1))+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"),
        axis.title.y = element_text(size = 16), 
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"))+ 
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(2,12),breaks = c(2,7,12))
p2
dat <- ggplot_build(p2)$data[[1]]
p2a<- p2+ geom_segment(data=dat,aes(x=xmin, xend=xmax,y=middle, yend=middle),colour="white", size=0.5)
p2a
ggsave("p2-Fungi.pdf", dpi=1000, width=62,height =65, units="mm")

######p3-AMF######
fit<- read_excel("Figure2A.xlsx",3)
head(fit)
######T test######
Result1<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result1
fit$Time<-factor(fit$Time,levels=c("May","July"))
p3<- ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),linewidth=1,outlier.shape =1,outlier.size= 1)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab(expression("AMF (nmol/g)"))+ 
  theme_bw() + 
  theme(panel.grid =element_blank(),axis.text.x = element_text(hjust = 1))+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"),
        axis.title.y = element_text(size = 16), 
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"))+ 
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(20,70),breaks = c(20,45,70))
p3
dat <- ggplot_build(p3)$data[[1]]
p3a<- p3 + geom_segment(data=dat,aes(x=xmin, xend=xmax,y=middle, yend=middle),colour="white", size=0.5)
p3a
ggsave("p3-AMF.pdf", dpi=1000, width=62,height =65, units="mm")

######p4-Protozoa######
fit<- read_excel("Figure2A.xlsx",4)
head(fit)
######T test######
Result1<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result1
fit$Time<-factor(fit$Time,levels=c("May","July"))
p4<- ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),linewidth=1,outlier.shape =1,outlier.size= 1)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab(expression("Protozoa (10^3 ind/g)"))+ 
  theme_bw() + 
  theme(panel.grid =element_blank(),axis.text.x = element_text(hjust = 1))+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"),
        axis.title.y = element_text(size = 16), 
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"))+ 
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(5,35),breaks = c(5,20,35))
p4
dat <- ggplot_build(p4)$data[[1]]
p4a<- p4+ geom_segment(data=dat,aes(x=xmin, xend=xmax,y=middle, yend=middle),colour="white", size=0.5)
p4a
ggsave("p4-Protozoa.pdf", dpi=1000, width=62,height =65, units="mm")

######p5-BN######
fit<- read_excel("Figure2A.xlsx",5)
head(fit)
######T test######
Result1<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result1
fit$Time<-factor(fit$Time,levels=c("May","July"))
p5<- ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),linewidth=1,outlier.shape =1,outlier.size= 1)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab(expression("BN (indi/100 g dry soil)"))+ 
  theme_bw() + 
  theme(panel.grid =element_blank(),axis.text.x = element_text(hjust = 1))+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"),
        axis.title.y = element_text(size = 16), 
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"))+ 
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(100,800),breaks = c(100,450,800))
p5
dat <- ggplot_build(p5)$data[[1]]
p5a<- p5+ geom_segment(data=dat,aes(x=xmin, xend=xmax,y=middle, yend=middle),colour="white", size=0.5)
p5a
ggsave("p5-BN.pdf", dpi=1000, width=62,height =65, units="mm")


######p6-FN######
fit<- read_excel("Figure2A.xlsx",6)
head(fit)
######T test######
Result1<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result1
fit$Time<-factor(fit$Time,levels=c("May","July"))
p6<- ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),linewidth=1,outlier.shape =1,outlier.size= 1)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab(expression("FN (indi/100 g dry soil)"))+ 
  theme_bw() + 
  theme(panel.grid =element_blank(),axis.text.x = element_text(hjust = 1))+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"),
        axis.title.y = element_text(size = 16), 
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"))+ 
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(0,300),breaks = c(0,150,300))
p6
dat <- ggplot_build(p6)$data[[1]]
p6a<- p6+ geom_segment(data=dat,aes(x=xmin, xend=xmax,y=middle, yend=middle),colour="white", size=0.5)
p6a
ggsave("p6-FN.pdf", dpi=1000, width=62,height =65, units="mm")

######p7-OPN######
fit<- read_excel("Figure2A.xlsx",7)
head(fit)
######T test######
Result1<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result1
fit$Time<-factor(fit$Time,levels=c("May","July"))
p7<- ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),linewidth=1,outlier.shape =1,outlier.size= 1)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab(expression("OPN (indi/100 g dry soil)"))+ 
  theme_bw() + 
  theme(panel.grid =element_blank(),axis.text.x = element_text(hjust = 1))+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"),
        axis.title.y = element_text(size = 16), 
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"))+ 
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(0,400),breaks = c(0,200,400))
p7
dat <- ggplot_build(p7)$data[[1]]
p7a<- p7+geom_segment(data=dat,aes(x=xmin, xend=xmax,y=middle, yend=middle),colour="white", size=0.5)
p7a
ggsave("p7-OPN.pdf", dpi=1000, width=62,height =65, units="mm")

######p7-FN######
fit<- read_excel("Figure2A.xlsx",8)
head(fit)
######T test######
Result1<-compare_means(value~Treatment, data=fit, method = "t.test",
                       paired = FALSE, group.by = "Time")
Result1
fit$Time<-factor(fit$Time,levels=c("May","July"))
p8<- ggplot(fit, aes(x=Time, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),linewidth=1,outlier.shape =1,outlier.size= 1)+
  scale_fill_manual(values = c("#D73027","#4575B4",
                               "#D73027","#4575B4"))+
  scale_color_manual(values = c("#D73027","#4575B4",
                                "#D73027","#4575B4"))+
  xlab("") +  ylab(expression("HN (indi/mg root)"))+ 
  theme_bw() + 
  theme(panel.grid =element_blank(),axis.text.x = element_text(hjust = 1))+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"),
        axis.title.y = element_text(size = 16), 
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=14,color = "black"))+ 
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(0,100),breaks = c(0,50,100))
p8
dat <- ggplot_build(p8)$data[[1]]
p8a<- p8+geom_segment(data=dat,aes(x=xmin, xend=xmax,y=middle, yend=middle),colour="white", size=0.5)
p8a
ggsave("p8-HN.pdf", dpi=1000, width=62,height =65, units="mm")
