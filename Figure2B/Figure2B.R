library(ggpubr)
library(reshape2)
library(readxl)

setwd(readClipboard())
getwd()


###p1_bacteria###
data <- read_excel("Richness_violin.xlsx",1)
names(data)
data1=melt(data)
Result<-compare_means(value~type, data=data1, method = "t.test",
                       paired = FALSE, group.by = "variable")
Result
ggviolin(data1, x="variable", y="value", color = "type",
         fill="type",palette =c("#D73027","#4575B4"),
         add = "boxplot",add.params = list(color="white"),
         xlab =NULL, ylab = "Bacterial richness")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size = 9),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(500,3000,1250),breaks = seq(500,3000,1250))
ggsave("p1_bacteria_richness.pdf", dpi=1200, width=67,height =65, units="mm")

####p2_fungi###
data1 <- read_excel("Richness_violin.xlsx",2)
names(data1)
data2=melt(data1)
Result<-compare_means(value~type, data=data2, method = "t.test",
                      paired = FALSE, group.by = "variable")
Result
ggviolin(data2, x="variable", y="value", 
         color = "type",fill="type",palette =c("#D73027","#4575B4"),
         add = "boxplot",add.params = list(color="white"),
         xlab = "", ylab = "Fungal richness")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+#,angle=90
  theme(axis.text.x= element_blank())+
  theme(axis.title.y = element_text(size = 9),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(0,2000,1000),breaks = seq(0,2000,1000))
ggsave("p2_fungi_richenss.pdf", dpi=1200, width=67,height =65, units="mm")

####p3_AMF###
data2<- read_excel("Richness_violin.xlsx",3)
names(data2)
data3=melt(data2)
Result<-compare_means(value~type, data=data3, method = "t.test",
                      paired = FALSE, group.by = "variable")
Result
ggviolin(data3, x="variable", y="value", color = "type",fill="type",trim = F,
         palette =c("#D73027","#4575B4"),add = "boxplot",add.params = list(color="white"),
         xlab = "", ylab = "AMF richness")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(axis.text.x= element_blank())+
  theme(axis.title.y = element_text(size = 9),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+ylim(0,300)+
    scale_y_continuous(expand = c(0,0),limits = c(-50,300,100),breaks = seq(0,300,100))
ggsave("p3_AMF_richness.pdf", dpi=1200, width=65,height =65, units="mm")

####p4_protozoa###
data3 <- read_excel("Richness_violin.xlsx",4)
names(data3)
data4=melt(data3)
Result<-compare_means(value~type, data=data4, method = "t.test",
                      paired = FALSE, group.by = "variable")
Result
ggviolin(data4, x="variable", y="value", color = "type",fill="type",
         palette =c("#D73027","#4575B4"),add = "boxplot",add.params = list(color="white"),
         xlab = "", ylab = "Protozoal richness")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+#,angle=90
  theme(axis.text.x= element_blank())+
  theme(axis.title.y = element_text(size = 9),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(100,400,100),breaks = seq(100,400,100))
ggsave("p4_protozoa_richness.pdf", dpi=1200, width=65,height =65, units="mm")

####p5_BN###
data4 <- read_excel("Richness_violin.xlsx",5)
names(data4)
data5=melt(data4)
Result<-compare_means(value~type, data=data5, method = "t.test",
                      paired = FALSE, group.by = "variable")
Result
ggviolin(data5, x="variable", y="value", color = "type",fill="type",palette =c("#D73027","#4575B4"),
         add = "boxplot",add.params = list(color="white"),
         xlab = "", ylab = "BN richness")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(axis.text.x= element_blank())+
  theme(axis.title.y = element_text(size = 9),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(2,14,6),breaks = seq(2,14,6))
ggsave("p5_BN_richness.pdf", dpi=1200, width=62,height =65, units="mm")

###p6_FN###
data5 <- read_excel("Richness_violin.xlsx",6)
names(data5)
data6=melt(data5)
Result<-compare_means(value~type, data=data6, method = "t.test",
                      paired = FALSE, group.by = "variable")
Result
ggviolin(data6, x="variable", y="value", color = "type",fill="type",palette =c("#D73027","#4575B4"),
         add = "boxplot",add.params = list(color="white"),xlab = "", ylab = "FN richness")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(axis.text.x= element_blank())+
  theme(axis.title.y = element_text(size = 9),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+ylim(0,10)+
  scale_y_continuous(expand = c(0,0),limits = c(-2,9,3),breaks = seq(0,9,3))
ggsave("p6_FN_richenss.pdf", dpi=1200, width=62,height =65, units="mm")

###p7-OPN###
data6 <- read_excel("Richness_violin.xlsx",7)
names(data6)
data7=melt(data6)
Result<-compare_means(value~type, data=data7, method = "t.test",
                      paired = FALSE, group.by = "variable")
Result
ggviolin(data7, x="variable", y="value", color = "type",fill="type",
         palette =c("#D73027","#4575B4"),add = "boxplot",add.params = list(color="white"),
         xlab = "", ylab = "OPN richness")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(axis.text.x= element_blank())+
  theme(axis.title.y = element_text(size = 9),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(0,16,8),breaks = seq(0,16,8))
ggsave("p7_OPN.pdf", dpi=1200, width=62,height =65, units="mm")

###p8_HN###
data7 <- read_excel("Richness_violin.xlsx",8)
names(data7)
data8=melt(data7)
Result<-compare_means(value~type, data=data8, method = "t.test",
                      paired = FALSE, group.by = "variable")
Result
ggviolin(data8, x="variable", y="value", color = "type",fill="type",palette =c("#D73027","#4575B4"),
         add = "boxplot",add.params = list(color="white"),
         xlab = "", ylab = "HN richness")+
  theme(legend.position="none",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=9,color = "black")+#,angle=90
  theme(axis.text.x= element_blank())+
  theme(axis.title.y = element_text(size = 9),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(3,15,6),breaks = seq(3,15,6)))
ggsave("p8_HN_richness.pdf", dpi=1200, width=62,height =65, units="mm")

###legend##
data7 <- read_excel("Richness_violin.xlsx",8)
names(data7)
data8=melt(data7)
ggviolin(data8, x="variable",y="value",color = "type",fill="type",
         palette =c("#D73027","#4575B4"),add = "boxplot",add.params = list(color="white"),
         xlab = "", ylab = "HN richness")+
  theme(legend.position="right",strip.text = element_text( size = rel(0.90)))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(axis.title.y = element_text(size = 9),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=9,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())+
  scale_y_continuous(expand = c(0,0),limits = c(3,15,6),breaks = seq(3,15,6))
ggsave("legend.pdf", dpi=1200, width=38,height =38, units="mm")
