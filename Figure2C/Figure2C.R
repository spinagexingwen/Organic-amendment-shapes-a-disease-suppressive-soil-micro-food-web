library(readxl)
library(ggplot2)
library(grid)
library(ggpubr)
library(gridExtra)
library(tidyr)
library(RColorBrewer)


setwd(readClipboard())
getwd()

abu=read.csv("Biotic community abundance.csv",header=T,row.names=1)
abu
#Data centralization
abu.z<-scale(abu,center = F,scale = T)
abu.z
write.csv(abu.z,"1abu.z.csv")

div=read.csv("Biotic community diversity.csv",header=T,row.names=1)
div
#Data centralization
div.z<-scale(div,center = F,scale = T)
div.z
write.csv(div.z,"1div.z.csv")

###relative ratio based on abundance##
df <- read_excel("SFW_abu_div.xlsx",1)
df
df$Group<- factor(df$Group,levels=c("Bacteria","Fungi","AMF","Protozoa","BN","FN","OPN" ,"HN"),
                  labels = c("Bacteria","Fungi","AMF", "Protozoa","BN","FN","OPN" ,"HN"))
df$Time<- factor(df$Time,levels=c("May", "July"),labels = c("May", "July"))

p1<- ggplot(data=df, aes(x=Treatment, y=Relativeabundance,fill=Group))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  scale_fill_manual(values =  rev(c( "#c00000","#203864", "#2F5596","#3A68BD",
                                     "#5C84CC","#8FAADD","#B4C7E7","#DAE3F4"))) +
  facet_wrap(~Time)+
  theme_bw() + theme(panel.grid =element_blank())+
  labs( X= NULL ,y='Specific abundance / Total abundance (%)')+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size=7.5))+
  theme(legend.key.size=unit(0.5,'cm'))+
  theme(strip.text.x = element_text(size =7.5),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.5,hjust=0.5,color = "black",size=7.5))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,color = "black",size=7.5,angle =90))+ 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=7.5,color = "black"))
p1
ggsave("Abu1.pdf", dpi=1200, width=80,height =50, units="mm")


###relative ratio based on richness##
df <- read_excel("SFW_abu_div.xlsx",2)
df
df$Group<- factor(df$Group,levels=c("Bacteria","Fungi","AMF","Protozoa", "BN","FN","OPN" ,"HN"),
                  labels = c("Bacteria","Fungi","AMF","Protozoa","BN","FN","OPN","HN"))
df$Time<- factor(df$Time,levels=c("May","July"),labels = c("May","July"))

p2<- ggplot(data=df, aes(x=Treatment, y=Relativeabundance,fill=Group))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  scale_fill_manual(values =  rev(c( "#c00000","#203864", "#2F5596","#3A68BD",
                                     "#5C84CC","#8FAADD","#B4C7E7","#DAE3F4")))+
  facet_wrap(~Time)+
  theme_bw() + theme(panel.grid =element_blank())+
  labs(x= NULL ,y='Specific diversity / Total diversity (%)')+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size=7.5))+
  theme(legend.key.size=unit(0.5,'cm'))+
  theme(strip.text.x = element_text(size =7.5),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.5,hjust=0.5,color = "black",size=7.5))+
  theme(axis.text.x=element_text(vjust=0.5,hjust=0.5,color = "black",size=7.5,angle =90))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=7.5,color = "black"))
p2
ggsave("Div1.pdf", dpi=1200, width=80,height =50, units="mm") 
