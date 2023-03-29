setwd("D:/DOC/R.projects")
library(plyr)
library(tidyverse)
library(Rmisc)
install.packages("hrbrthemes")
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
unique(UCL$club)
UCL<-read.csv("disciplinary.csv")
UCL<-UCL[2:10] %>% drop_na()
UCL<-UCL%>% rename(club =Custom.club)
UCL$club<-gsub("Beiktas","Besiktas",UCL$club)
str(UCL)
head(UCL)

names(UCL)<-tolower(names(UCL))
UCL<-UCL %>% 
  mutate(position=tolower(position))
summary(UCL)
summarySE(data=UCL,measurevar = "fouls_committed",groupvars = "position")
summarySE(data=UCL,measurevar = "fouls_suffered",groupvars = "position")

library(Hmisc)
UCL_matrix<-as.matrix(UCL[,4:9])
corr_matrix<-rcorr(UCL_matrix, type = "pearson")
corr_matrix$P
corr_matrix$r##negative correlation btn yellow cards and fouls suffered

library("corrplot")
corrplot(corr_matrix$r, type="upper", order="hclust", 
         p.mat = corr_matrix$P, sig.level = 0.05, 
         insig = "label_sig",pch.cex = 2,tl.col = "black",tl.srt =45)
UCL_filtred<-UCL %>% 
  filter(position%in%c("midfielder", "forward", "defender")) %>% 
  filter(minutes_played>=45)
library(ggplot2)
library(ggthemes)
plot1<-ggplot(UCL_filtred,aes(position,fouls_committed,group=position,color=position))+
  geom_boxplot()+
  geom_jitter( aes( color=position) , size=2, alpha=1,width=0.3) +
  scale_color_manual(values = c("#386cb0", "darkgoldenrod", "#7fc97f"))+
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("Position")+ylab("Fouls committed")+
  labs(title = "Overview of Fouls committed at player positions",
       tag = "(a)")

plot2<-ggplot(UCL_filtred,aes(position,fouls_suffered,color=position))+
  geom_boxplot()+
  geom_jitter( aes( color=position) , size=2, alpha=1,width=0.3) +
  scale_color_manual(values = c("#386cb0", "darkgoldenrod", "#7fc97f"))+
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  labs(title = "Overview of Fouls suffered at player positions",
       tag = "(b)" ) +
  xlab("Position")+ylab("Fouls suffered")

new_dat<-UCL_filtred %>% 
  select(c("minutes_played","fouls_suffered","fouls_committed"))  
##OVERALL
library(ggrepel)
plot5<-ggplot(new_dat, aes(x = minutes_played)) +
  geom_point(aes(y = fouls_suffered, color = "Fouls Suffered")) +
  geom_point(aes(y = fouls_committed, color = "Fouls Committed")) +
  scale_y_continuous(
    name = "Fouls Suffered",
    sec.axis = sec_axis(~./1, name = "Fouls Committed")
  ) +
  geom_text(aes(x=480,y=23,label="Jao Palhinha"),color="red")+
  geom_text(aes(x=1143,y=24,label="Vincius Junior"),color="red")+
  scale_color_manual(
    values = c("#386cb0", "orange"),
    guide = guide_legend(title = NULL)
  ) +
  theme_classic()+
  labs(title = "Distribution of fouls for the minutes played",
       tag = "(c)")+
  xlab("Minutes Played") +
  labs(y = NULL)+
  theme(legend.position=c(.1, .7))

##BAYERN
Bayern<-UCL_filtred %>% 
  filter(UCL_filtred$club=="Bayern",UCL_filtred$red>0)
Bayern <- Bayern %>% 
  mutate(player_name = fct_reorder(player_name, red,
                                   .desc = FALSE))
######################3
plot6<-ggplot(Bayern, aes(player_name,red)) +
  geom_bar(stat = "identity",fill="#CD6155") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("")+
  labs(tag="(d)", y="red card",title ="BAYERN MUNICH-UCL 2021/22",
       subtitle = "red cards recived by players",caption ="datasource:Kaggle,analized by Abdul") + 
  theme(plot.subtitle = element_text(size = 16))

library(Rmisc)
multiplot(plot1,plot2,plot5,plot6,cols = 2)
dev.off()
ggsave("plot.png", multiplot(plot1, plot2, plot5, plot6, cols = 2), 
       width = 35, height = 20, units = "cm", dpi = 300, bg = "white")
