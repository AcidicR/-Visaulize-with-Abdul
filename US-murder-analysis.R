setwd("D:/DOC/ANOVA.PRACTICE/ANOVA.PRACTICE")
library(plyr)
library(tidyverse)
library(dslabs)
library(ggplot2)
data("murders")
view(murders)
?murders  ##see background of data(2010 @ FBI)
sort(murders$total)  #sorts from low-high(no indexes assigned)
order(murders$total)  ##positions(index)of sorted observ's in the data
ind<-order(murders$population)  ###helps in reordering related variables 
rank<-rank(murders$population)  ##lowest is given a rank(1),("-" means otherwise)
new_df<-data.frame(states=murders$state[ind],region=murders$region[ind],
                   population=murders$population[ind],rank=rank[ind])
print(new_df)
##distribution of population
p1<-ggplot(new_df, aes(population,fill=population))+
  geom_histogram()+
  labs(x="population",title ="population distribution(US-2010)")+
  theme_bw()

p2<-ggplot(new_df,aes(population,fill=region))+
  geom_density(stat = "density",position="stack")+
  labs(xlab="Region",ylab="Population", title="Population distribution by region(US-2010)")+
  theme_bw()+
  theme(legend.position ="top")
###now we want to see occurrence of regions in the dataset and avg population
library(Rmisc)
SSE<-summarySE(new_df,measurevar = "population",groupvar ="region")###N,AVG,sd,se,ci

###let's define a new variable using arithmetic(murder rate)
murder_rate<-murders$total/murders$population*100000
murders$state[order(murder_rate)]##States and their corresponding murder rate(low-high)]
murder_rank<-rank(murder_rate)
##let's "order" our data in order of murder rate
ind2<-order(murder_rate)
###create new data frame an use the index "ind2",(u can mutate "new_data" if u like)
df_2<-data.frame(state=murders$state[ind2],region=murders$region[ind2],
                 population=murders$population[ind],total_murder=murders$total[ind2],
                 murder_rate=murder_rate[ind2],murder_rank=murder_rank[ind2])
####convinced by murder rate, define safe and dangerous states
max(df_2$murder_rate)
min(df_2$murder_rate)
mean(df_2$murder_rate)
df_2<-df_2 %>% 
  mutate(condition = if_else(murder_rate <= 3, "Safe","Dangerous"))

cor(df_2[3:5])##positive correlation(population & murder rate had a strong +corr)
#lets find out with scartter_plot
p3<-ggplot(df_2, aes(population,murder_rate)) +
  geom_point() +
  geom_smooth()+
  labs(x = "Population", y = "Murder rate", 
       title = "Relationship between population and murder rate")+
  theme_bw()

p4<-ggplot(df_2,aes(region,murder_rate))+
  geom_boxplot(aes(color=region)) +
  labs(x="region",y="murder rate", title="murder rate across region")+
  theme_bw()+
  theme(legend.position ="none")

p5<-ggplot(df_2, aes(x = region, y = murder_rate,fill=region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Region", y = "Murder rate", title = "murder rate across regions") +
  theme_bw() +
  theme(legend.position = "top")
###all in one plot
library(gridExtra)
mult<-grid.arrange(p1,p2,p3,p4,ncol=2)


