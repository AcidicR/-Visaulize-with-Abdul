setwd("D:/DOC/ANOVA.PRACTICE/ANOVA.PRACTICE")
library(plyr)
library(tidyverse)
library(dslabs)
library(ggplot2)
library(RColorBrewer)
my_palette<-brewer.pal(4,"BuGn")
rm(murders)
data("murders")
view(murders)
?murders  ##see background of data(2010 @ FBI)
sort(murders$total)  #sorts from low-high(no indexes assigned)
order(murders$total)  ##positions(index)of sorted observ's in the data
ind<-order(murders$population)  ###helps in reordering related variables 
rank<-rank(murders$population)  ##lowest is given a rank(1),("-" means otherwise)
new_df<-data.frame(states=murders$state[ind],region=murders$region[ind],
                   population=murders$population[ind],rank=rank[ind])
head(new_df)
rm(p1)
##distribution of population
p1<-ggplot(new_df, aes(population,fill=population))+
  geom_histogram(fill="skyblue")+
  labs(x="population",title ="population distribution(US-2010)")+
  theme_classic()

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
new_df<-data.frame(state=murders$state[ind2],region=murders$region[ind2],
                 population=murders$population[ind],total_murder=murders$total[ind2],
                 murder_rate=murder_rate[ind2],murder_rank=murder_rank[ind2])
head(new_df)
####convinced by murder rate, define safe and dangerous states
max(df_2$murder_rate)
min(df_2$murder_rate)
mean(df_2$murder_rate)
new_df<-new_df%>% 
  mutate(condition = if_else(murder_rate <= 3, "Safe","Dangerous"))
head(new_df)
cor(new_df[3:5])##positive correlation(population & murder rate had a strong +corr)
#lets find out with scartter_plot
p3<-ggplot(new_df, aes(population,murder_rate)) +
  geom_point() +
  geom_smooth()+
  labs(x = "Population", y = "Murder rate", 
       title = "Relationship between population and murder rate")+
  theme_bw()

p4<-ggplot(new_df,aes(region,murder_rate))+
  geom_boxplot(aes(color=region)) +
  scale_color_brewer(palette = "Set1") +
  labs(x="region",y="murder rate", title="murder rate across region")+
  theme_bw()+
  theme(legend.position ="none")

p5<-ggplot(new_df, aes(x = region, y = murder_rate,fill=region)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = condition), vjust = -0.5, color = "black") +
  labs(x = "Region", y = "Murder rate", title = "murder rate across regions") +
  theme_bw() +
  theme(legend.position = "top")+
  coord_flip()
###all in one plot
library(gridExtra)
mult<-grid.arrange(p1,p2,p3,p4,ncol=2)
#################################MAPS 
install.packages("ggmap")
library(ggmap)
 library(sf)
 library(ggmap)
 library(maps)
 library(mapdata)
 library(leaflet)
 library(usmap)
library(htmltools)

US<- new_df[, c("state","population", "murder_rate","condition")]
# install and load required packages
install.packages("rnaturalearth")
library(rnaturalearth)
update.packages("leaflet")
update.packages("RColorBrewer")
# download US state shapefile
states <- ne_states(country = "united states of america", returnclass = "sf")
US<-US[order(match(US$state,states$name)),]
bins<-c(0.3,0.5,1,3,6,16)
pal<-colorBin("YlOrRd",domain = US$murder_rate,bins = 4)
# view the first few rows of the data
head(US)
##lets plot our map
m <- leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = states,
              weight = 1,
              smoothFactor = .5,
              fillOpacity = .8,
              color = "white",
              fillColor = ~pal(US$murder_rate),
              label = paste(states$name, "<br>",
                            "Murder rate: ", round(US$murder_rate,3)),
              labelOptions = labelOptions(
                direction = "auto",
                permanent = FALSE,
                opacity = 0.9,
                style = list(
                  "max-width" = "200px",
                  "padding" = "5px 10px",
                  "font-size" = "14px",
                  "background-color" = "white",
                  "border" = "1px solid gray")
  )) %>%
  addLegend(pal = pal, values = US$murder_rate,
            position = "bottomright",
            title = "Murder Rate",
            opacity = 1)

 
  


