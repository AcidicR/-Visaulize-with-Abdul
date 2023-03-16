datasets::airquality#load data set
View(airquality)
my_data<-airquality
missing_vars <- colnames(my_data)[apply(is.na(my_data), 2, any)]#which column contains the missing values?
complete.cases(my_data)#(TRUE for when there is missing value,FALSE means otherwise)

#install necessary packages
library(plyr)
library(tidyverse)
library(Rmisc)
library(mice)#access to imputation function

imputing<-mice(my_data,m=5,maxit = 50)#produces 5,data sets by 50 iterations(your choice)
imputed_data<-complete(imputing)#combine imputed data with rest of data,and assigned..

###########################################################
# Calculate summary statistics for the ozone variable
summary(imputed_data$Ozone)
mean(imputed_data$Ozone)
sd(imputed_data$Ozone)
###########check for correlation of only weather parameters
library(Hmisc)
library(corrplot)
Mat<-imputed_data[1:4]
##just correlation no,p-value
M<-cor(Mat)
#####correlation and p_values
cor_matix<-rcorr(as.matrix(Mat),type ="pearson")
r<-cor_matix$r
p<-cor_matix$P

p1<- corrplot(r, method = "color", type = "upper", order = "hclust",
               addCoef.col = NULL, tl.col = "black",
               sig.level = 0.05, insig = "label_sig",p.mat = p,
               title = "Influence of the Four Parameters on One Another", 
               mar = c(0, 0, 1, 0))+
               theme_minimal()

library(ggplot2)
my_colors <-colorRampPalette(c("#097969","#FF0000","#FF3131","#EC5800","#228B22"))(5)

p2<-ggplot(imputed_data,aes(Month,Temp,group=Month,color=Month))+
 geom_boxplot(outlier.size = 1)+
  scale_color_manual(values = my_colors,
                     labels=c("May","June","July","August","September"))+
                     theme_bw()

p3<-ggplot(imputed_data,aes(Month,Solar.R,group=Month,color=Month))+
  geom_boxplot()+
  scale_color_manual(values = my_colors,
                     labels=c("May","June","July","August","September"))+
                     theme_bw()
##testing linear model(ignore)
library(lme4)
library(lmerTest)
model <- lm(Temp ~ Solar.R + Wind+Month, data = imputed_data)
model1<-lm(Solar.R~Ozone+Month,data=imputed_data)
summary(model)
anova(model)
# Assuming your linear regression model is named "model"
plot(x = fitted(model), y = resid(model), xlab = "Predicted values", 
     ylab = "Residuals", main = "Residual plot")
abline(h = 0, col = "red")
shapiro.test(resid(model))
qqnorm(residuals(model))##all assumtions met
             
#time series visualization
# Create a data frame with Date and Temp columns
df <- data.frame(Date = as.Date(paste0(imputed_data$Month, 
                 "-", imputed_data$Day, "-1973"),
                 format = "%m-%d-%Y"),
                 Temp = imputed_data$Temp,Ozone=imputed_data$Ozone,
                 Solar.R=imputed_data$Solar.R,Wind=imputed_data$Wind)

p4<-ggplot(df, aes(x = Date, y = Temp)) +
  geom_line() +
  labs(x = "Month", y = "Temperature")+
  theme_bw()+
  theme(legend.position = "none")

p5<-ggplot(df,aes(Date,Solar.R))+
  geom_line()+
  labs(x="Month",y="Solar radiation")+
  theme_bw()+
  theme(legend.position = "none")

library(gridExtra)
grid.arrange(p2, p3, p4, p5, ncol = 2, top = "Distribution of Solar radiation and Temperature over Time")
