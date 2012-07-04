#setwd("C:/Documents and Settings/Phillip Hughes/Desktop/Rstudio/Vanessa")
#setwd("C:/Users/bmal4866/Desktop/TXTBIN")
data<-read.csv ("rpskb1_3.csv")
plot(data)
datac<-read.csv ("rpskb1_3_c.csv")
datam<-read.csv ("rpskb1_3_m.csv")
data_aov<-read.csv("rpskbAOV.csv")

#layout(1)
#plot(rep(1:3,length.out=nrow(data)),
plot(data$Call, data$Gene.RPS6)

library(ggplot2)     
qplot(Call, Gene.RPS6, colour=Treat, data=data)
qplot(Call, Gene.RPS6KB1, colour=Treat, data=data)

qplot(Call, Gene.RPS6, data=data)+
  facet_grid(Treat~.)+
  geom_violin()+
  geom_point(colour="red")
     
plot (datac$Call, datac$Gene.RPS6)
plot (datam$Call, datam$Gene.RPS6)
library(car)
stripchart(data$Call ~ data$Gene.RPS6)

plot (data$Call, data$Gene.RPS6KB1)
plot (datac$Call, datac$Gene.RPS6KB1)
plot (datam$Call, datam$Gene.RPS6KB1)

    #a one panel graph
plot (data$Call, data$Gene.RPS6)    #x, y plot
lines(lowess(data$Call,data$RPS6))   #fit with lowess function

data$Call<-as.factor(data$Call)

par(mfrow=c(2,2))

model1 <- lm(Gene.RPS6~Call, data=data)
plot(model1)
summary(model1) 
anova(model1)

model2 <- lm(Gene.RPS6~Call, data=datac)
plot(model2)
summary(model2)
anova(model2)

model3 <- lm(lm(Gene.RPS6~Call, data=datac))
plot(model3)
summary(model3)
anova(model3)

datac$Call<-as.factor(datac$Call)

model4<-aov(Gene.RPS6~Treat*Call, data=data)
plot(model4)
summary(model4)


model5<-aov(log(Gene.RPS6)~Treat*Call, data=data)
plot(model5)
summary(model5)

model6<-aov(Gene.RPS6KB1~Treat*Call, data=data)
plot(model6)
summary(model6)

model7<-aov(log(Gene.RPS6KB1)~Treat*Call, data=data)
par(mfrow=c(2,2))
plot(model7)
summary(model7)

##plot(datac)


##pairs(data$Call,data$Gene.RPS6)

##plot (data$Call, data$Gene.RPS6)
##plot (data$Call, data$Gene.RPS6KB1)
##plot (data$Call, data$Gene.RPS6)
##plot (data$Call, data$Gene.RPS6KB1, type=scatterplot)

