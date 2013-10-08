##an analysis of Vince Langs soil taxonomy data.

source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/akro.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/akro.write.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/meson.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/weighting.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/mp.plot.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/point_euclid.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/find.k.r")


vince<-read.csv("C:\\Users\\phug7649\\Desktop\\TXTBIN\\CentInput.csv",sep=",",header=TRUE,skip=1)
str(vince)

vince2<-rbind(vince,vince)
result<-princomp(vince2[,2:ncol(vince)],cor=TRUE)
str(result)
vince.scores<-result$scores
great.groups<-read.csv("C:\\Users\\phug7649\\Desktop\\TXTBIN\\great_groups.csv",header=TRUE)
great.groups2<-rbind(great.groups,great.groups)
vince2<-cbind(vince2,great.groups2)

plotting<-data.frame(prin1=vince.scores[,1],prin2=vince.scores[,2],order=great.groups2[,1])
plotting[,3]<-factor(plotting[,3],
levels = c(1:12),
labels = c("alfisols", "ultisols", "vertisols","mollisols","inceptisols","andisols","oxisols",
           "gelisols","histosols","aridisols","entisols","spodosols"))
library(ggplot2)
with(plotting, plot(prin1,prin2, col=order))
legend('topright', names(a)[-1] , 
       lty=1, col=c('red', 'blue', 'green',' brown'), bty='n', cex=.75)
library(rgl)
col.des<-as.factor(great.groups2[,1])
soil12<-colorRampPalette(c("orange3","orangered","orangered4","orchid4","palegreen4","palevioletred",
                           "royalblue","seagreen","plum1","slategray","slategray1","peachpuff3"))(nlevels(col.des))[as.numeric(col.des)]

pdf("C:\\Users\\phug7649\\Desktop\\TXTBIN\\soilplot.pdf", width=40, height=15)
plot = ggplot(plotting, aes(x=prin1, y=prin2,colour=order)) +
geom_point(shape = 20,size=10,alpha=.5)

#write.csv(plotting,"plotting.csv")

plot
dev.off()
  geom_point(method=lm, se=FALSE, size=1, aes(colour="Fitted", linetype="Fitted")) +
  geom_smooth(method=lm, fill="red", colour="red", linetype="twodash", size=1) +
  geom_line(data = data.frame(x=0, y=0), aes(colour = "Ideal", linetype = "Ideal"), size=1) +
  #geom_abline(intercept=0, slope=1, aes(colour = "Ideal", linetype = "Ideal"), size=0) +
  geom_abline(intercept=0, slope=1, colour = "blue", linetype = "solid", size=1) +
  geom_point(shape=ifelse(nrow(df)>49, 1, 16)) +
  scale_colour_manual(name="Lines", values=c("Ideal"="blue", "Fitted"="red")) +
  scale_linetype_manual(name="Lines", values=c("Ideal"="solid", "Fitted"="twodash")) +
  scale_x_continuous(name="Control", limits=xy.range) +
  scale_y_continuous(name="Evaluation", limits=xy.range) +
  opts(title="Method Comparison", aspect.ratio=1) +
  theme_bw() 



n.dots<-nrow(vince.scores)
plot3d(vince.scores[,1],vince.scores[,2],vince.scores[,3],col=soil12,size=10)
plot3d(vince.scores[,1],vince.scores[,2],vince.scores[,3],col=rep(soil12,each=n.dots),size=10)
name<-c("alfisols","ultisols","vertisols","mollisols","inceptisols","andisols","oxisols","gelisols","histosols","aridisols","entisols","spodisols")
number<-c(1:12)
legend<-cbind(number,name)
colorRampPalette(c(“red”, “blue”))[as.numeric(fac)]
setwd()
plot(vince.scores[,1],vince.scores[,2], col=vince.scores$)
##loading plots
library(pls) 

pdf("C:\\Users\\phug7649\\Desktop\\TXTBIN\\vloadings.pdf", width=40, height=15)
loadingplot(result$loadings, comps = 1:2, scatter = TRUE, labels="names")
points(vince.scores[,1],vince.scores[,2])
dev.off()


#with(data,plot3d(Class,FPI,Phi, main="FKM~phi,FPI and class",col=rep(contrast(n.phi), each=n.dots) , size=size))

##clustering vinces stuff
library(ape)

hc.all<-hclust(dist(vince[c(2:ncol(vince))]), "ward")
pdf("C:\\Users\\phug7649\\Desktop\\TXTBIN\\vdendraw.pdf", width=40, height=15)
plot(hc.all, hang=-1,labels=vince[,1],main="Dendrogram of all the clusters using raw data")
dev.off()

#hc.all<-hclust(dist(vince.scores[1:299,][c(2:ncol(vince.scores[1:299,]))]), "ward")

vince.scoresII<-as.matrix(vince.scores[1:299,])
groups<-vince$X.NO.of.profiles.5.
vince.scoresII<-cbind(groups,as.data.frame(vince.scoresII))
hc.all<-hclust(dist(vince.scoresII[c(2:ncol(vince.scoresII))]), "ward")
pdf("C:\\Users\\phug7649\\Desktop\\TXTBIN\\vdendcomp.pdf", width=40, height=15)
plot(hc.all, hang=-1,labels=vince.scoresII[,1],main="Dendrogram of all the clusters using principal components")
dev.off()



hc <- hclust(dist(EP[,c(2:10)]), "ward")
plot(hc, hang=-1,labels=EP$natural_key,main="Dendrogram of the fixed clusters")

HCE = hclust(dist(C[,2:10]))
plot(HCE, hang=-1,labels=C$natural_key,main="Dendrogram of the non-fixed clusters")

vmeans<-colMeans(vince[,2:ncol(vince)])
vmeans<-c("mean",as.numeric(vmeans))

i<-2


dist.c<- abs(vince[,2]-as.numeric(vmeans[2]))
dist.c[which.min(dist.c)]
point_euclid(vince.mean[,2])


VINCE<-rbind(vince,vince)
write.csv(vince[,2:ncol(vince)],"C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\vince.csv",row.names=TRUE)
write.csv(VINCE[,2:ncol(VINCE)],"C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\vince_double.csv",row.names=TRUE)
write.csv(vince.scores[1:299,],"C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\vpca.csv",row.names=TRUE)

r <- cor(vince[,2:ncol(vince)], use = "all.obs")
X1_00.10<-(vince$X1_0.5+vince$X1_5.10)/2
X1_10.20<-(vince$X1_10.15+vince$X1_15.20)/2
X1_20.30<-(vince$X1_20.25+vince$X1_25.30)/2
X1_30.40<-(vince$X1_30.35+vince$X1_35.40)/2
X1_40.50<-(vince$X1_40.45+vince$X1_45.50)/2
X1_50.70<-(vince$X1_50.60+vince$X1_60.70)/2
X1_70.90<-(vince$X1_70.80+vince$X1_80.90)/2
X1_90.110<-(vince$X1_90.100+vince$X1_100.110)/2
X1_110.150<-(vince$X1_110.130+vince$X1_130.150)/2
X1<-cbind(X1_00.10,X1_10.20,X1_20.30,X1_30.40,X1_40.50,X1_50.70,X1_70.90,X1_90.110,X1_110.150)

x2_00.10<-(vince$X2_0.5+vince$X2_5.10)/2
x2_10.20<-(vince$X2_10.15+vince$X2_15.20)/2
x2_20.30<-(vince$X2_20.25+vince$X2_25.30)/2
x2_30.40<-(vince$X2_30.35+vince$X2_35.40)/2
x2_40.50<-(vince$X2_40.45+vince$X2_45.50)/2
x2_50.70<-(vince$X2_50.60+vince$X2_60.70)/2
x2_70.90<-(vince$X2_70.80+vince$X2_80.90)/2
x2_90.110<-(vince$X2_90.100+vince$X2_100.110)/2
x2_110.150<-(vince$X2_110.130+vince$X2_130.150)/2
x2<-cbind(x2_00.10,x2_10.20,x2_20.30,x2_30.40,x2_40.50,x2_50.70,x2_70.90,x2_90.110,x2_110.150)

x2cor<-cor(x2, use = "all.obs")

x3_00.10<-(vince$X3_0.5+vince$X3_5.10)/2
x3_10.20<-(vince$X3_10.15+vince$X3_15.20)/2
x3_20.30<-(vince$X3_20.25+vince$X3_25.30)/2
x3_30.40<-(vince$X3_30.35+vince$X3_35.40)/2
x3_40.50<-(vince$X3_40.45+vince$X3_45.50)/2
x3_50.70<-(vince$X3_50.60+vince$X3_60.70)/2
x3_70.90<-(vince$X3_70.80+vince$X3_80.90)/2
x3_90.110<-(vince$X3_90.100+vince$X3_100.110)/2
x3_110.150<-(vince$X3_110.130+vince$X3_130.150)/2
x3<-cbind(x3_00.10,x3_10.20,x3_20.30,x3_30.40,x3_40.50,x3_50.70,x3_70.90,x3_90.110,x3_110.150)

x3cor<-cor(x3, use = "all.obs")

x4_00.10<-(vince$X4_0.5+vince$X4_5.10)/2
x4_10.20<-(vince$X4_10.15+vince$X4_15.20)/2
x4_20.30<-(vince$X4_20.25+vince$X4_25.30)/2
x4_30.40<-(vince$X4_30.35+vince$X4_35.40)/2
x4_40.50<-(vince$X4_40.45+vince$X4_45.50)/2
x4_50.70<-(vince$X4_50.60+vince$X4_60.70)/2
x4_70.90<-(vince$X4_70.80+vince$X4_80.90)/2
x4_90.110<-(vince$X4_90.100+vince$X4_100.110)/2
x4_110.150<-(vince$X4_110.130+vince$X4_130.150)/2
x4<-cbind(x4_00.10,x4_10.20,x4_20.30,x4_30.40,x4_40.50,x4_50.70,x4_70.90,x4_90.110,x4_110.150)

x4cor<-cor(x4, use = "all.obs")


x5_00.10<-(vince$X5_0.5+vince$X5_5.10)/2
x5_10.20<-(vince$X5_10.15+vince$X5_15.20)/2
x5_20.30<-(vince$X5_20.25+vince$X5_25.30)/2
x5_30.40<-(vince$X5_30.35+vince$X5_35.40)/2
x5_40.50<-(vince$X5_40.45+vince$X5_45.50)/2
x5_50.70<-(vince$X5_50.60+vince$X5_60.70)/2
x5_70.90<-(vince$X5_70.80+vince$X5_80.90)/2
x5_90.110<-(vince$X5_90.100+vince$X5_100.110)/2
x5_110.150<-(vince$X5_110.130+vince$X5_130.150)/2
x5<-cbind(x5_00.10,x5_10.20,x5_20.30,x5_30.40,x5_40.50,x5_50.70,x5_70.90,x5_90.110,x5_110.150)

x5cor<-cor(x5, use = "all.obs")

x6_00.10<-(vince$X6_0.5+vince$X6_5.10)/2
x6_10.20<-(vince$X6_10.15+vince$X6_15.20)/2
x6_20.30<-(vince$X6_20.25+vince$X6_25.30)/2
x6_30.40<-(vince$X6_30.35+vince$X6_35.40)/2
x6_40.50<-(vince$X6_40.45+vince$X6_45.50)/2
x6_50.70<-(vince$X6_50.60+vince$X6_60.70)/2
x6_70.90<-(vince$X6_70.80+vince$X6_80.90)/2
x6_90.110<-(vince$X6_90.100+vince$X6_100.110)/2
x6_110.150<-(vince$X6_110.130+vince$X6_130.150)/2
x6<-cbind(x6_00.10,x6_10.20,x6_20.30,x6_30.40,x6_40.50,x6_50.70,x6_70.90,x6_90.110,x6_110.150)

x6cor<-cor(x6, use = "all.obs")

X7_00.10<-(vince$X7_0.5+vince$X7_5.10)/2
X7_10.20<-(vince$X7_10.15+vince$X7_15.20)/2
X7_20.30<-(vince$X7_20.25+vince$X7_25.30)/2
X7_30.40<-(vince$X7_30.35+vince$X7_35.40)/2
X7_40.50<-(vince$X7_40.45+vince$X7_45.50)/2
X7_50.70<-(vince$X7_50.60+vince$X7_60.70)/2
X7_70.90<-(vince$X7_70.80+vince$X7_80.90)/2
X7_90.110<-(vince$X7_90.100+vince$X7_100.110)/2
X7_110.150<-(vince$X7_110.130+vince$X7_130.150)/2
X7<-cbind(X7_00.10,X7_10.20,X7_20.30,X7_30.40,X7_40.50,X7_50.70,X7_70.90,X7_90.110,X7_110.150)

X7cor<-cor(X7, use = "all.obs")



X8_00.10<-(vince$X8_0.5+vince$X8_5.10)/2
X8_10.20<-(vince$X8_10.15+vince$X8_15.20)/2
X8_20.30<-(vince$X8_20.25+vince$X8_25.30)/2
X8_30.40<-(vince$X8_30.35+vince$X8_35.40)/2
X8_40.50<-(vince$X8_40.45+vince$X8_45.50)/2
X8_50.70<-(vince$X8_50.60+vince$X8_60.70)/2
X8_70.90<-(vince$X8_70.80+vince$X8_80.90)/2
X8_90.110<-(vince$X8_90.100+vince$X8_100.110)/2
X8_110.150<-(vince$X8_110.130+vince$X8_130.150)/2
X8<-cbind(X8_00.10,X8_10.20,X8_20.30,X8_30.40,X8_40.50,X8_50.70,X8_70.90,X8_90.110,X8_110.150)

X8cor<-cor(X8, use = "all.obs")


X9_00.10<-(vince$X9_0.5+vince$X9_5.10)/2
X9_10.20<-(vince$X9_10.15+vince$X9_15.20)/2
X9_20.30<-(vince$X9_20.25+vince$X9_25.30)/2
X9_30.40<-(vince$X9_30.35+vince$X9_35.40)/2
X9_40.50<-(vince$X9_40.45+vince$X9_45.50)/2
X9_50.70<-(vince$X9_50.60+vince$X9_60.70)/2
X9_70.90<-(vince$X9_70.80+vince$X9_80.90)/2
X9_90.110<-(vince$X9_90.100+vince$X9_100.110)/2
X9_110.150<-(vince$X9_110.130+vince$X9_130.150)/2
X9<-cbind(X9_00.10,X9_10.20,X9_20.30,X9_30.40,X9_40.50,X9_50.70,X9_70.90,X9_90.110,X9_110.150)

X9cor<-cor(X9, use = "all.obs")

X10_00.10<-(vince$X10_0.5+vince$X10_5.10)/2
X10_10.20<-(vince$X10_10.15+vince$X10_15.20)/2
X10_20.30<-(vince$X10_20.25+vince$X10_25.30)/2
X10_30.40<-(vince$X10_30.35+vince$X10_35.40)/2
X10_40.50<-(vince$X10_40.45+vince$X10_45.50)/2
X10_50.70<-(vince$X10_50.60+vince$X10_60.70)/2
X10_70.90<-(vince$X10_70.80+vince$X10_80.90)/2
X10_90.110<-(vince$X10_90.100+vince$X10_100.110)/2
X10_110.150<-(vince$X10_110.130+vince$X10_130.150)/2
X10<-cbind(X10_00.10,X10_10.20,X10_20.30,X10_30.40,X10_40.50,X10_50.70,X10_70.90,X10_90.110,X10_110.150)

X10cor<-cor(X10, use = "all.obs")

X11_00.10<-(vince$X11_0.5+vince$X11_5.10)/2
X11_10.20<-(vince$X11_10.15+vince$X11_15.20)/2
X11_20.30<-(vince$X11_20.25+vince$X11_25.30)/2
X11_30.40<-(vince$X11_30.35+vince$X11_35.40)/2
X11_40.50<-(vince$X11_40.45+vince$X11_45.50)/2
X11_50.70<-(vince$X11_50.60+vince$X11_60.70)/2
X11_70.90<-(vince$X11_70.80+vince$X11_80.90)/2
X11_90.110<-(vince$X11_90.100+vince$X11_100.110)/2
X11_110.150<-(vince$X11_110.130+vince$X11_130.150)/2
X11<-cbind(X11_00.10,X11_10.20,X11_20.30,X11_30.40,X11_40.50,X11_50.70,X11_70.90,X11_90.110,X11_110.150)

X11cor<-cor(X11, use = "all.obs")


X12_00.10<-(vince$X12_0.5+vince$X12_5.10)/2
X12_10.20<-(vince$X12_10.15+vince$X12_15.20)/2
X12_20.30<-(vince$X12_20.25+vince$X12_25.30)/2
X12_30.40<-(vince$X12_30.35+vince$X12_35.40)/2
X12_40.50<-(vince$X12_40.45+vince$X12_45.50)/2
X12_50.70<-(vince$X12_50.60+vince$X12_60.70)/2
X12_70.90<-(vince$X12_70.80+vince$X12_80.90)/2
X12_90.110<-(vince$X12_90.100+vince$X12_100.110)/2
X12_110.150<-(vince$X12_110.130+vince$X12_130.150)/2
X12<-cbind(X12_00.10,X12_10.20,X12_20.30,X12_30.40,X12_40.50,X12_50.70,X12_70.90,X12_90.110,X12_110.150)

X12cor<-cor(X12, use = "all.obs")


X13_00.10<-(vince$X13_0.5+vince$X13_5.10)/2
X13_10.20<-(vince$X13_10.15+vince$X13_15.20)/2
X13_20.30<-(vince$X13_20.25+vince$X13_25.30)/2
X13_30.40<-(vince$X13_30.35+vince$X13_35.40)/2
X13_40.50<-(vince$X13_40.45+vince$X13_45.50)/2
X13_50.70<-(vince$X13_50.60+vince$X13_60.70)/2
X13_70.90<-(vince$X13_70.80+vince$X13_80.90)/2
X13_90.110<-(vince$X13_90.100+vince$X13_100.110)/2
X13_110.150<-(vince$X13_110.130+vince$X13_130.150)/2
X13<-cbind(X13_00.10,X13_10.20,X13_20.30,X13_30.40,X13_40.50,X13_50.70,X13_70.90,X13_90.110,X13_110.150)

X13cor<-cor(X13, use = "all.obs")

X14_00.10<-(vince$X14_0.5+vince$X14_5.10)/2
X14_10.20<-(vince$X14_10.15+vince$X14_15.20)/2
X14_20.30<-(vince$X14_20.25+vince$X14_25.30)/2
X14_30.40<-(vince$X14_30.35+vince$X14_35.40)/2
X14_40.50<-(vince$X14_40.45+vince$X14_45.50)/2
X14_50.70<-(vince$X14_50.60+vince$X14_60.70)/2
X14_70.90<-(vince$X14_70.80+vince$X14_80.90)/2
X14_90.110<-(vince$X14_90.100+vince$X14_100.110)/2
X14_110.150<-(vince$X14_110.130+vince$X14_130.150)/2
X14<-cbind(X14_00.10,X14_10.20,X14_20.30,X14_30.40,X14_40.50,X14_50.70,X14_70.90,X14_90.110,X14_110.150)

X14cor<-cor(X14, use = "all.obs")


X15_00.10<-(vince$X15_0.5+vince$X15_5.10)/2
X15_10.20<-(vince$X15_10.15+vince$X15_15.20)/2
X15_20.30<-(vince$X15_20.25+vince$X15_25.30)/2
X15_30.40<-(vince$X15_30.35+vince$X15_35.40)/2
X15_40.50<-(vince$X15_40.45+vince$X15_45.50)/2
X15_50.70<-(vince$X15_50.60+vince$X15_60.70)/2
X15_70.90<-(vince$X15_70.80+vince$X15_80.90)/2
X15_90.110<-(vince$X15_90.100+vince$X15_100.110)/2
X15_110.150<-(vince$X15_110.130+vince$X15_130.150)/2
X15<-cbind(X15_00.10,X15_10.20,X15_20.30,X15_30.40,X15_40.50,X15_50.70,X15_70.90,X15_90.110,X15_110.150)

X15cor<-cor(X15, use = "all.obs")





X16_00.10<-(vince$X16_0.5+vince$X16_5.10)/2
X16_10.20<-(vince$X16_10.15+vince$X16_15.20)/2
X16_20.30<-(vince$X16_20.25+vince$X16_25.30)/2
X16_30.40<-(vince$X16_30.35+vince$X16_35.40)/2
X16_40.50<-(vince$X16_40.45+vince$X16_45.50)/2
X16_50.70<-(vince$X16_50.60+vince$X16_60.70)/2
X16_70.90<-(vince$X16_70.80+vince$X16_80.90)/2
X16_90.110<-(vince$X16_90.100+vince$X16_100.110)/2
X16_110.150<-(vince$X16_110.130+vince$X16_130.150)/2
X16<-cbind(X16_00.10,X16_10.20,X16_20.30,X16_30.40,X16_40.50,X16_50.70,X16_70.90,X16_90.110,X16_110.150)

X16cor<-cor(X16, use = "all.obs")


X17_00.10<-(vince$X17_0.5+vince$X17_5.10)/2
X17_10.20<-(vince$X17_10.15+vince$X17_15.20)/2
X17_20.30<-(vince$X17_20.25+vince$X17_25.30)/2
X17_30.40<-(vince$X17_30.35+vince$X17_35.40)/2
X17_40.50<-(vince$X17_40.45+vince$X17_45.50)/2
X17_50.70<-(vince$X17_50.60+vince$X17_60.70)/2
X17_70.90<-(vince$X17_70.80+vince$X17_80.90)/2
X17_90.110<-(vince$X17_90.100+vince$X17_100.110)/2
X17_110.150<-(vince$X17_110.130+vince$X17_130.150)/2
X17<-cbind(X17_00.10,X17_10.20,X17_20.30,X17_30.40,X17_40.50,X17_50.70,X17_70.90,X17_90.110,X17_110.150)

X17cor<-cor(X17, use = "all.obs")

X18_00.10<-(vince$X18_0.5+vince$X18_5.10)/2
X18_10.20<-(vince$X18_10.15+vince$X18_15.20)/2
X18_20.30<-(vince$X18_20.25+vince$X18_25.30)/2
X18_30.40<-(vince$X18_30.35+vince$X18_35.40)/2
X18_40.50<-(vince$X18_40.45+vince$X18_45.50)/2
X18_50.70<-(vince$X18_50.60+vince$X18_60.70)/2
X18_70.90<-(vince$X18_70.80+vince$X18_80.90)/2
X18_90.110<-(vince$X18_90.100+vince$X18_100.110)/2
X18_110.150<-(vince$X18_110.130+vince$X18_130.150)/2
X18<-cbind(X18_00.10,X18_10.20,X18_20.30,X18_30.40,X18_40.50,X18_50.70,X18_70.90,X18_90.110,X18_110.150)

X18cor<-cor(X18, use = "all.obs")

X19_00.10<-(vince$X19_0.5+vince$X19_5.10)/2
X19_10.20<-(vince$X19_10.15+vince$X19_15.20)/2
X19_20.30<-(vince$X19_20.25+vince$X19_25.30)/2
X19_30.40<-(vince$X19_30.35+vince$X19_35.40)/2
X19_40.50<-(vince$X19_40.45+vince$X19_45.50)/2
X19_50.70<-(vince$X19_50.60+vince$X19_60.70)/2
X19_70.90<-(vince$X19_70.80+vince$X19_80.90)/2
X19_90.110<-(vince$X19_90.100+vince$X19_100.110)/2
X19_110.150<-(vince$X19_110.130+vince$X19_130.150)/2
X19<-cbind(X19_00.10,X19_10.20,X19_20.30,X19_30.40,X19_40.50,X19_50.70,X19_70.90,X19_90.110,X19_110.150)

X19cor<-cor(X19, use = "all.obs")


X20_00.10<-(vince$X20_0.5+vince$X20_5.10)/2
X20_10.20<-(vince$X20_10.15+vince$X20_15.20)/2
X20_20.30<-(vince$X20_20.25+vince$X20_25.30)/2
X20_30.40<-(vince$X20_30.35+vince$X20_35.40)/2
X20_40.50<-(vince$X20_40.45+vince$X20_45.50)/2
X20_50.70<-(vince$X20_50.60+vince$X20_60.70)/2
X20_70.90<-(vince$X20_70.80+vince$X20_80.90)/2
X20_90.110<-(vince$X20_90.100+vince$X20_100.110)/2
X20_110.150<-(vince$X20_110.130+vince$X20_130.150)/2
X20<-cbind(X20_00.10,X20_10.20,X20_20.30,X20_30.40,X20_40.50,X20_50.70,X20_70.90,X20_90.110,X20_110.150)

X20cor<-cor(X20, use = "all.obs")

X21_00.10<-(vince$X21_0.5+vince$X21_5.10)/2
X21_10.20<-(vince$X21_10.15+vince$X21_15.20)/2
X21_20.30<-(vince$X21_20.25+vince$X21_25.30)/2
X21_30.40<-(vince$X21_30.35+vince$X21_35.40)/2
X21_40.50<-(vince$X21_40.45+vince$X21_45.50)/2
X21_50.70<-(vince$X21_50.60+vince$X21_60.70)/2
X21_70.90<-(vince$X21_70.80+vince$X21_80.90)/2
X21_90.110<-(vince$X21_90.100+vince$X21_100.110)/2
X21_110.150<-(vince$X21_110.130+vince$X21_130.150)/2
X21<-cbind(X21_00.10,X21_10.20,X21_20.30,X21_30.40,X21_40.50,X21_50.70,X21_70.90,X21_90.110,X21_110.150)

X21cor<-cor(X21, use = "all.obs")

X22_00.10<-(vince$X22_0.5+vince$X22_5.10)/2
X22_10.20<-(vince$X22_10.15+vince$X22_15.20)/2
X22_20.30<-(vince$X22_20.25+vince$X22_25.30)/2
X22_30.40<-(vince$X22_30.35+vince$X22_35.40)/2
X22_40.50<-(vince$X22_40.45+vince$X22_45.50)/2
X22_50.70<-(vince$X22_50.60+vince$X22_60.70)/2
X22_70.90<-(vince$X22_70.80+vince$X22_80.90)/2
X22_90.110<-(vince$X22_90.100+vince$X22_100.110)/2
X22_110.150<-(vince$X22_110.130+vince$X22_130.150)/2
X22<-cbind(X22_00.10,X22_10.20,X22_20.30,X22_30.40,X22_40.50,X22_50.70,X22_70.90,X22_90.110,X22_110.150)

X22cor<-cor(X22, use = "all.obs")

X23_00.10<-(vince$X23_0.5+vince$X23_5.10)/2
X23_10.20<-(vince$X23_10.15+vince$X23_15.20)/2
X23_20.30<-(vince$X23_20.25+vince$X23_25.30)/2
X23_30.40<-(vince$X23_30.35+vince$X23_35.40)/2
X23_40.50<-(vince$X23_40.45+vince$X23_45.50)/2
X23_50.70<-(vince$X23_50.60+vince$X23_60.70)/2
X23_70.90<-(vince$X23_70.80+vince$X23_80.90)/2
X23_90.110<-(vince$X23_90.100+vince$X23_100.110)/2
X23_110.150<-(vince$X23_110.130+vince$X23_130.150)/2
X23<-cbind(X23_00.10,X23_10.20,X23_20.30,X23_30.40,X23_40.50,X23_50.70,X23_70.90,X23_90.110,X23_110.150)

X23cor<-cor(X23, use = "all.obs")

vince.merge1<-cbind(X1,x2,x3,x4,x5,x6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23)
write.csv(vince.merge1,"C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\vince_m1.csv",row.names=TRUE)

X1_00.20<-(vince.merge1[,1]+vince.merge1[,2])/2
X1_20.40<-(vince.merge1[,3]+vince.merge1[,4])/2
X1_40.70<-(vince.merge1[,5]+vince.merge1[,6])/2
X1_70.110<-(vince.merge1[,7]+vince.merge1[,8])/2
X1_110.150<-vince.merge1[,9]
X1<-cbind(X1_00.20,X1_20.40,X1_40.70,X1_70.110,X1_110.150)
X1cor<-cor(X1, use = "all.obs")
X1cor

X2_00.20<-(vince.merge1[,10]+vince.merge1[,11])/2
X2_20.40<-(vince.merge1[,12]+vince.merge1[,13])/2
X2_40.70<-(vince.merge1[,14]+vince.merge1[,15])/2
X2_70.110<-(vince.merge1[,16]+vince.merge1[,17])/2
X2_110.150<-vince.merge1[,18]
X2<-cbind(X2_00.20,X2_20.40,X2_40.70,X2_70.110,X2_110.150)
X2cor<-cor(X2, use = "all.obs")
X2cor

X3_00.20<-(vince.merge1[,19]+vince.merge1[,20])/2
X3_20.40<-(vince.merge1[,21]+vince.merge1[,22])/2
X3_40.70<-(vince.merge1[,23]+vince.merge1[,24])/2
X3_70.110<-(vince.merge1[,25]+vince.merge1[,26])/2
X3_110.150<-vince.merge1[,27]
X3<-cbind(X3_00.20,X3_20.40,X3_40.70,X3_70.110,X3_110.150)
X3cor<-cor(X3, use = "all.obs")
X3cor

X4_00.20<-(vince.merge1[,28]+vince.merge1[,29])/2
X4_20.40<-(vince.merge1[,30]+vince.merge1[,31])/2
X4_40.70<-(vince.merge1[,32]+vince.merge1[,33])/2
X4_70.110<-(vince.merge1[,34]+vince.merge1[,35])/2
X4_110.150<-vince.merge1[,36]
X4<-cbind(X4_00.20,X4_20.40,X4_40.70,X4_70.110,X4_110.150)
X4cor<-cor(X4, use = "all.obs")
X4cor

X5_00.20<-(vince.merge1[,37]+vince.merge1[,38])/2
X5_20.40<-(vince.merge1[,39]+vince.merge1[,40])/2
X5_40.70<-(vince.merge1[,41]+vince.merge1[,42])/2
X5_70.110<-(vince.merge1[,43]+vince.merge1[,44])/2
X5_110.150<-vince.merge1[,45]
X5<-cbind(X5_00.20,X5_20.40,X5_40.70,X5_70.110,X5_110.150)
X5cor<-cor(X5, use = "all.obs")
X5cor

X6_00.20<-(vince.merge1[,46]+vince.merge1[,47])/2
X6_20.40<-(vince.merge1[,48]+vince.merge1[,49])/2
X6_40.70<-(vince.merge1[,50]+vince.merge1[,51])/2
X6_70.110<-(vince.merge1[,52]+vince.merge1[,53])/2
X6_110.150<-vince.merge1[,54]
X6<-cbind(X6_00.20,X6_20.40,X6_40.70,X6_70.110,X6_110.150)
X6cor<-cor(X6, use = "all.obs")
X6cor

X7_00.20<-(vince.merge1[,55]+vince.merge1[,56])/2
X7_20.40<-(vince.merge1[,57]+vince.merge1[,58])/2
X7_40.70<-(vince.merge1[,59]+vince.merge1[,60])/2
X7_70.110<-(vince.merge1[,61]+vince.merge1[,62])/2
X7_110.150<-vince.merge1[,63]
X7<-cbind(X7_00.20,X7_20.40,X7_40.70,X7_70.110,X7_110.150)
X7cor<-cor(X7, use = "all.obs")
X7cor

X8_00.20<-(vince.merge1[,64]+vince.merge1[,65])/2
X8_20.40<-(vince.merge1[,66]+vince.merge1[,67])/2
X8_40.70<-(vince.merge1[,68]+vince.merge1[,69])/2
X8_70.110<-(vince.merge1[,70]+vince.merge1[,71])/2
X8_110.150<-vince.merge1[,72]
X8<-cbind(X8_00.20,X8_20.40,X8_40.70,X8_70.110,X8_110.150)
X8cor<-cor(X8, use = "all.obs")
X8cor

X9_00.20<-(vince.merge1[,73]+vince.merge1[,74])/2
X9_20.40<-(vince.merge1[,75]+vince.merge1[,76])/2
X9_40.70<-(vince.merge1[,77]+vince.merge1[,78])/2
X9_70.110<-(vince.merge1[,79]+vince.merge1[,80])/2
X9_110.150<-vince.merge1[,81]
X9<-cbind(X9_00.20,X9_20.40,X9_40.70,X9_70.110,X9_110.150)
X9cor<-cor(X9, use = "all.obs")
X9cor

X10_00.20<-(vince.merge1[,82]+vince.merge1[,83])/2
X10_20.40<-(vince.merge1[,84]+vince.merge1[,85])/2
X10_40.70<-(vince.merge1[,86]+vince.merge1[,87])/2
X10_70.110<-(vince.merge1[,88]+vince.merge1[,89])/2
X10_110.150<-vince.merge1[,90]
X10<-cbind(X10_00.20,X10_20.40,X10_40.70,X10_70.110,X10_110.150)
X10cor<-cor(X10, use = "all.obs")
X10cor

X11_00.20<-(vince.merge1[,91]+vince.merge1[,92])/2
X11_20.40<-(vince.merge1[,93]+vince.merge1[,94])/2
X11_40.70<-(vince.merge1[,95]+vince.merge1[,96])/2
X11_70.110<-(vince.merge1[,97]+vince.merge1[,98])/2
X11_110.150<-vince.merge1[,99]
X11<-cbind(X11_00.20,X11_20.40,X11_40.70,X11_70.110,X11_110.150)
X11cor<-cor(X11, use = "all.obs")
X11cor

X12_00.20<-(vince.merge1[,100]+vince.merge1[,101])/2
X12_20.40<-(vince.merge1[,102]+vince.merge1[,103])/2
X12_40.70<-(vince.merge1[,104]+vince.merge1[,105])/2
X12_70.110<-(vince.merge1[,106]+vince.merge1[,107])/2
X12_110.150<-vince.merge1[,108]
X12<-cbind(X12_00.20,X12_20.40,X12_40.70,X12_70.110,X12_110.150)
X12cor<-cor(X12, use = "all.obs")
X12cor

X13_00.20<-(vince.merge1[,109]+vince.merge1[,110])/2
X13_20.40<-(vince.merge1[,111]+vince.merge1[,112])/2
X13_40.70<-(vince.merge1[,113]+vince.merge1[,114])/2
X13_70.110<-(vince.merge1[,115]+vince.merge1[,116])/2
X13_110.150<-vince.merge1[,117]
X13<-cbind(X13_00.20,X13_20.40,X13_40.70,X13_70.110,X13_110.150)
X13cor<-cor(X13, use = "all.obs")
X13cor

X14_00.20<-(vince.merge1[,118]+vince.merge1[,119])/2
X14_20.40<-(vince.merge1[,120]+vince.merge1[,121])/2
X14_40.70<-(vince.merge1[,122]+vince.merge1[,123])/2
X14_70.110<-(vince.merge1[,124]+vince.merge1[,125])/2
X14_110.150<-vince.merge1[,126]
X14<-cbind(X14_00.20,X14_20.40,X14_40.70,X14_70.110,X14_110.150)
X14cor<-cor(X14, use = "all.obs")
X14cor

X15_00.20<-(vince.merge1[,127]+vince.merge1[,128])/2
X15_20.40<-(vince.merge1[,129]+vince.merge1[,130])/2
X15_40.70<-(vince.merge1[,131]+vince.merge1[,132])/2
X15_70.110<-(vince.merge1[,133]+vince.merge1[,134])/2
X15_110.150<-vince.merge1[,135]
X15<-cbind(X15_00.20,X15_20.40,X15_40.70,X15_70.110,X15_110.150)
X15cor<-cor(X15, use = "all.obs")
X15cor

X16_00.20<-(vince.merge1[,136]+vince.merge1[,137])/2
X16_20.40<-(vince.merge1[,138]+vince.merge1[,139])/2
X16_40.70<-(vince.merge1[,140]+vince.merge1[,141])/2
X16_70.110<-(vince.merge1[,142]+vince.merge1[,143])/2
X16_110.150<-vince.merge1[,144]
X16<-cbind(X16_00.20,X16_20.40,X16_40.70,X16_70.110,X16_110.150)
X16cor<-cor(X16, use = "all.obs")
X16cor

X17_00.20<-(vince.merge1[,145]+vince.merge1[,146])/2
X17_20.40<-(vince.merge1[,147]+vince.merge1[,148])/2
X17_40.70<-(vince.merge1[,149]+vince.merge1[,150])/2
X17_70.110<-(vince.merge1[,151]+vince.merge1[,152])/2
X17_110.150<-vince.merge1[,153]
X17<-cbind(X17_00.20,X17_20.40,X17_40.70,X17_70.110,X17_110.150)
X17cor<-cor(X17, use = "all.obs")
X17cor

X18_00.20<-(vince.merge1[,154]+vince.merge1[,155])/2
X18_20.40<-(vince.merge1[,156]+vince.merge1[,157])/2
X18_40.70<-(vince.merge1[,158]+vince.merge1[,159])/2
X18_70.110<-(vince.merge1[,160]+vince.merge1[,161])/2
X18_110.150<-vince.merge1[,162]
X18<-cbind(X18_00.20,X18_20.40,X18_40.70,X18_70.110,X18_110.150)
X18cor<-cor(X18, use = "all.obs")
X18cor

X19_00.20<-(vince.merge1[,163]+vince.merge1[,164])/2
X19_20.40<-(vince.merge1[,165]+vince.merge1[,166])/2
X19_40.70<-(vince.merge1[,167]+vince.merge1[,168])/2
X19_70.110<-(vince.merge1[,169]+vince.merge1[,170])/2
X19_110.150<-vince.merge1[,171]
X19<-cbind(X19_00.20,X19_20.40,X19_40.70,X19_70.110,X19_110.150)
X19cor<-cor(X19, use = "all.obs")
X19cor

X20_00.20<-(vince.merge1[,172]+vince.merge1[,173])/2
X20_20.40<-(vince.merge1[,174]+vince.merge1[,175])/2
X20_40.70<-(vince.merge1[,176]+vince.merge1[,177])/2
X20_70.110<-(vince.merge1[,178]+vince.merge1[,179])/2
X20_110.150<-vince.merge1[,180]
X20<-cbind(X20_00.20,X20_20.40,X20_40.70,X20_70.110,X20_110.150)
X20cor<-cor(X20, use = "all.obs")
X20cor

X21_00.20<-(vince.merge1[,181]+vince.merge1[,182])/2
X21_20.40<-(vince.merge1[,183]+vince.merge1[,184])/2
X21_40.70<-(vince.merge1[,185]+vince.merge1[,186])/2
X21_70.110<-(vince.merge1[,187]+vince.merge1[,188])/2
X21_110.150<-vince.merge1[,189]
X21<-cbind(X21_00.20,X21_20.40,X21_40.70,X21_70.110,X21_110.150)
X21cor<-cor(X21, use = "all.obs")
X21cor

X22_00.20<-(vince.merge1[,190]+vince.merge1[,191])/2
X22_20.40<-(vince.merge1[,192]+vince.merge1[,193])/2
X22_40.70<-(vince.merge1[,194]+vince.merge1[,195])/2
X22_70.110<-(vince.merge1[,196]+vince.merge1[,197])/2
X22_110.150<-vince.merge1[,198]
X22<-cbind(X22_00.20,X22_20.40,X22_40.70,X22_70.110,X22_110.150)
X22cor<-cor(X22, use = "all.obs")
X22cor

X23_00.20<-(vince.merge1[,199]+vince.merge1[,200])/2
X23_20.40<-(vince.merge1[,201]+vince.merge1[,202])/2
X23_40.70<-(vince.merge1[,203]+vince.merge1[,204])/2
X23_70.110<-(vince.merge1[,205]+vince.merge1[,206])/2
X23_110.150<-vince.merge1[,207]
X23<-cbind(X23_00.20,X23_20.40,X23_40.70,X23_70.110,X23_110.150)
X23cor<-cor(X23, use = "all.obs")
X23cor

vince.merge2<-cbind(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23)
write.csv(vince.merge2,"C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\vince_m2.csv",row.names=TRUE)

v2<-princomp(vince.merge2,cor=TRUE)
vince.merge2.scores<-v2$scores
write.csv(vince.merge2.scores,"C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\vince_m2_pca.csv",row.names=TRUE)

head(vince.merge2)

X1_00.20<-vince.merge2[,1]
X1_20.70<-(vince.merge2[,2]+vince.merge2[,3])/2
X1_70.150<-(vince.merge2[,4]+vince.merge2[,5])/2

X1<-cbind(X1_00.20,X1_20.70,X1_70.150)
X1cor<-cor(X1, use = "all.obs")
X1cor

X2_00.20<-vince.merge2[,6]
X2_20.70<-(vince.merge2[,7]+vince.merge2[,8])/2
X2_70.150<-(vince.merge2[,9]+vince.merge2[,10])/2

X2<-cbind(X2_00.20,X2_20.70,X2_70.150)
X2cor<-cor(X2, use = "all.obs")
X2cor

X3_00.20<-vince.merge2[,11]
X3_20.70<-(vince.merge2[,12]+vince.merge2[,13])/2
X3_70.150<-(vince.merge2[,14]+vince.merge2[,15])/2

X3<-cbind(X3_00.20,X3_20.70,X3_70.150)
X3cor<-cor(X3, use = "all.obs")
X3cor

X4_00.20<-vince.merge2[,16]
X4_20.70<-(vince.merge2[,17]+vince.merge2[,18])/2
X4_70.150<-(vince.merge2[,19]+vince.merge2[,20])/2

X4<-cbind(X4_00.20,X4_20.70,X4_70.150)
X4cor<-cor(X4, use = "all.obs")
X4cor

X5_00.20<-vince.merge2[,11]
X5_20.70<-(vince.merge2[,22]+vince.merge2[,23])/2
X5_70.150<-(vince.merge2[,24]+vince.merge2[,25])/2

X5<-cbind(X5_00.20,X5_20.70,X5_70.150)
X5cor<-cor(X5, use = "all.obs")
X5cor

X6_00.20<-vince.merge2[,26]
X6_20.70<-(vince.merge2[,27]+vince.merge2[,28])/2
X6_70.150<-(vince.merge2[,29]+vince.merge2[,30])/2

X6<-cbind(X6_00.20,X6_20.70,X6_70.150)
X6cor<-cor(X6, use = "all.obs")
X6cor

X7_00.20<-vince.merge2[,31]
X7_20.70<-(vince.merge2[,32]+vince.merge2[,33])/2
X7_70.150<-(vince.merge2[,34]+vince.merge2[,35])/2

X7<-cbind(X7_00.20,X7_20.70,X7_70.150)
X7cor<-cor(X7, use = "all.obs")
X7cor

X8_00.20<-vince.merge2[,36]
X8_20.70<-(vince.merge2[,37]+vince.merge2[,38])/2
X8_70.150<-(vince.merge2[,39]+vince.merge2[,40])/2

X8<-cbind(X8_00.20,X8_20.70,X8_70.150)
X8cor<-cor(X8, use = "all.obs")
X8cor

X9_00.20<-vince.merge2[,41]
X9_20.70<-(vince.merge2[,42]+vince.merge2[,43])/2
X9_70.150<-(vince.merge2[,44]+vince.merge2[,45])/2

X9<-cbind(X9_00.20,X9_20.70,X9_70.150)
X9cor<-cor(X9, use = "all.obs")
X9cor

X10_00.20<-vince.merge2[,46]
X10_20.70<-(vince.merge2[,47]+vince.merge2[,48])/2
X10_70.150<-(vince.merge2[,49]+vince.merge2[,50])/2

X10<-cbind(X10_00.20,X10_20.70,X10_70.150)
X10cor<-cor(X10, use = "all.obs")
X10cor

X11_00.20<-vince.merge2[,51]
X11_20.70<-(vince.merge2[,52]+vince.merge2[,53])/2
X11_70.150<-(vince.merge2[,54]+vince.merge2[,55])/2

X11<-cbind(X11_00.20,X11_20.70,X11_70.150)
X11cor<-cor(X11, use = "all.obs")
X11cor

X12_00.20<-vince.merge2[,56]
X12_20.70<-(vince.merge2[,57]+vince.merge2[,58])/2
X12_70.150<-(vince.merge2[,59]+vince.merge2[,60])/2

X12<-cbind(X12_00.20,X12_20.70,X12_70.150)
X12cor<-cor(X12, use = "all.obs")
X12cor

X13_00.20<-vince.merge2[,61]
X13_20.70<-(vince.merge2[,62]+vince.merge2[,63])/2
X13_70.150<-(vince.merge2[,64]+vince.merge2[,65])/2

X13<-cbind(X13_00.20,X13_20.70,X13_70.150)
X13cor<-cor(X13, use = "all.obs")
X13cor

X14_00.20<-vince.merge2[,66]
X14_20.70<-(vince.merge2[,67]+vince.merge2[,68])/2
X14_70.150<-(vince.merge2[,69]+vince.merge2[,70])/2

X14<-cbind(X14_00.20,X14_20.70,X14_70.150)
X14cor<-cor(X14, use = "all.obs")
X14cor

X15_00.20<-vince.merge2[,71]
X15_20.70<-(vince.merge2[,72]+vince.merge2[,73])/2
X15_70.150<-(vince.merge2[,74]+vince.merge2[,75])/2

X15<-cbind(X15_00.20,X15_20.70,X15_70.150)
X15cor<-cor(X15, use = "all.obs")
X15cor

X16_00.20<-vince.merge2[,76]
X16_20.70<-(vince.merge2[,77]+vince.merge2[,78])/2
X16_70.150<-(vince.merge2[,79]+vince.merge2[,80])/2

X16<-cbind(X16_00.20,X16_20.70,X16_70.150)
X16cor<-cor(X16, use = "all.obs")
X16cor

X17_00.20<-vince.merge2[,81]
X17_20.70<-(vince.merge2[,82]+vince.merge2[,83])/2
X17_70.150<-(vince.merge2[,84]+vince.merge2[,85])/2

X17<-cbind(X17_00.20,X17_20.70,X17_70.150)
X17cor<-cor(X17, use = "all.obs")
X17cor

X18_00.20<-vince.merge2[,86]
X18_20.70<-(vince.merge2[,87]+vince.merge2[,88])/2
X18_70.150<-(vince.merge2[,89]+vince.merge2[,90])/2

X18<-cbind(X18_00.20,X18_20.70,X18_70.150)
X18cor<-cor(X18, use = "all.obs")
X18cor

X19_00.20<-vince.merge2[,91]
X19_20.70<-(vince.merge2[,92]+vince.merge2[,93])/2
X19_70.150<-(vince.merge2[,94]+vince.merge2[,95])/2

X19<-cbind(X19_00.20,X19_20.70,X19_70.150)
X19cor<-cor(X19, use = "all.obs")
X19cor

X20_00.20<-vince.merge2[,96]
X20_20.70<-(vince.merge2[,97]+vince.merge2[,98])/2
X20_70.150<-(vince.merge2[,99]+vince.merge2[,100])/2

X20<-cbind(X20_00.20,X20_20.70,X20_70.150)
X20cor<-cor(X20, use = "all.obs")
X20cor

X21_00.20<-vince.merge2[,101]
X21_20.70<-(vince.merge2[,102]+vince.merge2[,103])/2
X21_70.150<-(vince.merge2[,104]+vince.merge2[,105])/2

X21<-cbind(X21_00.20,X21_20.70,X21_70.150)
X21cor<-cor(X21, use = "all.obs")
X21cor

X22_00.20<-vince.merge2[,106]
X22_20.70<-(vince.merge2[,107]+vince.merge2[,108])/2
X22_70.150<-(vince.merge2[,109]+vince.merge2[,110])/2

X22<-cbind(X22_00.20,X22_20.70,X22_70.150)
X22cor<-cor(X22, use = "all.obs")
X22cor

X23_00.20<-vince.merge2[,111]
X23_20.70<-(vince.merge2[,112]+vince.merge2[,113])/2
X23_70.150<-(vince.merge2[,114]+vince.merge2[,115])/2

X23<-cbind(X23_00.20,X23_20.70,X23_70.150)
X23cor<-cor(X23, use = "all.obs")
X23cor

vince.merge3<-cbind(X1,X2,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23)
write.csv(vince.merge3,"C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\vince_m3.csv",row.names=TRUE)
write.csv(vince.merge3,"C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\vince_m3_am.csv",row.names=FALSE)
write.table(vince.merge3,"C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\vince_m3_am.csv",row.names=FALSE,col.names=TRUE)

v3<-princomp(vince.merge3,cor=TRUE)
vince.merge3.scores<-v3$scores
write.csv(vince.merge3.scores,"C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\vince_m3_pca.csv",row.names=TRUE)
write.csv(vince.merge3.scores,"C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\vince_m3_pca_am.csv",row.names=FALSE)

setwd("C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50")
find.k()
setwd("C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\50_90")
find.k()
# levels(vince$X.NO.of.profiles.5.) <- c(levels(vince$X.NO.of.profiles.5.),'mean')
# 
# vince.mean<-rbind(vince,vmeans)
# dist(vince.mean,method="euclidean")
# 
# vince.mean<-vince
# vince.mean[300,]<-vmeans

source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/akro.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/akro.write.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/meson.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/weighting.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/mp.plot.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/point_euclid.r")

# file<-read.csv("USnoa20170709_60065.csv",sep=",", header=T)
# aaaa<-am(file)

#write.table(akro.parameters,"control.txt",row.names=F,col.names=F)  
k.out<-read.csv("C:/Users/phug7649/Desktop/kmeans/Vince_centroids/5_50/f1.05 17_class.txt",header=T,sep="")
k.out<-read.csv("C:/Users/phug7649/Desktop/kmeans/Vince_centroids/5_50/cent1_05_17.csv",header=T,sep="")
cent1_05_17
file<-read.csv("C:\\Users\\phug7649\\Desktop\\kmeans\\Vince_centroids\\5_50\\vince_m3_am.csv",header=T,sep="")
noa.200<-file
##seeing if the function works.
object<-akro(noa.200,20,1.06,3,30)#,factor=0.6,YScrit=9)##breaks at 0.8
#object <- akro(noa.200,35,1.2,3,1,factor=0.35)
#objectII<- akro(noa.200,38,1.3,3,2000)
akro.write(object)
check<-meson()
weight<-weighting(check)
I<-check[['centroids.complete']]
II<-check[['data.complete']]
III<-check[['data_ratio']]
mp.plot(check,weight)


library(rgl)
with(check$data.complete,plot3d(Comp.1,Comp.2,Comp.3, main="clusters and components",col=max, size=1))
{Sys.sleep(2)
 start <- proc.time()[3]
 while ((i <- 50*(proc.time()[3]-start)) < 360) rgl.viewpoint(i,1/6) 
}

fuzzyk<-read.csv("C:/Users/phug7649/Desktop/kmeans/Vince_centroids/5_50/f1.05 17_class.txt",sep="")

fuzzyk.ci<-fuzzyk[order(fuzzyk$CI),]
vince.merge3.scores.fuz<-cbind(vince.merge3.scores,fuzzyk)
plot3d(vince.merge3.scores.fuz$Comp.1,vince.merge3.scores.fuz$Comp.2,vince.merge3.scores.fuz$Comp.3,col=vince.merge3.scores.fuz$MaxCls,size=10)
plot3d(vince.merge3.scores.fuz$Comp.1,vince.merge3.scores.fuz$Comp.2,vince.merge3.scores.fuz$Comp.3,col=soil12,size=10)
group.vs.fuzzy<-cbind(vince.merge3.scores.fuz,great.groups)
group.vs.fuzzy

full.comparison<-cbind(vince$X.NO.of.profiles.5.,group.vs.fuzzy)
full.comp<-cbind(as.character(full.comparison$'vince$X.NO.of.profiles.5.'),full.comparison$great.group,full.comparison$MaxCls)
full.comp[order(full.comp[,3]), ]
ordered.orders<-full.comp[order(full.comp[,3]), ]
ordered.orders

for (i in 1:17)
{i<-subset(ordered.orders,ordered.orders[,3]==i)
 subset(group.vs.fuzzy,group.vs.fuzzy$great.group==1) 
  
}
g1<-subset(ordered.orders,ordered.orders[,3]==1)
g2<-subset(ordered.orders,ordered.orders[,3]==2)
g3<-subset(ordered.orders,ordered.orders[,3]==3)
g4<-subset(ordered.orders,ordered.orders[,3]==4)
g5<-subset(ordered.orders,ordered.orders[,3]==5)
g6<-subset(ordered.orders,ordered.orders[,3]==6)
g7<-subset(ordered.orders,ordered.orders[,3]==7)
g8<-subset(ordered.orders,ordered.orders[,3]==8)
g9<-subset(ordered.orders,ordered.orders[,3]==9)
g11<-subset(ordered.orders,ordered.orders[,3]==10)
g12<-subset(ordered.orders,ordered.orders[,3]==12)
g13<-subset(ordered.orders,ordered.orders[,3]==13)
g14<-subset(ordered.orders,ordered.orders[,3]==14)
g15<-subset(ordered.orders,ordered.orders[,3]==15)
g16<-subset(ordered.orders,ordered.orders[,3]==16)
g17<-subset(ordered.orders,ordered.orders[,3]==17)

write.csv(ordered.orders,"ordered.csv")

# library(plyr)
# summary_soils <- setNames(ddply(group.vs.fuzzy, "great.group", summarise, table(MaxCls)), c("group", "count"))
# matrix(summary_soils$count, nrow=nlevels(as.factor(summary_soils$group)), byrow=TRUE)

alfisols<-subset(group.vs.fuzzy,group.vs.fuzzy$great.group==1)
ultisols<-subset(group.vs.fuzzy,group.vs.fuzzy$great.group==2)
vertisols<-subset(group.vs.fuzzy,group.vs.fuzzy$great.group==3)
mollisols<-subset(group.vs.fuzzy,group.vs.fuzzy$great.group==4)
inceptisols<-subset(group.vs.fuzzy,group.vs.fuzzy$great.group==5)
andisols<-subset(group.vs.fuzzy,group.vs.fuzzy$great.group==6)
oxisols<-subset(group.vs.fuzzy,group.vs.fuzzy$great.group==7)
gelisols<-subset(group.vs.fuzzy,group.vs.fuzzy$great.group==8)
histosols<-subset(group.vs.fuzzy,group.vs.fuzzy$great.group==9)
aridosols<-subset(group.vs.fuzzy,group.vs.fuzzy$great.group==10)
entisols<-subset(group.vs.fuzzy,group.vs.fuzzy$great.group==11)
spodosols<-subset(group.vs.fuzzy,group.vs.fuzzy$great.group==12)

plot3d(alfisols$Comp.1,alfisols$Comp.2,alfisols$Comp.3,col=alfisols$MaxCls)
hist(table(alfisols$MaxCls))
with(ultisols,plot3d(Comp.1,Comp.2,Comp.3,col=MaxCls))

for.image<-rbind(
  table(alfisols$MaxCls),
  table(ultisols$MaxCls),
  table(vertisols$MaxCls),
  table(mollisols$MaxCls),
  table(inceptisols$MaxCls),
  table(andisols$MaxCls),
  table(oxisols$MaxCls),
  table(gelisols$MaxCls),
  table(histosols$MaxCls),
  table(aridosols$MaxCls),
  table(entisols$MaxCls),
  table(spodosols$MaxCls))
soils<-c("alfisols","ultisols","vertisols","mollisols","inceptisols",
         "andisols","oxisols","gelisols","histosols","aridisols","entisols","spodosols")
row.names(for.image)<-soils
sums<-(for.image[,1]+for.image[,2]+for.image[,3]+for.image[,4]+for.image[,5]+for.image[,6]
       +for.image[,7]+for.image[,8]+for.image[,9]+for.image[,10]+for.image[,11]
       +for.image[,12]+for.image[,13]+for.image[,14]+for.image[,15]+for.image[,16]+for.image[,17])

danger<-cbind(for.image,sums)
#for.image<-cbind(soils,for.image)
#image(t(for.image),ylim=1:12)

value<-as.numeric(for.image[,2]+for.image[,3]+for.image[,4]+for.image[,5]+for.image[,6]+for.image[,7]+for.image[,8]+for.image[,9]+for.image[,10]+for.image[,11]+for.image[,12])
                  
ordersum<-as.numeric(for.image[,2])+as.numeric(for.image[,3])+as.numeric(for.image[,4])+as.numeric(for.image[,5])+as.numeric(for.image[,6])+as.numeric(for.image[,7])+as.numeric(for.image[,8])+as.numeric(for.image[,9])+as.numeric(for.image[,10])+as.numeric(for.image[,11])+as.numeric(for.image[,12])
for.image$ordersum<-ordersum

##alex requested a simplified centroid table. Here is the code.

cent.105.17<-read.csv("C:/Users/phug7649/Desktop/kmeans/Vince_centroids/5_50/cent1_05_17.csv", header=T,sep=",")
mean<-colMeans(cent.105.17[2:ncol(cent.105.17)])
sx<-cov(cent.105.17[2:ncol(cent.105.17)])
D2<-mahalanobis(cent.105.17[2:ncol(cent.105.17)],mean,sx)

library(ape)

hc.all<-hclust(dist(cent.105.17[c(2:ncol(cent.105.17))]), "ward")
pdf("C:\\Users\\phug7649\\Desktop\\TXTBIN\\cent10517.pdf", width=40, height=15)
plot(hc.all, hang=-1,labels=cent.105.17[,1],main="Dendrogram of all the clusters")
dev.off()
library(ape)

scaled.cent.105.17<-scale(cent.105.17[,2:ncol(cent.105.17)])
dist.scaled.cent.105.17<- dist(scaled.cent.105.17)

hc.all<-hclust(dist(vince[c(2:ncol(vince))]), "ward")
pdf("C:\\Users\\phug7649\\Desktop\\TXTBIN\\vdendraw.pdf", width=40, height=15)
plot(hc.all, hang=-1,labels=vince[,1],main="Dendrogram of all the clusters using raw data")
dev.off()

k.out.double=rbind(k.out,k.out)
plotting<-cbind(plotting,k.out.double$MaxCls)
colnames(plotting)[4]<-"clusters"
head(plotting)

##the reset button
plotting<-plotting[,1:3]

pdf("C:\\Users\\phug7649\\Desktop\\TXTBIN\\17xplot.pdf", width=40, height=15)
ggplot(plotting, aes(x=prin1, y=prin2,colour=clusters)) +
  geom_point(shape = 20,size=10,alpha=.5)

dev.off()

##Adding centroid data to the overall dataset.
colnames(vince.merge3)<-colnames(cent.105.17)
vince.merge3$class<-k.out$MaxCls


pc.reduced<-rbind(cent.105.17,vince.merge3)

##making principal components

comps<-princomp(pc.reduced[,2:ncol(pc.reduced)],cor=TRUE)
p.red.scores<-comps$scores
pc.reduced<-cbind(pc.reduced,p.red.scores)

##covariance matrix

covar.pc<-cov(p.red.scores[,2:4])

##plotting data with centroids

pdf("C:\\Users\\phug7649\\Desktop\\TXTBIN\\17xfancyplot.pdf", width=40, height=15)
ggplot(pc.reduced, aes(x=Comp.1, y=Comp.2,colour=class)) +
  theme_bw()+
  geom_point(shape = 20,size=10,alpha=.5)+
  #geom_point(data=pc.reduced[1:17,],aes(x=Comp.1,y=Comp.2),colour="black",size=100,alpha=.05,inherit.aes=FALSE)+
  geom_point(data=pc.reduced[1:17,],aes(x=Comp.1,y=Comp.2,colour=class),size=10,shape=2,inherit.aes=FALSE)
dev.off()

pdf("C:\\Users\\phug7649\\Desktop\\TXTBIN\\17xfancyplotII.pdf", width=40, height=15)
ggplot(pc.reduced, aes(x=Comp.1, y=Comp.2,colour=class)) +
  theme_bw()+
  geom_point(shape = 20,size=10,alpha=.5)+
  #geom_point(data=pc.reduced[1:17,],aes(x=Comp.1,y=Comp.2),colour="black",size=100,alpha=.05,inherit.aes=FALSE)+
  geom_text(data=pc.reduced[1:17,],aes(x=Comp.1,y=Comp.2,label=class),size=10,inherit.aes=FALSE)
dev.off()
vince.ct<-vince[,2:ncol(vince)]
cent.thin(rbind(vince.ct,vince.ct))
#czrsum<-rowSums(vince)
