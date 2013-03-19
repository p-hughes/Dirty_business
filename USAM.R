##this script is to create a complete set of records for the US data. 


##################################################################################################
###  IT IS VITALLY important that I name each column, rather than using the column number!!!!  ###
##################################################################################################


setwd("C:/Users/phug7649/Desktop/txtbin")
library(ggplot2)



ph<-read.csv("phinfo.txt")
sum(duplicated(ph$natural_key))
ec<-read.csv("ec.txt")
anyDuplicated(as.character(ec$natural_key))
cec<- read.csv("CEC.txt")
anyDuplicated(as.character(cec$natural_key))
horizon<-read.csv("DEPTH.txt")
anyDuplicated(as.character(horizon$natural_key))
LNkey<-read.csv("LNkey.txt")##this guy may be the rosetta stone of the data
anyDuplicated(as.character(LNkey$natural_key))
colour<-read.csv("colour.txt")
sum(duplicated(colour$natural_key))

## Duplicates found, I should go to the source and correct that but I am taking the cowards way out...

head(colour)
sub_colour<-colour$natural_key
nodup_colour <- which(!duplicated(sub_colour))
colour_no_dup <- colour[nodup_colour,]
sum(duplicated(colour_no_dup))
colour<-colour_no_dup
rm(colour_no_dup)
##pulling the cieLAB data out of the munsell information 
colour<-colour[,c(1,2,3,10,11,12)]




psa<-read.csv("PSA.txt")
anyDuplicated(as.character(psa$natural_key))
carbon<-read.csv("carbon.txt")
anyDuplicated(as.character(carbon$natural_key))
Nitrogen<-read.csv("nitrogen.txt")
anyDuplicated(as.character(Nitrogen$natural_key))##I may not need nitrogen, so leave well alone!!!
## v This data may be obsolete. I will add each column manually. 
#data<-read.csv("phil1.txt")
#data<-merge(data,ec, by="natural_key", all=TRUE )
#data<-(unique(data))

##some research on the nasis pH data

##there is a ph value of 35 in here. I need to remove this at some stage
##I suggest its a missing full stop. 3.5 makes way more sense than 35.
#regression for Cacl2~KCl 

lmph<-lm(ph[,4]~ph[,2])
#Regression inversed
ilmph<-lm(ph[,2]~ph[,4])
summary(lmph)
#getting the regression formula
ilmph

##Simple plot
plot(ph[,2],ph[,4],xlim=c(0,15),ylim=c(0,15),xlab="CaCl2", ylab="KCl", main="pH measurement in different media")
abline(lmph, col="red")

##Hex plot of pH
png("hextbin.png", type="cairo", width=4196*2, height=2048*2)
d <- ggplot(ph, aes(x = ph_cacl2, y = ph_kcl))
d + geom_hex(binwidth = c(0.25, 0.25)) +
  stat_smooth(method="lm", colour="red", size=5)+
  coord_equal()+
  theme_bw()+
  labs(x=expression(pH~(CaCl[2])), y=expression(ph~(KCl)), fill="Frequency\n") + 
  theme(axis.title=element_text(size=72), 
        legend.text=element_text(size=72), 
        legend.key.size=unit(10, "cm"), 
        legend.title=element_text(size=80),
        axis.text=element_text(size=50))
dev.off()

##I may need to import the carbonates as a seperate column, then do a funky heat map like in the pH, then perform a regression.
##Hex plot of carbonates
pdf("carbhex.pdf")
d <- ggplot(ph, aes(y = caco3, x = ph_h2o))
d + geom_hex(binwidth = c(.4, 4)) +
  coord_cartesian(ylim = c(-0, 100))+ 
  theme_bw()
 dev.off()


##Applying regression to data to estimate pH
y=ifelse(is.na(ph[,2]),0.5995 + 0.9735*ph[,4],ph[,2])
ph[,2]<-y
hist(y) ##Still need to get rid of that dodgy pH value!
kcl_reg<-na.exclude(y)

##Merging ec data so pH can be accurately estimated from CaCl2
ph_ec<-merge(ph,ec, by="natural_key", all=TRUE )

##Adding sp data
#replace missing pH values (1:5 in water) with paste pH values (which are roughly equivalent). 

y=ifelse(is.na(ph_ec[,3]),ph_ec[,6],ph[,3])
ph_ec[,3]<-y
head(ph_ec)
sp_ph<-na.exclude(y)

##estimating pH from Caco3 and ec
#first, NA needs to be converted to 1 so absence of carbonates will be ignored, rather than returning NA
y=ifelse(is.na(ph_ec[,9]),1,ph_ec[,9])
check<-na.exclude(y)##check step. "check" should be the identical to y.
identical(check,y)
ph_ec[,9]<-y

##Next: convert CaCl2 pH to water pH via budi's equation.
y=ifelse(is.na(ph_ec[,3]),(ph_ec[,2]+0.5-0.14*log(ph_ec[,9]))/.9,ph_ec[,3])
ec_ph<-na.exclude(y)
ph_ec[,3]<-y

## Creating a usable Caco3 column.
y=ifelse(is.na(ph_ec[,7]),ifelse(ph_ec[,3]<7.5,0,NA),ph_ec[,7])
caco3<-na.exclude(y)
ph_ec[,7]<-y

##Merging CEC data with the rest of it..
CEC<-na.exclude(cec)
ph_ec_cec<-merge(ph_ec,CEC, by="natural_key", all=TRUE )

##checking to see how much usable data is available
cec_check<-ph_ec_cec[,c(3,7,10)]
c_check<-na.exclude(cec_check)
rowloss<-nrow(cec_check)-nrow(c_check)

ph_ec_cec<-as.data.frame(ph_ec_cec)
horizon<-as.data.frame(horizon)

ph_ec_cec_horizon<-merge(ph_ec_cec,horizon, by= "natural_key", all= TRUE)

## merging colour data to see what we have left. The nasis database has proved itself to be particularly unhelpful.
## ID columns can be merged to create a common row id, but not knowing if a designation is common, dupicated or missing 
## creates real hassles when trying to create massive data sets from different areas. 

col_pech<-merge(ph_ec_cec_horizon,colour, by="natural_key", all=TRUE)
##check step
head(col_pech)
combined<-col_pech[,c(1,3,7,10,11,15:17)]
comcheck<-na.exclude(combined)
tex_pechc<-merge(col_pech,psa, by="natural_key", all=TRUE)

##fixing texture data by composition
head(tex_pechc)

y=ifelse(is.na(tex_pechc[,18]),100-tex_pechc[,19]+tex_pechc[,20],tex_pechc[,18])
tex_pechc[,18]<-y

y=ifelse(is.na(tex_pechc[,19]),100-tex_pechc[,18]+tex_pechc[,20],tex_pechc[,18])
tex_pechc[,19]<-y

y=ifelse(is.na(tex_pechc[,20]),100-tex_pechc[,19]+tex_pechc[,18],tex_pechc[,20])
tex_pechc[,20]<-y
##check step
head(tex_pechc)
combined<-tex_pechc[,c(1,3,7,10,11,15:20)]
comcheckII<-na.exclude(combined)

## check for duplicates please
## add carbon data please

##consultation with experts indicates that if the pH is below 7.5 then all of the carbon in the profile should be organic.

##creating a c-oc ratio to plot against pH

##estimating carbon using nitrogen and c_n ratio
#cn<-merge(carbon,Nitrogen, by="natural_key")

#head(cn)
y=ifelse(is.na(carbon[,"oc"]),carbon[,"c_n_ra"]*carbon[,"n_tot"],carbon[,"oc"])
a<-carbon[,"oc"]
sum(is.na(a))
sum(is.na(y))
carbon$oc<-y

head(carbon)
carbon$c_ratio<-carbon[,"c_tot"]/(carbon[,"c_tot"]+carbon[,"oc"])
cph<-merge(carbon,ph_ec, by= "natural_key", all= TRUE)
head(cph)
  
plot(cph$ratio,cph$ph_h2o)

lmcph<-lm(cph[,3]~cph[,2])
lmcphi<-lm(cph[,2]~cph[,3])
plot(cph[,2],cph[,3])
abline(lmcph,col="red")
abline(lmcphi,col="blue")

pdf("chex.pdf")
d <- ggplot(cph, aes(y = ph_h2o, x = c_ratio))
d + geom_hex(binwidth = c(.06, .6)) +
  coord_cartesian(ylim = c(-0, 14))+ 
  theme_bw()
dev.off()

pdf("cchex.pdf")
d <- ggplot(cph, aes(y = c_tot, x = oc))
d + geom_hex(binwidth = c(7,7)) +
  coord_cartesian(ylim = c(-0, 90))+ 
  theme_bw()
dev.off()
head(cph)

# Multiple linear regression prediction (oc ~ carbon and pH)
# > stackloss.lm = lm(stack.loss ~ 
#                       +     Air.Flow + Water.Temp + Acid.Conc., 
#                     +     data=stackloss)
oc.pred<-lm(cph$oc~
              +cph$c_tot
            +cph$ph_h2o)
summary(oc.pred)
oc.pred
y<-0.04936+(1.08923*cph$c_tot)+ (-0.09031*cph$ph_h2o)  


#This produces results that sometimes dive below zero and climb above the maximum total carbon values
#Make negative values zero and values above the maximum equal to the maximum.

z=ifelse(y>cph$c_tot,cph$c_tot,y)
y=ifelse(z<0,0,z)
z=ifelse(is.na(cph$oc),y,cph$oc)
zna<-na.exclude(z)
cph$oc<-z
head(cph)


carb<-carbon[,c("natural_key","oc")]

carb_tpechc<-merge(tex_pechc,carb, by="natural_key", all= TRUE)
head(carb_tpechc)

check<-carb_tpechc[,c(1,3,7,10,11,15:21)]
checkII<-na.exclude(check)
anyDuplicated(as.character(checkII$natural_key))
sum(duplicated(checkII$natural_key))

##a small number of duplicates found. Taking the cowards way out again.

sub_checkII<-checkII$natural_key
nodup_checkII <- which(!duplicated(sub_checkII))
checkII_no_dup <- checkII[nodup_checkII,]
sum(duplicated(checkII_no_dup))
checkII<-checkII_no_dup
rm(checkII_no_dup)

##subsetting
head(checkII)

# Just 0
subset0 <- subset(checkII, hzn_top.x==0)
# Between 0 and 5 
subset0_5 <- subset(checkII, hzn_top.x >= 0 & hzn_top.x < 5) 

##preparing for fuzzy k algorithm. No extraneous columns, keep an id column.
subset0_5<-subset0_5[,-5]

##outputting data into a csv
write.csv(subset0_5, "USII_0_5.csv")

##reading in the results from FKM in its appropriate directory

setwd("C:/Users/phug7649/Desktop/kmeans/Paper_2/0-5")

clusfind<-read.csv("summary.txt",sep="", header=TRUE)
head(clusfind)
# 3D Scatterplot
install.packages("scatterplot3d")
library(scatterplot3d)
attach(clusfind)
scatterplot3d(Phi,Class,FPI, main="3D Scatterplot")

# Spinning 3d Scatterplot
library(rgl)
attach(clusfind)
plot3d(Class,FPI,Phi, main="FKM~phi,FPI and class", col="red", size=3)

#from this plot, phi of 1.25, with 4 major classes may be the best. This means that we will move on to AM, but first 
#the principal component issue needs to be resolved.
               






## Consider using the nitrogen data as a guide to fill out the carbon column. It seems as though the CN ratio only applies
## to the OC column. 

##today:
##Use regression to turn munsell into cie lab.




##select out top horizons
##use fuzzy k to estimate number of clusters:
#Cluster estimation can go either by nested clusters or total clusters.

##subsetting for regression in jmp



# plot(oc~y,data=cphy)
# a<-cbind(cphy$oc,cphy$y)
# a<-na.exclude(a)
# plot(a)
# plot(caco3~oc,data=cphy)
###########################################################################################################################





#y is a bin column which will contain the results of this action.


#y is a bin column which will contain the results of this action.




##Using composition to ensure the sand/silt clay fraction is as complete as possible
# y=ifelse(is.na(data[,11]),100-data[,12]+data[,13],data[,11])
# data[,11]<-y
# 
# y=ifelse(is.na(data[,12]),100-data[,11]+data[,13],data[,12])
# data[,12]<-y
# 
# y=ifelse(is.na(data[,13]),100-data[,12]+data[,11],data[,13])
# data[,13]<-y
# 
# #check step
# data <- data[,c(1,4,6:8,11:13,26:31)]
# check<-na.exclude(data)
# 
# ##have a look at the CN ratio. May be able to pick up missing C or N values.
# 
# head(data)
# plot(data[,6],data[,4])
# 
# 
# 
# 
# 
# 
# # a<-ph[,1:2]
# # b<-ph[,4]
# # c<-cbind(a,b)
# # d<-na.exclude(c)
# # ##adding each data column together to see how much extra data a linear model would produce
# # y=ifelse(is.na(c[,2]),c[,3],c[,2])
# # e<-na.exclude(y)
# 
# 
# # ##Estimating 
# # plot(ph[,4],ph[,2],xlim=c(0.5995,15),ylim=c(0,15),ylab="CaCl2", xlab="KCl", main="pH measurement in different media")
# # abline(ilmph, col="red")
# 
# 
# 















