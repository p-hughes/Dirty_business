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

###################################################################################################################
##                                       ## Checkpoint -- 11 gold get ##                                         ##
###################################################################################################################

setwd("C:/Users/phug7649/Desktop/txtbin")
subset0_5<-read.csv("USII_0_5.csv")
##this data needs the silt fraction removed. This may cause problems down the line
one<-subset0_5[,1:9]
two<-subset0_5[,11:12]
subset0_5<-cbind(one,two)


##reading in the results from FKM in its appropriate directory

setwd("C:/Users/phug7649/Desktop/kmeans/Paper_2/0-5")

clusfind<-read.csv("summary.txt",sep="", header=TRUE)
head(clusfind)

# Spinning 3d Scatterplot
library(rgl)
attach(clusfind)
redblue.colors <- colorRampPalette(c("red", "blue"))
contrast<-colorRampPalette(c("red","blue","green","red","blue","green"))
plot3d(Class,FPI,Phi, main="FKM~phi,FPI and class",col=rep(contrast(6), each=34) , size=3)
plot3d(Class,FPI,Phi, main="FKM~phi,FPI and class",col=rep(redblue.colors(6), each=34) , size=3)
#col=rainbow(500)
#from this plot, phi of 1.25, with 4 major classes may be the best. This means that we will move on to AM, but first 
#the principal component issue needs to be resolved.

##a correlation matrix needs to be used, not a covariance matrix.
##Code is:
##result<-princomp(x,cor=TRUE)
##comps<-result$scores

classes<-read.csv("f1.25 11_class.txt",sep="")


##how do you rename a header? heres how!
names(classes)[1]<-"natural_key"

class_subset0_5<-merge(subset0_5,classes, by= "natural_key",all=TRUE)
##you must be vewwy vewwy qwiet, im hunting outliers!
a<-which.max(class_subset0_5[,"ph_h2o"])
class_subset0_5[a,"ph_h2o"]
hist(class_subset0_5[,"ph_h2o"])
##outliers must have been culled earlier... checked the data is complete. On with your scheduled number crunching..
##making principal components from the data...
head(subset0_5)
y<-ncol(subset0_5)

##should have used "row.names=FALSE when making this csv. I will fix the problem later.
subset0_5<-subset0_5[,2:y]# remove this when the issue is fixed.
y<-ncol(subset0_5)
a<-princomp(subset0_5[,2:y], cor=TRUE)
prin0_5<-a$scores
csprin0_5<-cbind(class_subset0_5,prin0_5)


####################################################################################################################
#################################### Time to use the script found in EMII.r ########################################
####################################################################################################################

#I need to make a function out of this...

############################################# THE CONVEX BICYCLE ###################################################

##A script made to identify a small number of points around the periphery of a data cloud. This should coincide with
##the location of end members. It works by creating an n-dimensional convex hull (thanks seb),finding a point with a 
##maximum distance from zero then finding the maximum distance of this point from all other points in the hull.

####################################################################################################################





##constructing the dataset. Required: 1 column (column.30 with the components arranged after that.)
Column.30<-1:nrow(prin0_5)
z<-cbind(Column.30,prin0_5)
z<-z[,2:ncol(z)]

##scripts required for this algorithm to work...
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/point_euclid.R")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/qhull_algorithm.R")

################################################# control panel ####################################################
## there are two control methods atm; the first is to define the length of the yardstick. Provides an undefined number
## of end-members. the second is to use an equation which most likely is data specific.
ys<-10      ##starting parameter for yardstick
factor<-.52  ##creating the factor by which the yardstick length is modified (previous run was 0.8)
YScrit<-3.1   ##Stopping criteria; when the overall size of the hull is less than this, the algorithm stops.
####################################################################################################################
rm(bin)
file.create("bin.csv")##creating a file to dump values
bin<-c()
cz<-quick_hull(z)##Using sebs script to create hulls 


while (ys>YScrit)##I want the loop to start here
  
{
  
  czr<-z[cz,]##sum of rows
  czr<-czr^2
  czrsum<-rowSums(czr)
  fin<-sqrt(czrsum)
  finm<-as.matrix(fin)      
  refmax<-which.max(finm)##rows with max and min euclidean distance from zero 
  BLARG<-as.data.frame(z)[cz[refmax],]##getting maximum value and anchoring it to the row number in the master data set (z)    
  rowx<-BLARG##retrieving all the principal component data from rows that contain maximum and minimum euclidean distances  
  object<-z[cz,] ## retrieving all pc data from cz    
  pcdist<-as.matrix(point_euclid(object,rowx))##getting distances 
  b<-as.numeric(pcdist[which.max(pcdist),])##max distance
  ys<-b*factor##yardstick
  new <- ys < as.vector(pcdist)##compare yardstick to the convex hull  
  or <- cz[which(pcdist == 0)]##Placing maximum (maxi) and minimum (origin) points in the final file
  bin <- rbind(or,bin)    
  cz <- cz[which(new)]##Exclude any values inferior to yardstick (this file should be renamed cz when its time to reiterate)  
  print(ys) #print the yardstick value to see if the script is running  
  
}
paste0("your algorithm has returned ",nrow(bin), " end points")
paste0("Yardstick factor is ",factor,","," stopping criterion is ",YScrit)
ys<-10
####################################################################################################################


##plotting components

bincomp<-z[bin,]
plot(bincomp[,1],bincomp[,2])
plot3d(bincomp[,1],bincomp[,2],bincomp[,3],size=10)
plot3d(bincomp[,4],bincomp[,5],bincomp[,6])
plot3d(bincomp[,7],bincomp[,8],bincomp[,9])
plot3d(bincomp[,10],bincomp[,2],bincomp[,1])

##plotting hull

cz<-quick_hull(z)
head(cz)
test<-z[cz,]
head(test)
plot(test[,1],test[,2])
plot3d(test[,1],test[,2],test[,3])

z3<-z[,1:3]
cz<-quick_hull(z3)
test<-z[cz,]
plot3d(test[,1],test[,2],test[,3])

z4_6<-z[,4:6]
cz<-quick_hull(z4_6)
test<-z[cz,]
plot3d(test[,1],test[,2],test[,3])

##creating an identity matrix
matrix<-diag(nrow(bin))
##creating end point matrices
points<-subset0_5[bin,]
verify<-cbind(bin,points)

#creating control file
crow1<-c("weights","phi","nend","nclass")
crow2<-c(w,p,ncol(bin),total)
#writing files

setwd("C:\\Users\\phug7649\\Documents\\MATLAB")


write.table(matrix,"matrix.csv",row.names=FALSE,col.names=FALSE,sep=",")
write.csv(points,"EP.csv",row.names=FALSE)
write.csv(subset0_5,"DATA.csv",row.names=FALSE)

 checkdata<-read.csv("edg_2072_ep_k_2072_II.csv")
 head(checkdata)
 data<-read.csv("DATA.csv")
 head(data)
 checkep<-read.csv("edg_2072_ep_k_5_II.csv")
 head(checkep)
 ep<-read.csv("EP.csv")
 head(ep)
 checkmatrix<-read.csv("edg_2073_ep_k_id.csv")
 head(checkmatrix)
 matII<-read.csv("matrix.csv")
 head(matII)

Clusters<-11

message(paste0("nclass needs to be ", Clusters+nrow(bin)))

shell("matlab -nodesktop -nosplash -wait -r rep")














