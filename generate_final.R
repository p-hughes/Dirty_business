##This script creates a data set to test various clustering algorithms. It first creates an imaginary cluster set which
##can be exported to each cluster program. The results from each cluster trial are then re-imported back into the script
##to be plotted and analysed.


##setting working directory, importing the neccesary scripts.

setwd("C:/Users/phug7649/Desktop/TXTBIN")
source("./R-scripts/functions/qhull_algorithm.R")
library(ggplot2)
library(grid)

##for consistent results, set the seed.
set.seed(20120927)


##Alex/budis suggestion- A square data set.

de <- 2
points <- 5
ends <- 1

f <- data.frame(id="Extreme point",x=rnorm(ends, 5,de), y=rnorm(ends, 5,de))
g <- data.frame(id="Extreme point",x=rnorm(ends, 100,de), y=rnorm(ends, 5,de))
h <- data.frame(id="Extreme point",x=rnorm(ends, 5,de), y=rnorm(ends, 95,de))
i <- data.frame(id="Extreme point",x=rnorm(ends, 95,de), y=rnorm(ends, 95,de))
#i <- data.frame(id="EM4",x=130, y=130)

a <- data.frame(id="Cluster a",x=rnorm(points, 50,de), y=rnorm(points, 25,de))
b <- data.frame(id="Cluster b",x=rnorm(points, 25,de), y=rnorm(points, 50,de))
c <- data.frame(id="Cluster c",x=rnorm(points, 50,de), y=rnorm(points, 50,de))
d <- data.frame(id="Cluster d",x=rnorm(points, 75,de), y=rnorm(points, 50,de))
e <- data.frame(id="Cluster e",x=rnorm(points, 50,de), y=rnorm(points, 75,de))

w <- data.frame(id="Outlying point",x=rnorm(1, 80,de), y=rnorm(1, 15,de))
y <- data.frame(id="Outlying point",x=rnorm(1, 20,de), y=rnorm(1, 15,de))
x <- data.frame(id="Outlying point",x=rnorm(1, 80,de), y=rnorm(1, 80,de))
z <- data.frame(id="Outlying point",x=rnorm(1, 20,de), y=rnorm(1, 80,de))

data <- rbind(a,b,c,d,e,f,g,h,i,w,x,y,z)

##plotting

hull <- quick_hull(data[,2:3])
hull_data <- data[c(hull,hull[1]),]

ggplot(data, aes(x=x, y=y))+
  #theme_grey()+
  theme_bw()+
  geom_text(aes(label=id))+
  scale_colour_brewer(palette="Set1")+
  #par(lty="dashed",lwd=0.1)+  ##a new addition
  geom_path(data=hull_data,colour="red")+
  coord_equal()

hist (data[,3],main="Histogram of y column",nclass=10,col="cornflowerblue")
hist (data[,2],main="Histogram of x column",nclass=10,col="steelblue")

####################################output here###############################################

#If you need the CSV of this data, unhash below!
#write.csv(data,file="square.csv")

# Plotting the data
ex1 <- ggplot(data, aes(x=x, y=y))+

  theme_bw()+
  geom_point(aes(shape=id),size=6, lwd=100)+
  scale_shape_manual('',values=c(1:7))+
  theme(legend.text=element_text(size=20)) +
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  coord_equal()

ex1

grid.edit("geom_point.points", grep=TRUE, gp=gpar(lwd=2))
write.csv(data, "data.csv")

  ggsave("ex1.png",last_plot(), type="cairo")


#Input centroid data and membership data from FKM without extragrades
kNOEXcent <- read.table(text="

5a       50.1421         48.8607    
5b       79.0612         59.1939    
5c       22.0861         38.3820    
5d       40.1836         78.0915    
5e       61.1895         19.6632 ") 
names(kNOEXcent) <- c('id','x','y')

FKMNOEX <- read.table(text="
id       MaxCls      CI      5a       5b       5c       5d       5e 
a          5e     0.00547  0.00231  0.00002  0.00081  0.00001  0.99685
a          5e     0.02298  0.01054  0.00006  0.00182  0.00002  0.98756
a          5e     0.00875  0.00410  0.00003  0.00052  0.00001  0.99535
a          5e     0.02058  0.00955  0.00005  0.00141  0.00001  0.98897
a          5e     0.00006  0.00003  0.00000  0.00000  0.00000  0.99997
b          5c     0.00189  0.00086  0.00000  0.99897  0.00016  0.00001
b          5c     0.00652  0.00307  0.00001  0.99655  0.00036  0.00002
b          5c     0.00317  0.00155  0.00000  0.99838  0.00006  0.00001
b          5c     0.00029  0.00014  0.00000  0.99985  0.00001  0.00000
b          5c     0.03747  0.01719  0.00003  0.97972  0.00300  0.00007
c          5a     0.00000  1.00000  0.00000  0.00000  0.00000  0.00000
c          5a     0.00000  1.00000  0.00000  0.00000  0.00000  0.00000
c          5a     0.00000  1.00000  0.00000  0.00000  0.00000  0.00000
c          5a     0.00000  1.00000  0.00000  0.00000  0.00000  0.00000
c          5a     0.00000  1.00000  0.00000  0.00000  0.00000  0.00000
d          5b     0.24204  0.10902  0.86698  0.00015  0.00028  0.02357
d          5b     0.00164  0.00079  0.99915  0.00000  0.00001  0.00005
d          5b     0.00102  0.00045  0.99943  0.00000  0.00001  0.00011
d          5b     0.00003  0.00001  0.99998  0.00000  0.00000  0.00000
d          5b     0.02278  0.00973  0.98694  0.00003  0.00006  0.00324
e          5d     0.00416  0.00194  0.00027  0.00002  0.99777  0.00000
e          5d     0.00035  0.00016  0.00003  0.00000  0.99981  0.00000
e          5d     0.00337  0.00156  0.00024  0.00001  0.99819  0.00000
e          5d     0.00146  0.00065  0.00015  0.00001  0.99919  0.00000
e          5d     0.00348  0.00160  0.00026  0.00001  0.99812  0.00000
EM1        5c     0.06483  0.01362  0.00064  0.95956  0.00179  0.02439
EM2        5e     0.12509  0.02038  0.05005  0.00333  0.00126  0.92497
EM3        5d     0.08153  0.01439  0.00164  0.03245  0.95092  0.00060
EM4        5b     0.11978  0.01884  0.92886  0.00108  0.04864  0.00258
EX1        5e     0.01016  0.00229  0.00384  0.00015  0.00004  0.99368
EX3        5b     0.00782  0.00193  0.99505  0.00004  0.00287  0.00011
EX2        5c     0.02909  0.00534  0.00010  0.98262  0.00022  0.01171
EX4        5d     0.01195  0.00296  0.00018  0.00438  0.99242  0.00005", header=T)


##putting data together so memberships are in a column with x-y values

dataFKM <- cbind(FKMNOEX,data)
values<-dataFKM[,4:8]
dataFKM$maxnumber<-apply(values,1,max)
str(dataFKM)

#Plotting data so memberships affect the size of the plot

ex2 <- ggplot(dataFKM, aes(x=x, y=y))+
 
  theme_bw()+
  geom_point(data=kNOEXcent, aes(size="Centroid"), shape=16)+ 
  geom_point(aes(shape=MaxCls),size=6)+
  scale_size_discrete("", range=c(4, 4)) +
  scale_shape_manual('Clusters',values=c(1:7))+
  theme(legend.text=element_text(size=20)) +
  theme(legend.title=element_text(size=19)) +
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  coord_equal()

ex2

#Saving
ggsave("ex2.png",ex2, type="cairo")

#merge data with kNOEXcent **possibly redundant**
datak1 <- rbind(kNOEXcent,data)




##Input the text output from fuzzy k means with extragrades.
##5_class

data2 <- read.table(text="id       MaxCls      CI      5a       5b       5c       5d       5e       5* 
  a          5d     0.19363  0.02631  0.04049  0.04417  0.85053  0.01713  0.02137
                  a          5d     0.08901  0.01215  0.02039  0.02254  0.93353  0.00790  0.00349
                  a          5d     0.04958  0.00716  0.01152  0.01266  0.96308  0.00447  0.00110
                  a          5d     0.08437  0.01160  0.01940  0.02143  0.93706  0.00745  0.00306
                  a          5d     0.37571  0.05180  0.06932  0.07456  0.69952  0.02957  0.07523
                  b          5*     0.57573  0.04948  0.11794  0.12522  0.07971  0.07816  0.54949
                  b          5*     0.64893  0.05332  0.13569  0.14485  0.08561  0.08461  0.49592
                  b          5*     0.71004  0.05600  0.14944  0.16245  0.09800  0.08171  0.45241
                  b          5*     0.60155  0.05081  0.12317  0.13223  0.08810  0.07501  0.53068
                  b          5*     0.67028  0.05450  0.14156  0.14989  0.08213  0.09232  0.47961
                  c          5b     0.37320  0.00985  0.79877  0.17197  0.00919  0.00934  0.00088
                  c          5b     0.18232  0.00260  0.90489  0.08722  0.00264  0.00258  0.00006
                  c          5c     0.80853  0.01400  0.38042  0.57189  0.01580  0.01516  0.00273
                  c          5b     0.39630  0.00647  0.79178  0.18808  0.00653  0.00670  0.00045
                  c          5c     0.19412  0.00399  0.09068  0.89657  0.00478  0.00380  0.00017
                  d          5a     0.55765  0.55273  0.11037  0.10457  0.07226  0.05337  0.10670
                  d          5a     0.22240  0.82700  0.04940  0.04564  0.02749  0.03063  0.01984
                  d          5a     0.15379  0.87928  0.03306  0.03114  0.02187  0.02146  0.01319
                  d          5a     0.30484  0.75616  0.06100  0.05669  0.03600  0.04332  0.04684
                  d          5a     0.28066  0.77766  0.05832  0.05526  0.04003  0.03300  0.03575
                  e          5e     0.06540  0.00986  0.01620  0.01477  0.00624  0.95080  0.00212
                  e          5e     0.12826  0.01885  0.02975  0.02746  0.01235  0.90149  0.01010
                  e          5e     0.03320  0.00514  0.00832  0.00760  0.00325  0.97512  0.00058
                  e          5e     0.06548  0.01026  0.01581  0.01453  0.00647  0.95033  0.00259
                  e          5e     0.03213  0.00499  0.00805  0.00735  0.00315  0.97592  0.00054
                  EM1        5*     0.16363  0.01805  0.02601  0.02721  0.03713  0.01811  0.87349
                  EM2        5*     0.15973  0.03268  0.02320  0.02331  0.03129  0.01657  0.87296
                  EM3        5*     0.17712  0.01937  0.02869  0.02851  0.01985  0.04035  0.86323
                  EM4        5*     0.18474  0.03687  0.02686  0.02573  0.01852  0.03838  0.85364
                  EX1        5*     0.40893  0.08768  0.05660  0.05709  0.08558  0.03430  0.67875
                  EX3        5*     0.44406  0.09691  0.06184  0.05805  0.03669  0.09366  0.65285
                  EX2        5*     0.38518  0.03530  0.05677  0.06059  0.09947  0.03357  0.71429
                  EX4        5*     0.32813  0.03174  0.05202  0.05147  0.03202  0.08043  0.75230"
,header=TRUE)

##Plotting the data 
data <- rbind(a,b,c,d,e,f,g,h,i,w,x,y,z)
square_mem <- cbind(data2[,1:2],data[c("x","y")])
centroids <- read.csv("output.csv")
names(centroids)[1] <- "MaxCls"

#geom_point(data=kNOEXcent, aes(size="Centroid"), shape=16)+

ex3 <- ggplot(square_mem, aes(x=x, y=y))+  ##, colour=MaxCls
  theme_bw()+
  geom_point(data=centroids, aes(size="Centroid"), shape=16)+
  geom_point(aes(shape=MaxCls),size=6)+
  scale_shape_manual('Clusters',values=c(1,2,3,5,6,4))+
  
  scale_size_discrete("", range=c(4, 4)) +
  
  theme(legend.text=element_text(size=20)) +
  theme(legend.title=element_text(size=19)) +
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  coord_equal()
ex3

ggsave("ex3.png",ex3, type="cairo")

# data <- rbind(centroids,data)
# ggplot(data, aes(x=x, y=y))+
#   #theme_grey()+
#   theme_bw()+
#   geom_text(aes(label=id,colour=id))+
#   geom_path(data=hull_data,colour="red")+
#   coord_equal()
# data3 <- 1:9

## arcomeson, phi of 1.75

data3 <- read.table(text="
5.11436026881286  6.17629562291158
99.5253593351536	1.67680127618063
7.03427985605290	90.2434560476277
95.0126321256778	95.6006103195999
50.7449937463849	74.9852419153125
75.3907715715157	48.3006145741301
49.9267185742559	48.9158968876554
25.4561609649128	48.7931616031220
50.0885015208430	23.4160900083503")
names(data3) <- c("x","y")
plot(data3)
id <- c("c1","c2","c3","c4","c5","c6","c7","c8","c9")
data3 <- cbind(id,data3)


dataark1 <- rbind(data3,data)

AM<- read.table(text="

1.30131016391783e-10  3.42621694485530e-11	1.64062506358242e-12	7.51135124388973e-13	8.09748272554570e-09	0.999999990086510	1.03301636153639e-09	5.76481881942685e-10	3.97242197037922e-11
1.21892020802994e-11	3.59145294041856e-12	2.68877663188978e-13	1.22911174804457e-13	2.99327773180239e-09	0.999999996635315	2.22202336357355e-10	1.24608898779777e-10	8.42335861720673e-12
2.10854626374732e-13	9.45642065313977e-14	5.18952542003429e-15	2.78733627312179e-15	6.03069643081366e-11	0.999999999932321	3.57549068362582e-12	3.30795481491644e-12	1.75214999217477e-13
8.05230787487714e-12	2.69036505535712e-12	1.91767210554516e-13	9.17369937579991e-14	2.32837360937027e-09	0.999999997401212	1.53670049362489e-10	9.94419671799367e-11	6.27645172178866e-12
2.69081558596237e-09	3.65678136786301e-09	5.84172775941811e-11	4.64580878589466e-11	3.51153279016180e-07	0.999999557935478	2.18858091156948e-08	6.07037403234521e-08	1.86922106121138e-09
4.71998217400408e-11	2.66750225186236e-13	8.74288887300231e-11	3.70214980131092e-13	4.04648876385376e-09	2.94054857553328e-10	0.999999995240952	1.92270265438168e-11	2.64011857889137e-10
6.63477431601364e-15	5.58765838963016e-17	1.28525337027555e-14	7.92846663565234e-17	1.58643439942061e-12	7.43302063167467e-14	0.999999999998244	5.00006169915789e-15	7.03540076492841e-14
3.65976158584045e-10	3.11646865918048e-12	2.94452701653068e-10	3.27610263067338e-12	1.22015123692577e-07	6.91589347744505e-09	0.999999867698820	2.83212553618284e-10	2.42012823641890e-09
1.18065894507638e-10	5.60759894102520e-13	8.98232963148377e-11	5.94746271686475e-13	8.90264052948018e-09	8.89778295838746e-10	0.999999989611883	3.79837660056585e-11	3.48669913209692e-10
3.04685895485733e-10	3.65798457310993e-12	1.18778074401355e-09	6.58697211524823e-12	1.40432783120577e-07	3.99385513400342e-09	0.999999845586679	3.90909130518244e-10	8.09306256750132e-09
3.19365912985964e-13	2.13033860781755e-13	5.36152763482368e-13	3.28586889776078e-13	0.999999997867574	4.10940253889773e-10	3.95505186942717e-10	8.07778790878764e-10	5.16804728263958e-10
3.58846667248627e-17	1.94645392244620e-17	5.71331169343641e-17	2.82509708353521e-17	0.999999999999799	4.53664999095611e-14	5.58459548402057e-14	5.45103528035830e-14	4.55807749819849e-14
1.80558859689507e-13	6.80263823259794e-14	2.81190902475447e-13	9.56350162434270e-14	0.999999999064095	1.93636372322084e-10	4.51358664008553e-10	1.19976062839026e-10	1.70308587677357e-10
3.03906425492351e-16	1.54403924061417e-16	5.25045394140914e-16	2.41169729411722e-16	0.999999999998277	3.36999385585381e-13	5.34801141199420e-13	4.07567274277939e-13	4.42645310348619e-13
6.84512877918339e-13	2.96091672898581e-13	7.61582191749243e-13	3.09311286136295e-13	0.999999996712666	1.22954955686741e-09	1.15514259234754e-09	5.43389771250540e-10	3.57200441707422e-10
8.54285343995394e-10	2.62276105999554e-08	7.01376030321977e-10	1.00423913243497e-08	1.74761989255939e-05	1.61837587663613e-06	4.79473120634763e-08	0.999980523344943	2.96307279584224e-07
3.23835402553476e-12	8.55757266099625e-11	5.54650120093602e-12	2.00221423232078e-10	5.83074252799294e-08	2.17999825634136e-09	2.18693400690699e-10	0.999999934307053	4.69224805795623e-09
1.78009864034312e-12	1.08259469165206e-10	2.34932229189465e-12	1.27469100207233e-10	1.13186610961665e-08	1.13238308204534e-09	7.79617572601910e-11	0.999999986104115	1.12702125445869e-09
1.01009897823265e-10	3.69299853614575e-09	2.00554004999625e-10	1.46974260469010e-08	8.86512910177476e-07	4.62941731240827e-08	5.66644712972671e-09	0.999998886018570	1.56815911121594e-07
3.00476145550118e-12	1.64036488316805e-10	2.87429707418477e-12	7.91977378890775e-11	2.69760175349596e-08	3.31850858049588e-09	1.34349600474953e-10	0.999999968156815	1.16519658048588e-09
2.94125564846370e-15	1.94964955049643e-15	2.65310713744326e-13	1.60570834216210e-13	4.21799864490478e-11	1.47977074570526e-13	2.21302799240825e-12	2.50667859868369e-12	0.999999999952522
1.18186600289253e-13	6.90328570812844e-14	1.83206674412764e-11	7.46190111998516e-12	8.39065222317648e-10	4.48870045928463e-12	8.11845357606908e-11	6.14401135583625e-11	0.999999998987852
7.28176310632891e-17	4.87995423119469e-17	6.95311806078609e-15	4.34563832941062e-15	9.35224949946341e-13	3.54559847015669e-15	5.23928721123025e-14	6.07626745652360e-14	0.999999999998937
1.04880828159051e-15	7.14838697432674e-16	1.27289144821522e-13	8.41559941749049e-14	8.88520291842442e-12	4.46229102775093e-14	6.49627767302831e-13	7.64723579394158e-13	0.999999999989443
5.57240395501738e-17	3.77250376271632e-17	5.30774089026006e-15	3.41689557709506e-15	7.09100366568114e-13	2.71475826652406e-15	3.95474169955732e-14	4.72053489588593e-14	0.999999999999193
1	2.19623900072037e-19	5.90211025909212e-19	2.03720296096611e-20	6.11689063239493e-18	4.45060925115839e-17	5.44008398858096e-17	6.55063519469892e-19	6.31988366102792e-19
4.29871788134433e-19	1	4.08451932898055e-20	4.82752176905953e-19	5.94496199770815e-18	3.86423560091409e-17	8.08060619926809e-19	5.08265939453476e-17	8.12282359169944e-19
7.15440493224300e-20	2.54366626666117e-21	1	4.55602600242829e-20	1.13207592417915e-18	1.13729687734019e-19	1.01267359002337e-17	1.02901418736437e-19	8.11526267626353e-18
3.13496431697662e-20	3.79697697940294e-19	5.78812909781536e-19	1	6.48152726975316e-18	7.73004808582970e-19	8.37305648775063e-19	4.41743940727967e-17	6.11168012566582e-17
4.30672895488485e-05	0.896926402012657	4.27342528838031e-06	3.77503872067953e-05	0.00336943645661619	0.0443330495111827	0.000187972600468519	0.0549250468121821	0.000173001504849470
2.05557761981857e-06	2.33409314035940e-05	3.11118329569456e-05	0.930035214549317	0.00227147011508621	0.000115659682743157	0.000111914685233214	0.0347776729553721	0.0326315596702683
0.961303742243768	1.08578525863864e-05	1.54576104342350e-05	8.21508683255300e-07	0.00117641914217531	0.0269213668618850	0.0104637992597190	6.35890337814067e-05	4.39464869675821e-05
1.19741813309994e-06	5.30814154416264e-08	0.998018722282669	1.00661029064479e-06	8.07806263316094e-05	3.95609612354813e-06	0.000895002740605654	3.98691517011212e-06	0.000995294229260647
",col.names=c("End member cluster a","End member cluster b","End member cluster c","End member cluster d","Intergrade cluster e","Intergrade cluster f","Intergrade cluster g","Intergrade cluster h","Intergrade cluster i"))

#names(AM) <- letters[1:9]


aa<-as.matrix(AM)
library(plyr)
AM$max<-apply(aa,1,which.max)
AM$max<-as.factor(paste0(letters[AM$max]))
AM$designation<-ifelse(AM$max%in%letters[1:4],"End point cluster", "Intragrade cluster")
ex4data<-cbind(data,AM)


##ifelse

#intergrade clusters are "Intergrade cluster e to i"
#extragrade clusters are "Extragrade clusters a to d"

#AM$designation<- ifelse(AM$max == "a"|AM$max=="b"|AM$max=="c"|AM$max=="d", "End member cluster", "Intragrade cluster")
##This is extremely handy to think about. The | means "or" with what ever function you are using eg "if" becomes "or if"


ex4 <- ggplot(ex4data, aes(x=x, y=y))+
  theme_bw()+
  geom_point(data=data3, size=4,shape=16)+
  geom_point(aes(shape=designation),size=6)+
  scale_shape_manual('Cluster membership\n',values=c(2:1))+
 # scale_size_discrete("", range=c(4, 4)) +
  geom_path(data=hull_data,size=2, alpha=.2)+
  
  
  theme(legend.text=element_text(size=20)) +
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  theme(legend.title = element_text(size=21))+
  coord_equal()

ex4

ex4 <- ggplot(data, aes(x=x, y=y))+
  #theme_grey()+
  theme_bw()+
  geom_path(data=hull_data,size=2, alpha=.2)+
  #geom_text(aes(label=id),size=7)+
  geom_point(aes(shape=id),size=6)+
  scale_shape_manual('',values=c(1:7))+
  geom_point(data=data3, shape=16, size=4)+
  theme(legend.text=element_text(size=20)) +
  theme(axis.text.x=element_text(size=20))+
  theme(axis.text.y=element_text(size=20))+
  theme(axis.title.x = element_text(size=20))+
  theme(axis.title.y = element_text(size=20))+
  coord_equal()
ex4

ggsave("ex4.png",ex4, type="cairo")

ggplot(data, aes(x=x, y=y))+
  #theme_grey()+
  theme_bw()+
  geom_text(aes(label=id))+
  geom_path(data=hull_data,colour="red")+
  coord_equal()


plot(data3)

cent <- read.table(text="
5.34763718189816  6.30850329247068
99.2727532058492	1.89753243390930
7.13103087543316	90.1718221913140
94.8196869276890	95.3581002431606
50.6140606993855	74.9821188430513
49.8546503118155	48.9590240173225
25.4550233534268	48.8920774762062
50.1317573381585	23.4707210967361
75.2746395048472	48.2415808097987")


##Alex asked me to plot this...
r <- read.table(text="
  5.04726740724799  6.13860091676842
99.5765614833474  1.63206937047374
6.96489098839824	90.2947839467634
95.0565161905211	95.6560145047676
50.6580426269852	74.9582735883809
49.9189401454671	48.9314562481603
75.4495280907533	48.4084793675342
25.4446555974805	48.8345824245828
50.0000399263199	23.5201253591199")

plot(r)

##jose's craziness
j <- read.table(text="
5.09057342606623  6.17042684481357
99.5167266821952	1.66289082679495
6.96085973909758	90.2985533734585
129.993644277014	129.993304572335
50.2009329092989	75.3569771906972
50.5628319776097	49.5394080781998
26.6119918930182	48.5514722716749
74.3365735449373	49.8801326094652
48.5777791439808	25.1782186471451
")

plot(data[,2:3])
points(j,pch=4, col='red')

##a quick demonstration of the convex bicycle
de <- 5
points <- 5000
c <- data.frame(id="c",x=rnorm(points, 50,de), y=rnorm(points, 50,de))
plot(c[,2],c[,3],main="Cluster",xlab="x",ylab="y")

ggplot(hull_data, aes(x=x, y=y))+
  theme_bw()+
  geom_path(data=hull_data,colour="red")



c <- data.frame(x=c$x,y=c$y)
plot(c)
cz <- chull(c)
hull_data <- c[c(cz,cz[1]),]
ggplot(hull_data, aes(x=x, y=y))+
  theme_bw()+
  geom_path(data=hull_data,colour="red")  
#cz <- quick_hull(c)

hull_data <- c[c(cz,cz[1]),]
plot(c[cz,2],c[cz,3])
plot(hull_data,type=scatter)
which.max(dist(cz[,2],cz[,3]))

ggplot(data, aes(x=x, y=y))+
  #theme_grey()+
  theme_bw()+
  geom_text(aes(label=id))+
  geom_path(data=hull_data,colour="red")+
  coord_equal()
