am()

source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/akro.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/akro.write.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/meson.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/weighting.r")
source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/mp.plot.r")


# file<-read.csv("USnoa20170709_60065.csv",sep=",", header=T)
# aaaa<-am(file)

#write.table(akro.parameters,"control.txt",row.names=F,col.names=F)  
file<-read.csv("C:/Users/phug7649/Desktop/kmeans/US_thickness_analysis_09072013/noa/depth_200/data_depth200.csv",header=TRUE,sep=",")
noa.200<-file
##seeing if the function works.

object <- akro(noa.200,35,1.2,3,2000)
objectII<- akro(noa.200,38,1.3,3,2000)
akro.write(object)
check<-meson()
weight<-weighting(check)
I<-check[['centroids.complete']]
II<-check[['data.complete']]
III<-check[['data_ratio']]
mp.plot(check,weight)

##there needs to be a file with: 
# weight, the weighting applied to the endpoints
# clusters, the number of clusters identified by FKM
# endpoints, the number of points identified by am
# distype, the distance method by the final fkm (euclidean is 1, diagonal is 2, mahalanobis is 3) 
# phi, the fuzziness explonent

# nend=10;%Number of end points-was 23!
#   nclass=58; % Check r script for correct number- was 29
# phi=1.2;  %Set the phi was 1.25 (1.3 for surface data with thicknesses)
# weight=1500; % I may have just broke it... was 2000 
# distype=1; %%1 is euclidean, 2 is diagonal, 3 is mahalanobis.
#shell("matlab -nodesktop -nosplash -wait -r am")





#setwd("C:/Users/phug7649/Desktop/txtbin")



