source("C:/Users/phug7649/Desktop/TXTBIN/R-scripts/functions/qhull_algorithm.r")
a<-1.7
number<-750
Var.1<-c(rnorm(number,mean=0,sd=a),rnorm(number,mean=4,sd=a),rnorm(number,mean=9,sd=a),rnorm(number,mean=5,sd=a), rnorm(number, mean=5,sd=a))
Var.2<-c(rnorm(number,mean=3,sd=a),rnorm(number,mean=0,sd=a),rnorm(number,mean=5,sd=a),rnorm(number,mean=8,sd=a), rnorm(number, mean=5,sd=a))
C1<-c(0,4,9,5,5)
C2<-c(3,0,5,8,5)
centroids<-cbind(C1,C2)
Example<-cbind(Var.1,Var.2)
plot(Example, col="grey28")
points(centroids,col="blue",pch=19,cex=2)

cz <- chull(Example)

plot(Example)
lines(Example[c(cz, cz[1]),], col="red")
min_x<-which.min(Example[,'Var.1'])
max_x<-which.max(Example[,'Var.1'])
lines(data.frame(x=c(Example[min_x,'Var.1'],Example[max_x,'Var.1']),y=c(Example[min_x,'Var.2'],Example[max_x,'Var.2'])))

plot(Example[cz,])
lines(Example[c(cz, cz[1]),], col="red")
lines(data.frame(x=c(Example[min_x,'Var.1'],Example[max_x,'Var.1']),y=c(Example[min_x,'Var.2'],Example[max_x,'Var.2'])))
lines(data.frame(x=c(Example[min_x,'Var.1'],Example[max_x,'Var.1']),y=c(Example[min_x,'Var.2'],Example[max_x,'Var.2'])))