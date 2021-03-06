##grid clustering.

x<-runif(10000)
y<-runif(10000)
test.data.frame<-cbind(x,y)
max.x<-.8*max(x)
min.x<-1.2*min(x)
max.y<-.8*max(y)
min.y<-1.2*min(y)
grid.x<-seq(min.x:max.x)
gridx<-seq(from=min.x,to=max.x,length.out=10)
gridy<-seq(from=min.y,to=max.y,length.out=10)
grid<-expand.grid(gridx,gridy)
plot(grid)
points(test.data.frame, col="red")

xy.km <- kmeans(test.data.frame, centers=grid)
plot(centers<-xy.km$centers)
points(grid,col=34)
arrows(x0=grid[,1], y0=grid[,2], x1=centers[,1], y1=centers[,2], col="blue")
