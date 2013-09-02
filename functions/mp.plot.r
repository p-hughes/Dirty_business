mp.plot<-function(data,ratio,
                  matlab="C:\\Users\\phug7649\\Documents\\MATLAB",
                  weighting_factor=read.csv(paste(matlab, "weighting.csv", sep="\\"),sep=","))
  {
  if (missing(data)) stop('You need the data from meson and a ratio from weighting. Data missing')
  if (missing(ratio)) stop('You need the data from meson and a ratio from weighting. Ratio missing.')
  data.complete<-data[['data.complete']]
  centroids.complete<-data[['centroids.complete']]
  
  
  library(Cairo)
  library(plyr)
  library(ggplot2)
  library(grid)
  
  NUTS<-ggplot(data.complete, aes(x=Comp.1, y=Comp.2), group=max)+
    theme_bw() +
    geom_point(colour="grey40")+
    #stat_bin2d(binwidth=c(1, 1),colour=gray) +
    facet_wrap(~ max, nrow=5)+
    geom_point(data=centroids.complete, aes(shape=soil.id),size=4, colour="black")+  
    scale_shape_manual(values=c(16, 17))+
    #theme(plot.background = element_rect(fill = w))+
    #    xlim(-12,8)+
    #    ylim(-7.5,5)+
    
    ggtitle(paste0("Weighting ", weighting_factor[1,1],", creating ",ratio, " percent end point memberships"))+
    # theme(panel.margin = unit(5, "lines"))+
    coord_equal()
  
  NUTS
}