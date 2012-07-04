data<-read.csv ("rpskb1_3.csv")

trt1<-data[data$Call=="Homozygote Allele 1",]
trt2<-data[data$Call=="Heterozygote",]
trt3<-data[data$Call=="Homozygote Allele 2",]
trt1
trt2
trt3
for (i in 1:3) print(i)
fnord <- list("Cheese",TRUE,27.5)
for (i in fnord) print(i)

seq(0, 1, by=.1) == .3
unique(c(.3, .4 - .1, .5 - .2, .6 - .3, .7 - .4))