##biomass plots with polygon
spwn_ratio <- function (dat,color="light blue",main="Model 1",fy=1978,ly=2012)
{
  titl<-"Spawning Biomass relative to unfished"
  typ <- "SSB_NoFishR"
  cl<-paste(color)
  attach(dat)
  Data<-get(typ)
  #windows(12,8)
  Data<-Data[Data[,1]<=ly&Data[,1]>=fy,]
  ylim=c(0,max(Data[,2])*1.1)
  plot(Data[,1],Data[,2],
  ylim=ylim,
  main=main,
  type="l",lwd=2,
  xlab="Year",ylab=paste(titl),lab=c(10,10,7),xlim=c(min(Data[,1]),max(Data[,1])))
  x=c(Data[,1],Data[length(Data[,1]):1,1])
  y=c(Data[,4],Data[length(Data[,1]):1,5])
  polygon(x,y,col=cl,lty=3,border="grey")
  points(Data[,1],Data[,2],ylim=c(0,max(Data[,2]*1.5)),type="l",lwd=2)
  detach(dat)
}
