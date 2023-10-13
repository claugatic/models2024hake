plt_proj<-function(dat,Title="Model 1")    {
  #windows(,6.5,9)
  FutBiom(dat)
  #windows(,6.5,9)
  FutCatch(dat)
  # mtext(Title,3,0,outer=T, cex=2, col="blue",family="sans",font=2)
}

FutBiom <- function (dat, new=TRUE,ylab="Female SSB(kt)",fy=1978,ly=2017,nproj=5)
{
  #ltmp=c("F35","F=SQ","F=.5SQ","CT_Rule", "F=0.0")
  ltmp=c("F35","F=Fmsy","F=.5F","F=0.5Catch","F=0.0")
  attach(dat)
  #Data = subset(get("SSB"),Yr>=fy)
  Data = subset(SSB,SSB[,1]>=fy)
  print(Data)
  xlim = c(min(Data[,1]),max(Data[,1])+nproj)
  ymax = max(Data[,5])
  ylim = c(0,ymax)
  plot(Data[,1],Data[,2],type="n",lwd=2,
  xlab="Year",ylab=ylab,
  #lab=c(10,10,7),
  xlim=xlim, ylim=ylim, col="red")
  n=length(Data[,1])
  x=c(Data[,1],Data[length(Data[,1]):1,1])
  y=c(Data[,4],Data[length(Data[,1]):1,5])
  polygon(x,y,col="salmon",lty=3,border="grey")
  lines(Data[13:n,1],Data[13:n,2],lwd=2, col="darkred")
  Data = get("SSB_fut_5")  
  x=c(Data[,1],Data[length(Data[,1]):1,1])
  y=c(Data[,4],Data[length(Data[,1]):1,5])
  polygon(x, y,col=rainbow(1,alpha=.2,start=.8),
          lty=3,border="grey")
  Data = get("SSB_fut_2")  
  x=c(Data[,1],Data[length(Data[,1]):1,1])
  y=c(Data[,4],Data[length(Data[,1]):1,5])
  polygon(x, y,col=rainbow(1,alpha=.5,start=.12),
          lty=3,border="grey")
  for (i in 1:5)
  {
    Data = get(paste("SSB_fut_",i,sep=""))  
    lines(Data[,1],Data[,2],lwd=2, col=i)
    points(Data[,1],Data[,2],pch=i, col=i)
  }

  # legend("topright", ltmp, col=1:5,lwd=2, title="          ", inset = .02)
  legend("bottomleft", ltmp, pch=1:5,lwd=2,col=1:5,       title="Future SSB", inset = .02)
   detach(dat)
}

FutCatch <- function (dat, new=TRUE,ylab="Total Catch (kt)",fy=1990,ly=2040)
{
  require(lattice)
  attach(dat)
  nfsh = length(Fshry_names)
  catch_tot=0
 for (i in 1:nfsh) 
 {
   ctmp=get(paste("Obs_catch_",i,sep=""))
   ctmp=subset(ctmp,Yr>=fy)
     catch_tot = catch_tot + ctmp 
  }
 xlim = c(fy,ly)
 ylim = c(0,max(catch_tot))
 plot(fy:(fy+length(catch_tot)-1),catch_tot,
    type="l",lwd=2,
    xlab="Year",ylab=ylab,xlim=xlim, ylim=ylim, col="red")
  x= Catch_fut_1[,1]
  y= Catch_fut_1[,2]
  ltmp=c("F35","F=SQ","F=.5SQ","CT_RULE")
  for (i in 1:4)
  {
    y = get(paste("Catch_fut_",i,sep=""))
    #lines(x,y[,2],lty=i,lwd=3)
    points(x,y[,2],pch=i      )
  }
  # legend("topright", ltmp, lty=1:4,lwd=3, title="Future catch", inset = .02)
  legend("bottomleft", ltmp, pch=1:4,       title="Future catch", inset = .02)
  detach(dat)
}
