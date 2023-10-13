IndexFit<-function(dat=mod1,f=1,ylab="", yf=1978,yl=2011 ,main="Model 1",Doleg=T)
{
  attach(dat)
  require(plotrix)
  x<-paste("Obs_Survey_",f,sep="")
  Data<-get(x)
  d1<-subset(Data,Data[,2]>0)
#  windows(8,8)
  plotCI( d1[,1], d1[,2], 2*d1[,4], new=T, main=main,
  cex.main=1, cex.lab=2,
  ylim=c(0,(max(d1[,2])*1.9)), xlim=c(yf,yl),pch=19, ylab=ylab,
  xlab="Year",col="blue",scol="black"#,lab=c(10,10,7)
)
  points(Data[,3]~Data[,1],type="l",col="red",lwd=2)
  points(Data[Data[,2]>0,1],Data[Data[,2]>0,2],type="p",pch=19,col="blue",cex=1)
  if (Doleg){
    legend("topleft", c("Observed","Model fit"), pch=c(19,-1), 
           title=Index_names[f],lty=c(-1,1),col=c("blue","red"),inset = .02)
  }
  detach(dat)
}

Indices<-function(dat=mod1,Title="Model 1",fy=1982,ly=2010){
  # Index fits
  # windows(,11, 6.5)
  oldpar<-par
  par(mfrow=c(3,1), oma=c(0,1,3,1) )
  for(i in 1:length(dat$Index_names))
  {
    if (i==1) 
      Doleg=T
    else 
      Doleg=F
  
   IndexFit(dat,f=i,Doleg=Doleg,yf=fy,yl=ly)
  }
  # text(2000,  20, Title , cex=3, col="black",family="sans serif",font=1)
  mtext(Title,3,-0,outer=T, cex=2, col="blue",family="sans",font=2)
  par<-oldpar
}

CatchFit<-function(dat=mod1,nfsh=1,Title="Model 1"){
 par(mfrow=c(2,2), oma=c(0,1,3,1) )
 for (i in 1:nfsh){
   p.catch.fit(dat,f=i)
 }
 mtext(Title,3,-0,outer=T, cex=2, col="blue",family="sans",font=2)
 }
