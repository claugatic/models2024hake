library(KernSmooth)


source("R_Functions/mntns.r")
source("R_Functions/IndexFit.r")
source("R_Functions/spwn_rat.r")
source("R_Functions/proj.r")
source("R_Functions/agefits_hake.r")
#source("R_Functions/Plots_tables_2011.r")
##source("common.r")


# NOTE: the functions below are a hodge podge of various routines, they may not have been tested

##plot survey fit
p.sur.stk<-function(dat=data1,f=1,ylab="Index", yf=1992,yl=2012)
  {
  attach(dat)
  require(plotrix)
  x<-paste("Obs_Survey_",f,sep="")
  Data<-get(x)
  d1<-subset(Data,Data[,2]>0)
  windows(8,8)
  plotCI(d1[,1],d1[,2],2*d1[,4],
  main=Index_names[f],
  ylim=c(0,(max(max(Data[,3],max(d1[,2])*1.9)))),xlim=c(yf,yl),pch=19,
  ylab=ylab,
  xlab="Year",col="blue",scol="black",lab=c(10,10,7))

  points(Data[,3]~Data[,1],type="l",col="red",lwd=2)

  text(min(Data[,1])+2,(max(d1[,2])*1.5),"Survey Point Estimate",pos=4,cex=0.8)
  text(min(Data[,1])+2,(max(d1[,2])*1.4),"Model Fit",pos=4,cex=0.8)
  points(min(Data[,1])+2,(max(d1[,2])*1.5),pch=19,col="blue")
  text(min(Data[,1])+2,(max(d1[,2])*1.4),"--",cex=1.5,col="red")
  detach(dat)

  }

##filled contour plot of F by age

cont.f.mort<-function(dat,f=1,lage=2,hage=12,cl="BW") ## data file, fishery, lowest and highest ages
{
  attach(dat)
  require(lattice)
  
   if(cl=="BW"){clr<-grey(seq(0.9,0.1,length=20))}
   if(cl!="BW"){clr<-rainbow(25,start=0.5,end=1)}
  
  x<-paste("F_age_",f,sep="")
  xx<-get(x)
  al<-2
  ah<-ncol(xx)
  windows(12,8)
  filled.contour(y=xx[,1],x=c(lage:hage),t(xx[,al:ah]),
  ,ylab="Year",xlab="Age",xlim=c(0.5,hage),ylim=c((min(xx[,1])-0.5),(max(xx[,1])-0.5)),col=clr,lab=c(10,10,7))
  detach(dat)
  mtext("F",4)
}

##bubble plot of numbers or biomass at age CA=20000,B=200000,NA=500000
p.bub.age<-function(dat,typ="NA",f=1,lage=2,hage=12,fy=1992,ly=2012,siz=200000) ## input:  data file,lowest and highest age,first and last year, scaler for graphic
{

  attach(dat)
  ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1
  if(typ=="NA"){
     x<-N
     titl<-"Numbers at Age"
     }

  if(typ=="CB"){
     y<-paste("C_fsh_",f,sep="")
     x<-get(y)
     titl<-"Catch at Age"
     }

  if(typ=="B"){
      titl<-"Biomass at Age"
      yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=length(N[1,]),nrow=length(N[,1]))
     la<-2
     ha<-ncol(x)
    for (i in tt:yrs){
        x[i,(lage):(hage)]<-N[i,(la):(ha)]*wt_a_pop
     }}


     windows(6,8)

     ti<-(ly-min(N[,1]))+1
     tt<-max(fy,min(N[,1]))-min(N[,1])+1
     la<-2
     ha<-ncol(x)
     plot(c(lage,hage),c(fy,ly),type="n",ylab="Year",xlab="Age",lab=c(10,10,7),main=titl)

     for (i in tt : ti){
         points(c(lage:hage),rep(N[i,1],length(lage:hage)),cex=c(x[i,la:ha]/siz))
         }
     detach(dat)
}


## Bubble plot of numbers at age overlaid with F mortality in filled contours

cont.f.mort2<-function(dat=data1,typ="NA",f=1,lage=2,hage=12,fy=1992,ly=2012,siz=500000,cl="BW")## input:  data file, fishery number,lowest and highest age,first and last year, scaler for graphic
{
  attach(dat)
  require(lattice)
   if(cl=="BW"){clr<-grey(seq(0.9,0.1,length=20))}
   if(cl!="BW"){clr<-rainbow(25,start=0.5,end=1)}
  la=2
  y<-paste("F_age_",f,sep="")
  Data<-get(y)
  
    ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1
  if(typ=="NA"){
     x<-N
     titl<-"Numbers at Age"
     ha=ncol(x)
     }

  if(typ=="CB"){
     y<-paste("C_fsh_",f,sep="")
     x<-get(y)
     ha=ncol(x)
     titl<-"Catch at Age"
    }

  if(typ=="B"){
      titl<-"Biomass at Age"
      yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=length(N[1,]),nrow=length(N[,1]))
     ha=ncol(x)
    for (i in tt:yrs){
        x[i,2:ha]<-N[i,2:ha]*wt_a_pop
     }}
  windows(6,8)
  filled.contour(y=Data[tt:ti,1],x=c(lage:hage),t(Data[tt:ti,la:ha]),
  ,ylab="Year",xlab="Age",xlim=c(0.5,hage),ylim=c(fy-0.5,ly+0.5),main=titl,col=clr,
  plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage));
             for (i in tt : ti){
             points(c(lage:hage),rep(N[i,1],length(lage:hage)),cex=c(x[i,la:ha]/siz))
             }
            }
  )
  mtext("F",4)
  detach(dat)
}

## Filled contour plots of residuals from the fishery catch at age data
cont.f.age.res<-function(dat,typ="S",f=1,lage=2,hage=12,cl="BW",nl=40){        ## input:  data file, fishery number,lowest age, highest age
  if (typ=="S"){
      x<-paste("phat_ind_",f,sep="")
      y<-paste("pobs_ind_",f,sep="")
      }
  if (typ!="S"){
      x<-paste("phat_fsh_",f,sep="")
      y<-paste("pobs_fsh_",f,sep="")
      }

      if(cl=="BW"){clr<-grey(seq(0.9,0.1,length=nl))}
      if(cl!="BW"){clr<-rainbow(nl,start=0.1,end=0.9)}

attach(dat)
require(lattice)
al<-2


xx<-get(x)
yy<-get(y)
rr<-yy-xx
ah=ncol(xx)
windows(6,8)
filled.contour(y=xx[,1],x=c(lage:hage),t(rr[,al:ah]),
,ylab="Year",xlab="Age",col=clr,
nlevels=nl,main="Proportion Catch-at-age Residuals",
plot.axes={axis(2,xx[,1],las=2);axis(1,c(lage:hage));
contour(y=xx[,1],x=c(lage:hage),t(rr[,al:ah]),col="grey",add=T)}
)

detach(dat)
}

##bimoass plots with error bars
p.biom.stk<-function (dat,typ="SSB")
{

  if(typ=="SSB")titl<-"Spawning Biomass (t)"
  if(typ=="TB"){typ="TotBiom"
                titl<-"Total Biomass (t)"
                }
  if(typ=="R"){typ="R"
                titl<-"Age 1 Recruites(N)"
                            }
  attach(dat)
  require(lattice)
  Data<-get(typ)
  windows(8,8)
  plot(Data[,1],Data[,2],xlab="Year",ylab=paste(titl),
  pch=19,col="blue",ylim=c(0,(max(Data[,2])*1.5)),lab=c(10,10,7))

  arrows(Data[,1],Data[,4],Data[,1],Data[,5],col="black",angle=90,code=3,length=0.05)
  detach(dat)
  }
  
##effective N Plots

p.eff.n<-function(dat,typ="S",f=1)
  {
      if (typ=="S"){
         x<-paste("EffN_Survey_",f,sep="")
         titl<-"Survey Mean Age"
      }

      if (typ=="F"){
         x<-paste("EffN_Fsh_",f,sep="")
         titl<-"Fishery Mean Age"
      }

      attach(dat)
      require(plotrix)
      Data<-get(x)
      windows(12,8)
      plot(Data[,1],Data[,4],
      ylim=c(0,(max(Data[,4])*1.5)),
      ylab=paste(titl),xlab="Year",pch=19,col="blue",lab=c(10,10,7))

      arrows(Data[,1],Data[,7],Data[,1],Data[,8],col="black",angle=90,code=3,length=0.05)
      points(Data[,1],Data[,5],type="l",col="red",lwd=2)

      text(min(Data[,1])+2,2,"Observed mean age",pos=4,cex=0.8)
      text(min(Data[,1])+2,1.6,"Model-predicted mean age",pos=4,cex=0.8)
      points( min(Data[,1])+2,2,pch=19,col="blue")
      text(min(Data[,1])+2,1.6,"--",cex=1.5,col="red")


      detach(dat)

  }
  
## proportion at age fit plots

p.age.fit<-function(dat,typ="F",f=1,lage=2,hage=12,fy=1992,ly=2012)
{         
   if (typ=="S"){
      x<-paste("phat_ind_",f,sep="")
      y<-paste("pobs_ind_",f,sep="")
      }
  if (typ!="S"){
      x<-paste("phat_fsh_",f,sep="")
      y<-paste("pobs_fsh_",f,sep="")
      }

  attach(dat)
  Data<-get(y)
  Data2<-get(x)
  n1<- max(fy,min(Data[,1]))-min(Data[,1])+1
  n2<-ly-min(Data[,1])+1
  g<-min(n2,length(Data[,1]))
  al=2
  ah=ncol(Data2)
  if (((g-n1+1)/2)-trunc((g-n1+1)/2)==0)b<-(g-n1+1)/2
  if (((g-n1+1)/2)-trunc((g-n1+1)/2)!=0) b<-(g-n1+2)/2
  windows(12,8)
  par(mfrow=c(2,4))
  for ( i in n1:g)
      {
      xx<-list(breaks=seq((lage-0.5),(hage+0.5),by=1),counts=c(hage:lage),
      intensities=as.numeric(Data[i,al:ah]),density=as.numeric(Data[i,al:ah]),
      mids=seq((lage),(hage),by=1),xname="Age",equidist=TRUE)

      attr(xx,"class")="histogram"

      plot(xx,freq=F,ylab="Proportion at Age",xlim=c(0,(hage+1)),
      ylim=c(0,(max(Data[,al:ah])*1.1)),main=paste(Data[i,1]))
      points(xx$mids,Data2[i,al:ah],type="l",col="red",lwd=2)

      }
detach(dat)
}
##fit to catch
p.catch.fit<-function(dat,f=1,ylab="Catch biomass (kt)" )
{
  x<-paste("Obs_catch_",f,sep="")
  y<-paste("Pred_catch_",f,sep="")
  attach(dat)
  Data<-get(y)
  Data2<-get(x)
  xx<-list(breaks=seq(min(Yr)-0.5,max(Yr)+0.5,1),counts=Yr,
  intensities=as.numeric(Data),density=as.numeric(Data),
  mids=seq(min(Yr),max(Yr),by=1),xname="Year",equidist=TRUE)

  attr(xx,"class")="histogram"
  #windows(8,8)
  plot(xx,freq=F,
  ylab=ylab,
  xlim=c(min(Yr)-1,(max(Yr)+1)),
  main=Fshry_names[f],
  ylim=c(0,(max(Data)*1.25)),col="light grey")
  points(xx$mids,as.numeric(Data2),type="l",col="red",lwd=2)
  text(min(xx$mids)+2,(max(Data)),"Model Fit",pos=4,cex=0.8)
  text(min(xx$mids)+2,(max(Data)),"--",col="red",cex=1.5)
detach(dat)
}


##histogram or line plots of selectivity
p.select.hist<-function(dat,typ="F",h="T",f=1,lage=2,hage=15,fy=1978,ly=1984)
{
  attach(dat)

  if (typ=="S"){
      x<-paste("sel_ind_",f,sep="")
      }
  if (typ!="S"){
      x<-paste("sel_fsh_",f,sep="")
      }

  xx<-get(x)
  la=3
  ha=ncol(xx)
  n1<-max(fy,min(xx[,2]))-min(xx[,2])+1
  n2<-ly-min(xx[,2])+1

  if (((n2-n1+1)/2)-trunc((n1-n2+1)/2)==0)b<-(n2-n1+1)/2
  if (((n2-n1+1)/2)-trunc((n1-n2+1)/2)!=0) b<-(n2-n1+2)/2

  Data<-matrix(ncol=((hage-lage)+1),nrow=length(xx[,1]))

  for ( i in 1:length(xx[,1])){  ##create matrix of selectivities scaled to 1.0
      m<-max(xx[i,la:ha])
      Data[i,1:((hage-lage)+1)]<-xx[i,la:ha]/m
   }

  g<-min(n2,length(Data[,1]))
  windows(12,8)
  par(mfrow=c(2,b))
    if(h=="T"){

     for ( i in n1:g)
          {
          xxx<-list(breaks=seq((lage-0.5),(hage+0.5),by=1),counts=c(hage:lage),
          intensities=as.numeric(Data[i,1:ncol(Data)]),density=as.numeric(Data[i,1:ncol(Data)]),
          mids=seq((lage),(hage),by=1),xname="Age",equidist=TRUE)

          attr(xxx,"class")="histogram"

          plot(xxx,freq=F,ylab="Selectivity at Age",xlim=c(0,(hage+1)),
          ylim=c(0,(max(Data[,1:ncol(Data)])*1.1)),main=paste(xx[i,2]),col="salmon")
          }
      }


  if(h!="T"){
      for ( i in n1:n2)
      {
      plot(seq((lage),(hage),by=1),Data[i,1:ncol(Data)],type="l",lwd=2,
      main=paste(xx[i,2]),col="red",ylab="Selectivity at Age",xlab="Age")
      }

  }


detach(dat)
}



##filled countour plots of selectivity

c.select<-function(dat,typ="F",f=1,lage=2,hage=12,fy=1992,ly=2012,cl="BW")
{
  if (typ=="S"){
      x<-paste("sel_ind_",f,sep="")
      }
  if (typ!="S"){
      x<-paste("sel_fsh_",f,sep="")
      }
  if(cl=="BW"){clr<-grey(seq(1,0.13,length=30))}
  if(cl!="BW"){clr<-rainbow(30,start=0.18,end=0.9)}
  
  attach(dat)

  x<-get(x)
  n1<-max(fy,min(x[,2]))-min(x[,2])+1
  n2<-ly-min(x[,2])+1
  
  la=3
  ha=ncol(x)
  
  Data<-matrix(ncol=((hage-lage)+1),nrow=length(x[,1]))

  for ( i in 1:length(x[,1])){  ##create matrix of selectivities scaled to 1.0
      m<-max(x[i,la:ha])
      Data[i,1:(hage-lage+1)]<-x[i,la:ha]/m
   }

  g<-min(n2,length(Data[,2]))

  ti<-(g-n1+1)

  windows(6,8)
  filled.contour(y=x[n1:g,2],x=c(lage:hage),t(Data[n1:g,]),xlim=c(0.5,hage),
  ,ylab="Year",xlab="Age",col=clr,levels=seq(0,max(Data[,]),length=30),nlevels=30,
  plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage))})

  mtext("Selectivity",4)

detach(dat)
}



##filled countour plots of selectivity  with numbers at age

c.select.b<-function(dat,typ="F",typ2="NA",f=1,lage=2,hage=12,fy=1992,ly=2012,siz=500000,cl="BW")
{
  if (typ=="S"){
      y<-paste("sel_ind_",f,sep="")
      }
  if (typ!="S"){
      y<-paste("sel_fsh_",f,sep="")
      }
  if(cl=="BW"){clr<-grey(seq(1,0.2,length=30))}
  if(cl!="BW"){clr<-rainbow(30,start=0.18,end=1)}

 

  attach(dat)

  ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1


  if(typ2=="NA"){
     x<-N
     titl<-"Numbers at Age"
     }

  if(typ2=="CB"){
     z<-paste("C_fsh_",f,sep="")
     x<-get(z)
     titl<-"Catch at Age"
    }

  if(typ2=="B"){
      titl<-"Biomass at Age"
      yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=length(N[1,]),nrow=length(N[,1]))
    for (i in tt:yrs){
        x[i,lage:hage]<-N[i,(lage):(hage)]*wt_a_pop
     }}



  y<-get(y)
  n1<-max(fy,min(y[,2]))-min(y[,2])+1
  n2<-ly-min(y[,2])+1
  
  la=3
  ha=ncol(y)
  



 Data<-matrix(ncol=((hage-lage)+1),nrow=length(y[,1]))

  for ( i in 1:length(y[,1])){
      m<-max(y[i,la:ha])
      Data[i,1:(hage-lage+1)]<-y[i,la:ha]/m
  }

  g<-min(n2,length(Data[,2]))

  ti<-(g-n1+1)

  windows(6,8)
  filled.contour(y=y[n1:g,2],x=c(lage:hage),t(Data[n1:g,]),xlim=c(0.5,hage),ylim=c(fy-0.5,ly+0.5)
  ,ylab="Year",xlab="Age",col=clr,levels=seq(0,max(Data[,]),length=30),nlevels=30,
  main=titl,plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage));
             for (i in 1 : ti){
             points(c(lage:hage),rep(N[i,1],length(lage:hage)),cex=c(x[i,(lage):(hage)]/siz))
             }
            })

  mtext("Selectivity",4)



detach(dat)
}





##plotting spawner recruit curve

p.stock.rec<-function(dat,xlab="Spawning biomass",ylab="Age 1 recruits")
{
 attach(dat)
 windows(8,8)
 plot(Stock_Rec[,2],Stock_Rec[,4],xlim=c(0,max(Stock_Rec[,2])),type="o",
 xlab=xlab, ylab=ylab, pch=19,col="blue")
 points(stock_Rec_Curve, type="l",col="red",lwd=2)
 detach(dat)
 }
 
##histogram of Recruitement
p.rec.hist<-function(dat,fy=1978,ly=2012,ylab=expression(paste("Recruits at age 2 (number x ",10^9," )",sep="")))
{
  attach(dat)
  Data<-R
  Data<-subset(Data,Data[,1]<=ly&Data[,1]>=fy)
  t2<-length(Data[,1])-3
  menr<-mean(Data[1:t2,2])
  windows(12,8)
  xx<-list(breaks=seq((fy-0.5),(ly+0.5),by=1),counts=c(fy:ly),
          intensities=as.numeric(Data[,2]),density=as.numeric(Data[,2]),
          mids=seq(fy,ly,by=1),xname="Year",equidist=TRUE)
          attr(xx,"class")="histogram"
          plot(xx,freq=F,ylab=ylab,
          xlim=c((fy-1),(ly+1)),
          ylim=c(0,(max(Data[,5])*1.1)),main="",col="#ffff00",lab=c(10,10,7))
          arrows(Data[,1],Data[,4],Data[,1],Data[,5],col="black",angle=90,code=3,length=0.05)
          lines(Data[,1],rep(menr,length(Data[,1])),type="l",lty=3,col="blue",lwd=2)
          # text(fy+2,(max(Data[,2])*1.1),paste("Mean recruitment for ",fy," to ",ly-1,sep=""),pos=4,cex=0.8)
          # text(fy+2,(max(Data[,2])*1.1),"--",col="blue",cex=1.5)
detach(dat)
}


## Full selection F over time plot

p.full.f<-function(dat,f=1)## input:  data file, fishery number
{
  attach(dat)

 x<-paste("F_fsh_",f,sep="")
 xx<-get(x)
 windows(6,6)
 plot(xx[,1],xx[,3],ylab="Fishing Mortality",xlab="Year",type="o",pch=19,col="black",ylim=c(0,(max(xx[,3]*1.25))))

  detach(dat)
}

## Survey Selectivity Curve single plot

p.survey.curve<-function(dat,f=1,lage=2,hage=12)## input:  data file, survey number
{
attach(dat)
 x<-paste("sel_ind_",f,sep="")
 xx<-get(x)
 windows(6,6)
 la<-3
 ha<-ncol(xx)
 plot(c(lage:hage),xx[1,la:ha]/max(xx[1,la:ha]),ylab="Survey Selectivity"
 ,xlab="Age",type="o",pch=19,col="black",ylim=c(0,1))

  detach(dat)
}

## Fish Selectivity Curve plot

p.fish.curve<-function(dat,f=1,lage=2,hage=12)## input:  data file, survey number
{
attach(dat)
 x<-paste("sel_fsh_",f,sep="")
 xx<-get(x)
 xxx<-matrix(ncol=((hage-lage)+1),nrow=length(xx[,1]))
 la<-3
 ha<-ncol(xx)
 k<-length(xx[,1])

 for ( i in 1:length(xx[,1])){
 m<-max(xx[i,la:ha])
 xxx[i,1:((hage-lage)+1)]<-xx[i,la:ha]/m
 }


 windows(6,6)

 men_a<-array(dim=(hage-lage+1))
 for (i in lage:hage){
 men_a[((i-lage)+1)]<-mean(xxx[(k-6):(k-1),i])
 }

 plot(c(lage:hage),xxx[k-3,lage:hage],ylab="Selectivity",
 xlab="Age",type="o",pch=19,col="salmon",ylim=c(0,(max(xxx[k-3,lage:hage]*1.25))))

 points(c(lage:hage),xxx[k-2,lage:hage],type="o",col="blue",pch=3)
 points(c(lage:hage),men_a,type="l",col="black",lwd=2)
 points(c(lage:hage),2*mature_a,type="o",col="dark green",pch=17)

 text(4,0.4,paste(max(xx[,2])-2," Fishery Selectivity",sep=""),pos=4,cex=0.8 )
 text(4,0.3,paste(max(xx[,2])-1," Fishery Selectivity",sep=""),pos=4,cex=0.8)
 text(4,0.2,"Maturity at Age",pos=4,cex=0.8)
 text(4,0.1,"Five year ave. Fishery Selectivity",pos=4,cex=0.8)


 points(4,0.4,type="p",col="salmon",pch=19)
 points(4,0.2,type="p",col="dark green",pch=17)
 points(4,0.1,type="p",col="blue",pch=3)

 text(4,0.4,"--",cex=1.2,col="salmon")
 text(4,0.3,"--",cex=1.2,col="blue")
 text(4,0.2,"--",cex=1.2,col="dark green")
 text(4,0.1,"--",cex=1.2)

 detach(dat)
}
##biomass plots with polygons
## this function can handle the results of up to 2 separate models
# Still to do: 
#	make it able to handle more models (how many?)
#	make polygons transparent
p.biom.pol <- function (dat1, dat2, n.mod=1, typ="SSB",
        color=c("light blue", 'darkblue', 'light green', 'darkgreen'), new=TRUE)
{
  if(typ=="SSB")titl<-"Spawning Biomass (t)"
  if(typ=="TB"){typ="TotBiom"
                titl<-"Total Biomass (t)"
                }
  if(typ=="R"){typ="R"
                titl<-"Age 1 Recruites(N)"
                            }
  require(lattice)
  
  attach(dat1)
  Data = get(typ)  
  xlim1 = c(min(Data[,1]),max(Data[,1]))
  ymax1 = max(Data[,2])
  if(n.mod==1){
  	xlim = xlim1
  	ylim = c(0, ymax1*1.5)
  }
  detach(dat1)
  if(n.mod!=1){
  	attach(dat2)
	Data = get(typ)  
 	xlim2 = c(min(Data[,1]),max(Data[,1]))
 	ymax2 = max(Data[,2])
  	detach(dat2)
  	xlim = range(c(xlim1, xlim2))
  	ylim = c(0, max(ymax1, ymax2)*1.5)
  }
 
  attach(dat1)
  Data=get(typ)
  if(new==TRUE) windows(,6,6)
  par(mar=c(4,5,0.2,0.2))
  plot(Data[,1],Data[,2],type="n",lwd=2,cex.axis=2,cex.lab=2,
  xlab="Year",ylab=paste(titl),lab=c(10,10,7),xlim=xlim, ylim=ylim, col=color[2])
  x=c(Data[,1],Data[length(Data[,1]):1,1])
  y=c(Data[,4],Data[length(Data[,1]):1,5])

  polygon(x,y,col=color[1],lty=3,border="grey")
  lines(Data[,1],Data[,2],lwd=2, col=color[2])
  detach(dat1)
  
  if(n.mod!=1){
  	attach(dat2)
  Data=get(typ)
  x=c(Data[,1],Data[length(Data[,1]):1,1])
  y=c(Data[,4],Data[length(Data[,1]):1,5])

  polygon(x,y,col=color[3],lty=3,border="grey")
  lines(Data[,1],Data[,2], lwd=2, col=color[4])
  detach(dat2)
  
  attach(dat1)
  Data=get(typ)
  lines(Data[,1],Data[,2], lwd=2, col=color[2])
  detach(dat1)	
  }
  
}

mountains <-
  function(zmat, xvec=NULL, yvec=NULL, zscale=3, nshades=100,
           xaxs='i', yaxs='i', xlab="", ylab="", las=1, addbox=FALSE, cex.xax=1, cex.yax=1,...)
{
  ## DESCRIPTION:
  # a function by Ian Taylor designed to look like the cool-looking Figure 7 in
  # Butterworth D.S., Ianelli J.N., Hilborn R. (2003) A statistical model for
  # stock assessment of southern bluefin tuna with temporal changes in selectivity.
  # South African Journal of Marine Science 25:331-362.

  errors <- FALSE
  for(icol in 1:ncol(zmat)){
    if(!is.numeric(zmat[,icol])){
      errors <- TRUE
      print(paste("error: column",icol,"of zmat is not numeric"))
    }
  }
  if(errors) return(invisible())

  # fill in vectors if not provided
  nrowz <- nrow(zmat)
  ncolz <- ncol(zmat)
  if(is.null(yvec)) yvec <- 1:nrowz
  if(is.null(xvec)) xvec <- 1:ncolz

  # define some limits
  xmin <- min(xvec)
  xmax <- max(xvec)
  zmax <- zscale*max(zmat)

  ny <- length(yvec)
  if(ny!=nrowz){
      print("length(yvec) must equal nrow(zmat)",quote=FALSE)
      return()
  }
  if(length(xvec)!=ncolz){
      print("length(xvec) must equal ncol(zmat)",quote=FALSE)
      return()
  }

  zseq <- seq(0, zmax, length=nshades)
  xvec2 <- c(xmin, xvec, xmax) # adding extra points for bottom corners of polygon

  # plot(0, type='n', xlim=c(xmin, xmax), ylim=c(0, 1.1*(ymax+ny)), xaxs='i', yaxs='i', ...)
  plot(0, type='n', xlim=c(xmin, xmax), ylim=c(min(yvec), (max(yvec) + 1.1*zmax)),
       xaxs=xaxs, yaxs=yaxs, xlab=xlab, ylab=ylab, axes=F, ...)

  for(iy in ny:1){
    zvec <- as.numeric(zmat[iy, ])
    zvec2 <- c(0, zscale*zvec, 0) # row from z matrix

    # calculate set of all intersections between polygon and the horizontal lines
    x3list <- list()
    for(iz in 1:nshades){
      z <- zseq[iz]
      x3 <- numeric()
      for(ix in 2:length(xvec2)){
          z1 <- zvec2[ix-1]
          z2 <- zvec2[ix]
          x1 <- xvec2[ix-1]
          x2 <- xvec2[ix]
          if(z >= min(z1, z2) & z < max(z1, z2)){
              x3 <- c(x3, (z-z1)*(x2-x1)/(z2-z1)+x1)
          }
      }
      x3list[[iz]] <- x3
    }
    # draw little polygons between each pair of horizontal lines
    for(iz in 2:length(x3list)){

      z2 <- zseq[iz]
      z1 <- zseq[iz-1]

      x3hi <- x3list[[iz]] # x-values of intersections along upper line
      x3lo <- x3list[[iz-1]]   # x-values of intersections along lower line

      npoly <- length(x3lo)/2

      for(ipoly in 1:npoly){
        xlo <- x3lo[ipoly*2 + -1:0] # lower line intersections for individual polygon
        xhi <- x3hi[x3hi>=xlo[1] & x3hi<=xlo[2]] # upper line intersections
        extra <- (zvec2 >= z1 & zvec2 <= z2 & xvec2>=xlo[1] & xvec2<=xlo[2]) # identifying extra points to add
        xhi2 <- c(xhi,xvec2[extra]) # adding extra points to vector of upper x-values
        zhi <- c(rep(z2,length(xhi)), zvec2[extra]) # add corresponding z-values
        zhi2 <- zhi[order(xhi2)] # put the z-values in order based on order of x-values
        xhi2 <- sort(xhi2) # now order the x-values

        # make polygon
        polygon(x = c(xlo[2:1],xhi2),
                y = yvec[iy]+ c(z1,z1,zhi2),
                col = grey(1-.9*z1/zmax),border=grey(1-.9*z1/zmax))
      }
    }
    # black polygon around the outside
    polygon(xvec2, yvec[iy]+zvec2)
  }

  # add axes
  axis(1,at=xvec, cex.axis=cex.xax)
  axis(2,at=yvec,las=las, cex.axis=cex.yax)
#  axis(4,at=yvec,las=las,labels=FALSE) # extra ticks on right hand side
  if(addbox) box() # add box if desired
}


## Fish Selectivity curve over ages and years, for fisheries or surveys
sel.age.mountain = function(dat=data1, f=1, typ='F', xvec=c(2:12), yvec=NULL, zscale=3, nshades=100, xaxs='i', yaxs='i', xlab="", ylab="", las=1, new=TRUE, addbox=FALSE, cex.xax=1, cex.yax=1, ...)
{	
	attach(dat)
	if(typ=='F'){
		x = paste('sel_fsh_', f, sep='')
		mtitl=paste(Fshry_names[f], 'Selectivity')
	}
	if(typ=='S'){
		x = paste('sel_ind_', f, sep='')
		mtitl=paste(Index_names[f], 'Selectivity')
	}
	Select = get(x)
	zmat = Select[,-c(1:2)]
	yrs = Select[,2]
	if(new==TRUE) windows(,5,6.5)
	mountains(zmat, xvec=xvec, yvec=yrs, zscale=zscale, nshades=nshades, xaxs=xaxs, yaxs=yaxs, xlab=xlab, ylab=ylab, las=1, addbox=addbox, cex.xax=cex.xax, cex.yax=cex.yax)
	title(main=mtitl)
	detach(dat)	
}





cont.CB_TB<-function(dat,mod="1",f=1,f1=1,lage=2,hage=12,fy=1992,ly=2012,siz=20000,cl="BW",maxb=50000,ncol1=50)## input:  data file, fishery number,lowest and highest age,first and last year, scaler for graphic
{
  attach(dat)
  require(lattice)
   if(cl=="BW"){clr<-grey(seq(0.9,0.1,length=ncol1))}
   if(cl!="BW"){clr<-rainbow(ncol1,start=0.5,end=1)}


  la<-2
  ha<-length(N[1,])

  ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1

   titl="Biomass at Age"

     y<-paste("C_fsh_",f1,sep="")
     z<-paste("wt_fsh_",f1,sep="")

     x1<-get(y)
     x2<-get(z)
     Data<-x1*x2

        yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=length(N[1,]),nrow=length(N[,1]))
    for (i in tt:yrs){
        x[i,(lage):(hage)]<-N[i,(lage):(hage)]*wt_a_pop
     }


  #windows(8,8)
  filled.contour(y=N[tt:ti,1],x=c(lage:hage),t(Data[tt:ti,la:ha]),levels=seq(0,maxb,length=ncol1),
  ,ylab="Year",xlab="Age",xlim=c(0.5,hage),ylim=c(fy-0.5,ly+0.5),main=titl,col=clr,
  plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage));
             for (i in tt : ti){
             points(c(lage:hage),rep(N[i,1],hage-lage+1),cex=c(x[i,la:ha]/siz))
             }
            }
  )
  mtext("Catch (t)",4)
  mtext(paste("Model ",mod,sep=""),cex=0.5,side=3,padj=-10)
  detach(dat)
}



bp.f.age.res<-function(dat=data1,mod="1",typ="S",f=1,f1=1,lage=2,hage=12,siz=3){        ## input:  data file, fishery number,lowest age, highest age
  if (typ=="S"){
      x<-paste("phat_ind_",f,sep="")
      y<-paste("pobs_ind_",f,sep="")
      titl="Survey Standardized Residuals"
      }
  if (typ!="S"){
      x<-paste("phat_fsh_",f1,sep="")
      y<-paste("pobs_fsh_",f1,sep="")
      titl="Fishery Standardized Residuals"
      }

attach(dat)
require(lattice)
la<-2
ha<-length(N[1,])

xx<-get(x)
yy<-get(y)
rr<-xx-yy
mean_rr<-mean(c(rr[,lage:hage]))
sd_rr<-sd(c(rr[,lage:hage]))

ti<-(min(xx[,1]) )
tt<-max(1978,min(N[,1]))-min(N[,1])+1
sr<-rr-(mean_rr)/sd_rr

#windows(8,8)

     la<-2
     ha<-length(N[1,])
     plot(c(lage,hage),c(min(xx[,1]),max(xx[,1])),type="n",ylab="Year",xlab="Age",lab=c(10,10,7),main=titl)
     for (i in 1 : length(xx[,1])){
     for (j in lage:hage){
         if(sr[i,j]>0){symbols(j,xx[i,1],circles=(siz*sr[i,j]),inches=F,bg="red",fg="red",add=T)}
         if(sr[i,j]<0){symbols(j,xx[i,1],circles=(-siz*sr[i,j]),inches=F,fg="blue",add=T)}
         if(sr[i,j]==0){points(j,xx[i,1],pch=4)}
         }}
         mtext(paste("Model ",mod,sep=""),cex=0.5,side=3,padj=-10)
         
        # windows(2,6)
        # d<-seq(max(sr),min(sr),length=10)
        # plot(seq(2,15,length=10),d,type="n",xlab="",ylab="Standardized Residuals",xaxt="n")
        # for ( i in 1:10){
        # if (d[i]>0){ symbols(8,d[i],circles=(3*d[i]),inches=F,fg="red",bg="red",add=T) }
        # if (d[i]<0){ symbols(8,d[i],circles=(-3*d[i]),inches=F,fg="blue",add=T) }
        # if (d[i]==0){ points(8,d[i],pch=4) }
        # }
         
         


detach(dat)
}


##F with biomass or catch biomass
cont.f.mort3<-function(dat=data1,typ="CB",f=1,lage=2,hage=12,fy=1992,ly=2012,siz=20000,cl="BW")## input:  data file, fishery number,lowest and highest age,first and last year, scaler for graphic
{
  attach(dat)
  require(lattice)
   if(cl=="BW"){clr<-grey(seq(0.9,0.1,length=20))}
   if(cl!="BW"){clr<-rainbow(25,start=0.5,end=1)}

  al<-2
  ah<-ncol(N)
  y<-paste("F_age_",f,sep="")
  Data<-get(y)

  ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1

  if(typ=="CB"){
     y<-paste("C_fsh_",f,sep="")
     y1<-paste("wt_fsh_",f,sep="")
     x<-get(y)
     x1<-get(y1)
     x=x*x1
     titl<-"Catch at Age"
    }

  if(typ=="B"){
      titl<-"Biomass at Age"
      yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=length(N[1,]),nrow=length(N[,1]))
    for (i in tt:yrs){
        x[i,(lage):(hage)]<-N[i,(lage):(hage)]*wt_a_pop
     }}
  windows(6,8)
  filled.contour(y=Data[tt:ti,1],x=c(lage:hage),t(Data[tt:ti,al:ah]),
  ,ylab="Year",xlab="Age",xlim=c(0.5,hage),ylim=c(fy-0.5,ly+0.5),main=titl,col=clr,
  plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage));
             for (i in tt : ti){
             points(c(lage:hage),rep(N[i,1],length(lage:hage)),cex=c(x[i,al:ah]/siz))
             }
            }
  )
  mtext("F",4)
  detach(dat)
}



c.select.b2<-function(dat=data1,typ="F",typ2="CB",f=1,lage=2,hage=12,fy=1992,ly=2012,siz=20000,cl="BW")
{
  if (typ=="S"){
      y<-paste("sel_ind_",f,sep="")
      }
  if (typ!="S"){
      y<-paste("sel_fsh_",f,sep="")
      }
  if(cl=="BW"){clr<-grey(seq(1,0.2,length=30))}
  if(cl!="BW"){clr<-rainbow(30,start=0.18,end=1)}

  la=3

  attach(dat)

  ti<-(ly-min(N[,1]))+1
  tt<-max(fy,min(N[,1]))-min(N[,1])+1


  if(typ2=="NA"){
     x<-N
     titl<-"Numbers at Age"
     }

  if(typ2=="CB"){
     z<-paste("C_fsh_",f,sep="")
     z2=paste("wt_fsh_",f,sep="")
     x<-get(z)
     x1<-get(z2)
     x<-x*x1
     titl<-"Catch at Age"
    }

  if(typ2=="B"){
      titl<-"Biomass at Age"
      yrs<-ti-tt+1
      ags<-hage-lage+1
     x<-matrix(ncol=length(N[1,]),nrow=length(N[,1]))
    for (i in tt:yrs){
        x[i,2:ncol(x)]<-N[i,2:ncol(x)]*wt_a_pop
     }}



  y<-get(y)
  n1<-max(fy,min(y[,2]))-min(y[,2])+1
  n2<-ly-min(y[,2])+1

 Data<-matrix(ncol=((hage-lage)+1),nrow=length(y[,1]))
 ha<-ncol(y)
  for ( i in 1:length(y[,1])){
      m<-max(y[i,la:ha])
      Data[i,1:(hage-lage+1)]<-y[i,la:ha]/m
  }

  g<-min(n2,length(Data[,2]))

  ti<-(g-n1+1)

  windows(6,8)
  filled.contour(y=y[n1:g,2],x=c(lage:hage),t(Data[n1:g,]),xlim=c(0.5,hage),ylim=c(fy-0.5,ly+0.5)
  ,ylab="Year",xlab="Age",col=clr,levels=seq(0,max(Data[,]),length=30),nlevels=30,
  main=titl,plot.axes={axis(2,seq(fy,ly,by=2));axis(1,c(lage:hage));
             for (i in 1 : ti){
             points(c(lage:hage),rep(N[i,1],length(lage:hage)),cex=c(x[i,2:ncol(x)]/siz))
             }
            })

  mtext("Selectivity",4)



detach(dat)
}


spwn_ratio <- function (dat,color="light blue",main="Model 1",fy=1992,ly=2011)
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



plot_FSSB<-function(dat=data1){
  attach(dat)
  limy<-max(c(SSB_fut_1[,5],SSB_fut_2[,5],SSB_fut_3[,5],SSB_fut_4[,5],SSB_fut_5[,5]))
  limy<-limy+limy*0.1
  plot(SSB_fut_1[,2]~SSB_fut_1[,1],type="l",ylim=c(0,limy),ylab="SSB (t)",xlab="Year")
  points(SSB_fut_1[,4]~SSB_fut_1[,1],type="l",lty=3)
  points(SSB_fut_1[,5]~SSB_fut_1[,1],type="l",lty=3)
  points(SSB_fut_2[,2]~SSB_fut_2[,1],type="l",col="red",lwd=2)
  points(SSB_fut_2[,4]~SSB_fut_2[,1],type="l",lty=3,col="red")
  points(SSB_fut_2[,5]~SSB_fut_2[,1],type="l",lty=3,col="red")
  
  points(SSB_fut_3[,2]~SSB_fut_3[,1],type="l",col="blue",lwd=2)
  points(SSB_fut_3[,4]~SSB_fut_2[,1],type="l",lty=3,col="blue")
  points(SSB_fut_3[,5]~SSB_fut_2[,1],type="l",lty=3,col="blue")
  
  
  points(SSB_fut_4[,2]~SSB_fut_4[,1],type="l",col="green",lwd=2)
  points(SSB_fut_4[,4]~SSB_fut_4[,1],type="l",lty=3,col="green")
  points(SSB_fut_4[,5]~SSB_fut_4[,1],type="l",lty=3,col="green")
  
  
  points(SSB_fut_5[,2]~SSB_fut_5[,1],type="l",col="purple",lwd=2)
  points(SSB_fut_5[,4]~SSB_fut_5[,1],type="l",lty=3,col="purple")
  points(SSB_fut_5[,5]~SSB_fut_5[,1],type="l",lty=3,col="purple")
  text(2012,limy,"Alt1",pos=4)
  text(2012,limy-limy*0.05,"Alt2",pos=4,col="red")
  text(2012,limy-limy*0.1,"Alt3",pos=4,col="blue")
  text(2012,limy-limy*0.15,"Alt4",pos=4,col="green")
  text(2012,limy-limy*0.2,"Alt5",pos=4,col="purple")
  
  detach(dat)
  }
  
  
  
  
plot_FCATCH<-function(dat=data1){
  attach(dat)
  limy<-max(c(Catch_fut_1[,2],Catch_fut_2[,2],Catch_fut_3[,2],Catch_fut_4[,2],Catch_fut_5[,2]))
  limy<-limy+limy*0.1
  plot(Catch_fut_1[,2]~Catch_fut_1[,1],type="l",ylim=c(0,limy),ylab="Catch (t)",xlab="Year")
  points(Catch_fut_2[,2]~Catch_fut_2[,1],type="l",col="red",lwd=2)  
  points(Catch_fut_3[,2]~Catch_fut_3[,1],type="l",col="blue",lwd=2)  
  points(Catch_fut_4[,2]~Catch_fut_4[,1],type="l",col="green",lwd=2)  
  points(Catch_fut_5[,2]~Catch_fut_5[,1],type="l",col="purple",lwd=2)
  
  text(2012,limy,"Alt1",pos=4)
  text(2012,limy-limy*0.05,"Alt2",pos=4,col="red")
  text(2012,limy-limy*0.1,"Alt3",pos=4,col="blue")
  text(2012,limy-limy*0.15,"Alt4",pos=4,col="green")
  text(2012,limy-limy*0.2,"Alt5",pos=4,col="purple")
  
  detach(dat)
  }
  

