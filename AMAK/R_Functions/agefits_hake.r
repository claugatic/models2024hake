AgeFits <- function(dat=data1,case_label="2012 assessment",lage=2,hage=12,lyr=1992,f=1) 
{
   subtle.color <- "gray40"
   # ages <- c(1,length(x$pobs_fsh[1,-1]) ) #age range
   attach(dat)
   ages <- c(lage,hage) #age range
   tmp1 <- paste("phat_fsh_",f,sep="")
   tmp2 <- paste("pobs_fsh_",f,sep="")
   tmp3 <- paste("pobs_fsh_",f,sep="")
   ages.list <- ages[1]:ages[2]
   print(ages.list)
   print(tmp1)
   pred.data = get(tmp1)[,-1]
   obs.data  = get(tmp2)[,-1]
   years     = get(tmp3)[,1] # use
   cyear<-as.numeric(format(Sys.Date(), "%Y"))
   years1<-seq((lyr-1),cyear)
   print(tmp3)
   print(dim(obs.data))
   nyears <- length(years)
   #nyears1<-cyear-lyr-1     
   nyears1<-cyear-lyr+1     
   
   nages <- length(ages.list)  
   mfcol <- c(ceiling(nyears/3),3)
   mfcol1<- c(ceiling(nyears1/3),3)
   par(mfcol=mfcol,oma=c(3.5,4.5,3.5,1),mar=c(0,0,0,0))
   cohort.color <- rainbow(mfcol1[1]+min(10,nages))[-c(1:2)]   #use hideous rainbow colors because they loop more gracefully than rich.colors
   cohort.colors<-matrix(ncol=nyears1,nrow=length(cohort.color))
   cohort.colors<-data.frame(cohort.colors)
   ncolors <- length(cohort.color)

for(i in 1:nyears1){
   cohort.color <- c(cohort.color[ncolors],cohort.color[-1*ncolors])
   cohort.colors[,i]<- cohort.color
   }
   
 
   ylim <- c(0,1.05*max(obs.data,pred.data))
   for (yr in 1:nyears) {
                                                      
      year1<-years[yr]
            
      names.arg <- rep("",nages)
      #print(length(names.arg))
      x <- barplot(obs.data[yr,],space=0.2,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",border=subtle.color,
                  col=cohort.colors[1:nages,year1-(lyr-1)],axes=F,ylab="",xlab="")
      #cohort.color <- c(cohort.color[ncolors],cohort.color[-1*ncolors])  #loop around colors
      if (yr %% mfcol[1] == 0) {
         axis(side=1,at=x,lab=ages.list, line=-0.1,col.axis=subtle.color, col=subtle.color,lwd=0,lwd.ticks=0)  #just use for the labels, to allow more control than names.arg
      }
      if (yr <= mfcol[1]) {
        axis(2,las=1,at=c(0,0.5),col=subtle.color,col.axis=subtle.color,lwd=0.5)
      }
      par(new=T)
      par(xpd=NA)
      plot(x=x,y=pred.data[yr,],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",
          bg="white",fg="brown",
          pch=19,cex=0.8,axes=F,ylab="",xlab="")
      box(col=subtle.color,lwd=0.5)
      x.pos <- par("usr")[1] + 0.85*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
      y.pos <- par("usr")[3] + 0.75*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
      text(x=x.pos,y=y.pos,years[yr],cex=1.2, col=subtle.color)
      par(xpd=T)
   }
   mtext(side=1,outer=T,"Age",line=2)
   mtext(side=2,outer=T,"Proportion",line=3.2)
   mtext(side=3,outer=T,line=1.2,paste(dat$Fshry_names[f],"fishery age composition data"))
   mtext(side=3,outer=T,line=0.2,paste("(",case_label,")",sep=""),cex=0.6)
   detach(dat)
}

AgeFitsSrv <- function(dat=data1, main="",case_label="2012 assessment",lyr=1992,lage=2,hage=12,f=1,rec_age=2) {
   subtle.color <- "gray40"
   attach(dat)
   ages <- c(lage,hage) #age range
   
   tmp1 <- paste("phat_ind_",f,sep="")
   tmp2 <- paste("pobs_ind_",f,sep="")
   tmp3 <- paste("pobs_ind_",f,sep="")
   
   ages.list <- ages[1]:ages[2]
   
   print(ages.list)
   print(tmp1)
   
   pred.data = get(tmp1)[,-1]
   obs.data  = get(tmp2)[,-1]
   years     = get(tmp3)[,1]
  
   cyear<-as.numeric(format(Sys.Date(), "%Y"))
   
   years1<-seq((lyr-1),cyear)
   
   nyears <- length(years)
   nyears1<- cyear-(lyr-1)
    
   nages <- length(ages.list)  
   mfcol <- c(ceiling(nyears/3),3)
   mfcol1<- c(ceiling(nyears1/3),3)
   par(mfcol=mfcol,oma=c(3.5,4.5,3.5,1),mar=c(0,0,0,0))
   cohort.color <- rainbow(mfcol1[1]+10)[-c(1:2)]   #use hideous rainbow colors because they loop more gracefully than rich.colors
   cohort.colors<-matrix(ncol=nyears1,nrow=length(cohort.color))
   cohort.colors<-data.frame(cohort.colors)
   ncolors <- length(cohort.color)

for(i in 1:nyears1){
   cohort.color <- c(cohort.color[ncolors],cohort.color[-1*ncolors])
   cohort.colors[,i]<- cohort.color
   }
     
ylim <- c(0,1.05*max(obs.data,pred.data))
   for (yr in 1:nyears) {
                                                      
      year1<-years[yr]
            
      names.arg <- rep("",nages)
      #print(length(names.arg))
      x <- barplot(obs.data[yr,],space=0.2,ylim=ylim,las=1,names.arg=names.arg, cex.names=0.5, xaxs="i",yaxs="i",border=subtle.color,
                  col=cohort.colors[1:nages,year1-(lyr-1)],axes=F,ylab="",xlab="")
      #cohort.color <- c(cohort.color[ncolors],cohort.color[-1*ncolors])  #loop around colors
      if (yr %% mfcol[1] == 0) {
         axis(side=1,at=x,lab=ages.list, line=-0.1,col.axis=subtle.color, col=subtle.color,lwd=0,lwd.ticks=0)  #just use for the labels, to allow more control than names.arg
      }
      if (yr <= mfcol[1]) {
        axis(2,las=1,at=c(0,0.5),col=subtle.color,col.axis=subtle.color,lwd=0.5)
      }
      par(new=T)
      par(xpd=NA)
      plot(x=x,y=pred.data[yr,],ylim=ylim, xlim=par("usr")[1:2], las=1,xaxs="i",yaxs="i",
          bg="white",fg="brown",
          pch=19,cex=0.8,axes=F,ylab="",xlab="")
      box(col=subtle.color,lwd=0.5)
      x.pos <- par("usr")[1] + 0.85*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
      y.pos <- par("usr")[3] + 0.75*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
      text(x=x.pos,y=y.pos,years[yr],cex=1.2, col=subtle.color)
      par(xpd=T)
   }   
   mtext(side=1,outer=T,"Age",line=2)
   mtext(side=2,outer=T,"Proportion",line=3.2)
   mtext(side=3,outer=T,line=1.2,paste(dat$Index_names[f],"index age composition data"))
   mtext(side=3,outer=T,line=0.2,paste("(",case_label,")",sep=""),cex=0.6)
   detach(dat)
   par(mfcol=c(1,1), mar=c(5.1,4.1,4.1,2.1))
}
##effective N Plots
p.eff.n<-function(dat,typ="F",f=1)
  {
      if (typ=="S"){
         x<-paste("EffN_Survey_",f,sep="")
         titl<-"Survey Mean Age"
      }

      if (typ!="S"){
         x<-paste("EffN_Fsh_",f,sep="")
         titl<-"Fishery Mean Age"
      }
      attach(dat)
      require(plotrix)
      Data<-get(x)
      plot(Data[,1],Data[,4],
        ylim=c(0,(max(Data[,4])*1.1)),cex.lab=1.7,
        ylab=paste(titl),xlab="Year",pch=19,col="blue",lab=c(10,10,7))

      arrows(Data[,1],Data[,7],Data[,1],Data[,8],col="black",angle=90,code=3,length=0.05)
      points(Data[,1],Data[,5],type="l",col="red",lwd=2)

      # Legend
      legx = max(Data[,1])-9
      text(  legx,2.0,"Observed       ",pos=4,cex=1.1)
      text(  legx,1.6,"Model-predicted",pos=4,cex=1.1)
      points(legx,2.0,pch=19,col="blue")
      text(  legx,1.6,"--",cex=1.5,col="red")
      detach(dat)
  }

# pdf("figs\\AussieAge.pdf",width=6, height=11.5)
#win.graph(width=8,height=11.5)
# AussieAgeFits(labrep.file="sbtmod22_lab.rep",case_label="c1s1l1orig.5_h1m1M1O1C2a1")
# dev.off()
#[7:13:24 AM] *** Niels Hintzen sent trend in catch.png ***
#The easy fit would be: just replace the code: 
#axis(2,las=1,at=c(0,0.5),col=subtle.color,col.axis=subtle.color,lwd=0.5) 
#With
#axis(2,las=1,at=c(0,0.25,0.5),col=subtle.color,col.axis=subtle.color,lwd=0.5)
#[1:14:05 PM] Niels Hintzen: and for the colors: change the line that states: 
#cohort.color <-
#[1:14:12 PM] Niels Hintzen: with
#cohort.color <- rainbow(ages[2]+2)[-c(1:2)]   
#[1:18:18 PM] JNI: AgeFitsSrv
#pdf("figs\\agefits1.pdf",width=9, height=7)
#  AgeFits(mod4,f=1)
#  AgeFits(mod4,f=2)
#  AgeFits(mod4,f=3)
#  AgeFits(mod4,f=4)
#  AgeFitsSrv(mod4,f=1)
#  AgeFitsSrv(mod4,f=4)
#dev.off()
