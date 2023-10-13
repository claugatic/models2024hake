filled.contour.TAB <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1,
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE),
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE),
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors,
    col = color.palette(length(levels) - 1), plot.title, plot.axes,
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1,
    axes = TRUE, frame.plot = axes, ...)
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
        stop("increasing 'x' and 'y' values expected")
     plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
        stop("no proper 'z' matrix specified")
    if (!is.double(z))
        storage.mode(z) <- "double"
    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels),
        col = col))
    invisible()
}

#######################################################################
#Plot of B/Bmsy against F/Fmsy for the most recent year
#Trevor A. Branch  10 September 2009  tbranch@gmail.com
#######################################################################
plot.phase.plane <- function(SSB0,Fabc,Fmsy,BoverBmsy, FoverFmsy,xlim,ylim,header,bw.mult=1,jitter.fac=0) {
   #plot(x=BoverBmsy,y=FoverFmsy,xlim=xlim,ylim=ylim,las=1,
   #       yaxs="i",xaxs="i",xlab="",ylab="")
   require(KernSmooth)
   crosshair.data.uncen <- cbind(BoverBmsy,FoverFmsy)
   #APRIL 22 version: References in Scott 1992 and Bowman and Azzalini 1997
   d<-2 # the bandwidth dimension
   bmsy.bw<-sqrt(var(crosshair.data.uncen[,1]))*(4/((d+2)*length(crosshair.data.uncen[,1])))^(1/(d+4))
   umsy.bw<-sqrt(var(crosshair.data.uncen[,2]))*(4/((d+2)*length(crosshair.data.uncen[,2])))^(1/(d+4))
   # please note the range restrictions at 2.01 to include the points that line up at the boundaries
   kernel.dens <- bkde2D(crosshair.data.uncen[,c(1,2)], bandwidth=c(bmsy.bw*bw.mult,umsy.bw*bw.mult), range.x=list(xlim,ylim))

   # generate color palette
   paletteable.egg<-colorRampPalette(c("#BFEFFF","white","white", "yellow","#FFC125"))
   filled.contour.TAB(kernel.dens$x1, kernel.dens$x2, kernel.dens$fhat, nlevels=15, color.palette =paletteable.egg,
               xlab="", ylab="", xlim=xlim, ylim=ylim, cex.lab=1.3)
   par(new=T)

   plot(x=jitter(BoverBmsy,jitter.fac),y=jitter(FoverFmsy,jitter.fac),type="l",xlim=xlim,ylim=ylim,las=1,
          yaxs="i",xaxs="i",xlab="",ylab="",col="gray50",pch=20)

  yr<-test$F_fsh_1[,1]-1900
  yr[yr>=100]<-yr[yr>=100]-100

  for(i in 1:length(BoverBmsy)){
    text(BoverBmsy[i],FoverFmsy[i],paste(yr[i]),cex=0.85)
    }

    k=Fabc/Fofl*(((SSB0*0.2)/(SSB0*0.35))-0.05)/(1-0.05)
    k2<-0.05
    k3<-(0.2*SSB0)/(SSB0*0.35)
    points(c(k3,k3),c(0,10),type="l",lty=3)
    points(c(1,xlim[2]),c(Fabc/Fmsy,Fabc/Fmsy),type="l",lwd=2)
    points(c(1,xlim[2]),c(1,1),type="l",lwd=2,col="red")
    points(c(k3,1.0),c(k,Fabc/Fmsy),type="l",lwd=2)
    points(c(k3,k3),c( 0,k),type="l",lwd=2)
    points(c(k2,1),c(0,1),type="l",lwd=2,col="red")

    text(xlim[2]-1,ylim[2]-0.1,"OFL Definition",pos=4)
    text(xlim[2]-1,ylim[2]-0.2,"ABC Control Rule",pos=4)
    text (xlim[2]-1,ylim[2]-0.3,"B20%",pos=4)

    points(c(xlim[2]-1.2,xlim[2]-1),c(ylim[2]-0.1,ylim[2]-0.1),lwd=2,type="l",col="red")
    points(c(xlim[2]-1.2,xlim[2]-1),c(ylim[2]-0.2,ylim[2]-0.2),lwd=2,type="l")
    points(c(xlim[2]-1.2,xlim[2]-1),c(ylim[2]-0.3,ylim[2]-0.3),type="l",lty=3)

   mtext(side=1,outer=F,line=3.2,expression(B/B[MSY]),cex=1.3)
   mtext(side=2,outer=F,line=3,expression(F/F[MSY]),cex=1.3)
   mtext(side=3,outer=F,line=0.5,header,cex=1.3)
   abline(h=1,lty=2)
   abline(v=1,lty=2)
}

