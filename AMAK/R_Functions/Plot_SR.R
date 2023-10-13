Plot_SR <- function (Rec, SSB, res=mod1, class, cols) {
  pic <- xyplot(Rec~SSB,data=res,groups=class,
                ylab="Recruitment",xlab="Spawning Stock Biomass",main="Stock Recruitment",
                  panel=function(x,y){
                  panel.grid(h=-1, v= -1)
                  idxobs <- which(res$SSB %in% x & res$class == "observed")
                  idxmod <- which(res$SSB %in% x & res$class == "modelled")
                  panel.xyplot(x[idxobs],y[idxobs],type="l",lwd=3,col=1,lty=3)
                  panel.points(x[idxobs],y[idxobs],type="p",cex=0.6,pch=19,col="darkgrey")
                  panel.xyplot(x[idxmod],y[idxmod],type="l",lwd=5,col=rev(cols)[1],lty=2)
                })
  print(pic)
}
Plot_SR()