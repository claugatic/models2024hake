biom<-read.table(paste(getwd(),"/proj/",data_file_name,"_out/percentiles.out",sep=""),nrows=1,header=T)

r12<-test2$est[test2$name=="recruits"][35]
nage12<-(test$N[test$Yr==2011][2:15])-(test$N[test$Yr==2011][2:15])*test$F_age_1[test$Yr==2011][2:15]-(test$N[test$Yr==2011][2:15])*test$M[2]

nage12<-c(r12,nage12[1:12],nage12[13]+nage12[14] )
wage12<-nage12*test$wt_a_pop


a1<-subset(test$Obs_Survey_1,test$Obs_Survey_1[,2]>0)
rmse<-sqrt(mean(log((a1[,2])/(a1[,3]))^2))


rec_dev<-sd(log(test2$est[test2$name=="recruits"]))



effnFish<-mean(test$EffN_Fsh_1[,2])
effnSurv<-mean(test$EffN_Survey_1[,2])


b_bzero<-test$SSB[,2]/biom$SB0

plot_comp_old<-function(){
  maxy<-max(OLD_SPBIOM[1:43,2:5])
  maxy<-max(maxy,max(test$SSB[,2]))
  par(mar=c(4,5,0.2,0.2))
  plot(OLD_SPBIOM$YEAR,as.numeric(OLD_SPBIOM$Model_2010),ylim=c(0,maxy),xlim=c(1963,2012),type="l",lwd=2,ylab=expression(paste("Female spawning biomass (t x",10^6,")",sep="")),xlab="Year",cex.lab=1.5,cex.axis=1.5)
  points(OLD_SPBIOM$YEAR,as.numeric(OLD_SPBIOM$Model_2009),type="l",col="blue",lty=2,lwd=2)
  points(OLD_SPBIOM$YEAR,as.numeric(OLD_SPBIOM$Model_2008),type="l",col="dark green",lty=4,lwd=2)
  points(OLD_SPBIOM$YEAR,as.numeric(OLD_SPBIOM$Model_2007),type="l",col="orange",lty=5,lwd=2)
  points(test$SSB[,1],test$SSB[,2],type="o",col="red",lwd=3)

  text(1970,maxy-50,"2007 Model",pos=4)
  text(1970,maxy-75,"2008 Model",pos=4)
  text(1970,maxy-100,"2009 Model",pos=4)
  text(1970,maxy-125,"2010 Model",pos=4)
  text(1970,maxy-150,"2011 Model",pos=4)
    
  points(c(1965,1970),rep(maxy-50,2),type="l",col="orange",lty=5,lwd=2)
  points(c(1965,1970),rep(maxy-75,2),type="l",col="dark green",lty=5,lwd=2)
  points(c(1965,1970),rep(maxy-100,2),type="l",col="blue",lty=5,lwd=2)
  points(c(1965,1970),rep(maxy-125,2),type="l",col="black",lty=5,lwd=2)
  points(c(1965,1970),rep(maxy-150,2),type="o",col="red",lwd=2)
    
    
  windows()
  maxy<-max(OLD_TOTBIOM[1:31,2:5])
  maxy<-max(maxy,max(test$TotBiom[,2]))
  par(mar=c(4,5,0.2,0.2))
  plot(OLD_TOTBIOM$YEAR,as.numeric(OLD_TOTBIOM$Model_2010),ylim=c(0,maxy),xlim=c(1978,2012),type="l",lwd=2,ylab=expression(paste("Total biomass (t x",10^6,")",sep="")),xlab="Year",cex.lab=1.5,cex.axis=1.5)
  points(OLD_TOTBIOM$YEAR,as.numeric(OLD_TOTBIOM$Model_2009),type="l",col="blue",lty=2,lwd=2)
  points(OLD_TOTBIOM$YEAR,as.numeric(OLD_TOTBIOM$Model_2008),type="l",col="dark green",lty=4,lwd=2)
  points(OLD_TOTBIOM$YEAR,as.numeric(OLD_TOTBIOM$Model_2007),type="l",col="orange",lty=5,lwd=2)
  points(test$TotBiom[,1],test$TotBiom[,2],type="o",col="red",lwd=3)

  text(2005,maxy-50,"2007 Model",pos=4)
  text(2005,maxy-110,"2008 Model",pos=4)
  text(2005,maxy-170,"2009 Model",pos=4)
  text(2005,maxy-230,"2010 Model",pos=4)
  text(2005,maxy-290,"2011 Model",pos=4)
  points(c(2000,2005),rep(maxy-50,2),type="l",col="orange",lty=5,lwd=2)
  points(c(2000,2005),rep(maxy-110,2),type="l",col="dark green",lty=5,lwd=2)
  points(c(2000,2005),rep(maxy-170,2),type="l",col="blue",lty=5,lwd=2)
  points(c(2000,2005),rep(maxy-230,2),type="l",col="black",lty=5,lwd=2)
  points(c(2000,2005),rep(maxy-290,2),type="o",col="red",lwd=3)
}


##

x<-hist(test.mcmc$Sbiom[3:5000]/biom$SB0,breaks=seq(0,0.6,0.01))
plot(x$counts/sum(x$counts)~x$mids,type="l",lwd=3,ylab="Probability",xlab=expression(paste("2012 SS",B[2012],"/SS",B[0],sep="")),main=expression(paste("2012 SS",B[2012],"/SS",B[0]," posterior density",sep="")))
points(c(0,100)~c(0.2,0.2),lwd=2,type="l",col="red",lty=2)
Prob20<-length(test.mcmc$Sbiom[3:5000][test.mcmc$Sbiom[3:5000]<=0.2*biom$SB0])/4998


## plot projections
plot_proj<-function(alt=1){
  alt1<-subset(percentdb,percentdb$ALT==alt)

  windows()
  par(mar=c(4,5,0.2,0.2))
  plot(alt1$VALUE[alt1$NAME=="SSBMean"]~alt1$YEAR[alt1$NAME=="SSBMean"],type="o",pch=16,lwd=3,ylim=c(0,biom$SB0),ylab="Projected female spawning biomass (1000t)",xlab="Year",cex.lab=1.5,cex.axis=1.2)
  points(alt1$VALUE[alt1$NAME=="SSBLCI"]~alt1$YEAR[alt1$NAME=="SSBMean"],type="l",lty=3)
  points(alt1$VALUE[alt1$NAME=="SSBUCI"]~alt1$YEAR[alt1$NAME=="SSBMean"],type="l",lty=3)
  points(rep(biom$SB40,14)~c(2011:2024),type="l",lty=2,col="red")
  points(rep(biom$SB35,14)~c(2011:2024),type="l",lty=2,col="orange",lwd=2)
  points(rep(biom$SB0,14)~c(2011:2024),type="l",lty=2,col="blue",lwd=3)

  text(2012,biom$SB0-50,"Projected SSB",pos=4)
  text(2012,biom$SB0-60,"SSB0",pos=4)
  text(2012,biom$SB0-70,"SSB40",pos=4)
  text(2012,biom$SB0-80,"SSB35",pos=4)


    points(c(2011,2012),rep(biom$SB0-50,2),type="o",lwd=3)
    points(c(2011,2012),rep(biom$SB0-60,2),type="l",col="blue",lty=2,lwd=3)
    points(c(2011,2012),rep(biom$SB0-70,2),type="l",col="red",lty=2,lwd=1)
    points(c(2011,2012),rep(biom$SB0-80,2),type="l",col="orange",lty=2,lwd=2)

  windows()
  par(mar=c(4,5,0.2,0.2))
  plot(alt1$VALUE[alt1$NAME=="CMean"]~alt1$YEAR[alt1$NAME=="CMean"],type="o",pch=16,lwd=3,ylim=c(0,Cofl+100),ylab="Projected Catch (1000t)",xlab="Year",cex.lab=1.5,cex.axis=1.2)
  points(alt1$VALUE[alt1$NAME=="CLCI"]~alt1$YEAR[alt1$NAME=="CMean"],type="l",lty=3)
  points(alt1$VALUE[alt1$NAME=="CUCI"]~alt1$YEAR[alt1$NAME=="CMean"],type="l",lty=3)
  points(rep(Cofl,14)~c(2011:2024),type="l",lty=2,col="red")
  points(rep(Cabc,14)~c(2011:2024),type="l",lty=2,col="orange",lwd=2)

  text(2012,Cofl+100-50,"Projected Catch",pos=4)
  text(2012,Cofl+100-60,"Cofl",pos=4)
  text(2012,Cofl+100-70,"Cabc",pos=4)

  points(c(2011,2012),rep(Cofl+100-50,2),type="o",lwd=3)
  points(c(2011,2012),rep(Cofl+100-60,2),type="l",col="red",lty=2,lwd=1)
  points(c(2011,2012),rep(Cofl+100-70,2),type="l",col="orange",lty=2,lwd=2)

  windows()
  par(mar=c(4,5,0.2,0.2))
  plot(alt1$VALUE[alt1$NAME=="F_Mean"]~alt1$YEAR[alt1$NAME=="F_Mean"],type="o",pch=16,lwd=3,ylim=c(0,1.0),ylab="F",xlab="Year",cex.lab=1.5,cex.axis=1.2)
  points(alt1$VALUE[alt1$NAME=="F_LCI"]~alt1$YEAR[alt1$NAME=="F_Mean"],type="l",lty=3)
  points(alt1$VALUE[alt1$NAME=="F_UCI"]~alt1$YEAR[alt1$NAME=="F_Mean"],type="l",lty=3)
  points(rep(Fofl,14)~c(2011:2024),type="l",lty=2,col="red")
  points(rep(Fabc,14)~c(2011:2024),type="l",lty=2,col="orange",lwd=2)

  text(2012,0.8,"Projected F",pos=4)
  text(2012,0.75,"Fofl",pos=4)
  text(2012,0.7,"Fabc",pos=4)


    points(c(2011,2012),rep(0.8,2),type="o",lwd=3)
    points(c(2011,2012),rep(0.75,2),type="l",col="red",lty=2,lwd=1)
    points(c(2011,2012),rep(0.7,2),type="l",col="orange",lty=2,lwd=2)


}





# function: my.rtf.table
# purpose: convert a matrix, data.frame, or array into a rtf table
# output: text for RTF, possibly written to a file
# inputs:
# tab - a table, dataframe, or array (needs rownames and colnames)
# outfile - name of file (or console if NULL, which is default)
# rtffile - if T (default) then add {\rtf1 to beginning and } to end, making
# a full RTF file, if F then leave these off
# header - if T (default) then bold the table header
# ... - passed to format for the table body only
# tips: output to tempfile and use WordInsertFile(...) from the svViews
# package to easily convert a table to Microsoft Word
my.rtf.table <- function(tab,outfile=NULL,rtffile=T,header=T,...) {
if (!is.null(outfile)) sink(outfile)
tab.nrow<-nrow(tab)
tab.ncol<-ncol(tab)
if (rtffile) {
#begin RTF document
cat("{\\rtf1\n")
}
#populate header row
cat("\\trowd\\trautofit1\\intbl\n")
j <- 1
for (i in 1:(tab.ncol+1)) {
cat("\\cellx",j,'\n',sep='')
j<-j+1
}
cat("{\n")
# loop through and write column headers
cat(" \\cell\n")
for (i in 1:tab.ncol) {
if (header) {
cat('\\b ',colnames(tab)[i],"\\b0\\cell \n",sep='')
} else {
cat(colnames(tab)[i],"\\cell \n",sep='')
}
}
cat("}\n")
cat("{\n")
cat("\\trowd\\trautofit1\\intbl\n")

j<-1
for (i in 1:(tab.ncol+1)) {
cat("\\cellx",j,'\n',sep='')
j<-j+1
}
cat("\\row }\n")

#write table contents
for (k in 1:tab.nrow) {
cat("\\trowd\\trautofit1\\intbl\n")

j<-1
for (i in 1:(tab.ncol+1)) {
cat("\\cellx",j,'\n',sep='')
j<-j+1
}
cat("{\n")
cat(rownames(tab)[k],'\\cell\n',sep='')
for (i in 1:tab.ncol) {
cat(format(tab[k,i],...),"\\cell \n",sep='')
}
cat("}\n")
cat("{\n")
cat("\\trowd\\trautofit1\\intbl\n")
j<-1
for (i in 1:(tab.ncol+1)) {
cat("\\cellx",j,'\n',sep='')
j<-j+1
}
cat("\\row }\n")
}
if (rtffile) {
# end the rtffile
cat("}\n")
}
if (!is.null(outfile)) sink()
}



##You'll need the package svViews (part of the SciViews series of packages) for this one.




library(svViews)
myfile <- paste(tempfile(),".rtf")
my.rtf.table(table,outfile=myfile)
# use svViews commands to set up Word, if needed
WordInsertFile(myfile)
unlink(myfile)
