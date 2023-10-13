rm(list=ls())
WD<- getwd()
setwd(WD)
library(PBSmodelling)
data1<-readList("For_R.rep")
#source("E:\wsea2012\R_Functions\ADfunctions_hake.r")
#source("ADfunctions_hake.r")
p.sur.stk(dat=data1,f=1,ylab="Index", yf=1992,yl=2011)
cont.f.mort(dat=data1,f=1,lage=2,hage=12,cl="BW") ## data file, fishery, lowest and highest ages
##bubble plot of numbers or biomass at age CA=20000,B=200000,NA=500000
p.bub.age(dat=data1,typ="NA",f=1,lage=2,hage=12,fy=1992,ly=2011,siz=500000)
p.bub.age(dat=data1,typ="CB",f=1,lage=2,hage=12,fy=1992,ly=2011,siz=20000)
p.bub.age(dat=data1,typ="B",f=1,lage=2,hage=12,fy=1992,ly=2011,siz=200000)

cont.f.mort2(dat=data1,typ="NA",f=1,lage=2,hage=12,fy=1992,ly=2011,siz=500000,cl="BW")
cont.f.mort2(dat=data1,typ="CB",f=1,lage=2,hage=12,fy=1992,ly=2011,siz=20000,cl="BW")
cont.f.mort2(dat=data1,typ="B",f=1,lage=2,hage=12,fy=1992,ly=2011,siz=200000,cl="BW")

p.eff.n(dat=data1,typ="S",f=1)
p.eff.n(dat=data1,typ="F",f=1)

p.catch.fit(dat=data1,f=1,ylab="Catch biomass (kt)" )

p.select.hist(dat=data1,typ="F",h="T",f=1,lage=2,hage=12,fy=1992,ly=2011)
c.select(dat=data1,typ="F",f=1,lage=2,hage=12,fy=1992,ly=2011,cl="BW")
c.select(dat=data1,typ="S",f=1,lage=2,hage=12,fy=1992,ly=2011,cl="BW")


c.select.b(dat=data1,typ="F",typ2="NA",f=1,lage=2,hage=12,fy=1992,ly=2011,siz=500000,cl="CL")
c.select.b(dat=data1,typ="F",typ2="CB",f=1,lage=2,hage=12,fy=1992,ly=2011,siz=20000,cl="CL")
c.select.b(dat=data1,typ="F",typ2="B",f=1,lage=2,hage=12,fy=1992,ly=2011,siz=200000,cl="CL")
p.full.f(dat=data1,f=1)
p.survey.curve(dat=data1,f=1,lage=2,hage=12)


p.biom.pol(dat1=data1, dat2=data1, n.mod=1, typ="SSB",color=c("light blue", 'darkblue', 'light green', 'darkgreen'), new=TRUE)

sel.age.mountain(dat=data1, f=1, typ='F', xvec=c(2:12), yvec=NULL, zscale=10, nshades=100, xaxs='i', yaxs='i', xlab="", ylab="", las=1, new=TRUE, addbox=FALSE, cex.xax=1, cex.yax=1)
sel.age.mountain(dat=data1, f=1, typ='S', xvec=c(2:12), yvec=NULL, zscale=10, nshades=100, xaxs='i', yaxs='i', xlab="", ylab="", las=1, new=TRUE, addbox=FALSE, cex.xax=1, cex.yax=1)

cont.CB_TB(dat=data1,mod="1",f=1,f1=1,lage=2,hage=12,fy=1992,ly=2011,siz=200000,cl="BW",ncol1=100,maxb=33000)

bp.f.age.res(dat=data1,mod="1",typ="S",f=1,f1=1,lage=2,hage=12,siz=1)
bp.f.age.res(dat=data1,mod="1",typ="F",f=1,f1=1,lage=2,hage=12,siz=1)

cont.f.mort3(dat=data1,typ="CB",f=1,lage=2,hage=12,fy=1992,ly=2011,siz=20000,cl="CL")
cont.f.mort3(dat=data1,typ="B",f=1,lage=2,hage=12,fy=1992,ly=2011,siz=200000,cl="CL")

c.select.b2(dat=data1,typ="F",typ2="CB",f=1,lage=2,hage=12,fy=1992,ly=2010,siz=20000,cl="BW")
c.select.b2(dat=data1,typ="F",typ2="B",f=1,lage=2,hage=12,fy=1992,ly=2010,siz=200000,cl="BW")


IndexFit(dat=data1)

AgeFits(dat=data1,case_label="2011 assessment",f=1,lyr=1992,lage=2,hage=12)
AgeFitsSrv(dat=data1, main="",case_label="2011 assessment",lyr=1992,lage=2,hage=12,f=2,rec_age=2) {


plot_FCATCH()
plot_FSSB()

FutBiom(dat=data1, new=TRUE,ylab="Female SSB(kt)",fy=1992,ly=2016,nproj=5)





FutCatch(dat=data1,fy=1992,ly=2016)