

p.sur.stk(test,yf=1978,yl=2012)
cont.f.mort(test)

p.bub.age(test,siz=0.1)
p.bub.age(test,"CB",siz=0.01)
p.bub.age(test,"B",siz=100)


cont.f.mort2(test,siz=0.1)
cont.f.mort2(test,"CB",siz=0.01)
cont.f.mort2(test,"B",siz=100)

cont.f.age.res(test,nl=40)
cont.f.age.res(test,typ="F",nl=45)

p.biom.stk(test)
p.biom.stk(test,"TB")
p.biom.stk(test,"R")

p.eff.n(test,"S")
p.eff.n(test,"F")

p.age.fit(test,typ="F",f=1,lage=2,hage=15,fy=1978,ly=1985)
p.age.fit(test,typ="F",f=1,lage=2,hage=15,fy=1986,ly=1993)
p.age.fit(test,typ="F",f=1,lage=2,hage=15,fy=1994,ly=1995)

p.age.fit(test,typ="S",f=1,lage=2,hage=15,fy=1978,ly=1987)
p.age.fit(test,typ="S",f=1,lage=2,hage=15,fy=1988,ly=1990)

p.catch.fit(test)

c.select.b(test)
c.select.b(test,typ="S")
c.select.b(test,typ2="CB",siz=0.01)
c.select.b(test,typ="S",typ2="CB",siz=0.01)
c.select.b(test,typ2="B",siz=100)
c.select.b(test,typ="S",typ2="B",siz=100)

p.stock.rec(test)
p.rec.hist(test)
p.full.f(test)
p.survey.curve(test)

p.biom.pol(test,"TB")
p.biom.pol(test,typ="TB")
p.biom.pol(test,typ="R")



sel.age.mountain(test,typ="F")
sel.age.mountain(test,typ="S")

AgeFits(test)
AgeFitsSrv(test)

spwn_ratio(test)

plot_comp_old()

for(i in 1:7)
  plot_proj(i)
  }
  
## Phase plane plots  
  
x<-(test$SSB[1:49,2][test$SSB[1:49,1]>1977]/biom$SB35)
##plot(test$F_fsh_1[,2]/Fofl~x,type="l",ylim=c(0,1.5),xlim=c(0,2),)
 


plot.phase.plane(x,test$F_fsh_1[,3]/Fofl,xlim=c(0,5),ylim=c(0,1.5))
 
## without fishing analys
library(Hmisc)
errbar(test$TotBiom_NoFish[,1],test$TotBiom_NoFish[,2],yminus=test$TotBiom_NoFish[,4],yplus=test$TotBiom_NoFish[,5],ylim=c(0,max(test$TotBiom_NoFish[,5])),ylab="Total age 2+ biomass (kt)", xlab="Year",cex.lab=1.5,cex.axis=1.5)
errbar(test$TotBiom[,1],test$TotBiom[,2],yminus=test$TotBiom[,4],yplus=test$TotBiom[,5],col="red",ylim=c(0,max(test$TotBiom[,5])),add=T)
text(2000,max(test$TotBiom_NoFish[,5])-50,"Total biomass w/o fishing",pos=4) 
text(2000,max(test$TotBiom_NoFish[,5])-150,"Total biomass",pos=4)
points(2000,max(test$TotBiom_NoFish[,5])-50,pch=16)
points(2000,max(test$TotBiom_NoFish[,5])-150,pch=16,col="red")


cont.CB_TB(test,mod="1",f=1,f1=1,lage=2,hage=15,fy=1978,ly=2011,siz=100,cl="BW")
cont.f.mort2(test,"CB",siz=0.01)
 
