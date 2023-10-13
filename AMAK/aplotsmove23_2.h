rm(list=ls())
WD<- getwd()
setwd(WD)
library(PBSmodelling)
data1<-readList("For_R.rep")
source("R_Functions/ADfunctions_hake.r")

# Plot reclutamiento, SSB y TB
p.biom.stk(dat=data1,typ="R")
savePlot(filename = "run2/f1.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)
p.biom.stk(dat=data1,typ="SSB")
savePlot(filename = "run2/f2.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)
p.biom.stk(dat=data1,typ="TB")
savePlot(filename = "run2/f3.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# fit index
p.sur.stk(dat=data1,f=1,ylab="Index", yf=1992,yl=2023)
savePlot(filename = "run2/f4.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# fit cathc
p.catch.fit(dat=data1,f=1,ylab="Catch biomass (kt)" )
savePlot(filename = "run2/f5.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# fit index
IndexFit(dat=data1)
savePlot(filename = "run2/f6.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# CAA fit
AgeFits(dat=data1,case_label="2012 assessment",f=1,lyr=1992,lage=2,hage=12)
savePlot(filename = "run2/f7.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# CAA survey
AgeFitsSrv(dat=data1, main="",case_label="2023 assessment",lyr=1992,lage=2,hage=12,f=1,rec_age=2)
savePlot(filename = "run2/f8.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# Grafico contorno de F
cont.f.mort(dat=data1,f=1,lage=2,hage=12,cl="BW") ## data file, fishery, lowest and highest ages
savePlot(filename = "run2/f9.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# Burbujas NAA. CAA. BAA.
p.bub.age(dat=data1,typ="NA",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=500000)     # NAA
savePlot(filename = "run2/f10.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)
p.bub.age(dat=data1,typ="CB",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=20000) 		# CAA
savePlot(filename = "run2/f11.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)
p.bub.age(dat=data1,typ="B",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=55000)		# BAA
savePlot(filename = "run2/f12.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# NAA. CAA. BAA con F 
cont.f.mort2(dat=data1,typ="NA",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=500000,cl="BW")
savePlot(filename = "run2/f13.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)
cont.f.mort2(dat=data1,typ="CB",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=20000,cl="BW")
savePlot(filename = "run2/f14.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)
cont.f.mort2(dat=data1,typ="B",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=200000,cl="BW")
savePlot(filename = "run2/f15.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# Edad Media
p.eff.n(dat=data1,typ="S",f=1) 	# crucero
savePlot(filename = "run2/f16.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)
p.eff.n(dat=data1,typ="F",f=1)	# Pesqueria
savePlot(filename = "run2/f17.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# Select a la edad
p.select.hist(dat=data1,typ="F",h="T",f=1,lage=2,hage=12,fy=1992,ly=2012)   # Select barr
savePlot(filename = "run2/f18.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

c.select(dat=data1,typ="F",f=1,lage=2,hage=12,fy=1992,ly=2023,cl="BW")		# Select F
savePlot(filename = "run2/f19.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

c.select(dat=data1,typ="S",f=1,lage=2,hage=12,fy=1992,ly=2023,cl="BW")      # Select Survey
savePlot(filename = "run2/f20.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# Select a la edad color
c.select.b(dat=data1,typ="F",typ2="NA",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=500000,cl="CL")
c.select.b(dat=data1,typ="F",typ2="CB",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=20000,cl="CL")
c.select.b(dat=data1,typ="F",typ2="B",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=200000,cl="CL")

# Fishing mortality
p.full.f(dat=data1,f=1)
savePlot(filename = "run2/f21.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# Survey selectivity
p.survey.curve(dat=data1,f=1,lage=2,hage=12)
savePlot(filename = "run2/f22.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# SSB with error bar
p.biom.pol(dat1=data1, dat2=data1, n.mod=1, typ="SSB",color=c("light blue", 'darkblue', 'light green', 'darkgreen'), new=TRUE)
savePlot(filename = "run2/f23.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# R with error bar
p.biom.pol(dat1=data1, dat2=data1, n.mod=1, typ="R",color=c("light blue", 'darkblue', 'light green', 'darkgreen'), new=TRUE)
savePlot(filename = "run2/f24.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# TB with error bar
p.biom.pol(dat1=data1, dat2=data1, n.mod=1, typ="TB",color=c("light blue", 'darkblue', 'light green', 'darkgreen'), new=TRUE)
savePlot(filename = "run2/f25.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# Selectividad F y S
sel.age.mountain(dat=data1, f=1, typ='F', xvec=c(2:12), yvec=NULL, zscale=10, nshades=100, xaxs='i', yaxs='i', xlab="", ylab="", las=1, new=TRUE, addbox=FALSE, cex.xax=1, cex.yax=1)
savePlot(filename = "run2/f26.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

sel.age.mountain(dat=data1, f=1, typ='S', xvec=c(2:12), yvec=NULL, zscale=10, nshades=100, xaxs='i', yaxs='i', xlab="", ylab="", las=1, new=TRUE, addbox=FALSE, cex.xax=1, cex.yax=1)
savePlot(filename = "run2/f27.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# shawdow the biomass
cont.CB_TB(dat=data1,mod="1",f=1,f1=1,lage=2,hage=12,fy=1992,ly=2023,siz=2000000,cl="BW",ncol1=100,maxb=33000)
savePlot(filename = "run2/f28.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# Residuales del modelo
bp.f.age.res(dat=data1,mod="1",typ="S",f=1,f1=1,lage=2,hage=12,siz=1) # Survey
savePlot(filename = "run2/f29.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

bp.f.age.res(dat=data1,mod="1",typ="F",f=1,f1=1,lage=2,hage=12,siz=1) # fishery
savePlot(filename = "run2/f30.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

#Mortalidades por pesca
cont.f.mort3(dat=data1,typ="CB",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=20000,cl="CL")
savePlot(filename = "run2/f31.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

cont.f.mort3(dat=data1,typ="B",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=200000,cl="CL")
savePlot(filename = "run2/f32.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# CAA and B with Selectiviy
c.select.b2(dat=data1,typ="F",typ2="CB",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=20000,cl="BW")
savePlot(filename = "run2/f33.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

c.select.b2(dat=data1,typ="F",typ2="B",f=1,lage=2,hage=12,fy=1992,ly=2023,siz=200000,cl="BW")
savePlot(filename = "run2/f34.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# Proyecciones
plot_FCATCH()
savePlot(filename = "run2/f35.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)
plot_FSSB()
savePlot(filename = "run2/f36.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

FutBiom(dat=data1, new=TRUE,ylab="Female SSB(kt)",fy=1992,ly=2027,nproj=5)
savePlot(filename = "run2/f37.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

FutCatch(dat=data1,fy=1992,ly=2053) #42
savePlot(filename = "run2/f38.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

# razon de biomasa
spwn_ratio (dat=data1)
savePlot(filename = "run2/f39.pdf",type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)

 

# Out_put Tablas # 13 al 33
#tabla.sr<-       data.frame(list(Año=data1$SSB[13:33,1], BD=data1$SSB[13:33,2], F=data1$F_fsh_1[,2]))

#write.table(tabla.sr, file = "tablasr1.csv", sep = ",", col.names = NA,qmethod = "double")

#no_fish= rbind(0,data1$SSB_NoFishR)


#tabla.summary<- data.frame(list(Año=data1$SSB[13:33,1], BT=data1$TotBiom[,2], BD=data1$SSB[13:33,2], F=data1$F_fsh_1[,2],R=data1$R[,2],SSB_F0=no_fish[,2]))
#write.table(tabla.summary, file = "tablasummary1.csv", sep = ",", col.names = NA,qmethod = "double")
