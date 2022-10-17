library(rnirs)
library(nirsextra)
3
Load the data
f= "C:/Users/utente/OneDrive/Desktop/JRL AT 2022/NIR_data/Brimrose_JRL.txt"
f= "C:/Users/utente/OneDrive/Desktop/JRL AT 2022/NIR_data/Brimrose/Brimrose_JRL.txt"
sp=read.table(f,sep = ";", dec = ".", header = T)
str(sp)




fref="C:/Users/utente/OneDrive/Desktop/JRL AT 2022/JRL_CHN.csv"
ref=read.table(fref,sep=";",dec=",", header = T)



#Merge X and Y #

#sp2=sp2df(sp[,4:ncol(sp)])
colnames(sp)[2]="gn"
colnames(ref)[1]="gn"
sp2=merge(sp,ref,by = "gn")

spf=sp2df(as.matrix(sp[,4:ncol(sp)]))
spf=cbind(spf,sp2$X.N, sp2$gn)
colnames(spf)[2]="N"
colnames(spf)[3]="grnam"


plotsp(spf$x)
fm=pca(spf$x, ncomp=4)
plotxy(fm$Tr[,1:2])

spf$xp=snv(spf$x)
plotsp(spf$xp)
fm=pca(spf$xp, ncomp=4)
plotxy(fm$Tr[,1:2])

iok=which(fm$Tr[,1] > -2)
spfok=spf[iok,]
plotsp(spfok$xp)


n=108
alls=sample(1:108)
itest=alls[1:(n/3)]
ical=alls[((n/3)+1):n]


datcal=spfok[spfok$grnam %in% ical,]
dattest=spfok[spfok$grnam%in% itest,]

spav=aggregate(datcal$x, list(datcal$grnam), mean)[,-1]
refav=aggregate(datcal$N, list(datcal$grnam), mean)[,-1]

seg=segmkf(n=nrow(spav),K=4,type = "random", nrep=10)
fm= cvfit(spav,refav,segm=seg, fun=plsr,ncomp=20)
mse(fm, ~ ncomp)
fm1=lapply(fm[1:3],function (x) {x[x$ncomp==4,]})
plot(fm1$fit$y1,fm1$y$y1)
abline(0,1)






