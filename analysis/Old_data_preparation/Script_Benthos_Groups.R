require(reshape2)
require(ade4)

w=read.csv("grptyp.csv",h=T)
grp.typ=w$g
names(grp.typ)=w$tx

####################
#Individual density#
####################

#Total individual density
df=df.ab[!is.na(df.ab$dens),]
w=aggregate(data=df,dens~sta,FUN=sum)
tot.ab=w$dens
names(tot.ab)=w$sta
tot.ab=tot.ab[order(names(tot.ab))]

#Absolute individual density
df=df.ab[!is.na(df.ab$dens),]
df=df[df$data%in%c("PORT","PUCK")==F,]
df$tx=as.character(df$tx)
df=df[order(df$tx),]
df=df[df$tx%in%names(grp.typ),]
grp.typ.ab=grp.typ[names(grp.typ)%in%df$tx]
unique(unique(df$tx)==names(grp.typ.ab))
df$grp=rep(grp.typ.ab,table(df$tx))
df[sample(1:nrow(df),30),]
tab=dcast(df,data+x+y+sta~grp,value.var="dens",sum)
tab[is.na(tab)]=0
w=ncol(tab)
colnames(tab)[(w-2):w]=c("g1.abs","g2.abs","g3.abs")

#Adding absences
w=all.sta.ab[all.sta.ab$sta%in%tab$sta==F,]
w$g1.abs=rep(0,nrow(w))
w$g2.abs=rep(0,nrow(w))
w$g3.abs=rep(0,nrow(w))
tab=rbind(tab,w)

#Relative individual density
w=ncol(tab.ab)
tab=tab.ab[,(w-2):w]
tab=sweep(tab,1,apply(tab,1,sum),"/")
colnames(tab)=c("g1.rel","g2.rel","g3.rel")
tab[tab=="NaN"]=0
tab.ab=cbind(tab.ab,tab)
tab.ab[sample(1:nrow(tab.ab),30),]

###############################
#Documented taxonomic richness#
###############################

#Selection of documented taxa
df=df.tx
df=df[df$tx%in%names(grp.typ),]
df=df[order(as.character(df$tx)),]
unique(unique(df$tx)==names(grp.typ))

#Ascribing groups
tab=unique(df[c("data","gear","surf","year","month","x","y","sta","tx")])
tab$grp=rep(grp.typ,table(tab$tx))
tab[sample(1:nrow(tab),30),]

#Averaging taxa per time and space units
tab=dcast(tab,data+gear+surf+year+month+x+y+sta~grp,value.var="grp",length)
w=ncol(tab)
colnames(tab)[(w-2):w]=c("g1.abs","g2.abs","g3.abs")
tab=aggregate(data=tab,cbind(g1.abs,g2.abs,g3.abs)~data+gear+surf+year+x+y+sta,FUN=mean)
tab=aggregate(data=tab,cbind(g1.abs,g2.abs,g3.abs)~data+gear+surf+x+y+sta,FUN=mean)
tab=aggregate(data=tab,cbind(g1.abs,g2.abs,g3.abs)~data+gear+x+y+sta,FUN=mean)
tab=aggregate(data=tab,cbind(g1.abs,g2.abs,g3.abs)~data+x+y+sta,FUN=mean)
tab[sample(1:nrow(tab),30),]

#Adding absences
w=all.sta.tx[all.sta.tx$sta%in%tab$sta==F,]
w$g1.abs=rep(0,nrow(w))
w$g2.abs=rep(0,nrow(w))
w$g3.abs=rep(0,nrow(w))
tab.tx=rbind(tab,w)

#Relative toxonomic richness
w=ncol(tab.tx)
tab=tab.tx[,(w-2):w]
tab=sweep(tab,1,apply(tab,1,sum),"/")
colnames(tab)=c("g1.rel","g2.rel","g3.rel")
tab[tab=="NaN"]=0
tab.tx=cbind(tab.tx,tab)
tab.tx[sample(1:nrow(tab.tx),30),]

summary(tab.ab)

par(mfrow=c(3,2),cex.axis=0.8,mar=c(3,3,2,1))
for(i in 1:3){
  boxplot(log(tab.ab[,i+4]+1)~tab.ab$data,
    main=colnames(tab.ab)[i+4],col=8,srt=45)
  boxplot(tab.ab[,i+7]~tab.ab$data,
    main=colnames(tab.ab)[i+7],col=8)
}

par(mfrow=c(3,2),cex.axis=0.8,mar=c(3,3,2,1))
for(i in 1:3){
  boxplot(tab.tx[,i+4]~tab.tx$data,
    main=colnames(tab.tx)[i+4],col=8,srt=45)
  boxplot(tab.tx[,i+7]~tab.tx$data,
    main=colnames(tab.tx)[i+7],col=8)
}

apply(tab.ab[,5:10],2,function(x) tapply(x,tab.ab$data,mean))
apply(tab.tx[,5:10],2,function(x) tapply(x,tab.tx$data,mean))

write.csv(tab.ab,"Benthos_Total_densities_ab.csv",row.names=F)
write.csv(tab.tx,"Benthos_Total_densities_tx.csv",row.names=F)
