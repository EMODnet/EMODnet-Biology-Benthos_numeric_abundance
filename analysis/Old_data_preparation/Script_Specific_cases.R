require(reshape2)

#Specific cases
data.frame(sort(grp.typ))
df=df.ab[!is.na(df.ab$dens),]
df=df[df$data%in%c("PORT","PUCK")==F,]
df=unique(df[c("sta","x","y","tx","dens")])
df=df[df$tx%in%c(
  "Urothoe poseidonis",
  "Spiophanes",
  "Spiophanes bombyx",
  "Arctica",
  "Arctica islandica"),]
df$tx[df$tx=="Spiophanes"]="Spiophanes bombyx"
df$tx[df$tx=="Arctica"]="Arctica islandica"
tab=dcast(df,sta+x+y~tx,value.var="dens",sum)
tab[is.na(tab)]=0
w=all.sta.ab[all.sta.ab$sta%in%tab$sta==F,]
w=w[c("sta","x","y")]
w["Arctica islandica"]=rep(0,nrow(w))
w["Spiophanes bombyx"]=rep(0,nrow(w))
w["Urothoe poseidonis"]=rep(0,nrow(w))
tab=rbind(tab,w)
tab[sample(1:nrow(tab),30),]

par(mfrow=c(2,2),mar=c(2,2,3,1))
for(i in 4:6){
  z=tab[,c(2,3,i)]
  plot(tab[c("x","y")],pch=20,col=8,cex=1,main=colnames(tab)[i])
  map("worldHires",res=0.5,add=T)
  d=log(z[,3]+1)
  d=(d-min(d))/(max(d)-min(d))
  points(z[c("x","y")],pch=20,cex=3*d)
}

write.csv(tab,"Benthos_Specific_cases.csv",row.names=F)

