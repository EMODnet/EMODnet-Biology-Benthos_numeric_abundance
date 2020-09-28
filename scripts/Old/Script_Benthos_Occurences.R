##################
#Final data frame#
##################
# define the structure of the dataframe that will contain the results of this script
# this is a dataframe of occurrence data, including abundance (numeric)

df.occ=data.frame(
  data=character(),            # name of the data set, e.g. "HELCOM"
  gear=character(),            # gear type used. for some data sets, to be found in the csv. Others datasets just one type
  surf=numeric(),              # sample surface. For some datasets constant, for others to be found in the csv
  mesh=numeric(),              # mesh size in mm used for sorting the animals
  year=numeric(),
  month=numeric(),
  sta=character(),             # station name
  x=numeric(),                 # x coordinate (longitude)
  y=numeric(),                 # y coordinate (latitude)
  tx=character(),              # taxon name
  abund=numeric())             # numerical abundance of the taxon

#############
#Data HELCOM#
#############

df=read.csv("HELCOM.csv",h=T)
df$data=rep("HELCOM",nrow(df))
data.frame(colnames(df))
df=df[order(df$x,df$y),]                               # there are 56619 records, 11 variables
w=unique(df[c("x","y")])                               # there are 915 unique stations in this dataset
w$sta=paste(rep("HELCOM",nrow(w)),1:nrow(w),sep="")    # these stations receive the name HELCOM1..HELCOM915
a=aggregate(data=df,rep(1:nrow(df))~x+y,FUN=length)    # gives the number of records for each of the 915 unique stations
a=a[order(a[,1],a[,2]),]                               # now ordered by x, then by y
df$sta=rep(w$sta,a[,3])                                # correct station name HELCOM1..HELCOM915 added to df$sta
w=unique(df[,c("sta","x","y")])                        # list of station names and their coordinates
unique(table(w$sta))                                   # check that each station name only occurs once
df=df[,c("data","gear","surf","mesh","year",           # adjust order of variables in df
  "month","sta","x","y","tx","abund")]
df.occ=rbind(df.occ,df)                                # transfer df to df.occ

###############
#Data MACROBEL#
###############

df=read.csv("MACROBEL.csv",h=T)
data.frame(colnames(df))
df$data=rep("MACROBEL",nrow(df))
df$gear=rep("grab",nrow(df))
df$surf=rep(0.1,nrow(df))
df$mesh=rep(1,nrow(df))
df=df[order(df$lon,df$lat),]
w=unique(df[c("longitude","latitude")])
w$sta=paste(rep("MACROBEL",nrow(w)),1:nrow(w),sep="")
a=aggregate(data=df,rep(1:nrow(df))~longitude+latitude,FUN=length)
a=a[order(a[,1],a[,2]),]
df$sta=rep(w$sta,a[,3])
w=unique(df[,c("sta","longitude","latitude")])
unique(table(w$sta))
df=df[,c("data","gear","surf","mesh","yearcollected",
  "monthcollected","sta","longitude","latitude",
  "scientificname_accepted","observedindividualcount")]
colnames(df)=colnames(df.occ)
df.occ=rbind(df.occ,df)

##############
#Data MAREANO#
##############

df=read.csv("MAREANO.csv",h=T)
df.occ=rbind(df.occ,df)

###########
#Data MWTL#
###########

df=read.csv("MWTL.csv",h=T)
df$data=rep("MWTL",nrow(df))
data.frame(colnames(df))
df$gear=rep("corer",nrow(df))
df$surf=NA
df$surf[df$year<1999]=rep(0.068,length(df$surf[df$year<1999]))
df$surf[df$year>=1999]=rep(0.078,length(df$surf[df$year>=1999]))
df$mesh=rep(1,nrow(df))
df$month=substr(df$date,5,5)
df=df[order(df$x,df$y),]
w=unique(df[c("x","y")])
w$sta=paste(rep("MWTL",nrow(w)),1:nrow(w),sep="")
a=aggregate(data=df,rep(1:nrow(df))~x+y,FUN=length)
a=a[order(a[,1],a[,2]),]
df$sta=rep(w$sta,a[,3])
w=unique(df[,c("sta","x","y")])
unique(table(w$sta))
df=df[c("data","gear","surf","mesh",
  "year","month","sta","x","y","stax","ind")]
df$ind=df$ind*df$surf
colnames(df)=colnames(df.occ)
df.occ=rbind(df.occ,df)

###########
#Data NSBS#
###########

df=read.csv("NSBS.csv",h=T)
data.frame(colnames(df))
df$data=rep("NSBS",nrow(df))
df$gear=rep("corer",nrow(df))
df$surf=rep(1,nrow(df))
df$mesh=rep(1,nrow(df))
df=df[order(df$longitude,df$latitude),]
w=unique(df[c("longitude","latitude")])
w$sta=paste(rep("NSBS",nrow(w)),1:nrow(w),sep="")
a=aggregate(data=df,rep(1:nrow(df))~longitude+latitude,FUN=length)
a=a[order(a[,1],a[,2]),]
df$sta=rep(w$sta,a[,3])
w=unique(df[,c("sta","longitude","latitude")])
unique(table(w$sta))
df=df[c("data","gear","surf","mesh",
  "yearcollected","monthcollected","sta",
  "longitude","latitude","scientificname_accepted",
  "observedindividualcount")]
colnames(df)=colnames(df.occ)
df.occ=rbind(df.occ,df)

############
#Data ODAM #
############

df=read.csv("ODAM.csv",h=T)
data.frame(colnames(df))
df$data=rep("ODAM",nrow(df))
df$gear=rep("grab",nrow(df))
df$mesh=rep(1,nrow(df))
df$surf=gsub(" m²","",df$samplingeffort)
df$surf=gsub(",",".",df$surf)
df=df[order(df$longitude,df$latitude),]
w=unique(df[c("longitude","latitude")])
w$sta=paste(rep("ODAM",nrow(w)),1:nrow(w),sep="")
a=aggregate(data=df,rep(1:nrow(df))~longitude+latitude,FUN=length)
a=a[order(a[,1],a[,2]),]
df$sta=rep(w$sta,a[,3])
w=unique(df[,c("sta","longitude","latitude")])
unique(table(w$sta))
df=df[,c("data","gear","surf","mesh","yearcollected",
  "monthcollected","sta","longitude","latitude",
  "scientificname_accepted","observedindividualcount")]
colnames(df)=colnames(df.occ)
df.occ=rbind(df.occ,df)

##########
#Data PMP#
##########

df=read.csv("PMP.csv",h=T)
data.frame(colnames(df))
df$data=rep("PMP",nrow(df))
df$gear=rep("grab",nrow(df))
df$surf=rep(0.1,nrow(df))
df$mesh=rep(1,nrow(df))
df$abund=df$BedAbund_BE007117....m.2.*0.1
df=df[order(df$longitude,df$latitude),]
w=unique(df[c("longitude","latitude")])
w$sta=paste(rep("PMP",nrow(w)),1:nrow(w),sep="")
a=aggregate(data=df,rep(1:nrow(df))~longitude+latitude,FUN=length)
a=a[order(a[,1],a[,2]),]
df$sta=rep(w$sta,a[,3])
w=unique(df[,c("sta","longitude","latitude")])
unique(table(w$sta))
df=df[,c("data","gear","surf","mesh","yearcollected",
  "monthcollected","sta","longitude","latitude",
  "scientificname_accepted","abund")]
colnames(df)=colnames(df.occ)
df.occ=rbind(df.occ,df)

############
#Data POHJE#
############

df=read.csv("POHJE.csv",h=T)
df$data=rep("POHJE",nrow(df))
data.frame(colnames(df))
df$gear=df$Instrument..Dmnless.
df=df[df$gear%in%levels(df$gear)[7:11],]
df$gear=factor(as.character(df$gear))
levels(df$gear)=c(rep("grab",3),rep("corer",2))
df$surf=df$AreaBedSamp..m.2.
df$mesh=df$LowSieveMesh..mm.
df=df[order(df$longitude,df$latitude),]
w=unique(df[c("longitude","latitude")])
w$sta=paste(rep("POHJE",nrow(w)),1:nrow(w),sep="")
a=aggregate(data=df,rep(1:nrow(df))~longitude+latitude,FUN=length)
a=a[order(a[,1],a[,2]),]
df$sta=rep(w$sta,a[,3])
w=unique(df[,c("sta","longitude","latitude")])
unique(table(w$sta))
df=df[c("data","gear","surf","mesh",
  "yearcollected","monthcollected","sta",
  "longitude","latitude","scientificname_accepted",
  "BedAbund_BE007117....m.2.")]
colnames(df)=colnames(df.occ)
df$abund=df$abund*df$surf
df.occ=rbind(df.occ,df)

###########
#Data PORT#
###########

df=read.csv("PORT.csv",h=T)
data.frame(colnames(df))
df$abund=NA
df.occ=rbind(df.occ,df)

###########
#Data PUCK#
###########

df=read.csv("PUCK.csv",h=T)
df$data=rep("PUCK",nrow(df))
data.frame(colnames(df))
df$gear=rep("grab",nrow(df))
df$surf=rep(0.01,nrow(df))
df$mesh=rep(0.5,nrow(df))
df=df[order(df$longitude,df$latitude),]
w=unique(df[c("longitude","latitude")])
w$sta=paste(rep("PUCK",nrow(w)),1:nrow(w),sep="")
a=aggregate(data=df,rep(1:nrow(df))~longitude+latitude,FUN=length)
a=a[order(a[,1],a[,2]),]
df$sta=rep(w$sta,a[,3])
w=unique(df[,c("sta","longitude","latitude")])
unique(table(w$sta))
df$abund=rep(NA,nrow(df))
df=df[,c("data","gear","surf","mesh","yearcollected",
  "monthcollected","sta","longitude","latitude",
  "scientificname_accepted","abund")]
colnames(df)=colnames(df.occ)
df.occ=rbind(df.occ,df)

#############
#Data REBENT#
#############

df=read.csv("REBENT.csv",h=T)
df$data=rep("REBENT",nrow(df))
data.frame(colnames(df))
df$surf=df$samplingeffort
df$surf=gsub(" m2","",df$surf)
#df$surf[df$surf=="1"]=0.01
#df$surf[df$surf=="0.03"]=0.01
#df$surf[df$surf=="0.045"]=0.0113
df$gear=df$samplingprotocol
levels(df$gear)=c(rep("corer",15),rep("grab",8))
df$mesh=df$samplingprotocol
levels(df$mesh)=c(rep(1,8),2,rep(1,10),2,rep(1,3))
df=df[order(df$longitude,df$latitude),]
w=unique(df[c("longitude","latitude")])
w$sta=paste(rep("REBENT",nrow(w)),1:nrow(w),sep="")
a=aggregate(data=df,rep(1:nrow(df))~longitude+latitude,FUN=length)
a=a[order(a[,1],a[,2]),]
df$sta=rep(w$sta,a[,3])
w=unique(df[,c("sta","longitude","latitude")])
unique(table(w$sta))
df=df[c("data","gear","surf","mesh",
  "yearcollected","monthcollected","sta",
  "longitude","latitude","scientificname_accepted",
  "observedindividualcount")]
colnames(df)=colnames(df.occ)
df.occ=rbind(df.occ,df)

###########
#Data RSMP#
###########

df=read.csv("RSMP.csv",h=T)
df.occ=rbind(df.occ,df)
df.occ=df.occ[!is.na(df.occ$tx),]
df.occ=df.occ[df.occ$tx!="",]
rm(df)

############
#Data SHARK#
############

df=read.csv("SHARK.csv",h=T)
df$data=rep("SHARK",nrow(df))
data.frame(colnames(df))
df$gear=df$Instrument..Dmnless.
levels(df$gear)=c("","square","grab","corer",
  "corer","corer","grab","grab","grab")
df=df[order(df$longitude,df$latitude),]
w=unique(df[c("longitude","latitude")])
w$sta=paste(rep("SHARK",nrow(w)),1:nrow(w),sep="")
a=aggregate(data=df,rep(1:nrow(df))~longitude+latitude,FUN=length)
a=a[order(a[,1],a[,2]),]
df$sta=rep(w$sta,a[,3])
w=unique(df[,c("sta","longitude","latitude")])
unique(table(w$sta))
df=df[,c("data","gear","InstrumentSurfaceArea..m.2.","NetMesh..mm.",
  "yearcollected","monthcollected","sta","longitude","latitude",
  "scientificname_accepted","observedindividualcount")]
colnames(df)=colnames(df.occ)
df.occ=rbind(df.occ,df)

#######################
#Ascribing valid names#
#######################

n=read.csv("taxo.csv",h=T)
n$tx=as.character(n$tx)
df.occ$tx=as.character(df.occ$tx)
n=n[order(n$tx),]
df.occ=df.occ[order(df.occ$tx),]
df.occ=df.occ[df.occ$tx%in%n$tx,]
w1=n$tx
w2=unique(df.occ$tx)
unique(w1==w2)
df.occ$tx2=rep(n$txa,table(df.occ$tx))

w=unique(df.occ[,c("tx","tx2")])
w[sample(1:nrow(w),30),]

df.occ$tx=df.occ$tx2
df.occ=df.occ[,1:(ncol(df.occ)-1)]
head(df.occ)
df.occ=df.occ[df.occ$tx!="",]

######################
#Sampling corrections#
######################

levels(df.occ$gear)[levels(df.occ$gear)=="core"]="corer"
df.occ=df.occ[df.occ$gear%in%c("dredge","square","")==F,]
df.occ$gear=factor(as.character(df.occ$gear))
df.occ=df.occ[!is.na(df.occ$surf),]
df.occ=df.occ[df.occ$surf>=0.01,]
df.occ=df.occ[df.occ$surf%in%c("",0)==F,]
df.occ$surf=as.numeric(df.occ$surf)
df.occ$month=as.numeric(df.occ$month)
df.occ[df.occ$data=="SHARK",]=df.occ[df.occ$data=="SHARK",][df.occ$surf[df.occ$data=="SHARK"]>0.06 | 
    df.occ$surf[df.occ$data=="SHARK"]<0.15,]
df.occ$surf[df.occ$data=="SHARK"]=0.1
df.occ[df.occ$data=="ODAM",]=df.occ[df.occ$data=="ODAM",][df.occ$surf[df.occ$data=="ODAM"]>0.06 | 
    df.occ$surf[df.occ$data=="ODAM"]<0.15,]
df.occ$surf[df.occ$data=="ODAM"]=0.1
df.occ=df.occ[!is.na(df.occ$mesh),]

########################################
#Occurrence list for individual density#
########################################

df.ab=df.occ[df.occ$mesh=="1",]
df.ab=df.ab[!is.na(df.ab$mesh),]
#Densities not available for the following data sets
df.ab=df.ab[df.ab$data%in%c("PORT","PUCK")==F,]
df=df.ab

#Summing individual abundances by station
df.ab=aggregate(data=df.ab,abund~data+sta+surf+x+y+tx,
  FUN=sum,na.action=NULL)
df.ab=df.ab[order(as.character(df.ab$sta),df.ab$surf),]

#Sampling effort
eff=unique(df[,c("data","sta","surf","year","month")])
eff=aggregate(data=eff,rep(1:nrow(eff))~data+sta+surf,FUN=length)
colnames(eff)[4]="count"
eff=eff[order(as.character(eff$sta),eff$surf),]
df=df[order(as.character(df$sta),df$surf),]

unique(unique(df$sta)==unique(eff$sta))
unique(unique(df$surf)==unique(eff$surf))

w=aggregate(data=df.ab,rep(1,nrow(df.ab))~sta+surf,FUN=length)
w=w[order(as.character(w$sta),w$surf),]
df.ab$count.sta=rep(eff$count,w[,3])

head(eff[eff$data=="MWTL",])
head(df.ab[df.ab$sta=="MWTL1",],100)

#Calculating individual density
df.ab$dens=df.ab$abund/(df.ab$surf*df.ab$count.sta)
df.ab=aggregate(data=df.ab,dens~data+sta+x+y+tx,FUN=mean,na.action=NULL)
df.ab=df.ab[order(as.character(df.ab$sta)),]
head(df.ab)

#Verification
df.occ[df.occ$data=="MWTL" & df.occ$tx=="Modiolus",]
df.ab[df.ab$data=="MWTL" & df.ab$tx=="Modiolus",]
df.ab$tx[is.na(df.ab$tx)]

#All stations including true absences
all.sta.ab=unique(df.ab[,c("data","x","y","sta")])

#Selecting phyla
taxo=read.csv("taxo.csv",h=T)
tx.sel=c("Annelida","Arthropoda","Mollusca",
  "Echinodermata","Phoronida")
tx.sel=taxo[taxo$phy%in%tx.sel,]
df.ab=df.ab[df.ab$tx%in%tx.sel$txa,]
#Check the orders of magnitude
boxplot(log(df.ab$dens)~df.ab$data,col=8)

########################################
#Occurrence list for taxonomic richness#
########################################

df=df.occ
df=df[df$mesh=="1",]
w=df.occ[df.occ$data=="PUCK",]
df=rbind(df,w)
df$tx=as.character(df$tx)
df=df[df$tx%in%tx.sel$txa,]
df.tx=unique(df[,c("data","gear","surf","x","y","year","month","sta","tx")])

#All stations including true absences
all.sta.tx=unique(df.tx[,c("data","x","y","sta")])

