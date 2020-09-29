require(tidyverse)
require(svMisc)

downloadDir <- "data/raw_data"
dataDir <- "data/derived_data"
mapsDir <- "product/maps"
rasterDir <- "product/species_rasters"
plotsDir <- "product/species_plots"
##########################################################
#### load data
##########################################################
sbnsc<- read_delim(file.path(dataDir,"all_recs.csv"),
                   col_types = "ccTnnnnnnnncccccccccccnnnnnnnn",
                   delim=",")
splst<-read_delim(file.path(dataDir,"sp2use.csv"),
                  col_types = "dccccccclllllllllllllllll",
                  delim=",")
##########################################################
##### filter to true benthic species only
##########################################################
trec<- sbnsc %>% as_tibble() %>%
  mutate(datasetid=as.numeric(substr(datasetid,65,90))) %>%
  filter(!is.na(aphiaidaccepted))%>%
  mutate(tmp=substr(aphiaidaccepted,52,65))%>%
  filter(tmp!="NA")%>%
  mutate(AphiaID=as.numeric(tmp)) %>%
  dplyr::select(-tmp)%>%
  filter(AphiaID %in% splst$AphiaID)
##############################################################
# find occurrence frequency of all species, and rank the species accordingly
#
spfr<- trec %>% 
  group_by(AphiaID,scientificnameaccepted) %>%
  summarize(n_events=n()) %>%
  arrange(desc(n_events))
nsptoplot<-length(which(spfr$n_events>0))
###############################################################
# make a list of all sampling events
events<- trec %>% 
  dplyr::select(datasetid,datecollected,decimallongitude,decimallatitude,minimumdepthinmeters,sampid)%>%
  distinct()%>%
  mutate(eventNummer=row_number())
trec <- trec %>%
  left_join(events,by=c("datasetid","datecollected","decimallongitude","decimallatitude",
                        "minimumdepthinmeters","sampid"))
############################################################
# Store trec. This is our first data product!
benth_recs<-trec
save(benth_recs,file=file.path(mapsDir,"benth_recs.Rdata"))
write_delim(benth_recs,path=file.path(mapsDir,"benth_recs.csv"),delim=",")
rm(benth_recs)
############ end of the generic part. What follows is a loop over the species ##

spmin<-1
spmax<-nsptoplot
spesh<-events %>% arrange(across(eventNummer))
offs<-ncol(spesh)

for(ss in spmin:spmax){
  progress(value=ss,max.value=spmax,init=(ss=spmin))  
  spAphId<-spfr$AphiaID[ss]
  specname<-spfr$scientificnameaccepted[ss]
  spcolumn<-paste0("ab_",spAphId)

  spe<- trec%>% 
    filter(AphiaID==spAphId)  %>%
    dplyr::select(avg_bedabund,eventNummer)
  intmd<-events %>%
    left_join(spe,by="eventNummer")%>%
    mutate(avg_bedabund=ifelse(is.na(avg_bedabund),0,avg_bedabund))%>%
    arrange(across(eventNummer))  %>% 
    dplyr::select(avg_bedabund)
  spesh<- spesh %>%
    bind_cols(intmd)
  names(spesh)[offs+ss]<-spcolumn
}

save(spesh,file=file.path(mapsDir,"spesh.Rdata"))
write_delim(spfr,path=file.path(mapsDir,"specieslist.csv"),delim=",")
write_delim(spesh,path=file.path(mapsDir,"spesh.csv"),delim=",")
