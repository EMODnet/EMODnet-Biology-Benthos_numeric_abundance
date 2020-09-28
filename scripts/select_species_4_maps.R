require(tidyverse)
require(worrms)
dataDir     <- "data/derived_data"
outputDir   <- "product"

load(file.path(dataDir,"all_recs.Rdata"))

# we build the species list, keeping the taxonomic information we have in the total data set
# we foresee logical columns in the species list to group the species by in the rest of this script
### extracting numeric aphiaid: mutate(AphiaID=as.numeric(substr(aphiaidaccepted,52,65)))


splst <- all_recs %>%
  ungroup() %>%
  filter(!is.na(aphiaidaccepted))%>%
  mutate(tmp=substr(aphiaidaccepted,52,65))%>%
  filter(tmp!="NA")%>%
  mutate(AphiaID=as.numeric(tmp)) %>%
  select(AphiaID,scientificnameaccepted,phylum,class,order,family,genus,subgenus) %>% 
  distinct() %>%
  mutate(benthos=FALSE,endobenthos=FALSE,macrobenthos=FALSE,epibenthos=FALSE,
         meiobenthos=FALSE,phytobenthos=FALSE,
         plankton=FALSE,nekton=FALSE,Pisces=FALSE,Algae=FALSE,
         Aves_tax=FALSE,Pisces_tax=FALSE,Algae_tax=FALSE,Plants_tax=FALSE,
         meio_tax=FALSE,micro_tax=FALSE,misc_tax=FALSE)
# ###### determine, using attributes, which species are benthos #######
# ###### again, several hours download ##########
# (done once, result stored as delimited file)
# nsp_attr<-tibble()
# for(i in 1:nrow(splst)){
#   print(paste(i,"out of",nrow(splst),"downloading attributes of species",
#               splst$scientificnameaccepted[i],"AphiaID",splst$AphiaID[i]))
#   ttt<-NULL
#   try(ttt<-wm_attr_data(id=splst$AphiaID[i],include_inherited = T),silent = T)
#   if(! is.null(ttt)) nsp_attr<-rbind(nsp_attr,ttt[,1:9])
# }
# 
# nsp_attr <- nsp_attr %>%
#                   mutate(AphiaID=as.numeric(AphiaID)) %>%
#                   left_join(splst,by="AphiaID")
# write_delim(nsp_attr,file.path(dataDir,"nsp_attr.csv"),delim=",")
# save(nsp_attr,file=file.path(dataDir,"nsp_attr.Rdata"))
# 


nsp_attr <- read_delim(file.path(dataDir,"nsp_attr.csv"),delim=",")
# what Functional groups are there?
fg <- nsp_attr %>% filter(measurementType=="Functional group") %>% 
  select(measurementValue) %>% 
  distinct
print(fg)
# what Paraphyletic groups are there?
pfg <- nsp_attr %>% filter(measurementType=="Paraphyletic group") %>% 
  select(measurementValue) %>% 
  distinct
print(pfg)
# fill in attributes columns of splst based on the attributes downloaded from WoRMS
set_attr<-function(attr){
  tt <- nsp_attr                               %>% 
    filter(grepl(attr,measurementValue)) %>% 
    select(AphiaID)                      %>% 
    distinct()
  splst <- splst %>% 
    mutate(!!attr:=ifelse(AphiaID %in% tt$AphiaID,TRUE,FALSE))
  return(splst)
}
splst<-set_attr("benthos")
splst<-set_attr("endobenthos")
splst<-set_attr("macrobenthos")
splst<-set_attr("epibenthos")
splst<-set_attr("meiobenthos")
splst<-set_attr("phytobenthos")
splst<-set_attr("Pisces")
splst<-set_attr("Algae")
splst<-set_attr("plankton")
splst<-set_attr("nekton")
# fill in attributes columns based on taxonomic information
splst$Pisces_tax <- splst$Pisces_tax | splst$class  == "Actinopterygii" 
splst$Pisces_tax <- splst$Pisces_tax | splst$class  == "Elasmobranchii"
splst$Aves_tax   <- splst$Aves_tax   | splst$class  == "Aves"
splst$Algae_tax  <- splst$Algae_tax  | splst$phylum == "Chlorophyta"
splst$Algae_tax  <- splst$Algae_tax  | splst$phylum == "Rhodophyta"
splst$Algae_tax  <- splst$Algae_tax  | splst$phylum == "Ochrophyta"
splst$Algae_tax  <- splst$Algae_tax  | splst$phylum == "Charophyta"
splst$Algae_tax  <- splst$Algae_tax  | splst$phylum == "Cyanobacteria"
splst$Algae_tax  <- splst$Algae_tax  | splst$phylum == "Haptophyta"
splst$Plants_tax <- splst$Plants_tax | splst$Algae_tax 
splst$Plants_tax <- splst$Plants_tax | splst$phylum == "Tracheophyta"
splst$Plants_tax <- splst$Plants_tax | splst$phylum == "Bryophyta"
splst$micro_tax  <- splst$micro_tax | splst$phylum == "Ascomycota"
splst$micro_tax  <- splst$micro_tax | splst$phylum == "Proteobacteria"
splst$meio_tax   <- splst$meio_tax  | splst$phylum == "Nematoda"
splst$meio_tax   <- splst$meio_tax  | splst$phylum == "Foraminifera"
splst$meio_tax   <- splst$meio_tax  | splst$phylum == "Tardigrada"
splst$meio_tax   <- splst$meio_tax  | splst$phylum == "Gastrotricha"
splst$meio_tax   <- splst$meio_tax  | splst$phylum == "Kinorhyncha"
splst$meio_tax   <- splst$meio_tax  | splst$phylum == "Ciliophora"
splst$meio_tax   <- splst$meio_tax  | splst$class  == "Ostracoda"
splst$meio_tax   <- splst$meio_tax  | splst$order  == "Harpacticoida"
splst$misc_tax   <- splst$misc_tax  | splst$class  == "Arachnida"
splst$misc_tax   <- splst$misc_tax  | splst$class  == "Mammalia"
splst$misc_tax   <- splst$misc_tax  | splst$class  == "Insecta"
splst$misc_tax   <- splst$misc_tax  | splst$class  == "Ichthyostraca"
splst$misc_tax   <- splst$misc_tax  | splst$class  == "Diplopoda"
splst$misc_tax   <- splst$misc_tax  | splst$class  == "Collembola"
splst$misc_tax   <- splst$misc_tax  | splst$class  == "Chilopoda"
splst$misc_tax   <- splst$misc_tax  | splst$class  == "Clitellata"
# write splst to output
write_delim(splst,file.path(dataDir,"splst.csv"),delim=",")
# lists to be produced for WoRMS people
# list of fish species that do not have Paraphyletic group == Pisces
prob1 <- splst %>% filter (Pisces_tax & !Pisces)
write_delim(prob1,file.path(dataDir,"specieslist1.csv"),delim=",")
# list of algae species that do not have Paraphyletic group == Algae
prob2 <- splst %>% filter (Algae_tax & !Algae)
write_delim(prob2,file.path(dataDir,"specieslist2.csv"),delim=",")
# list of species that should have a paraphyletic group 'plants' or something
prob3 <- splst %>% filter (Plants_tax)
write_delim(prob3,file.path(dataDir,"specieslist3.csv"),delim=",")
# list of species that are likely meiobenthos (based on taxonomy) but no attribute meiobenthos
prob4 <- splst %>% filter (meio_tax & !meiobenthos)
write_delim(prob4,file.path(dataDir,"specieslist4.csv"),delim=",")
# list of bird species that maybe should get a Paraphyletic group 'Aves'
prob5 <- splst %>% filter (Aves_tax)
write_delim(prob5,file.path(dataDir,"specieslist5.csv"),delim=",")
# list of species that are classified as 'nekton' but are sometimes considered benthic
prob6 <- splst %>% filter (nekton)
write_delim(prob6,file.path(dataDir,"specieslist6.csv"),delim=",")
# list of species of odd taxa that do not really belong in benthos studies
prob7 <- splst %>% filter (misc_tax & !benthos)
write_delim(prob7,file.path(dataDir,"specieslist7.csv"),delim=",")
# list of species found in benthic datasets, but that are not benthos, not fish, not birds, 
# not plants, not micro-organisms, not meiofauna, not plankton and not nekton
prob8 <- splst %>% filter (!benthos&!Pisces&!Pisces_tax&!Aves_tax&!Plants_tax&!Algae&
                             !micro_tax&!meio_tax&!meiobenthos&!plankton&!nekton) %>%
  arrange(phylum,class,order,family,genus,subgenus,scientificnameaccepted)
write_delim(prob8,file.path(dataDir,"specieslist8.csv"),delim=",")
####### So, what species to use for the maps? #########################
# species should be:
# * not meiobenthos or meio_tax
# * not phytobenthos
# * not Pisces or Pisces_tax
# * not Plants_tax (which includes Algae_tax)
# * not Algae
# * not micro_tax
# * not Aves_tax
# * not misc_tax
# * not plankton (if they are not either benthos or nekton too)
sp2use <- splst %>% 
  filter (!meiobenthos & !meio_tax & !phytobenthos & !Pisces & !Pisces_tax & 
            !Plants_tax & !Algae & !micro_tax & ! Aves_tax & 
            !(plankton & !(benthos|nekton)) &
            !(misc_tax & !benthos))
write_delim(sp2use,file.path(dataDir,"sp2use.csv"),delim=",")
