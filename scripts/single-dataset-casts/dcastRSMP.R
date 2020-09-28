require(tidyverse)
require(reshape2)
totalsDir <- "data/raw_data/totals"
dataDir <- "data/derived_data"

# read the data
data<-read_delim(file.path(totalsDir,"RSMP_TOTAL.csv"),delim=",",
                 col_types = "ccnccccTnnnccnnnccccnccccccccccccccnccncccccccnccc")

# inspection of the parameters
print(unique(data$parameter))
# For the parameter AreaBedSamp:
print(unique(data$parameter_value[data$parameter=="AreaBedSamp (m^2)"]))
# two values occur, 0.1 and 0.2. We leave it in place

# what about LowSieveMesh (mm)?
print(unique(data$parameter_value[data$parameter=="LowSieveMesh (mm)"]))
# 0.5 and 1 occur. We keep it

# what about sampling instrument?
print(unique(data$parameter_value[data$parameter=="Instrument (Dmnless)"]))
# Several values occur, we leave it in place


# cast the parameters

datarb<-dcast(data,
              datasetid+institutioncode+collectioncode+eventid+
                datecollected+yearcollected+
                monthcollected+daycollected+recordnumber+fieldnumber+decimallongitude+
                decimallatitude+minimumdepthinmeters+occurrenceid+scientificname+
                scientificnameid+aphiaid+taxonrank+scientificnameaccepted+
                scientificnameauthorship+aphiaidaccepted+kingdom+phylum+class+order+family+
                genus+subgenus+specificepithet+infraspecificepithet+samplingeffort+samplingprotocol+
                qc
              ~ parameter,
              value.var="parameter_value"              
)

# make all numerical parameters numeric
datarr <- datarb %>% as_tibble() %>%
  mutate(across(c("AreaBedSamp (m^2)","Count (Dmnless)","LowSieveMesh (mm)"),
         as.numeric))

# what areabedsamp values occur?
print(unique(datarr$`AreaBedSamp (m^2)`))
# there are NA's. All these have 'unspecified grab' as instrument. We only have counts, so density cannot be
# calculated. We delete these records (appr. 3000 in 1 million total)
datarr <- datarr %>% filter (!is.na(`AreaBedSamp (m^2)`))

# different sieves within a single sample?
nsieves<- datarr %>%
  group_by(eventid) %>% 
  summarize(nrep=length(unique(`LowSieveMesh (mm)`))) %>%
  filter (nrep>1)
print(nsieves)
#OK, no replicates that have gone over different sieves.
# checked that every replicate has only one areabedsample

# determine number of replicates per sampling occasion (date, place,depth), and add column to the data frame
nrep<- datarr %>%
  group_by(datecollected,decimallatitude,decimallongitude,minimumdepthinmeters) %>% 
  summarize(nrep=length(unique(eventid))) %>% 
  ungroup() %>%
  mutate(sampid=row_number())

datarr <- datarr %>%
  left_join(nrep,by=c("datecollected","decimallatitude","decimallongitude","minimumdepthinmeters")) 


ttt<-datarr %>%
  group_by(sampid,eventid)%>%
  summarise(arsamp=mean(`AreaBedSamp (m^2)`)) %>%
  ungroup() %>%
  group_by(sampid) %>%
  summarise(sum_areasamp=sum(arsamp))
datarr<-datarr %>%
  left_join(ttt,by='sampid')

# Average densities over samples within Sampling, per species. Do this by calculating
# the sum and dividing by the number of replicates
# 

datar4<- datarr %>%
  group_by(datasetid,collectioncode,
           datecollected,yearcollected,monthcollected,daycollected,
           decimallongitude,decimallatitude,minimumdepthinmeters,sampid,
           nrep,
           scientificname,scientificnameid,aphiaid,taxonrank,
           scientificnameaccepted,scientificnameauthorship,aphiaidaccepted,
           kingdom,phylum,class,order,family,genus,subgenus,specificepithet,
           sum_areasamp) %>%
  summarise(avg_bedabund=NA/mean(nrep),
            avg_bedwetwtbiom=NA/mean(nrep),
            avg_beddrywtbiom=NA/mean(nrep),
            avg_bedafdwtbiom=NA/mean(nrep),
            sum_count=sum(`Count (Dmnless)`),
            sum_drywtbiomsamp=NA/mean(nrep),
            sum_wetwtbiomsamp=NA/mean(nrep)
           )

datar4 <- datar4 %>%
  mutate(avg_bedabund=sum_count/sum_areasamp)

write_delim(datar4,file.path(dataDir,"RSMP_cross.csv"),delim=",")
#save(datar4,file=file.path(dataDir,"RSMP_cross_SI.Rdata"))