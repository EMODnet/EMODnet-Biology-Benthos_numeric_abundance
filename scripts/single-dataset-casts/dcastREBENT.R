require(tidyverse)
require(reshape2)
totalsDir <- "data/raw_data/totals"
dataDir <- "data/derived_data"

# read the data
data<-read_delim(file.path(totalsDir,"REBENT_TOTAL.csv"),delim=",",
                 col_types = "ccnccccTnnnccnnnccccnccccccccccccccnccncccccccnccc")

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

print(names(datarb))

# make all numerical parameters numeric

datarr <- datarb %>% as_tibble() %>%
  mutate(across(c("Count (Dmnless)","SubSamplingCoefficient (Dmnless)","AreaBedSamp (m^2)" ),
         as.numeric))
# there are missing areabedsample values. all of these have been taken with a Smith-McIntyre grab
# and all the Smith-McIntyre grab samples that do have an area samples, have 0.1 for this area
# we replace the NA with 0.1
# In addition,  there are records without a count (mostly algae in quadrats, presence only). We drop these

datarr <- datarr %>% 
  mutate (`AreaBedSamp (m^2)`=ifelse(is.na(`AreaBedSamp (m^2)`),0.1,`AreaBedSamp (m^2)`)) %>%
  filter (!is.na(`Count (Dmnless)`))

# SubsampleCoefficient is always 1. We do not consider it further

# determine number of replicates per sampling occasion (date, place), and add column to the data frame
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

datar4<- datarr %>%
  group_by(datasetid,collectioncode,
           datecollected,yearcollected,monthcollected,daycollected,
           decimallongitude,decimallatitude,minimumdepthinmeters,
           sampid,nrep,
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


write_delim(datar4,file.path(dataDir,"REBENT_cross.csv"),delim=",")
