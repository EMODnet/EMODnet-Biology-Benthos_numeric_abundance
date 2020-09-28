require(tidyverse)
require(reshape2)
totalsDir <- "data/raw_data/totals"
dataDir <- "data/derived_data"

# read the data
data<-read_delim(file.path(totalsDir,"PUCK_TOTAL.csv"),delim=",",
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
  mutate(samplingeffort=gsub(" m²","",samplingeffort)) %>%
  mutate(`AreaBedSamp (m^2)`=gsub(",",".",samplingeffort)) %>%
  mutate(across(c("Count (Dmnless)","BedDryWtBiom (g/m^2)","AreaBedSamp (m^2)"),
         as.numeric))

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
            sum_drywtbiomsamp=sum(`BedDryWtBiom (g/m^2)`*`AreaBedSamp (m^2)`),
            sum_wetwtbiomsamp=NA/mean(nrep)
            )

datar4 <- datar4 %>%
  mutate(avg_bedabund=sum_count/sum_areasamp,
         avg_beddrywtbiom=sum_drywtbiomsamp/sum_areasamp)


write_delim(datar4,file.path(dataDir,"PUCK_cross.csv"),delim=",")
