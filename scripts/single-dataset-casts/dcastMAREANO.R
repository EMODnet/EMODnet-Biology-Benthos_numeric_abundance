require(tidyverse)
require(reshape2)
totalsDir <- "data/raw_data/totals"
dataDir <- "data/derived_data"

# read the data
data<-read_delim(file.path(totalsDir,"MAREANO_TOTAL.csv"),delim=",",
                 col_types = "ccnccccTnnnccnnnccccnccccccccccccccnccncccccccnccc")

# inspection of the parameters
print(unique(data$parameter))

# there are records with parameter==NA. I think these records just signal the presence of a species
# without any other measurement. WE have to drop these

datac<-data %>% filter (!is.na(parameter))

# cast the parameters

datarb<-dcast(datac,
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
  mutate(across(c("BedAbund (#/m^2)","BedWetWtBiom (g/m^2)"),
         as.numeric))

# determine number of replicates per sampling occasion (date, place,depth), and add column to the data frame
nrep<- datarr %>%
  group_by(datecollected,decimallatitude,decimallongitude,minimumdepthinmeters) %>% 
  summarize(nrep=length(unique(eventid))) %>% 
  ungroup() %>%
  mutate(sampid=row_number()) %>%
  mutate(sum_areasamp=NA)

datarr <- datarr %>%
  left_join(nrep,by=c("datecollected","decimallatitude","decimallongitude","minimumdepthinmeters")) 


# Average densities over samples within Sampling, per species. Do this by calculating
# the sum and dividing by the number of replicates
# 

datar4<- datarr %>%
  group_by(datasetid,collectioncode,
           datecollected,yearcollected,monthcollected,daycollected,
           decimallongitude,decimallatitude,minimumdepthinmeters,
           sampid,nrep,
           scientificname,scientificnameid,aphiaid,taxonrank,
           scientificnameaccepted,scientificnameauthorship,aphiaidaccepted,
           kingdom,phylum,class,order,family,genus,subgenus,specificepithet,
           sum_areasamp) %>%
  summarise(avg_bedabund=sum(`BedAbund (#/m^2)`)/mean(nrep),
            avg_bedwetwtbiom=sum(`BedWetWtBiom (g/m^2)`)/mean(nrep),
            avg_beddrywtbiom=NA/mean(nrep),
            avg_bedafdwtbiom=NA/mean(nrep),
            sum_count=NA/mean(nrep),
            sum_drywtbiomsamp=NA/mean(nrep),
            sum_wetwtbiomsamp=NA/mean(nrep)
  )

write_delim(datar4,file.path(dataDir,"MAREANO_cross.csv"),delim=",")
