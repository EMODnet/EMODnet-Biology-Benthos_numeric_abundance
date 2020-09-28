require(tidyverse)
require(reshape2)
totalsDir <- "data/raw_data/totals"
dataDir <- "data/derived_data"

# read the data
data<-read_delim(file.path(totalsDir,"Polish_TOTAL.csv"),delim=",",
                 col_types = "ccnccccTnnnccnnnccccnccccccccccccccnccncccccccnccc")

# inspection of the parameters
# For the parameter Gender, only 'undetermined' occurs
print(unique(data$parameter_value[data$parameter=="Gender (Dmnless)"]))
# we remove all records with this uninformative parameter
data<- data %>% filter (parameter != "Gender (Dmnless)")

# what about stage?
print(unique(data$parameter_value[data$parameter=="Stage (Dmnless)"]))
# 14 out of all the records have 'juvenile' and not 'undetermined'
# we remove this parameter too
data<- data %>% filter (parameter != "Stage (Dmnless)")

# what about sampling instrument?
print(unique(data$parameter_value[data$parameter=="Instrument (Dmnless)"]))
# only "Van Veen grab". So also removed
data<- data %>% filter (parameter != "Instrument (Dmnless)")


# cast the parameters

datarr<-dcast(data,
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
datarr <- datarr %>% as_tibble() %>%
  mutate(across(c("BedAbund (#/m^2)","BedWetWtBiom (g/m^2)","BedDryWtBiom (g/m^2)"),
         as.numeric))


# determine number of replicates per sampling occasion (date, place), and add column to the data frame
nrep<- datarr %>%
  group_by(datecollected,decimallatitude,decimallongitude,minimumdepthinmeters) %>% 
  summarize(nrep=length(unique(eventid))) %>% 
  ungroup() %>%
  mutate(sampid=row_number())


datarr <- datarr %>%
  left_join(nrep,by=c("datecollected","decimallatitude","decimallongitude","minimumdepthinmeters"))

# Average densities over samples within Sampling, per species. Do this by calculating
# the sum and dividing by the number of replicates
# sampling effort per sample is always 0.1 m2, so total surface is 0.1*nrep

datar4<- datarr %>%
  group_by(datasetid,collectioncode,
           datecollected,yearcollected,monthcollected,daycollected,
           decimallongitude,decimallatitude,minimumdepthinmeters,
           sampid,nrep,
           scientificname,scientificnameid,aphiaid,taxonrank,
           scientificnameaccepted,scientificnameauthorship,aphiaidaccepted,
           kingdom,phylum,class,order,family,genus,subgenus,specificepithet) %>%
  summarise(sum_areasamp=0.1*mean(nrep),
            avg_bedabund=sum(`BedAbund (#/m^2)`)/mean(nrep),
            avg_bedwetwtbiom=sum(`BedWetWtBiom (g/m^2)`)/mean(nrep),
            avg_beddrywtbiom=sum(`BedDryWtBiom (g/m^2)`)/mean(nrep),
            avg_bedafdwtbiom=NA/mean(nrep),
            sum_count=NA/mean(nrep),
            sum_drywtbiomsamp=NA/mean(nrep),
            sum_wetwtbiomsamp=NA/mean(nrep)
  )

write_delim(datar4,file.path(dataDir,"POLISH_cross.csv"),delim=",")
