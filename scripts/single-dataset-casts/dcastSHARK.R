require(tidyverse)
require(reshape2)
totalsDir <- "data/raw_data/totals"
dataDir <- "data/derived_data"

# read the data
data<-read_delim(file.path(totalsDir,"SHARK_TOTAL.csv"),delim=",",
                 col_types = "ccnccccTnnnccnnnccccnccccccccccccccnccncccccccnccc")

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
  mutate(across(c("BedAbund (#/m^2)","BedWetWtBiom (g/m^2)","Count (Dmnless)",
                  "DWBiom_Samp (kg)","InstrumentSurfaceArea (m^2)","NetMesh (mm)",
                  "Salinity (ppt)","VolWBodySamp (m^3)","WC_Temp (degC)",
                  "WWBiom_Samp (kg)" ),
         as.numeric))


# determine number of replicates per sampling occasion (date, place), and add column to the data frame
nrep<- datarr %>%
  group_by(datecollected,decimallatitude,decimallongitude,minimumdepthinmeters) %>% 
  summarize(nrep=length(unique(fieldnumber))) %>% 
  ungroup() %>%
  mutate(sampid=row_number())

datarr <- datarr %>%
  left_join(nrep,by=c("datecollected","decimallatitude","decimallongitude","minimumdepthinmeters")) 

# Average densities over samples within Sampling, per species. Do this by calculating
# the sum and dividing by the number of replicates

datar4<- datarr %>%
  group_by(datasetid,collectioncode,
           datecollected,yearcollected,monthcollected,daycollected,
           decimallongitude,decimallatitude,minimumdepthinmeters,
           sampid,nrep,
           scientificname,scientificnameid,aphiaid,taxonrank,
           scientificnameaccepted,scientificnameauthorship,aphiaidaccepted,
           kingdom,phylum,class,order,family,genus,subgenus,specificepithet) %>%
  summarise(sum_areasamp=mean(`InstrumentSurfaceArea (m^2)`)*mean(nrep),
            avg_bedabund=sum(`BedAbund (#/m^2)`)/mean(nrep),
            avg_bedwetwtbiom=sum(`BedWetWtBiom (g/m^2)`)/mean(nrep),
            avg_beddrywtbiom=NA/mean(nrep),
            avg_bedafdwtbiom=NA/mean(nrep),
            sum_count=sum(`Count (Dmnless)`),
            sum_drywtbiomsamp=sum(`DWBiom_Samp (kg)`),
            sum_wetwtbiomsamp=sum(`WWBiom_Samp (kg)`))

write_delim(datar4,file.path(dataDir,"SHARK_cross.csv"),delim=",")
