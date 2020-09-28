require(tidyverse)
require(reshape2)
totalsDir <- "data/raw_data/totals"
dataDir <- "data/derived_data"

# read the data
data<-read_delim(file.path(totalsDir,"MWTL_TOTAL.csv"),delim=",",
                 col_types = "ccnccccTnnnccnnnccccnccccccccccccccnccncccccccnccc")

# inspection of the parameters
print(unique(data$parameter))
# For the parameter AreaBedSamp:
print(unique(data$parameter_value[data$parameter=="AreaBedSamp (m^2)"]))
# two values occur, 0.1 and 0.2. We leave it in place

# what about sampling instrument?
print(unique(data$parameter_value[data$parameter=="Instrument (Dmnless)"]))
# Several values occur, we leave it in place

#check if all parameters are given
print(length(which(is.na(data$parameter))))

# 3736 problems!
print(unique(data$parameter_measurementtypeid[is.na(data$parameter)]))
datac<- data %>% 
  mutate(parameter_new=ifelse(parameter_measurementtypeid==
      "http://vocab.nerc.ac.uk/collection/P01/current/SACFOR01/","AbundCat (Dmnless)",parameter))%>%
  mutate(parameter_new=ifelse(parameter_measurementtypeid==
      "http://vocab.nerc.ac.uk/collection/P01/current/UKMH0405/","JNCC_Class (Dmnless)",parameter_new))%>%
  select(-parameter)%>%
  mutate(parameter=parameter_new,.keep="unused")


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
# restrict to cores only and remove records without abundance information
datarb <- datarb %>%
  filter(`Instrument (Dmnless)`=="unconsolidated sediment corers" |
           `Instrument (Dmnless)`== "Hamon happer") %>%
  filter(!is.na(`BedAbund (#/m^2)`))

# make all numerical parameters numeric
datarr <- datarb %>% as_tibble() %>%
  mutate(across(c("AreaBedSamp (m^2)","BedAbund (#/m^2)","BedAshFreeBiom (g/m^2)",
                  "BedCoverage (%)","BedWetWtBiom (g/m^2)","Length (mm)"),
         as.numeric))

# what areabedsamp values occur?
print(unique(datarr$`AreaBedSamp (m^2)`))
# there are NA's, but since we abundances, this does not matter too much

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
           decimallongitude,decimallatitude,minimumdepthinmeters,
           sampid,nrep,
           scientificname,scientificnameid,aphiaid,taxonrank,
           scientificnameaccepted,scientificnameauthorship,aphiaidaccepted,
           kingdom,phylum,class,order,family,genus,subgenus,specificepithet,
           sum_areasamp) %>%
  summarise(avg_bedabund=sum(`BedAbund (#/m^2)`)/mean(nrep),
            avg_bedwetwtbiom=NA/mean(nrep),
            avg_beddrywtbiom=NA/mean(nrep),
            avg_bedafdwtbiom=sum(`BedAshFreeBiom (g/m^2)`)/mean(nrep),
            sum_count=NA/mean(nrep),
            sum_drywtbiomsamp=NA/mean(nrep),
            sum_wetwtbiomsamp=NA/mean(nrep)
  )

write_delim(datar4,file.path(dataDir,"MWTL_cross.csv"),delim=",")
