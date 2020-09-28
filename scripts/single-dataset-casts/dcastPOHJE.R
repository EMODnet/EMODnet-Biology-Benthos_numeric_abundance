require(tidyverse)
require(reshape2)
totalsDir <- "data/raw_data/totals"
dataDir <- "data/derived_data"

# first cast the events in POHJE and rewrite the file
data<-read_delim(file.path(totalsDir,"POHJE_TOTAL.csv"),delim=",",
                 col_types = "ccnccccTnnnccnnnccccnccccccccccccccnccncccccccnccc")
datar<-dcast(data,
             id+datasetid+institutioncode+collectioncode+eventid+datecollected+yearcollected
             +monthcollected+daycollected+recordnumber+fieldnumber+decimallongitude+
               decimallatitude+minimumdepthinmeters+occurrenceid+scientificname+
               scientificnameid+aphiaid+taxonrank+scientificnameaccepted+
               scientificnameauthorship+aphiaidaccepted+kingdom+phylum+class+order+family+
               genus+subgenus+specificepithet+infraspecificepithet+samplingeffort+samplingprotocol
             +qc+parameter+parameter_value+parameter_group_id+parameter_measurementtypeid+
               parameter_bodcterm+parameter_bodcterm_definition+parameter_standardunit+
               parameter_standardunitid+parameter_original_measurement_type+
               parameter_original_measurement_unit+
               parameter_conversion_factor_to_standard_unit ~ event_type, 
             fun.aggregate=length,
             value.var="event_type_id")

datar$parameter[is.na(datar$parameter)&datar$parameter_bodcterm=="Description of the bed"]<-"BedDescrip"

# then parse the eventid field to obtain information on samples, replicates and subsamples by sieve

datar$eventid[regexpr("-",datar$eventid)==-1]<-paste0(datar$eventid[regexpr("-",datar$eventid)==-1],"-")
datar$Sampling<-substr(datar$eventid,regexpr("_",datar$eventid)+1,regexpr("-",datar$eventid)-1)
rest<-substr(datar$eventid,regexpr("-",datar$eventid)+1,150)

datar$Sample<-datar$Sieve<-rep(NA,nrow(datar))

tt<-grep("Sample",rest)
datar$Sample[tt]<-substr(rest[tt],regexpr("_",rest[tt])+1,regexpr("-",rest[tt])-1)
rest[tt]<-substr(rest[tt],regexpr("-",rest[tt])+1,150)

rest<-sub("-","",rest)
tt<-grep("Sieve",rest)
datar$Sieve[tt]<-substr(rest[tt],regexpr("_",rest[tt])+1,150)

# cast the parameters, keeping information of Sampling, Sample and Sieve

datarr<-dcast(datar,
              datasetid+institutioncode+collectioncode+eventid+Sampling+Sample+Sieve+
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
  mutate(across(c("AreaBedSamp (m^2)","BedAbund (#/m^2)","BedAshFreeBiom (g/m^2)",
                "BedDryWtBiom (g/m^2)","BedWetWtBiom (g/m^2)","Count (Dmnless)",
                "DWBiom_Samp (kg)","LowSieveMesh (mm)","UpSieveMesh (mm)","WWBiom_Samp (kg)"),
         as.numeric))


# calculate abundance where it is lacking but count and areasampled are given

tt<-which(is.na(datarr$`BedAbund (#/m^2)`)&!is.na(datarr$'Count (Dmnless)')&!is.na(datarr$'AreaBedSamp (m^2)'))
datarr$`BedAbund (#/m^2)`[tt]<-datarr$`Count (Dmnless)`[tt]/datarr$`AreaBedSamp (m^2)`[tt]

# only keep records with abundance available
datarr<-datarr[!is.na(datarr$`BedAbund (#/m^2)`),]

# sum abundances, biomasses etc. per species over the subsamples identified by Sieve, but within Sample

datar3<- datarr %>%
  group_by(datasetid,institutioncode,collectioncode,Sampling,Sample,
            datecollected,yearcollected,monthcollected,daycollected,recordnumber,
            fieldnumber,decimallongitude,decimallatitude,minimumdepthinmeters,
            scientificname,scientificnameid,aphiaid,taxonrank,
            scientificnameaccepted,scientificnameauthorship,aphiaidaccepted,
            kingdom,phylum,class,order,family,genus,subgenus,specificepithet,
            infraspecificepithet,samplingeffort,samplingprotocol,qc) %>%
  summarise(areabedsamp=mean(`AreaBedSamp (m^2)`),
            bedabund=sum(`BedAbund (#/m^2)`),
            bedashfreebiom=sum(`BedAshFreeBiom (g/m^2)`),
            beddrywtbiom=sum(`BedDryWtBiom (g/m^2)`),
            bedwetwtbiom=sum(`BedWetWtBiom (g/m^2)`),
            count=sum(`Count (Dmnless)`),
            dwbiom_samp=sum(`DWBiom_Samp (kg)`),
            wwbiom_samp=sum(`WWBiom_Samp (kg)`))

# determine number of replicates per Sampling, and add column to the data frame
nrep<- datar3 %>%
  group_by(Sampling) %>% 
  summarize(nrep=length(unique(Sample)))%>%
  ungroup() %>%
  mutate(sampid=row_number())

datar3 <- datar3 %>%
  left_join(nrep,by="Sampling")

# Average densities over samples within Sampling, per species. Do this by calculating
# the sum and dividing by the number of replicates

datar4<- datar3 %>%
  group_by(datasetid,collectioncode,
           datecollected,yearcollected,monthcollected,daycollected,
           decimallongitude,decimallatitude,minimumdepthinmeters,
           sampid,nrep,
           scientificname,scientificnameid,aphiaid,taxonrank,
           scientificnameaccepted,scientificnameauthorship,aphiaidaccepted,
           kingdom,phylum,class,order,family,genus,subgenus,specificepithet) %>%
  summarise(sum_areasamp=mean(areabedsamp)*mean(nrep),
            avg_bedabund=sum(bedabund)/mean(nrep),
            avg_bedwetwtbiom=sum(bedwetwtbiom)/mean(nrep),
            avg_beddrywtbiom=sum(beddrywtbiom)/mean(nrep),
            avg_bedafdwtbiom=sum(bedashfreebiom)/mean(nrep),
            sum_count=sum(count),
            sum_drywtbiomsamp=sum(dwbiom_samp),
            sum_wetwtbiomsamp=sum(wwbiom_samp))

write_delim(datar4,file.path(dataDir,"POHJE_cross.csv"),delim=",")
