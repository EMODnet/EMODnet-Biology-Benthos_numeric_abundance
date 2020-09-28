require(tidyverse)
downloadDir <- "data/raw_data/downloads"
dataDir <- "data/derived_data"
totalsDir<-"data/raw_data/totals"
getDatasets <- data.frame(name=c("Macrobel","Mareano","NSBS","MWTL","ODAM","POHJE",
                                 "Polish","Puck","Rebent","RSMP","SHARK"),
                          datasetid=c(145,4539,67,5759,4494,5725,2467,611,4412,5922,2454),
                          include=c(T,T,T,T,T,T,T,T,T,T,T))
getDatasets <- getDatasets %>% filter(include)
for(jj in 1:length(getDatasets$datasetid)){
  for(year in 1980:2019){
    datasetid <- getDatasets$datasetid[jj]
    datasetname<-getDatasets$name[jj]
    decadename<-decades$nams[ii]
    begindate <- paste0(year,"-01-01")
    enddate   <- paste0(year,"-12-31")
    print(paste("downloading data for dataset",datasetname,"ID nr: ", datasetid,"year",year))
    downloadURL <- paste0(
      "http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&",
      "request=GetFeature&typeName=Dataportal%3Aeurobis-obisenv&",
      "resultType=results&viewParams=where%3Adatasetid+IN+%28",
      datasetid,
      "%29+AND+%28%28observationdate+BETWEEN+%27",
      begindate,
      "%27+AND+%27",
      enddate,
      "%27+%29%29%3Bcontext%3A0100&propertyName=",
      "datasetid%2C",
      "datecollected%2C",
      "decimallatitude%2C",
      "decimallongitude%2C",
      "scientificname%2C",
      "aphiaid%2C",
      "scientificnameaccepted%2C",
      "institutioncode%2C",
      "collectioncode%2C",
      "yearcollected%2C",
      "monthcollected%2C",
      "daycollected%2C",
      "recordnumber%2C",
      "fieldnumber%2C",
      "minimumdepthinmeters%2C",
      "occurrenceid%2C",
      "scientificnameauthorship%2C",
      "scientificnameid%2C",
      "taxonrank%2C",
      "kingdom%2C",
      "phylum%2C",
      "class%2C",
      "order%2C",
      "family%2C",
      "genus%2C",
      "subgenus%2C",
      "specificepithet%2C",
      "infraspecificepithet%2C",
      "aphiaidaccepted%2C",
      "samplingeffort%2C",
      "samplingprotocol%2C",
      "qc%2C",
      "eventid%2C",
      "parameter%2C",
      "parameter_value%2C",
      "parameter_group_id%2C",
      "parameter_measurementtypeid%2C",
      "parameter_bodcterm%2C",
      "parameter_bodcterm_definition%2C",
      "parameter_standardunit%2C",
      "parameter_standardunitid%2C",
      "parameter_original_measurement_type%2C",
      "parameter_original_measurement_unit%2C",
      "parameter_conversion_factor_to_standard_unit%2C",
      "event%2C",
      "event_type%2C",
      "event_type_id",
      "&outputFormat=csv"
    )
    data <- read_csv(downloadURL) 
    filename = paste0(datasetname,"_",datasetid,"_",year,".csv")
    if(nrow(data) != 0){
      write_delim(data, file.path(downloadDir, filename), delim = ",")
    }
  }
}



filnams<-list.files(downloadDir)

for (ds in getDatasets$name){
  filelist<-filnams[grep(ds,filnams)]
 
  all2Data <- lapply(filelist, function(x) 
    read_delim(file.path(downloadDir, x), 
             delim = ",",
             col_types = "cnccccTnnnccnnnccccnccccccccccccccnccncccccccnccc"
    )
  ) %>%
  set_names(filelist) %>%
  bind_rows(.id = "fileID") 
  
  
  filename = paste0(ds,"_TOTAL.csv")
  write_delim(all2Data,file.path(totalsDir,filename),delim=",")
  rm(all2Data)
}
