require(tidyverse)
require(reshape2)
totalsDir <- "data/raw_data/totals"
dataDir <- "data/derived_data"

datfils<-c("SHARK_TOTAL.csv",
           "RSMP_TOTAL.csv",
           "REBENT_TOTAL.csv",
           "PUCK_TOTAL.csv",
           "POLISH_TOTAL.csv",
           "POHJE_TOTAL.csv",
           "ODAM_TOTAL.csv",
           "MWTL_TOTAL.csv",
           "MAREANO_TOTAL.csv")

retrieve_data<-function(fil){
  data<-read_delim(file.path(totalsDir,fil),delim=",",
                   col_types = "ccnccccTnnnccnnnccccnccccccccccccccnccncccccccnccc")
  return(data)
}

stand_par_nam<-tibble(oldnam=c("InstrumentSurfaceArea (m^2)"),
                      newnam=c("AreaBedSamp (m^2)"))

do_cast<-function(data,fil){
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
  parnams<-names(datarr)[34:length(names(datarr))]
  for(i in 1:nrow(stand_par_nam)){
    parnams[parnams==stand_par_nam$oldnam[i]]<-stand_par_nam$newnam[i]
  }
  names(datarr)[34:length(names(datarr))]<-parnams
  return(datarr)
}


numfields<-c("AreaBedSamp (m^2)",
             "BedAbund (#/m^2)",
             "BedAshFreeBiom (g/m^2)",
             "BedCoverage (%)",
             "BedDryWtBiom (g/m^2)",
             "BedWetWtBiom (g/m^2)",
             "Count (Dmnless)",
             "DWBiom_Samp (kg)",
             "Length (mm)",
             "LowSieveMesh (mm)",
             "NetMesh (mm)",
             "Salinity (ppt)",
             "SubSamplingCoefficient (Dmnless)",
             "UpSieveMesh (mm)",
             "VolWBodySamp (m^3)",
             "WC_Temp (degC)",
             "WWBiom_Samp (kg)"
             )

make_num<-function(datarr,fil){
  parnams<-names(datarr)
  datarr <- datarr %>% as_tibble() %>%
    mutate(across(parnams[parnams %in% numfields],as.numeric))
  
}

cleanPOHJE<-function(data){
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
  # correct missing parameter field when it should be description of the bed
  datar <- datar %>%
    mutate(parameter=ifelse(is.na(parameter) & parameter_bodcterm=="Description of the bed",
                            "BedDescrip",
                            parameter)
    )
  # remove Sieve information from eventid, so that events and not subsamples are indicated
  datar<- datar %>%
    mutate(dd=regexpr("-Sie",eventid)) %>%
    mutate(eventid=ifelse(dd>0,
                          substr(eventid,1,dd-1),
                          eventid
                          )
           )
  return(datar)
}



add_nrep<-function(datarr,fil){
  nrep<- datarr %>%
    group_by(datecollected,decimallatitude,decimallongitude,minimumdepthinmeters) %>% 
    summarize(nrep=length(unique(eventid))) %>% 
    ungroup() %>%
    mutate(sampid=row_number())
  
  datarr <- datarr %>%
    left_join(nrep,by=c("datecollected","decimallatitude","decimallongitude","minimumdepthinmeters")) 
  
  arsamps<-datarr %>%
    group_by(sampid,eventid)%>%
    summarise(arsamp=mean(`AreaBedSamp (m^2)`)) %>%
    ungroup() %>%
    group_by(sampid) %>%
    summarise(sum_areasamp=sum(arsamp))
  
  data_w_rep<-datarr %>%
    left_join(arsamps,by='sampid')
  
  return(data_w_rep)
}

sum_avg_param<-function(datarr,fil){
  pp<-names(datarr)
  data_sap<- datarr %>%
    group_by(datasetid,collectioncode,
             datecollected,yearcollected,monthcollected,daycollected,
             decimallongitude,decimallatitude,minimumdepthinmeters,
             sampid,nrep,scientificnameaccepted,
             scientificnameauthorship,aphiaidaccepted,
             kingdom,phylum,class,order,family,genus,subgenus,specificepithet,
             sum_areasamp) %>%
    summarise(avg_bedabund=
                ifelse("BedAbund (#/m^2)" %in% pp,
                  ifelse(!is.na(sum(`AreaBedSamp (m^2)`)),
                    sum(`BedAbund (#/m^2)`*`AreaBedSamp (m^2)`)/mean(sum_areasamp),
                    sum(`BedAbund (#/m^2)`)/mean(nrep)
                  ),
                  ifelse("Count (Dmnless)" %in% pp & !is.na(mean(sum_areasamp)),
                    sum(`Count (Dmnless)`)/mean(sum_areasamp),
                    NA/mean(nrep)
                  )
                ),
              avg_bedwetwtbiom=
                ifelse("BedWetWtBiom (g/m^2)" %in% pp,
                  ifelse(!is.na(sum(`AreaBedSamp (m^2)`)), 
                    sum(`BedWetWtBiom (g/m^2)`*`AreaBedSamp (m^2)`)/mean(sum_areasamp),
                    sum(`BedWetWtBiom (g/m^2)`)/mean(nrep)
                  ),
                  ifelse("WWBiom_Samp (kg)" %in% pp & !is.na(mean(sum_areasamp)),
                         sum(`WWBiom_Samp (kg)`)*1000/mean(sum_areasamp),
                         NA/mean(nrep)
                  )
                ),
              avg_beddrywtbiom=
                ifelse("BedDryWtBiom (g/m^2)" %in% pp,
                  ifelse(!is.na(sum(`AreaBedSamp (m^2)`)), 
                    sum(`BedDryWtBiom (g/m^2)`*`AreaBedSamp (m^2)`)/mean(sum_areasamp),
                    sum(`BedDryWtBiom (g/m^2)`)/mean(nrep)
                  ),
                  ifelse("DWBiom_Samp (kg)" %in% pp & !is.na(mean(sum_areasamp)),
                         sum(`DWBiom_Samp (kg)`)*1000/mean(sum_areasamp),
                         NA/mean(nrep)
                  )
                ),
              avg_bedafdwtbiom=
                ifelse("BedAshFreeBiom (g/m^2)" %in% pp,
                  ifelse(!is.na(`AreaBedSamp (m^2)`), 
                    sum(`BedAshFreeBiom (g/m^2)`*`AreaBedSamp (m^2)`)/mean(sum_areasamp),
                    sum(`BedAshFreeBiom (g/m^2)`)/mean(nrep)
                  ),
                  NA/mean(nrep)
                ),
              sum_count=
                ifelse("Count (Dmnless)"%in% pp, 
                  sum(`Count (Dmnless)`),
                  NA/mean(nrep)
                ),
              sum_drywtbiomsamp=
                ifelse("DWBiom_Samp (kg)" %in% pp,
                  sum(`DWBiom_Samp (kg)`),
                  NA/mean(nrep)
                ),
              sum_wetwtbiomsamp=
                ifelse("WWBiom_Samp (kg)" %in% pp,
                  sum(`WWBiom_Samp (kg)`),
                  NA/mean(nrep)
                )
    )
  return(data_sap)
}
# main loop over the datasets
nds<-length(datfils)
nams<-substr(datfils,1,regexpr("_",datfils)-1)

for(ds in 1:nds){
  fil<-datfils[ds]
  # read the data
  data<-retrieve_data(fil)

        # for POHJE, some initial cleaning of the subsampling mess is needed
          if(fil=="POHJE_TOTAL.csv")data<-cleanPOHJE(data)
        # for MWTL some parameter fields are empty. We fill them
          if (fil=="MWTL_TOTAL.csv"){
            data<- data %>% 
              mutate(parameter_new=ifelse(parameter_measurementtypeid==
                                            "http://vocab.nerc.ac.uk/collection/P01/current/SACFOR01/",
                                          "AbundCat (Dmnless)",parameter))%>%
              mutate(parameter_new=ifelse(parameter_measurementtypeid==
                                            "http://vocab.nerc.ac.uk/collection/P01/current/UKMH0405/",
                                          "JNCC_Class (Dmnless)",parameter_new))%>%
              select(-parameter)%>%
              mutate(parameter=parameter_new,.keep="unused")
          }
        # for MAREANO, some records have parameter= NA. We drop them
          if (fil=="MAREANO_TOTAL.csv") data<-data %>% filter (!is.na(parameter))

  # cast the parameters
  data1<-do_cast(data,fil)
  
        # derive the area sampled from the character field samplingeffort in PUCK and ODAM
          if (fil=="PUCK_TOTAL.csv" | fil=="ODAM_TOTAL.csv"){
            data1<-data1 %>%
              mutate(samplingeffort=gsub(" m²","",samplingeffort)) %>%
              mutate(`AreaBedSamp (m^2)`=gsub(",",".",samplingeffort))
          }
        # and just that slight little difference for POLISH
          if(fil=="POLISH_TOTAL.csv"){
            data1<-data1 %>%
              mutate(`AreaBedSamp (m^2)`=gsub(" m2","",samplingeffort))
          }
        # select only core samples from MWTL, and restrict to records with abundance
          if (fil=="MWTL_TOTAL.csv"){
            data1 <- data1 %>%
              filter(`Instrument (Dmnless)`=="unconsolidated sediment corers" |
                       `Instrument (Dmnless)`== "Hamon happer") %>%
              filter(!is.na(`BedAbund (#/m^2)`))
          }
  
  # make all numerical parameters numeric
  data2<-make_num(data1,fil)
  
        # for POHJE,calculate abundance where it is lacking but count and areasampled are given
          if(fil=="POHJE_TOTAL.csv"){
            data2 <- data2 %>%
              mutate(`BedAbund (#/m^2)`=
                       ifelse(
                         is.na(`BedAbund (#/m^2)`)&!is.na(`Count (Dmnless)`)&!is.na(`AreaBedSamp (m^2)`),
                         `Count (Dmnless)`/`AreaBedSamp (m^2)`,
                         `BedAbund (#/m^2)`
                       )
              ) %>%
              filter(!is.na(`BedAbund (#/m^2)`))
          }

        # remove records without area sampled in RSMP, as we only have counts
          if(fil=="RSMP_TOTAL.csv")data2 <- data2 %>% filter (!is.na(`AreaBedSamp (m^2)`))
        
        # In REBENT, there are missing areabedsample values. all of these have been taken with a Smith-McIntyre grab
        # and all the Smith-McIntyre grab samples that do have an area sampled, have 0.1 for this area
        # we replace the NA with 0.1
        # In addition, in REBENT there are records without a count (mostly algae in quadrats, presence only). We drop these
          if(fil=="REBENT_TOTAL.csv"){
            data2 <- data2 %>%
              mutate (`AreaBedSamp (m^2)`=ifelse(is.na(`AreaBedSamp (m^2)`),0.1,`AreaBedSamp (m^2)`)) %>%
              filter (!is.na(`Count (Dmnless)`))
          }
  
        # use fieldnumber instead of eventid to identify events in SHARK
          if(fil=="SHARK_TOTAL.csv") data2$eventid<-data2$fieldnumber
        
        # no information on area samples in MAREANO. Rename samplingeffort (only NA)
          if (fil=="MAREANO_TOTAL.csv") data2$`AreaBedSamp (m^2)`<-as.numeric(data2$samplingeffort)
  
  # determine number of replicates per sampling occasion (date, place), and add column to the data frame
  data3<-add_nrep(data2,fil)
  
  # Average/sum parameters over samples
  data4<-sum_avg_param(data3,fil)  
  
  # write results
  #pth<-file.path(dataDir,paste0(nams[ds],"_cross_CA.csv"))
  #write_delim(data4,pth,delim=",")

  pth<-file.path(dataDir,paste0(nams[ds],"_cross_CA.Rdata"))
  save(data4,file=pth)
  
  rm(data,data1,data2,data3,data4)
}


# collect all records and save them

fl<-tibble(fn=list.files(file.path(dataDir)))
fl<- fl %>% filter (grepl("_CA.Rdata",fn))

for(i in (1:nrow(fl))){
  f<-fl$fn[i]
  load(file.path(dataDir,f))
  if(i==1)all_recs<-data4 else all_recs<-rbind(all_recs,data4)
}

write_delim(all_recs,file.path(dataDir,"all_recs.csv"),delim=",")
save(all_recs,file=file.path(dataDir,"all_recs.Rdata"))



