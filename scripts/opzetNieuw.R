
# ODAM webservice:
dataset<-4494
url<-paste0("http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=Dataportal%3Aeurobis-obisenv&",
            "resultType=results&viewParams=where%3Adatasetid+IN+%28",dataset,"%29%3Bcontext%3A0100&propertyName=datasetid%2C",
            "datecollected%2Cdecimallatitude%2Cdecimallongitude%2Ccoordinateuncertaintyinmeters%2Cscientificname%2C",
            "aphiaid%2Cscientificnameaccepted%2Cmodified%2Cinstitutioncode%2Ccollectioncode%2Cyearcollected%2C",
            "startyearcollected%2Cendyearcollected%2Cmonthcollected%2Cstartmonthcollected%2Cendmonthcollected%2C",
            "daycollected%2Cstartdaycollected%2Cenddaycollected%2Cseasoncollected%2Ctimeofday%2Cstarttimeofday%2C",
            "endtimeofday%2Ctimezone%2Cwaterbody%2Ccountry%2Cstateprovince%2Ccounty%2Crecordnumber%2Cfieldnumber%2C",
            "startdecimallongitude%2Cenddecimallongitude%2Cstartdecimallatitude%2Cenddecimallatitude%2C",
            "georeferenceprotocol%2Cminimumdepthinmeters%2Cmaximumdepthinmeters%2Coccurrenceid%2Cscientificnameauthorship%2C",
            "scientificnameid%2Ctaxonrank%2Ckingdom%2Cphylum%2Cclass%2Corder%2Cfamily%2Cgenus%2Csubgenus%2Cspecificepithet%2C",
            "infraspecificepithet%2Caphiaidaccepted%2Coccurrenceremarks%2Cbasisofrecord%2Ctypestatus%2Ccatalognumber%2Creferences%2C",
            "recordedby%2Cidentifiedby%2Cyearidentified%2Cmonthidentified%2Cdayidentified%2Cpreparations%2Csamplingeffort%2C",
            "samplingprotocol%2Cqc%2Ceventid%2Cparameter%2Cparameter_value%2Cparameter_group_id%2Cparameter_measurementtypeid%2C",
            "parameter_bodcterm%2Cparameter_bodcterm_definition%2Cparameter_standardunit%2Cparameter_standardunitid%2C",
            "parameter_imisdasid%2Cparameter_ipturl%2Cparameter_original_measurement_type%2Cparameter_original_measurement_unit%2C",
            "parameter_conversion_factor_to_standard_unit%2Cevent%2Cevent_type%2Cevent_type_id&outputFormat=csv")
dats<-read.csv(url)
