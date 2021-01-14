# Numerical abundance of benthos in North and Baltic Seas

## Introduction

The data product on benthic living modes (Beauchard, 2018), was based on an extensive compilation of data on benthic abundance and biomass. However, this dataset was only present as a data file, without the underlying scripts to reproduce the result. With the present data product, we correct this procedural gap.

This repository contains the scripts used to compile different datasets in this area into a comprehensive summary of available data on abundance and (where available) biomass density of macrozoobenthos. This dataset differs in details from the file underlying the data product on living modes, as it is a new version. However, differences are minor. Datasets were selected that were sufficiently similar in methods for sampling (either boxcore or grab), sampled surface (in the order of 0.1 square meter, although the exact value is variable - it can be found back in the data files) and sieves (1 mm and 0.5 mm sieves were included). For all datasets, abundance was either used directly from the given abundance in the dataset, or calculated from the given counts and area sampled.

The compiled dataset is composed of approximately 80000 samples, many of which are themselves a composite of a number of replicate grabs or cores. Replicates were taken together during the processing of the data. Species recorded in the dataset were filtered as much as possible, based on whether they belong to the macrobenthos. The filtering method was automatic, based on WoRMS information. In general, WoRMS contains good quality trait information on benthic species. However, it cannot be guaranteed that all of the >4000 species are true benthic species, or that all true benthic species are included in the data set.

Information gained during the species selection was fed back to the WoRMS editors and will be used in future to further refine the WoRMS trait data.

The aim of this product is to underly analysis of such characteristics as living modes, bioturbation potential, role in food webs or indicators for environmental changes. Although the maps in the product show all information contained in the database over all years since 1980, time information is stored in the intermediary files and thus in principle available to investigate time evolution in the composition of benthos across Europe. Such applications have not been made, but it is hoped that several of this type of applications will be developed based on the present product.

## General procedure in preparing the data product

The entire workflow is documented in an R markdown document to be found in the root directory of the GitHub site. The pdf file based on this markdown document resides in the director 'docs'.

In short, the workflow is as follows. Data are retrieved by dataset and by year, using webservices. They are then recompiled into a single file by dataset. These files are very big.
Subsequently the data files per dataset are parsed to extract the abundance and biomass data. Based on the parsed results, a total data file is constructed that contains all relevant records. This dataset is still partially in a 'long' format. There is one record per sample and per species. It can be used to extract data on individual species, or on species groups. In a next step of the analysis the benthic species are filtered out. This procedure is essentially the same as that used in the product showing presence/abence of macrobenthos in the Greater North Sea. Finally, maps are produced, as well as a data file that can be used for interpolation by DIVA. The mapping is similar to the mapping procedure in the presence/absence data product, although details (e.g. the area covered) differ.

## Directory structure

```
{{directory_name}}/
├── data/
│   ├── derived_data/
│   └── raw_data/
│       └── downloads
│       └── totals
├── docs/
├── product/
│   ├── maps/
│   └── species_plots/
│   └── species_rasters/
└── scripts/
```

* **analysis** - The R Markdown document that documents and discusses the code
* **data** - the containers for data storage. Note that due to the size of the files, no actual data are stored in Github
* **docs** - Rendered report of the Markdown document
* **product** - containers for output product files- actually empty because of file size
* **scripts** - Code used for the product. As almost all code is contained in the Markdown file, this directory is almost empty. It only contains a slightly modified version of a routine in the Emodnet maps package, that was needed because of the small size of the raster cells in our maps. This version omits the border of the raster polygon and thus produces much clearer maps when the border is thick compared to the cell.

## Data series

Data used in this product are:

MAREANO - Base-line mapping of fauna obtained with grab (2006 – 2013) 	https://www.emodnet-biology.eu/data-catalog?module=dataset&dasid=4539

NSBS - North Sea Benthos Survey (1986) 	https://www.emodnet-biology.eu/data-catalog?module=dataset&dasid=67

MWTL - Dutch long term monitoring of macrobenthos in the Dutch Continental Economical Zone of the North Sea (since 1991 – 2015) 	https://www.emodnet-biology.eu/data-catalog?module=dataset&dasid=5759

ODAM - Danish benthic marine monitoring data (1911 – 2013) 	https://www.emodnet-biology.eu/data-catalog?module=dataset&dasid=4494

POHJE - Finnish Baltic Sea benthic monitoring (1964 – 2016) 	https://www.emodnet-biology.eu/data-catalog?module=dataset&dasid=5725

Polish Monitoring Programme - Monitoring of the Baltic Sea (1987 – 2013) 	https://www.emodnet-biology.eu/data-catalog?module=dataset&dasid=2467

Bay of Puck dataset (1996) 	https://www.emodnet-biology.eu/data-catalog?module=dataset&dasid=611

REBENT - Benthic Network (2003 – 2015) 	https://www.emodnet-biology.eu/data-catalog?module=dataset&dasid=4412

RSMP Baseline Dataset (1976 – 2016) 	https://www.emodnet-biology.eu/data-catalog?module=dataset&dasid=5922

SHARK - Marine soft bottom macrozoobenthos monitoring in Sweden (1971 – 2014) 	https://www.emodnet-biology.eu/data-catalog?module=dataset&dasid=2454

Note that a research version of North Sea Benthos Survey was used, as there were problems in the version stored in EMODnet

## Wfs request

The generic wfs request for these datasets was as follows. Note that the R statement paste0 concatenates all elements separated in the list by commas, after giving a value to begindate, enddate an ddatasetid.
```
paste0(
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
```

## Data product

The following data products are derived by this code.
1. a large file called ./product/maps/benth_recs.csv containing the records of benthic species including abundance and (if available) biomass density information, besides the taxonomic position of the species and the details of the sampling location, sampling institute etc. This file can be used as the basis for a number of analyses not explored further in this data product.
2. a large file, called ./product/maps/spesh.csv of sampling events (rows) x species (columns) containing a few columns with the specification of the sampling event, and then a column per species with its abundance value corresponding to all of the sampling events. This is the basic information underlying the maps produced here.
3. For all species with a frequency exceeding 100 samples (out of 80000), a raster (stored as a tif file in ./product/species_rasters) with rasterized abundance. A log-transformation has been applied prior to rasterization in this product.
4. For all these same species, a map plotting the raster values with a legend. These maps are to be found in ./product/species_plots/*.png 

## More information:

### References
Beauchard, O. 2018. Data product on benthic living modes. https://www.emodnet-biology.eu/distribution-benthic-macroinvertebrate-living-modes-european-seas

Beauchard, O.; Troupin, C.; (2018): Distribution of benthic macroinvertebrate living modes in European seas. Marine Data Archive. https://doi.org/10.14284/373

### Code and methodology

Code and methodology are documented in the file ./Assemble_abundances_benthos.Rmd.

### Citation and download link

This product should be cited as:

Beauchard, Olivier, Herman, Peter M.J., Fernandez, Salvador. 2021. Data product numerical abundance of benthic macroinvertebrates in North Sea and Baltic Sea. xxxxxx

Available to download in:

xxxxxx

### Authors

Olivier Beauchard, Peter M.J. Herman, Salvador Fernandez
