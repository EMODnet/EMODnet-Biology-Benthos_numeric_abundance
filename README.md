# Numerical abundance of benthos in North and Baltic Seas

## Introduction

This repository contains the scripts, but not the datasets, used to compile different benthic datasets in this area into a comprehensive summary of available data on abundance and (where available) biomass density.

The code is explained in the R markdown document in the analysis subdirectory. The generated pdf is to be found in the docs directory.

## General procedure in preparing the data product

Data are retrieved by dataset and by year, as explained in the Rmd file. They are then recompiled into a single file by dataset. These files are very big.
Subsequently the data files per dataset are parsed to extract the abundance and biomass data. Based on the parsed results, a total data file is constructed that contains all relevant records. This dataset is still partially in a 'long' format. There is one record per sample and per species. It can be used to extract data on individual species, or on species groups, and map them. The mapping is not yet part of the set.

## Directory structure

```
{{directory_name}}/
├── analysis
├── data/
│   ├── derived_data/
│   └── raw_data/
├── docs/
├── product/
└── scripts/
```

* **analysis** - Markdown or Jupyter notebooks
* **data** - Raw and derived data
* **docs** - Rendered reports
* **product** - Output product files
* **scripts** - Reusable code

## Data series

{{data_series}}

```
{{data_wfs_request}}
```

## Data product

{{data_product_description}}

## More information:

### References

### Code and methodology

{{link_code}}

### Citation and download link

This product should be cited as:

{{product_citation}}

Available to download in:

{{link_download}}

### Authors

{{product_authors}}
