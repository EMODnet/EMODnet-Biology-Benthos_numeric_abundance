require(raster)
require(sp)
require(RColorBrewer)
require(svMisc)
require(tidyverse)
library(ggplot2)
library('rnaturalearth')
library(magick)
library(rgeos)

downloadDir <- "data/raw_data"
dataDir <- "data/derived_data"
mapsDir <- "product/maps"
rasterDir <- "product/species_rasters"
plotsDir <- "product/species_plots"
proWG<-CRS("+proj=longlat +datum=WGS84")

##########################################################
# define a raster covering the grid. Set resolution of the raster here
##########################################################
r<-raster(ext=extent(-16,36,40,74),ncol=156,nrow=238,crs=proWG,vals=0)
##########################################################
# load some information needed for the maps
##########################################################
# Show countries
world <- ne_countries(scale = "medium", returnclass = "sf")
# EMODnet colors
emodnetColor <- list(
  # First palette
  blue = "#0A71B4",
  yellow = "#F8B334",
  darkgrey = "#555555",
  # Secondary palette,
  darkblue = "#012E58",
  lightblue = "#61AADF",
  white = "#FFFFFF",
  lightgrey = "#D9D9D9"
)
# EMODnet logo
logo_raw <- image_read("https://www.emodnet-biology.eu/sites/emodnet-biology.eu/files/public/logos/logo-footer.png") 
logo <- logo_raw %>% image_scale("150")
#
#########################################################
# load data
#
load(file.path(mapsDir,"spesh.Rdata"))
spfr<-read_delim(file.path(mapsDir,"specieslist.csv"),delim=",")

# set minimum species frequency for plot
nsptoplot<-length(which(spfr$n_events>100))

#################################################################3
# map production. Subsetting, e.g. by time, can occur here

spmin<-1
spmax<-nsptoplot

for(ss in spmin:spmax){
  spAphId<-spfr$AphiaID[ss]
  specname<-spfr$scientificnameaccepted[ss]
  spcolumn<-paste0("ab_",spAphId)
  progress(value=ss,max.value=spmax,init=(ss=spmin))  
  
  spe <- spesh %>%
    dplyr::select(decimallongitude,decimallatitude,all_of(spcolumn))
  names(spe)[3]<-"abund"
  
  # introduce a transformation of the data here
  spe <- spe %>%
    mutate(abund=log(abund+1)/log(10))
  r1nam<-"log10_abund"
  
  coordinates(spe)<- ~decimallongitude+decimallatitude
  projection(spe)<-proWG
  r1<-rasterize(spe,r,field="abund",fun=mean)
  names(r1)<-r1nam
  
  # Export rasters as tif
  raster::writeRaster(
    r1, 
    file.path(
      rasterDir, paste0(
        sprintf("%04d",ss), "_",
        spAphId, "_",
        gsub(" ", "-", specname),
        ".tif"
      )
    ),
    overwrite=TRUE
  )
  #
  # Transform raster to vector
  
  grid <- sf::st_as_sf(raster::rasterToPolygons(r1))
  grid_bbox <- sf::st_bbox(sf::st_transform(grid, 3035))
  
  # Plot the grid
  plot_grid <- ggplot() +
    geom_sf(data = world, 
            fill = emodnetColor$darkgrey, 
            color = emodnetColor$lightgrey, 
            size = 0.1) +
    geom_sf(data = grid, aes(fill = log10_abund), color=NA) +
    coord_sf(crs = 3035, xlim = c(grid_bbox$xmin, grid_bbox$xmax), ylim = c(grid_bbox$ymin, grid_bbox$ymax)) +
    scale_fill_viridis_c(alpha = 1, begin = 0, end = 1, direction = 1) +
    ggtitle(specname,
            subtitle = paste0('AphiaID ', spAphId)) +
    theme(
      panel.background = element_rect(fill = emodnetColor$lightgrey),
      plot.title = element_text(color= emodnetColor$darkgrey, size = 14, face="bold.italic", hjust = 0.5),
      plot.subtitle = element_text(color= emodnetColor$darkgrey, face="bold", size=10, hjust = 0.5)
    )
  
  # Inspect plot
  plot_grid
  
  # Save plot
  filnam<-file.path(plotsDir, 
                    paste0(sprintf("%04d",ss), "_",spAphId, "_",gsub(" ", "-", specname),".png"))
  ggsave(filename = filnam,width = 198.4375, height = 121.70833333, dpi = 300, units = "mm")
  
  # # Add emodnet logo
  # plot <- image_read(filnam)
  # final_plot <- image_composite(plot, logo, gravity = "southeast", offset = "-680-220")
  # image_write(final_plot, filnam)
  
}

