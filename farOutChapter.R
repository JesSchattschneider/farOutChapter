library(dplyr)
library(sf)
library(mapview)
library(stars)
library(ncmeta)
library(stringr)

# load data
data <- st_read("data/farout_sp.csv", 
                options=c("X_POSSIBLE_NAMES=lon","Y_POSSIBLE_NAMES=lat")) %>%
  rename(ID = id)
  
# specify CRS
st_crs(data) = 4326

# transform to projected CRS
data <- st_transform(data, 2193)

# investigate data:
data <- data[,c(1:10)] # subset
data <- data %>% 
  mutate(voyage = gsub("voyage", "Voyage_", voyage))


cruises <- data %>% distinct(voyage) %>% pull(voyage)

listAll <- list()
for(cruise in cruises){
  # sample data for a given cruise/voyage
  sample <- data %>%
    filter(voyage == cruise) %>%
    mutate(id = row_number())
  # cruise = "Voyage_1"
  # select netcdfs:
  fileDirs <- list.files(normalizePath(paste0("./data/SCENZ_by trip/", cruise, "/Month/")), full.names = T)
  
  for (fileDir in fileDirs){
    print(fileDir)
    ncExample <- read_ncdf(fileDir) # read
    st_crs(ncExample) <- 2193 # define CRS
    
    # cvreate a poly using bounding box of sample points with 10km buffer
    bounding_box <- st_bbox(sample) %>% st_as_sfc() %>%
      st_as_sf() %>% st_buffer(10000)
    
    # st_crs(data) == st_crs(ncExample) # making sure we have same crs between pts and .nc
    ncCrop <- st_crop(ncExample, bounding_box)
    
    # create a list with the extracted values
    l = list()
    name = paste0(cruise, "_",str_match(basename(fileDir),  "_MO_(.*?)_")[2])
    l[[name]] <- st_extract(ncCrop, sample) %>%
      mutate(voyage = cruise, id = row_number()) %>%
      merge(., st_drop_geometry(sample), id.vars = c("voyage", "id"))
    
    # append to a single list
    listAll <- append(listAll, l)
  }
}

allDf <- bind_rows(listAll) %>%
  mutate(SST = as.numeric(as.character(SST))) %>%
  tidyr::pivot_longer(cols = c("BBP", "CHL", "SST"), names_to = "vars", values_to = "value") %>%
  tidyr::drop_na() %>%
  tidyr::pivot_wider(names_from = "vars",  values_from = c("value")) %>%
  select(-id) %>%
  st_as_sf() %>%
  filter(SST > 0)

# load one example of raster ####
mapview(allDf, zcol = "SST")


