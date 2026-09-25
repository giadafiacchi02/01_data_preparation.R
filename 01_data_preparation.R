################################################################################
# FUEL PRICES ANALYSIS 
#
#What this script does:
#1) Reads all raw CSV files with UK fuel prices
#2) Cleans and standardizes the columns (names, timestamp, prices)
#3) Builds the spatial dataset with one row per station (sf object)
#4) Filters stations for a specific region (e.g. London)
#5) Saves the results (RDS + RData) to be reused in 02_maps_visualization.R
#
#Output produced:
#- ../<Region>_raw_prices.RDS
#- .../spatial_matrices.RData (contains: s, pts_inside, sp)
################################################################################

rm(list=ls())

# Change to your own directory
workdir <- 'C:/Users/giada/OneDrive/Documenti/Desktop/FUEL PRICES'
setwd(workdir)

# Get the names of all files in the folder
fns <- list.files()

# Initialise storage
dd <- NULL

# Columns to extract
# E10 and E5 are petrol grades
# B7 is diesel: P=premium, S=standard; B10 is not widely available in the UK
cols2extract <- c('forecourt_update_timestamp',
                  'forecourts.brand_name',
                  'forecourts.trading_name',
                  'forecourts.is_motorway_service_station',
                  'forecourts.is_supermarket_service_station',
                  'forecourts.location.postcode',
                  'forecourts.location.city',
                  'forecourts.location.county',
                  'forecourts.location.latitude',
                  'forecourts.location.longitude',
                  'forecourts.fuel_price.E5',
                  'forecourts.fuel_price.E10',
                  'forecourts.fuel_price.B7S',
                  'forecourts.fuel_price.B7P',
                  'forecourts.fuel_price.B10',
                  'forecourts.amenities.vehicle_services.car_wash',
                  'forecourts.amenities.air_pump_or_screenwash',
                  'forecourts.amenities.water_filling',
                  'forecourts.amenities.twenty_four_hour_fuel',
                  'forecourts.amenities.customer_toilets'
)

# Read each raw data file to:
# (a) construct a set of unique station names
# (b) create the "mega" dataset by stacking each file on top of another
for (fs in fns) {
  # Each file is either comma-separated (default) or semi-colon separated
  d <- read.csv(fs, header=T, sep=',', quote='"', fill=TRUE)
  if (dim(d)[2]==1) d <- read.csv(fs, header=T, sep=';', quote='"', fill=TRUE)

  # Rename column if old name is used
  new <- 'forecourt_update_timestamp'
  old <- 'latest_update_timestamp'
  if (length(grep(new, colnames(d)))==0) {
    colnames(d)[which(colnames(d)==old)] <- new
  }

  # Create a unique name for each station by combining name and postcode
  nm <- paste0(d$forecourts.trading_name,
               d$forecourts.location.postcode)

  # Stack files to form the mega dataset
  dd <- rbind(dd, d[, cols2extract])
}

# Clean up the time column
# Set locale to English to correctly parse English date strings
Sys.setlocale("LC_TIME", "English")

# Now the conversion will work correctly
tt <- gsub(" \\(Coordinated Universal Time\\)", "", dd[['forecourt_update_timestamp']])
tt <- trimws(tt)

# Extract only the timestamp pattern
pattern <- "[A-Z][a-z]{2} [A-Z][a-z]{2} \\d{2} \\d{4} \\d{2}:\\d{2}:\\d{2} GMT\\+\\d{4}"
matches <- regexpr(pattern, tt)
tt_all <- ifelse(matches > 0,
                 regmatches(tt, matches),
                 NA)

# Convert
dd$time <- as.POSIXct(tt_all, format="%a %b %d %Y %H:%M:%S GMT%z", tz="UTC")

# Order by time
dd <- dd[order(dd$time), ]

# Simplify column names
dd[['trading_name']] <- dd[['forecourts.trading_name']]
dd[['postcode']]     <- dd[['forecourts.location.postcode']]
dd[['lon']]          <- as.numeric(dd[['forecourts.location.longitude']])
dd[['lat']]          <- as.numeric(dd[['forecourts.location.latitude']])
dd[['E5_org']]       <- dd[['forecourts.fuel_price.E5']]
dd[['E10_org']]      <- dd[['forecourts.fuel_price.E10']]
dd[['B7S_org']]      <- dd[['forecourts.fuel_price.B7S']]
dd[['B7P_org']]      <- dd[['forecourts.fuel_price.B7P']]
dd[['B10_org']]      <- dd[['forecourts.fuel_price.B10']]

# Deal with errors in price entry:
#   - missing decimal point (divide by 100 to convert to pence)
#   - entered in pounds instead of pence (multiply by 100)
#   - less than 1 pound/pence (set to NA)
#   - entry like 12.3 should be 123 (multiply by 10)
#   - entry > 300 (over £3) is likely an error (set to NA)
for (p in c('E5_org','E10_org','B7S_org','B7P_org','B10_org')) {
  x <- as.numeric(dd[[p]])
  x[which(x>1000)]       <- x[which(x>1000)]/100
  x[which(x<2 & x>1)]   <- x[which(x<2 & x>1)]*100
  x[which(x<=1)]         <- NA
  x[which(x<=100)]       <- x[which(x<=100)]*10
  x[which(x>300)]        <- NA
  print(paste0(' --- summary for ', p))
  print(summary(x))
  dd[[sub('_org','',p)]] <- x
}

# Create a unique station identifier by combining trading name and postcode
dd[['uniq']] <- paste0(dd[['trading_name']], dd[['postcode']])

# Get list of unique stations
uniq_stations <- unique(dd[['uniq']])

# Columns to keep in the spatial dataset
cns <- c('uniq', 'trading_name', 'postcode', 'lon', 'lat',
         'forecourts.brand_name',
         'forecourts.location.city',
         'forecourts.location.county',
         'forecourts.is_motorway_service_station',
         'forecourts.is_supermarket_service_station',
         'forecourts.amenities.vehicle_services.car_wash',
         'forecourts.amenities.air_pump_or_screenwash',
         'forecourts.amenities.water_filling',
         'forecourts.amenities.twenty_four_hour_fuel',
         'forecourts.amenities.customer_toilets',
         'E5', 'E10', 'B7S', 'B7P', 'B10',
         'time')

# CREATION SPATIAL DATASET
# Create spatial dataset with one row per station
# Select the earliest available observation for each station
spatial_data <- NULL
for (i in 1:length(uniq_stations)) {
  sdata <- dd[dd$uniq == uniq_stations[i], cns]
  spatial_data <- rbind(spatial_data,
                        sdata[order(sdata$time)[1], ])
}
sp <- spatial_data

# Remove rows with missing coordinates
sp <- sp[!is.na(sp$lon) & !is.na(sp$lat), ]

# Convert to sf spatial object
library(sf)
sp_sf <- st_as_sf(sp, coords=c('lon','lat'), crs=4326)

View(sp)

# Select stations located in the UK and display their positions
ids <- which(sp$lon < 2.5 & sp$lat > 50)

# Control plot of station locations
plot(sp$lon[ids],
     sp$lat[ids],
     pch = 19, cex = 0.2,
     xlab = "Longitude",
     ylab = "Latitude",
     main = "UK Fuel Stations")

################################################################################
# SPATIAL ANALYSIS: filter stations within a chosen region
################################################################################

# Define the region of interest (London)
which_region <- 'London'

# Load the shapefile of English regions
# A shapefile is a standard format for representing vector geographic data.
# It allows us to define the boundaries of London, identify which stations
# fall inside the region, and create correct spatial maps.
shp <- st_read(
  "C:/Users/giada/OneDrive/Documenti/Desktop/Regions_December_2023_Boundaries_EN_BFE_-4531395057735466891/RGN_DEC_2023_EN_BFE.shp"
)

if (which_region!='')
  s <- shp[which(shp$RGN23NM==which_region),]

# Conversion lon e lat in numeric
dd[['lon']] <- as.numeric(dd[['forecourts.location.longitude']])
dd[['lat']] <- as.numeric(dd[['forecourts.location.latitude']])

# Filter rows with valid coordinates
fl <- which(!is.na(dd$lon) & !is.na(dd$lat))

pts <- dd[fl, ]
sf_pts <- st_as_sf(pts, coords=c('lon','lat'), crs=4326)
pts <- st_transform(sf_pts, st_crs(s))

pts_inside <- st_intersection(pts, s)

# Plot of London with london stations
plot(st_geometry(s), border=1, col=NA)
plot(st_geometry(pts_inside), col=2, add=T, cex=0.3, pch=19)

# Save the spatial dataset as an RDS file (R native format)
# RDS files preserve the exact R object structure, including sf geometry,
# and can be reloaded quickly in future sessions without reprocessing the data

# Set default output filename (one folder up from the working directory)
f <- '../England_raw_prices.RDS'
if (which_region != '') f <- paste0('../', sub(' ','', which_region), '_raw_prices.RDS')

# Save the spatial dataset containing only stations within the selected region
saveRDS(pts_inside, file=f)

################################################################################
# SAVE ALL MATRICES (used by 02_maps_visualization.R)
################################################################################
save(s, pts_inside, sp,
     file = "C:/Users/giada/OneDrive/Documenti/Desktop/RCODES THESIS/spatial_matrices.RData")

# To reload in the next script:
# setwd("C:/Users/giada/OneDrive/Documenti/Desktop/RCODES THESIS")
# load("spatial_matrices.RData")
