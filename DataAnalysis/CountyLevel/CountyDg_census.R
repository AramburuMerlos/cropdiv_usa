# Tidy census data and compute Crop Species diversity by county
library(data.table)
library(sp)
library(raster)

source("Functions/Diversity.R")

cpath <- "D:/CensusData"
dir.create(cpath)
# 2012 census data downloaded from:
# url <- "ftp://ftp.nass.usda.gov/quickstats/qs.census2012.txt.gz"
# download.file(file.path(cpath, url))
# the file needs to be decompressed (done outside R)
d <- fread(file.path(cpath,'qs.census2012.txt'), sep = '\t', header = T)
str(d)

# Inital clean up -------
# keep only crop data
d <- d[SECTOR_DESC == 'CROPS',]
# keep only area units
d <- d[UNIT_DESC == "ACRES",]

# remove useless columns
names(d)
crm <- c("SOURCE_DESC",
         "SECTOR_DESC",
         "ASD_CODE",
         "ASD_DESC",
         "REGION_DESC",
         "ZIP_5",
         "WATERSHED_CODE",
         "WATERSHED_DESC",
         "CONGR_DISTRICT_CODE",
         "COUNTRY_CODE",
         "COUNTRY_NAME",
         "LOCATION_DESC",
         "YEAR",
         "FREQ_DESC",
         "BEGIN_CODE",
         "END_CODE",
         "REFERENCE_PERIOD_DESC",
         "WEEK_ENDING",
         "LOAD_TIME",
         "CV_%")
d[, (crm) := NULL]

# keep only county data
d <- d[AGG_LEVEL_DESC == "COUNTY",] 
# remove TOTALS from COMMODITY_DESC
d <- d[!grepl("TOTALS", d$COMMODITY_DESC),]
# remove single level columns and short description
sapply(d, function(cl)length(unique(cl)))
crm2 <- c("UNIT_DESC","SHORT_DESC","DOMAIN_DESC", 
          "DOMAINCAT_DESC", "AGG_LEVEL_DESC")
d[, (crm2) := NULL]

# Manage (D) and (Z) and change VALUE to numeric
## I will change D to 1 and Z to 0.5 
d[VALUE == "(D)", VALUE := "1"]
d[VALUE == "(Z)", VALUE := "0.5"]
# delete commas and change to numeric
d[, VALUE := as.numeric(gsub(",","",d$VALUE))]
str(d)

# create geo.id column that matches the map of US county 
d[, geo.id := paste0(formatC(STATE_FIPS_CODE, width = 2, format = "d", flag = "0"),
                     formatC(COUNTY_CODE, width = 3, format = "d", flag = "0"))]

# remove all other state/county ids
crm3 <- names(d)[grepl("COUNTY",names(d)) | grepl("STATE", names(d))]
d[, (crm3) := NULL]
head(d)

# Aggregate by group ------
# function to sum or keep only max when there is a total row (not all crops have one)
mors <- function(x) if(abs(max(x) - sum(x[x != max(x)])) > 5) sum(x) else max(x)

# 1) STATISTICCAT_DESC
unique(d$STATISTICCAT_DESC) 
d <- d[, .(VALUE = mors(VALUE)), 
       by = c(names(d)[! names(d) %in% c("STATISTICCAT_DESC", "VALUE")])]

# 2) UTIL_PRACTICE_DESC
unique(d$UTIL_PRACTICE_DESC)
d <- d[, .(VALUE = mors(VALUE)), 
       by = c(names(d)[!names(d) %in% c("UTIL_PRACTICE_DESC", "VALUE")])]

# 3) PRODN_PRACTICE_DESC
unique(d$PRODN_PRACTICE_DESC)
d <- d[, .(VALUE = max(VALUE)),  # only max. Categories aren't exhaustive 
       by = c(names(d)[! names(d) %in% c("PRODN_PRACTICE_DESC", "VALUE")])]

# CLASS_DESC is necessary to identify some species commodities
str(d)
# create reclassification data frame outside R
crop_cat <- d[, .(VALUE = sum(VALUE)), by = .(GROUP_DESC, 
                                              COMMODITY_DESC, 
                                              CLASS_DESC)]
fwrite(crop_cat, "DataAnalysis/CountyLevel/CensusCropCatRaw.csv")
rc <- fread("DataAnalysis/CountyLevel/CensusReclassification.csv")
rc[, VALUE:= NULL]

# join (merge) reclassification data.table and merge same species categories
d <- d[rc, on = .NATURAL]
# remove rows torm
d <- d[CROP_SP != "torm",]

# remove other crop category columns and reorder cols
crm4 <- c("GROUP_DESC", "COMMODITY_DESC", "CLASS_DESC", "IN_CDL")
d[, (crm4) := NULL]
setcolorder(d, c("geo.id", "CROP_SP", "CDL_CAT", "VALUE"))


# county diversity considering all species in Census
d_allsp <- d[, .(VALUE = sum(VALUE)), by = .(geo.id, CROP_SP)]
countydiv_allsp <- d_allsp[, .(Dg_allSP = diver(VALUE), 
                               CropArea_acres = sum(VALUE)), 
                           by = geo.id]

# county diversity considering only CDL categories
d_cdlcat <- d[, .(VALUE = sum(VALUE)), by = .(geo.id, CDL_CAT)]
countydiv_cdlcat <- d_cdlcat[, .(Dg_CDLcat = diver(VALUE)), by = geo.id]

countydiv <- countydiv_allsp[countydiv_cdlcat, on = .NATURAL]

# US county sp ----
counties <- tigris::counties()
# keep only conterminous US
rmst <- formatC(c(02, 15, 60, 66, 69, 72, 78), width = 2, format = 'd', flag = 0)
counties <- counties[!counties$STATEFP %in% rmst,]
# project crs and estimate total area 
aea_proj <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
counties_aea <- spTransform(counties, CRS(aea_proj))

counties_aea$total_area <- raster::area(counties_aea)

countyarea <- counties_aea@data[c("GEOID", "total_area")]
setDT(countyarea)

# join (or merge)
countydiv <- countydiv[countyarea, on = .(geo.id = GEOID)]

countydiv[, CropArea_prop:= (CropArea_acres * 4046.8564224) / total_area]

fn <- "Results/CountyDg_2012census.csv"
fwrite(countydiv, file = fn)

