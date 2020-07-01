library(raster)
path <- "D:/USrasterized"
dir.create(path)

# rasters to use as model
r30m <- raster("D:/TmpDivData/TmpDiv.tif")
r1320m <- raster("D:/GammaDiversity/Dg_0001320m_Mean.tif")

# keep only conterminous US (remove States by FIPS codes)
rmst <- formatC(c(02, 15, 60, 66, 69, 72, 78), width = 2, format = 'd', flag = 0)

# states ----
states <- tigris::states()
states <- states[!states$STATEFP %in% rmst,]

stt <- sp::spTransform(states, crs(r30m))
stt$STATEFP <- as.numeric(stt$STATEFP)

rasterize(stt, r30m, field = "STATEFP", 
          filename = file.path(path, "StatesFP_30m.tif"),
          options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
          datatype="INT1U", progress = 'text')


## County ----
counties <- tigris::counties()
counties <- counties[!counties$STATEFP %in% rmst,]
ct <- sp::spTransform(counties, crs(r30m))
ct$GEOID <- as.numeric(ct$GEOID)

rasterize(ct, r30m, field = 'GEOID',
          filename = file.path(path, "CountiesGEOID_30m.tif"),
          options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
          datatype = "INT2U", progress = "text")

rasterize(ct, r1320m, field = 'GEOID',
          filename = file.path(path, "CountiesGEOID_1320m.tif"),
          options = c("COMPRESS=Deflate","PREDICTOR=1","ZLEVEL=6"),
          datatype = "INT2U", progress = "text")

