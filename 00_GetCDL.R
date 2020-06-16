# Download USDA-NASS Crop Data Layers from the web ----

# years to download 
yy <- formatC(08:17, width = 2, format = "d", flag = "0")
# url (checked on 6/16/2020)
urls <- paste0("ftp://ftp.nass.usda.gov/download/res/20", yy, "_30m_cdls.zip")
# create directory in D: (outside Rproject)
path <- "D:/CDL"
dir.create(path)
# file names. 
fns <- file.path(path,paste0(20, yy, "_30m_cdls.zip"))

# download and unzip file.
ny <- length(yy) 
for(y in 1:ny) download.file(urls[y], fns[y])
for(y in 1:ny) unzip(fns[y], exdir = path)


