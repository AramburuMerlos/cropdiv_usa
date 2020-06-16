# Download USDA-NASS Crop Data Layers from the web ----

# years to download 
yy <- formatC(08:17, width = 2, format = "d", flag = "0")
# url (checked on 6/16/2020)
urls <- paste0("ftp://ftp.nass.usda.gov/download/res/20", yy, "_30m_cdls.zip")
# create directory in D: (outside Rproject)
dir.create("D:/CDL")
# file names. 
fns <- paste0("D:/CDL/20", yy, "_30m_cdls.zip")

# download, unzip and delete zip file. 
for(y in 1:length(yy)){
  download.file(urls[y], fns[y])
  unzip(fns[y])
  file.remove(fns[y])
}

