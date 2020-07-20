# Frequency analysis
library(data.table)
path <- 'Results/CDLfrequencies'
csv.names <- Sys.glob(file.path(path, '*csv'))
csv.names <- csv.names[!grepl("Tot", csv.names)]
dt.list <- lapply(csv.names, fread, integer64 = 'double')
dt.list <- mapply(function(dt,y)dt[,year:=y], dt = dt.list, y = 2008:2017, SIMPLIFY = F)
dt.list <- lapply(dt.list, function(dt){names(dt)<- tolower(names(dt)); return(dt)})
dt.list <- lapply(dt.list, function(dt)dt[,c('%tot', '%ag') := NULL])

dt.bind <- rbindlist(dt.list, fill = T)

# sum frequencies
dt <- dt.bind[, lapply(.SD,sum), by= list(class, code), .SDcols = "freq"]
setorder(dt, -freq)

# crop code
cropcode <- read.csv('CropCode.csv')
head(cropcode)
cropcode <- cropcode[c('VALUE', 'ID','CLASS_NAME')]

# merge with crop code
setDF(dt)
df <- merge(dt, cropcode, by.x = 'code', by.y = 'VALUE', all.y = F)


# fixing name in rep codes
df[df$ID == 1, 'class'] <- 'Corn'
df[df$ID == 5, 'class'] <- 'Wheat'
df[df$ID == 41,'class'] <- 'Canola'
df[df$ID == 32,'class'] <- 'Peaches-Nectarines'
df[df$ID == 46,'class'] <- 'Brassica oleracea'
df[df$ID == 53,'class'] <- 'Cantaloupes'
df[df$ID == 55,'class'] <- 'Squash'
df$class <- sub('/','-',df$class)

df['CLASS_NAME'] <- NULL
df['code'] <- NULL
setDT(df)
df <- df[,list(ID = ID[1], freq = sum(freq)), by = class]
df

df$perc.tot <- (df$freq/sum(df$freq))*100
df$perc.ag <- (df$freq/sum(df$freq[df$ID!=0]))*100
df$perc.ag[df$ID==0] <- NA


df <- df[order(df$perc.ag, decreasing = T),]
df

write.csv(df, file.path(path, 'CDLfreq_Tot.csv'))

          