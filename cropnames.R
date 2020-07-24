cropcode <- read.csv('CropCode.csv', stringsAsFactors = FALSE)

cropcode <- cropcode[c('ID','CLASS_NAME', 'A_N')]
cropcode <- cropcode[cropcode$ID!=0,]
head(cropcode)

# fixing name in rep codes
cropcode[cropcode$ID == 1,2] <- 'Maize'
cropcode[cropcode$ID == 5,2] <- 'Wheat'
cropcode[cropcode$ID == 41,2] <- 'Canola'
cropcode[cropcode$ID == 32,2] <- 'Peach/Nectarine'
cropcode[cropcode$ID == 46,2] <- 'Cabbage'
cropcode[cropcode$ID == 53,2] <- 'Cantaloupe'
cropcode[cropcode$ID == 55,2] <- 'Squash'

cropcode <- unique(cropcode)
cropcode <- cropcode[cropcode$ID<200,]
cropcode
names(cropcode) <- c("ID", "Name", "A_N")

write.csv(cropcode, "cropnames.csv", row.names = FALSE)
