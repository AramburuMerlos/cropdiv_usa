## function to remove and merge rows based on GROUP_DESC	COMMODITY_DESC	CLASS_DESC
# for details, see CensusCommodityNotes.R

CensusToSpp <- function(data){
  # columns
  group <- as.character(data[ ,colnames(data) == "GROUP_DESC"])
  comm <- as.character(data[ ,colnames(data) == "COMMODITY_DESC"])
  class <- as.character(data[ ,colnames(data) == "CLASS_DESC"])
  
  #### Removing positions ####
  # I will do all the removing at the end so the vectors group, comm and class still works for the data. 
  # Don't touch the data until the end!
  rm.row.pos <- c(
    #1
    which(group == "CROP TOTALS"),
    #2
    which(class == "WHEAT & BARLEY & OATS & RYE"), 
    #3
    which(comm == "COTTON" & class == "ALL CLASSES"),
    #4
    which(comm == "WHEAT" & class != "ALL CLASSES"),
    #5
    which(group == "HORTICULTURE" & comm != "CUT CHRISTMAS TREES"),
    #6,7,8
    which(comm == "HAY" | comm == "HAYLAGE" | comm == "HAY & HAYLAGE"),
    #9
    which(comm == "LETTUCE" & class != "ALL CLASSES"),
    #10
    which(comm == "MINT" & class ==	"ALL CLASSES"),
    #11
    which(comm == "ORANGES"	& class != "ALL CLASSES"),
    #12
    which(comm == "ORCHARDS" & class ==	"ALL CLASSES"),
    #13
    which(comm == "PECANS" & class != "ALL CLASSES"),
    #14
    which(comm == "SQUASH" & class != "ALL CLASSES"),
    #15
    which(comm == "SUNFLOWER"	& class != "ALL CLASSES")
    )

  
  #### Merging and Reclassifing ####
  # creating rows to be merged and adding the new rows to the data frame
  # in addition, the merged rows has to be removed
  #1. Lima Beans  
  r1 = which(comm == "BEANS" & (class == "DRY EDIBLE, LIMA" | class == "GREEN, LIMA") )
  # if there is none row with these name, don't merge/replace. If there is only one it will replace it with a different name
  if(length(r1) > 0){
    v1 = sum(data[r1,"VALUE"])
    nr1 = data[r1[1],]
    nr1$CLASS_DESC <- "LIMA BEANS"
    nr1$VALUE <- v1  
  }else{
    nr1 = NULL
  }
  #2. Common Beans
  r2 = which(comm == "BEANS" & (class == "DRY EDIBLE, (EXCL LIMA)" | class == "SNAP") )
  if(length(r2) > 0){
    v2 = sum(data[r2,"VALUE"])
    nr2 = data[r2[1],]
    nr2$CLASS_DESC <- "COMMON BEANS"
    nr2$VALUE <- v2    
  }else{
    nr2 = NULL
  }
  #3. Brassica Oleracea
  r3 = which(comm == "BRUSSELS SPROUTS" | comm == "CABBAGE" |
               comm == "BROCCOLI" | comm == "CAULIFLOWER" |
               (comm == "GREENS" & (class == "KALE" | class == "COLLARD"))) 
  if(length(r3) > 0){
    v3 = sum(data[r3,"VALUE"])
    nr3 = data[r3[1],]
    nr3$CLASS_DESC <- "BRASSICA OLERACEA"
    nr3$VALUE <- v3    
  }else{
    nr3 = NULL
  }
  #4. Mustard
  r4 = which(comm == "MUSTARD" | (comm == "GREENS" & class == "MUSTARD"))
  if(length(r4) > 0){
        v4 = sum(data[r4,"VALUE"])
    nr4 = data[r4[1],]
    nr4$CLASS_DESC <- "MUSTARD"
    nr4$VALUE <- v4    
  }else{
    nr4 = NULL
  }
  #5. Radishes
  r5 = which(comm == "RADISHES" | comm == "DAIKON")
  if(length(r5) > 0){
    v5 = sum(data[r5,"VALUE"])
    nr5 = data[r5[1],]
    nr5$CLASS_DESC <- "MUSTARD"
    nr5$VALUE <- v5    
  }else{
    nr5 = NULL
  } 
  #6. Vines
  r6 = which(comm == "GRAPES" | comm == "GRAPEFRUIT")
  if(length(r6) > 0){
    v6 = sum(data[r6,"VALUE"])
    nr6 = data[r6[1],]
    nr6$CLASS_DESC <- "VINES"
    nr6$VALUE <- v6    
  }else{
    nr6 = NULL
  } 
  #7. Wheat
  r7 = which((comm == "WHEAT" & class == "ALL CLASSES")| (comm == "GRASSES" & class == "WHEATGRASS"))
  if(length(r7) > 0){
    v7 = sum(data[r7,"VALUE"])
    nr7 = data[r7[1],]
    nr7$CLASS_DESC <- "T.AESTIVUM"
    nr7$VALUE <- v7    
  }else{
    nr7 = NULL
  }   
  #8. ALFALFA
  r8 = which((comm == "HAY" & class == "ALFALFA")| 
               (comm == "HAYLAGE" & class == "ALFALFA")|
               (comm == "LEGUMES" & class == "ALFALFA"))
  if(length(r8) > 0){
    v8 = sum(data[r8,"VALUE"])
    nr8 = data[r8[1],]
    nr8$COMMODITY_DESC <- "ALFALFA"
    nr8$VALUE <- v8    
  }else{
    nr8 = NULL
  }   
  #9. CUCUMIS MELO
  r9 = which((comm == "MELONS" & class == "CANTALOUP") | (comm == "MELONS" & class == "HONEYDEW"))
  if(length(r9) > 0){
    v9 = sum(data[r9,"VALUE"])
    nr9 = data[r9[1],]
    nr9$CLASS_DESC <- "CUCUMIS MELO"
    nr9$VALUE <- v9    
  }else{
    nr9 = NULL
  }
  #10. PRUNUS PERSICA 
  r10 = which(comm == "NECTARINES" | comm == "PEACHES")
  if(length(r10) > 0){
    v10 = sum(data[r10,"VALUE"])
    nr10 = data[r10[1],]
    nr10$CLASS_DESC <- "PRUNUS PERSICA"
    nr10$VALUE <- v10    
  } else{
    nr10 = NULL
  } 
  #11. ONIONS
  r11 = which((comm == "ONIONS" & class == "DRY") | (comm == "ONIONS" & class == "GREEN"))
  if(length(r11) > 0){
    v11 = sum(data[r11,"VALUE"])
    nr11 = data[r11[1],]
    nr11$CLASS_DESC <- "ALLIUM CEPA"
    nr11$VALUE <- v11    
  }else{
    nr11 = NULL
  }
  #12. PEAS
  r12 = which((comm == "PEAS" & class == "AUSTRIAN WINTER") | 
                (comm == "PEAS" & class == "DRY EDIBLE") |
                (comm == "PEAS" & class == "CHINESE (SUGAR & SNOW)") |
                (comm == "PEAS" & class == "GREEN, (EXCL SOUTHERN)"))
  if(length(r12) > 0){
    v12 = sum(data[r12,"VALUE"])
    nr12 = data[r12[1],]
    nr12$CLASS_DESC <- "PISUM SATIVUM"
    nr12$VALUE <- v12    
  }else{
    nr12 = NULL
  }  
  #13. COWPEAS
  r13 = which((comm == "PEAS" & class == "DRY, SOUTHERN (COWPEAS)") | 
                (comm == "PEAS" & class == "GREEN, SOUTHERN (COWPEAS)"))
  if(length(r13) > 0){
    v13 = sum(data[r13,"VALUE"])
    nr13 = data[r13[1],]
    nr13$CLASS_DESC <- "COWPEAS"
    nr13$VALUE <- v13    
  }else{
    nr13 = NULL
  }
  #14. PEPPERS
  r14 = which(comm == "PEPPERS")
  if(length(r14) > 0){
    v14 = sum(data[r14,"VALUE"])
    nr14 = data[r14[1],]
    nr14$CLASS_DESC <- "ALL CLASSES"
    nr14$VALUE <- v14    
  }else{
    nr14 = NULL
  }
  #15. ZEA MAYS
  r15 = which(comm == "CORN" | comm == "POPCORN" | comm == "SWEET CORN")
  if(length(r15) > 0){
    v15 = sum(data[r15,"VALUE"])
    nr15 = data[r15[1],]
    nr15$CLASS_DESC <- "ZEA MAYS"
    nr15$VALUE <- v15    
  }else{
    nr15 = NULL
  }
  #16. CUCURBITA PEPO
  r16 = which(comm == "PUMPKINS" | (comm == "SQUASH" & class == "ALL CLASSES"))
  if(length(r16) > 0){
    v16 = sum(data[r16,"VALUE"])
    nr16 = data[r16[1],]
    nr16$CLASS_DESC <- "CUCURBITA PEPO"
    nr16$VALUE <- v16    
  }else{
    nr16 = NULL
  }
  #17. BRASSICA NAPUS
  r17 = which(comm == "RAPESEED" | comm == "CANOLA")
  if(length(r17) > 0){
    v17 = sum(data[r17,"VALUE"])
    nr17 = data[r17[1],]
    nr17$CLASS_DESC <- "BRASSICA NAPUS"
    nr17$VALUE <- v17    
  }else{
    nr17 = NULL
  }
    
  # extending the list of rows that must be removed. 
  rm.row.pos <- c(rm.row.pos, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17)
  # removing specific rows
  out <- data[-rm.row.pos,]  
  # adding the new rows
  out <- rbind(out, nr1, nr2, nr3, nr4, nr5, nr6, nr7, nr8, nr9, nr10, nr11, nr12, nr13, nr14, nr15, nr16, nr17)
  
  return(out)
}
