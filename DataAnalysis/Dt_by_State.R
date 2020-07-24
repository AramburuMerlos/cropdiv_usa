library(raster)
library(data.table)
source("Functions/CrossTab.R")

Dt <- raster("D:/TmpDivData/TmpDiv.tif")
states <- raster("D:/USrasterized/StatesFP_30m.tif")
d <- CrossTab(states, Dt) 
fwrite(d, "Results/Dt_by_State.csv")
d <- fread("Results/Dt_by_State.csv")

d[,prop:= freq/sum(freq), by = states]

# states id and names
states_pol <- tigris::states()
st_id <- data.table(states = as.numeric(states_pol$STATEFP), 
                    State = states_pol$NAME)
# add State names
d <- st_id[d, on = .NATURAL]

# reorder 
setorder(d, State, Dt)
# cumulative probabilities
d[,cumprop:= cumsum(prop), by = State]

# calculate summary stats and create out table
myquart <- function(x,cp,q) x[which.max(cp >= q)]
ot <- d[,.(mc= prop[Dt == 1], 
           q1= myquart(Dt,cumprop,0.25),
           q2= myquart(Dt,cumprop,0.5),
           mean= sum(Dt*prop),
           q3= myquart(Dt,cumprop,0.75)), by = State ]
# Remove District of Columbia and NA
ot <- ot[State != "District of Columbia" & !is.na(State),]

ot[,mc:=round(mc,2)*100]
for(j in c('q1','q2', 'mean', 'q3')) set(ot, j=j, value = round(ot[, j, with = F], 1))
write.csv(ot, 'Results/Dt_by_State_Table.csv', row.names = F)

