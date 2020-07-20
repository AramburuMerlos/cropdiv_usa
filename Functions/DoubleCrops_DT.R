dc_dt <- function(DT, m){
  DT_dc <- DT[cropID >200,]
  DT_dc <- DT_dc[, cropID:= m[match(cropID,m[,1]), 3]]
  DT[, cropID:= ifelse(cropID > 200, m[match(cropID,m[,1]), 2], cropID)]
  DT_ndc <- rbind(DT, DT_dc)
  DT_out <- DT_ndc[, .(freq = sum(freq)), by = .(GEOID, cropID)]
  return(DT_out)
}
