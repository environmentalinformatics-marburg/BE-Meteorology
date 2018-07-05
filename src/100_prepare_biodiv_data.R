source("C:/Users/tnauss/permanent/plygrnd/exploratorien/BE-Meteorology/src/000_set_environment.R")

#### Prepare forest species dataset
df_vegrecordsforest = readBExpVegRecordsForest(paste0(path_releves, "20366_Vegetation_Records_for_Forest_EPs_2009_2016_1.4.5.txt"))

# Herbaceous layer
df_h = df_vegrecordsforest[df_vegrecordsforest$Layer == "H", ]
head(df_h)

forest_diversity = lapply(c("H", "S"), function(l){
  divyear = lapply(unique(df_h$Year), function(y){
    
    act = df_vegrecordsforest[df_vegrecordsforest$Layer == l &
                          df_vegrecordsforest$Year == y, ]
    
    actm = acast(act, EPID ~ Species, value.var='Cover', FUN=mean)
    shannon = diversity(actm, index = "shannon")
    eveness = shannon/log(specnumber(actm))
    
    shannon = as.data.frame(shannon)
    shannon$EPID = rownames(shannon)
    
    eveness =as.data.frame(eveness)
    eveness$EPID = rownames(eveness)
    
    out = aggregate(act$Cover, by=list(act$EPID), FUN = sum)
    names(out) = c("EPID", "total.cover.cum")
    
    out = merge(out, shannon, by = "EPID")
    out = merge(out, eveness, by = "EPID")
    out$Year = y
    out$Layer = l
    return(out)
  })
  divyear = do.call("rbind", divyear)
})
forest_diversity = do.call("rbind", forest_diversity)
saveRDS(forest_diversity, paste0(path_rdata, "/forest_diversity.rds"))
