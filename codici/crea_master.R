source('model_functions.R')


#crea_db_perimetro('../data')

#crea_db_ews('../data/dati_ews/')



###### CREAZIONE MASTER ######

#lettura dati con perimetro corretto
dt_orig <- readRDS('../data/DB_perimetro.RDS')

#lettura dati ews
master_ews <- readRDS('../data/dati_ews/master_ews.RDS')

pdo_ews <- as.Date(sort(unique(master_ews$d_riferimento)))
cols_ews <- colnames(master_ews)

#lettura colonne da utilizzare
source('cols_tokeep.R')

## colonne che hanno un solo valore
col_1_val <- lapply(colnames(dt_orig),function(x){
    if(length(unique(unlist(dt_orig[, eval(x), with = FALSE])))<2){
        return(x)
    }else return(NULL)
})

col_1_val <- unlist(col_1_val)

dt <- copy(dt_orig)

dt[, d_riferimento := pdo_ews[[1]]]
dt[, pdo_int := findInterval(as.Date(DATA_DELIBERA_O_ANNULL_FIDO),pdo_ews)]
dt[pdo_int >0, d_riferimento := pdo_ews[pdo_int]]
dt[, pdo_int := NULL]
dt[, d_riferimento := as.Date(d_riferimento)]

cols_anagrafica <- c("NUM_PRATICA",'DATA_DELIBERA_O_ANNULL_FIDO',
"PROGRESS_PRATICA",
"STATO_PRATICA",
"SNDG_CEDUTO",'ID_CEDENTE','d_riferimento')

cols_master <- setdiff(c(cols_tokeep,cols_ews),cols_anagrafica)
cols_master <- setdiff(cols_master,col_1_val)

master_drop3 <- merge(dt, master_ews, all.x = TRUE, by = c("SNDG_CEDUTO",'d_riferimento'))[,..cols_master]

master_drop3 <- clean_data(master_drop3)

saveRDS(master_drop3, '../data/master_drop3.RDS')


####### FINE CREA MASTER #########
