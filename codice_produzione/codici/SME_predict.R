source('model_functions.R')

path_test <-'../data/test'

segmento = 'sme'
suffix = 'sme_test'

### Lettura file di configurazione
source(paste0('../config/',segmento,'_config_file_asis.R'))

## sovrascrivo il suffix
suffix = 'sme_test'
### LETTURA db test
path_test <- file.path(path_test,segmento,paste0(nome_db_test,".csv"))
test <- setDT(fread(path_test,stringsAsFactors = TRUE))
test[,DSI_tipo_pratica := as.factor(DSI_tipo_pratica) ]
test <- test[,..cols_modello]

modello <-readRDS('modelli/modello_sme_trim_asis.RDS')


### PREDICT SUL TEST SET E COSTRUZIONE DELLE TABELLE CON SCORE E NUMERICHE
pred_mod<- predict(modello,test)
pred_mod<- as.data.table(pred_mod)

test$pred_vpc <- as.numeric(pred_mod$DEFAULT)


foglie <- as.data.table(table(test[,c('pred_vpc','target')]))
foglie <- dcast(foglie, formula = pred_vpc ~target)
foglie[, prob := DEFAULT /(OK+DEFAULT) ]
foglie[, tot := OK+DEFAULT ]


mainfolder <- '../output/test'
subfolder <- segmento
if(!file.exists(mainfolder)){
    dir.create(mainfolder)
    dir.create(file.path(mainfolder,subfolder))
}else if(!file.exists(file.path(mainfolder,subfolder))){
    dir.create(file.path(mainfolder,subfolder))
}
path_name <- create_path(file.path(mainfolder,subfolder))


### PERFORMANCE
plot_roc(test, segmento,path_name,suffix = suffix)

print('calcolo performance')

dt_results <- copy(test)
dt_results[pred_vpc > soglia_vg & pred_vpc <=soglia_gr, col_new_vpc := "giallo"]
dt_results[pred_vpc <= soglia_vg, col_new_vpc := "verde"]
dt_results[pred_vpc > soglia_gr, col_new_vpc := "rosso"]

table_results <- rbindlist(lapply(c("verde","giallo","rosso"),function(x){
   dt_appo <- dt_results[col_new_vpc == x,
             list(colore = x,
                  DEFAULT = nrow(dt_results[target=='DEFAULT' &col_new_vpc==x ,]),
                  OK = nrow(dt_results[target=='OK' &col_new_vpc==x ,]),
                  tot = nrow(dt_results[col_new_vpc==x ,]))]
   dt_appo[,td := round(100*DEFAULT/tot,3)]
   dt_appo[,sample_perc := round(100*tot/nrow(dt_results),3)]
   })
)

write.xlsx(table_results, file.path(path_name$path,paste0("results_",suffix,'.xlsx')), rowNames= FALSE)
write.xlsx(dt_results, file.path(path_name$path,paste0(nome_db_test,"_pred",'.xlsx')), rowNames= FALSE)
