source('model_functions.R')

segmento<- 'co'
### Lettura file di configurazione
source(paste0('../config/',segmento,'_config_file_asis.R'))

### LETTURA MASTER
path_master <- file.path(path_master,segmento,"master_asis.csv")
master_modello <- setDT( fread(path_master))

### CALCOLO MODELLO
output_modello <- compute_model_asis(
    master_modello[,..cols_modello],
    suffix="corporate_asis",
    #folder = '../output/asis/co',
    seed = seed,
    segmento = segmento,
    perc_split = 0.80,
    desc = 'co_asis')


### DEFINIZIONE TRAIN E TEST
rows <- output_modello$rows
modello <- output_modello$model

train <- master_modello[,..cols_modello][rows]
test <- master_modello[,..cols_modello][-rows]



### PREDICT SUL TEST SET E COSTRUZIONE DELLE TABELLE CON SCORE E NUMERICHE
pred_mod<- predict(modello,test)
pred_mod<- as.data.table(pred_mod)

test$pred_vpc <- as.numeric(pred_mod$DEFAULT)


foglie <- as.data.table(table(test[,c('pred_vpc','target')]))
foglie <- dcast(foglie, formula = pred_vpc ~target)
foglie[, prob := DEFAULT /(OK+DEFAULT) ]
foglie[, tot := OK+DEFAULT ]


### PLOT
rosso <-   adjustcolor( "red", alpha.f = 0.5)
verde <-   adjustcolor( "darkolivegreen3", alpha.f = 0.6)
giallo <-   adjustcolor( "gold", alpha.f = 0.5)
options(repr.plot.width=8, repr.plot.height=7)
mainfolder <- '../output/asis'
subfolder <- segmento
if(!file.exists(mainfolder)){
    dir.create(mainfolder)
    dir.create(file.path(mainfolder,subfolder))
}else if(!file.exists(file.path(mainfolder,subfolder))){
    dir.create(file.path(mainfolder,subfolder))
}
path_name <- create_path(file.path(mainfolder,subfolder))
pdf(file.path(path_name$path,paste0("plot_",segmento,suffix,'.pdf')))
prp(modello, split.fun=split.fun, yesno=1, extra = 105,type = 1, varlen = 0,#box.palette="RdGn",
       box.col=c(verde,giallo,rosso)[findInterval(as.data.table(modello$frame)$yval2.V4, v = c(0,soglia_vg,soglia_gr))])
dev.off()


### PMML
pmod2 <- pmml(modello, model_name = "VPC Tree Model",
    app_name = NULL,
    description = NULL,
    copyright = NULL)

    saveXML(pmod2, file=file.path(path_name$path,paste0("tree_",segmento,suffix,'.xml')))


### PERFORMANCE
plot_roc(test, segmento,path_name,suffix = suffix)
auc_test <- check_overfit(test,modello)
auc_train <- check_overfit(train,modello)
#cat('\n auc train', auc_train)
#cat('\n auc test', auc_test)
if(auc_train - auc_test >5){
    print("controlla overfitting")
}else{
    print('no overfitting')
}
feature_imp(modello,segmento,suffix,path_name)
print('calcolo performance')
table_performance <- compute_tab_perf(modello, test,foglie,master_modello,dt_orig)$perf
table_results <- compute_tab_perf(modello, test,foglie,master_modello,dt_orig)$results
write.xlsx(table_performance, file.path(path_name$path,paste0("performance_",segmento,suffix,'.xlsx')), rowNames= FALSE)
write.xlsx(table_results, file.path(path_name$path,paste0("results_",segmento,suffix,'.xlsx')), rowNames= FALSE)
write.csv(train, file.path(path_name$path,paste0("train_",segmento,suffix,'.csv')), row.names= FALSE)
write.csv(test, file.path(path_name$path,paste0("test_",segmento,suffix,'.csv')), row.names= FALSE)
