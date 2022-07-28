source('model_functions.R')

segmento<- 'sme'

source(paste0('../config/',segmento,'_config_file_retrain.R'))

path_master <- file.path(path_master,segmento,"master_retrain.csv")
master_modello <- setDT( fread(path_master))


master_modello <- cbind.data.frame(lapply(master_modello, function(x) {
      if (all(unique(x) %in% c(0,1)))
        as.factor(c("NO", "YES")[x+1])
      else if (all(is.character(x)))
        as.factor(x)
      else
        x
    }))
setDT(master_modello)

output_modello <- compute_model(
    master_modello[,..cols_modello],
    segmento = segmento,
    cp = cp,
    seed = seed,
    use_surr = 2,
    minbuck= minbuck,
    perc_split = perc_split,
    flag_opt = flag_opt)


rows <- output_modello$rows
modello <- output_modello$model
saveRDS(modello,file.path('modelli',paste0('modello_',segmento,'.RDS')))

train <- master_modello[ ,..cols_modello][rows]
test <- master_modello[ ,..cols_modello][-rows]



if(flag_prune){
    suffix = '_trim'
    trimmed.tree <- rpart.plot(modello, snip=TRUE,split.fun=split.fun, yesno=1, extra = 105,type = 1, varlen = 0)$obj   # manually trim the tree

    saveRDS(trimmed.tree,file.path('modelli',paste0('modello_',segmento,'_trimmed.RDS')))
    modello <- trimmed.tree

}else{
    suffix = ''
}


pred_mod<- predict(modello,test)
pred_mod<- as.data.table(pred_mod)
# dt_appo_pred <- copy(test)
# dt_appo_pred$pred_vpc <- as.numeric(pred_mod$DEFAULT)
test$pred_vpc <- as.numeric(pred_mod$DEFAULT)

#modello$pred <- dt_appo_pred

foglie <- as.data.table(table(test[,c('pred_vpc','target')]))
foglie <- dcast(foglie, formula = pred_vpc ~target)
foglie[, prob := DEFAULT /(OK+DEFAULT) ]
foglie[, tot := OK+DEFAULT ]
#modello$tree_table <- foglie

rosso <-   adjustcolor( "red", alpha.f = 0.5)
verde <-   adjustcolor( "darkolivegreen3", alpha.f = 0.6)
giallo <-   adjustcolor( "gold", alpha.f = 0.5)
options(repr.plot.width=8, repr.plot.height=7)
mainfolder <- '../output/retrain'
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

pmod2 <- pmml(modello, model_name = "VPC Tree Model",
    app_name = NULL,
    description = NULL,
    copyright = NULL)

    saveXML(pmod2, file=file.path(path_name$path,paste0("tree_",segmento,suffix,'.xml')))

plot_roc(test, segmento,path_name,suffix = suffix)
auc_test <- check_overfit(test,modello)
auc_train <- check_overfit(train,modello)
cat('\n auc train', auc_train)
cat('\n auc test', auc_test)

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
