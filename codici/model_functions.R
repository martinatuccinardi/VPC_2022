require(xlsx)
library(readxl)
library(readxlsb)
library(data.table)
library(rpart)
library(rpart.plot)
library(readxl)
library(stringr)
library(pmml)
library(partykit)
library(ROCR)

create_path <- function(mainfolder){
    subfolder <- paste0(gsub('-',"",Sys.Date()))
    if(!file.exists(mainfolder)){
        dir.create(mainfolder)
        dir.create(file.path(mainfolder,subfolder))
    }else if (!file.exists(file.path(mainfolder,subfolder))){
        dir.create(file.path(mainfolder,subfolder))
    }else
        NULL
    path<- file.path(mainfolder,subfolder)
    index <- max(length(list.files(path)[grepl('xml',list.files(path))]),length(list.files(path)[grepl('pdf',list.files(path))]))
    output<- list(path, (index+1))
    names(output)<- c('path','index')
    return(output)
}

clean_data<- function(master){

    colnames(master)<-tolower(colnames(master))
    colnames(master)<- gsub("\\s+","_",gsub('[^a-zA-Z0-9]', " ",colnames(master)))

    ## estraggo alcuni dati presenti nelle colonne ma aggregati in stringhe, al momento dell'utilizzo tali variabili saranno input semplici
    ## Rating estratto da input - rating scaduto

    master[, rating := gsub(".*RATING:", "\\1",input_rating_scaduto )]
    master[,input_rating_scaduto:= NULL]
    master$target <- factor(master$target, levels = c('OK',"DEFAULT"))

    ## Causale negativa

    master[, appl_causale_negativa := gsub(".*CAUS:(.+)N.*", "\\1",input_applicazione_con_causale )]
    master[,input_applicazione_con_causale:= NULL]

    master[,input_bilanci_passivo_corrent := gsub("[^a-zA-Z0-9,]"," ",input_bilanci_passivo_corrent)]
    master[,input_bilanci_passivo_corrent := gsub("\\s+"," ",input_bilanci_passivo_corrent)]

    master[,bil_passivo := gsub(".*PASSIVO (.+) FATTUR.*", "\\1",input_bilanci_passivo_corrent )]
    master[,bil_fatturato := gsub(".*FATTUR", "\\1",input_bilanci_passivo_corrent )]

    master[,bil_fatturato := as.numeric(gsub(",",".",bil_fatturato))]
    master[,bil_passivo := as.numeric(gsub(",",".",bil_passivo))]

    master[,input_bilanci_passivo_corrent:= NULL]


    #master[,data_delibera_o_annull_fido:= as.Date(data_delibera_o_annull_fido)]
    master[,esito_rating_scaduto:=  as.numeric(as.Date(esito_rating_scaduto))]
    master[,esito_data_costituzione:=  as.numeric(as.Date(esito_data_costituzione))]
    #master[,mesi_scaduto:= esito_rating_scaduto - data_delibera_o_annull_fido]

    #elimino le colonne con tutti NA
    master<- master[,which(unlist(lapply(master, function(x)!all(is.na(x))))),with=F]

    #trasformo in factor le colonne non numeriche
    master <- cbind.data.frame(lapply(master, function(x) {
      if (all(unique(x) %in% c(0,1)))
        as.factor(c("NO", "YES")[x+1])
      else if (all(is.character(x)))
        as.factor(x)
      else
        x
    }))
    setDT(master)

    master[,numero_tot_fidi := as.numeric(numero_tot_fidi)]
    master[,count_fidi_in_conferma := as.numeric(count_fidi_in_conferma)]
    master[,sum_import_fidi_concessione := as.numeric(sum_import_fidi_concessione)]
    master[,sum_import_fidi_aumento := as.numeric(sum_import_fidi_aumento)]

    str(master)

    #salvo la master
    saveRDS(master,'../data/master_drop3.RDS')

    return(master)
}


table_soglie <- function(tree_table,soglie = c(0.03,0.04), var = 'pred_vpc'){
    lista_soglie <- list(paste(var,"<=",soglie[[1]]),
                         paste(var,"<=",soglie[[2]],"&",var,">",soglie[[1]]),
                         paste(var,">",soglie[[2]]))
    output <- rbindlist(lapply(lista_soglie,function(x){
                    tree_table[eval(parse(text = x)),
                           list(soglia = x,DEFAULT = sum(DEFAULT), OK = sum(OK),
                                tot = sum(tot), perc = round(100*sum(DEFAULT)/(sum(DEFAULT)+sum(OK)),3))]
                        })
                )
    return(output)

}

performance_oldvpc <- function(dt){
    rbindlist(lapply(c("VERDE","GIALLO","ROSSO"),function(x){
       dt_appo <- dt[ESITO_FINALE_VPC == x,
                 list(colore = x,
                      DEFAULT = nrow(dt_sample[target=='DEFAULT' &ESITO_FINALE_VPC==x ,]),
                      OK = nrow(dt_sample[target=='OK' &ESITO_FINALE_VPC==x ,]),
                      tot = nrow(dt_sample[ESITO_FINALE_VPC==x ,]))]
       dt_appo[,perc := round(100*DEFAULT/tot,3)]
       })
   )
}

crea_db_ews <- function(path){ #'../data/dati_ews/'
    lista_ews <- list.files(path)

    dati_ews <- rbindlist(lapply((lista_ews),function(file){
            path <- paste0(path,file)
            dt <- read.csv(path)
            setDT(dt)
        }),fill = TRUE
    )

    dati_ews[,SNDG_CEDUTO := f_pad_zero(COD_SNDG, width = 16)]

    master_ews <- dati_ews[,c('d_riferimento','pred_xra','pred_aris','pred_afi','pred_cashflow','pred_crsys','pred_cr0','pred_nopg','pred_regexp','pred_cartacomm','pred_prea','pred_bilancio','pred','SNDG_CEDUTO')]
    master_ews[, d_riferimento := as.Date(d_riferimento)]

    saveRDS(master_ews,'../data/dati_ews/master_ews.RDS')

}


crea_db_perimetro <- function(path){

    dtv1 <- read_excel(file.path(path,'DB_FINALE_INTEGRATO_v01.xlsx'), sheet = 'QUERY_FOR_DB_FINALE_INTEGRATO')
    setDT(dtv1)

    ## Lettura dati VPC con nuove colonne
    #excel_sheets(file.path(path,'DB_FINALE_INTEGRATO_v02.xlsx'))
    dtv2 <- read_xlsx(file.path(path,'DB_FINALE_INTEGRATO_v02.xlsx'), sheet = 'DB')
    setDT(dtv2)

    col_to_rm <- intersect(colnames(dtv2),colnames(dtv1))
    dt2 <- copy(dtv2[,c('NUM_PRATICA', 'PROGRESS_PRATICA', setdiff(colnames(dtv2),col_to_rm)),with = FALSE])

    dt <- merge(dtv1,dt2,by = c('NUM_PRATICA', 'PROGRESS_PRATICA'), all = TRUE)


    ### FIltro il DB eliminando le controparti già in default prima dell'osservazione.

    dt <- dt[`Perimetro analisi descrittive` == 1 & `Perimetro_finale_analisi TD` == 1 & `Perimetro_no_default_T0` == 1 ,]
    dt[,target :='OK']
    dt[`Perimetro_si_default_Tfinal` == 1, target := 'DEFAULT']

    saveRDS(dt,file.path(path,'DB_perimetro.RDS'))
}

compute_model <- function(master,dt_orig,flag_ews,minbuck= 0.0005, cp = 1e-4, perc_split =0.75,suffix = '',desc ="", seed = 1704){

    #creo il modello utilizzando tutte le variabili
    set.seed(seed)
    rows <- sample(1:nrow(master), perc_split*nrow(master), replace=F)
    if(!flag_ews){
        train_set <- master[rows,setdiff(colnames(master),cols_ews),with = FALSE]
        test_set <- master[-rows,setdiff(colnames(master),cols_ews),with = FALSE]

    }else{
        train_set <- master[rows,]
        test_set <- master[-rows,]
    }


    modello <- rpart(target ~.,
                    data=train_set,
                    method="class",#metodo class o anova
                    control=rpart.control(cp = cp,minbucket =minbuck*length(rows))) ##minbucket forza e foglie ad avere almeno quel numero di elementi

    path_name <- create_path('output')
    #salvo il plot
    options(repr.plot.width=8, repr.plot.height=7)
    pdf(file.path(path_name$path,paste0("tree_ews",flag_ews,suffix,"_",path_name$index,'.pdf')))
    rpart.plot(modello, type=2, fallen.leaves=F, yesno=2, extra = 107,box.palette="-RdGn")
    dev.off()

    # faccio il predict sul test set
    pred_mod<- predict(modello,test_set)
    pred_mod<- as.data.table(pred_mod)
    dt_appo_pred <- copy(test_set)
    dt_appo_pred$pred_vpc <- pred_mod$DEFAULT

    summ <- as.data.table(table(dt_appo_pred[,c('pred_vpc','target')]))
    summ <- dcast(summ, formula = pred_vpc ~target)
    summ[, prob := DEFAULT /(OK+DEFAULT) ]
    summ[, tot := OK+DEFAULT ]
    summ



    pmod2 <- pmml(modello, model_name = "VPC Tree Model",
    app_name = NULL,
    description = NULL,
    copyright = NULL)

    saveXML(pmod2, file=file.path(path_name$path,paste0("tree_ews",flag_ews,suffix,"_",path_name$index,'.xml')))

    line=paste(paste0("tree_ews",flag_ews,suffix,"_",path_name$index), " - ",as.character(Sys.time()), " - ", desc)
    write(line,file=file.path(path_name$path,"file_versioning.txt"),append=TRUE)
    output<- list(modello,dt_appo_pred,summ,rows)
    names(output)<- c('model','pred','tree_table','rows')
    return(output)
}
