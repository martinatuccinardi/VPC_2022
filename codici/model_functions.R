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


#master_ews <- readRDS('../data/dati_ews/master_ews.RDS')
dt_orig <- readRDS('~/Projects/VPC/VPC_2022/data/DB_perimetro.RDS')

#cols_ews <- colnames(master_ews)

split.fun <- function(x, labs, digits, varlen, faclen)
    {
        # replace commas with spaces (needed for strwrap)
        labs <- gsub(",", " ", labs)
        for(i in 1:length(labs)) {
            # split labs[i] into multiple lines
            labs[i] <- paste("\n \n \n \n",paste(strwrap(labs[i], width=40), collapse="\n"),"\n \n")
        }
        labs
    }
    
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


    #master$target <- factor(master$target, levels = c('OK',"DEFAULT"))

    master[,sab :=input_stato_sab]
    master[is.na(sab), sab := "non censito"]
    master[,input_stato_sab:= NULL]

    master[, col_presenza_pratica_pef := colore_presenza_pratica_in_pef]
    master[, col_fido_con_causale_negativa := colore_fido_con_causale_negati]
    master[, col_appl_con_causale_negativa := colore_applicazione_con_causal]

    master[,data_scadenza_rating := as.Date(esito_rating_scaduto)]
    master[,esito_rating_scaduto:= NULL]

    # master[, classe_rating := gsub(".*RATING:", "\\1",input_rating_scaduto )]
    # master[,input_rating_scaduto:= NULL]

    master[,parte_correlata := gsub("[^0-9]", "",input_parte_correlata )]
    master[ parte_correlata =='',parte_correlata := 'missing']
    master[ is.na(colore_parte_correlata),parte_correlata := 'non censito']

    # master[,flag_parte_correlata := colore_parte_correlata ]
    # # master[flag_parte_correlata == "VERDE",flag_parte_correlata := FALSE ]
    # # master[flag_parte_correlata == "GIALLO",flag_parte_correlata := TRUE ]
    # # master[flag_parte_correlata == "ND",flag_parte_correlata := NA ]
    # master[is.na(flag_parte_correlata),flag_parte_correlata := 'non censito' ]
    # master[,colore_parte_correlata:= NULL]

    master[,rapporti_in_osservazione := esito_rapporti_in_osservazione]
    master[is.na(rapporti_in_osservazione), rapporti_in_osservazione := -100]
    master[,esito_rapporti_in_osservazione:= NULL]

    master[, causale_negativa_fido := gsub(".*CAUS:(.+)STATO.*", "\\1",input_fido_con_causale_negativ )]
    master[is.na(causale_negativa_fido), causale_negativa_fido := "non censito"]
    master[,input_fido_con_causale_negativ:= NULL]

    master[, importo_approvato :=
           as.numeric(gsub(",","\\.",gsub(".*APPROVATO: (.+) APPL.*", "\\1",gsub("\\s+"," ",input_approvato))) )]
    master[is.na(colore_approvato), importo_approvato := -1000]

    master[, importo_applicazione :=
           as.numeric(gsub(",","\\.",gsub(".*RIC: (.+) DA.*", "\\1",gsub("\\s+"," ",input_approvato))) )]
    master[is.na(colore_approvato), importo_applicazione := -1000]

    master[, diff_appr_appl := importo_applicazione - importo_approvato ]

    master[,input_approvato:= NULL]
    master[,importo_approvato:= NULL]
    master[,importo_applicazione:= NULL]

    master[,num_fidi_scaduti := esito_fido_scaduto]
    master[colore_fido_scaduto == 'VERDE',num_fidi_scaduti:= 0 ]
    master[is.na(num_fidi_scaduti), num_fidi_scaduti := -100]

    master[,flag_fidi_scaduti := colore_fido_scaduto]
    master[flag_fidi_scaduti == "VERDE",flag_fidi_scaduti := FALSE ]
    master[flag_fidi_scaduti == "GIALLO",flag_fidi_scaduti := TRUE ]
    master[is.na(flag_fidi_scaduti), flag_fidi_scaduti := "non censito"]

    master[,esito_fido_scaduto:= NULL]


    master[,autonomia_fido := esito_autonomia_fido]
    #master[is.na(colore_autonomia_fido),autonomia_fido := "non censito"]
    master[,esito_autonomia_fido:= NULL]

    master[, importo_fido_richiesto :=
           as.numeric(gsub(",","\\.",gsub(".*RIS: (.+) APUT.*", "\\1",gsub("\\s+"," ",input_limiti_societari))) )]
    master[,input_limiti_societari:= NULL]

    master[,flag_fido_ridotto := colore_fido_ridotto ]
    master[flag_fido_ridotto == "VERDE",flag_fido_ridotto := FALSE ]
    master[flag_fido_ridotto == "GIALLO",flag_fido_ridotto := TRUE ]
    master[,colore_fido_ridotto:= NULL]

    master[, causale_negativa_appl := gsub(".*CAUS:(.+)N.*", "\\1",input_applicazione_con_causale )]
    master[is.na(causale_negativa_appl), causale_negativa_appl := "non censito"]
    master[,input_applicazione_con_causale:= NULL]

    # master[, tot_outstanding :=
    #        as.numeric(gsub(",","\\.",gsub(".*OUTS: (.+)", "\\1",gsub("\\s+"," ",input_scaduto_20_outstanding))) )]

    # master[, tot_scaduto :=
    #        as.numeric(gsub(",","\\.",gsub(".*SCAD: (.+) TOT.*", "\\1",gsub("\\s+"," ",input_scaduto_20_outstanding))) )]

    master[,flag_scaduto_20_outstanding := colore_scaduto_20_outstandin ]
    master[flag_scaduto_20_outstanding == "VERDE",flag_scaduto_20_outstanding := FALSE ]
    master[flag_scaduto_20_outstanding == "GIALLO",flag_scaduto_20_outstanding := TRUE ]
    master[is.na(flag_scaduto_20_outstanding), flag_scaduto_20_outstanding := "non censito"]

    master[, flag_presenza_sofferenze := colore_cr_presenza_sofferenze]
    master[is.na(flag_presenza_sofferenze), flag_presenza_sofferenze := 'non censito']

    master[, flag_cr_accordato_utilizzato := colore_cr_utilizzato_accord]
    master[is.na(flag_cr_accordato_utilizzato), flag_cr_accordato_utilizzato := 'non censito']

    master[, flag_cr_presenza_sconfinamenti := colore_cr_presenza_sconfiname]
    master[is.na(flag_cr_presenza_sconfinamenti), flag_cr_presenza_sconfinamenti := 'non censito']

    #
    # master[,flag_cr_accordato_utilizzato := colore_cr_utilizzato_accord ]
    # master[flag_cr_accordato_utilizzato == "VERDE",flag_cr_accordato_utilizzato := FALSE ]
    # master[flag_cr_accordato_utilizzato == "ROSSO",flag_cr_accordato_utilizzato := TRUE ]
    # master[flag_cr_accordato_utilizzato == "ND",flag_cr_accordato_utilizzato := NA ]
    #
    # master[,flag_cr_presenza_sconfinamenti := colore_cr_presenza_sconfiname ]
    # master[flag_cr_presenza_sconfinamenti == "VERDE",flag_cr_presenza_sconfinamenti := FALSE ]
    # master[flag_cr_presenza_sconfinamenti == "ROSSO",flag_cr_presenza_sconfinamenti := TRUE ]
    # master[flag_cr_presenza_sconfinamenti == "ND",flag_cr_presenza_sconfinamenti := NA ]

    master[,flag_patrimonio_netto_neg := colore_bilanci_patrimonio_net ]
    master[flag_patrimonio_netto_neg == "VERDE",flag_patrimonio_netto_neg := FALSE ]
    master[flag_patrimonio_netto_neg == "ROSSO",flag_patrimonio_netto_neg := TRUE ]
    master[flag_patrimonio_netto_neg == "ND",flag_patrimonio_netto_neg := NA ]


    master[,flag_debiti_bancari_abreve_fatturato := colore_bilanci_debiti_bancari ]
    master[flag_debiti_bancari_abreve_fatturato == "VERDE",flag_debiti_bancari_abreve_fatturato := FALSE ]
    master[flag_debiti_bancari_abreve_fatturato == "GIALLO",flag_debiti_bancari_abreve_fatturato := TRUE ]
    master[flag_debiti_bancari_abreve_fatturato == "ND",flag_debiti_bancari_abreve_fatturato := NA ]

    master[,colore_bilanci_debiti_bancari:= NULL]

    master[,flag_passivo_corrente_fatturato := colore_bilanci_passivo_corren ]
    master[flag_passivo_corrente_fatturato == "VERDE",flag_passivo_corrente_fatturato := FALSE ]
    master[flag_passivo_corrente_fatturato == "ROSSO",flag_passivo_corrente_fatturato := TRUE ]
    master[flag_passivo_corrente_fatturato == "ND",flag_passivo_corrente_fatturato := NA ]

    master[,esito_bilanci_passivo_corrent:= NULL]

    master[, stato_societario := input_stato_societario]

    master[,flag_data_costituzione := colore_data_costituzione ]
    master[flag_data_costituzione == "VERDE",flag_data_costituzione := FALSE ]
    master[flag_data_costituzione == "ROSSO",flag_data_costituzione := TRUE ]

    master[, protesti_certi := input_protesti_certi]
    master[, presenza_fidi_ced := esito_presenza_di_fidi_cedente]
    master[,input_di_fidi_cedente := NULL]

    master[, modello_rating_t0 := cod_mod_bws_t0 ]
    master[,cod_mod_bws_t0 := NULL]
    master[,input_protesti_certi:= NULL]

    master[, fallimenti_certi := input_fallimenti_certi]

    master[,input_fallimenti_certi:= NULL]

    master[,flag_fallimenti_dubbi := colore_fallimenti_dubbi ]
    master[flag_fallimenti_dubbi != "ROSSO",flag_fallimenti_dubbi := FALSE ]
    master[flag_fallimenti_dubbi == "ROSSO",flag_fallimenti_dubbi := TRUE ]

    master[,flag_pregiudizievoli_gravi := colore_pregiudizievoli_gravi ]
    master[flag_pregiudizievoli_gravi == "VERDE",flag_pregiudizievoli_gravi := FALSE ]
    master[flag_pregiudizievoli_gravi == "ROSSO",flag_pregiudizievoli_gravi := TRUE ]
    master[flag_pregiudizievoli_gravi == "ND",flag_pregiudizievoli_gravi := NA ]

    master[,flag_procedure_concorsuali := colore_procedure_concorsuali ]
    master[flag_procedure_concorsuali == "ROSSO",flag_procedure_concorsuali := TRUE ]
    master[flag_procedure_concorsuali == "VERDE",flag_procedure_concorsuali := FALSE ]

    master[, procedure_imprese_collegate := gsub(".* :(.+) STATO.*", "\\1",input_imprese_collegate_proce )]
    master[,input_imprese_collegate_proce:= NULL]

    master[,delta_scadenza_rating := as.numeric(as.Date(data_scadenza_rating)-as.Date(data_delibera_o_annull_fido))]
    master[,data_scadenza_rating := NULL]

    #master[,approvato_applicazione := importo_approvato - importo_applicazione]
    master[,imp_richiesto_lim_autonomia  := importo_richiesto - autonomia_fido]
    master[,importo_richiesto  := NULL]

    master[,filiale_factoring_ceduto:= as.character(filiale_factoring_ceduto)]
    master[,tipo_pratica:= as.character(tipo_pratica)]
    #master[,condice_consociata:= as.character(condice_consociata)]
    #master[,ft_factoring:= as.character(ft_factoring)]
    master[,rae_ceduto:= as.character(rae_ceduto)]

    #master[,numero_tot_fidi := as.numeric(numero_tot_fidi)]
    #master[,count_fidi_in_conferma := as.numeric(count_fidi_in_conferma)]
    # master[,sum_import_fidi_concessione := as.numeric(sum_import_fidi_concessione)]
    # master[,sum_import_fidi_aumento := as.numeric(sum_import_fidi_aumento)]

    #elimino le colonne con tutti NA
    master<- master[,which(unlist(lapply(master, function(x)!all(is.na(x))))),with=F]


    col_1_val <- unlist(lapply(colnames(master),function(x){
        if(length(unique(unlist(master[, eval(x), with = FALSE])))<2){
            return(NULL)
        }else return(x)
    }))

    cols_master<- col_1_val[!grepl('esito',col_1_val) & !grepl('colore',col_1_val) & !grepl('input',col_1_val)]

    master <- master[,..cols_master]
#print(colnames(master))
    #trasformo in factor le colonne non numeriche
    # master <- cbind.data.frame(lapply(master, function(x) {
    #   if (all(unique(x) %in% c(0,1)))
    #     as.factor(c("NO", "YES")[x+1])
    #   else if (all(is.character(x)))
    #     as.factor(x)
    #   else
    #     x
    # }))
    # setDT(master)

    master[,data_delibera_o_annull_fido := NULL]
    #str(master)

    #salvo la master

    ## v2 master iniziale
    ## v3 master con bilancio (quasi sempre vuoto)
    ## v4 master con prosolvendo
    #saveRDS(master,'../data/master_drop4_v4.RDS')

    return(master)
}



table_soglie <- function(tree_table,soglie = c(0.03,0.04), var = 'pred_vpc'){
    lista_soglie <- list(paste(var,"<=",soglie[[1]]),
                         paste(var,"<=",soglie[[2]],"&",var,">",soglie[[1]]),
                         paste(var,">",soglie[[2]]))
    colori <- c("VERDE","GIALLO","ROSSO")
    output <- rbindlist(lapply(lista_soglie,function(x){
                    tree_table[eval(parse(text = x)),
                           list(soglia = x,DEFAULT = sum(DEFAULT), OK = sum(OK),
                                tot = sum(tot), perc = round(100*sum(DEFAULT)/(sum(DEFAULT)+sum(OK)),3))]
                        })
                )
    setDT(output)
    output[,sample_perc := round(100*tot/sum(output$tot),3)]
    output[soglia == lista_soglie[[1]], colore := "VERDE" ]
    output[soglia == lista_soglie[[2]], colore := "GIALLO" ]
    output[soglia == lista_soglie[[3]], colore := "ROSSO" ]
    print(output)
    return(output)

}

performance_oldvpc <- function(dt){
    rbindlist(lapply(c("VERDE","GIALLO","ROSSO"),function(x){
       dt_appo <- dt[ESITO_FINALE_VPC == x,
                 list(colore = x,
                      DEFAULT = nrow(dt[target=='DEFAULT' &ESITO_FINALE_VPC==x ,]),
                      OK = nrow(dt[target=='OK' &ESITO_FINALE_VPC==x ,]),
                      old_tot = nrow(dt[ESITO_FINALE_VPC==x ,]))]
       dt_appo[,old_td := round(100*DEFAULT/old_tot,3)]
       dt_appo[,sample_perc := round(100*old_tot/nrow(dt),3)]
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
    dt <- dt[STATO_PRATICA == 10,]

    saveRDS(dt,file.path(path,'DB_perimetro.RDS'))
}

compute_model <- function(master,minbuck= 0.0005, cp = 1e-4, perc_split =0.75,use_surr = 1,cols_2rm =NA, maxsurr = 5,#cols_ews,flag_ews,
                            suffix = '',desc ="", seed = 1004,segmento = c("Altro", "Corporate", "Large corporate", "Sme Corporate", "Sme Retail")){

                                #str(master)
        split.fun <- function(x, labs, digits, varlen, faclen)
    {
        # replace commas with spaces (needed for strwrap)
        labs <- gsub(",", " ", labs)
        for(i in 1:length(labs)) {
            # split labs[i] into multiple lines
            labs[i] <- paste("\n \n \n \n",paste(strwrap(labs[i], width=40), collapse="\n"),"\n \n")
        }
        labs
    }
    #print(cols_2rm)
    master <-copy(master[macrosegmento_attuale_lc %in% segmento,setdiff(colnames(master),cols_2rm), with = FALSE])

    #creo il modello utilizzando tutte le variabili
    set.seed(seed)
    rows <- sample(1:nrow(master), perc_split*nrow(master), replace=F)
    # if(!flag_ews){
    #     train_set <- master[rows,setdiff(colnames(master),cols_ews),with = FALSE]
    #     test_set <- master[-rows,setdiff(colnames(master),cols_ews),with = FALSE]
    #
    # }else{
        train_set <- master[rows,]
        test_set <- master[-rows,]
    #}
    print(nrow(train_set))
    print(nrow(test_set))
    modello <- rpart(target ~. - NUM_PRATICA - PROGRESS_PRATICA ,
                    data=train_set,
                    method="class",#metodo class o anova
                    model=TRUE,
                    control=rpart.control(cp = cp,minbucket =minbuck*length(rows), maxsurrogate = maxsurr, xval = 100,usesurrogate = use_surr)) ##minbucket forza e foglie ad avere almeno quel numero di elementi
    print(maxsurr)
    path_name <- create_path('output')

    #salvo il plot
    options(repr.plot.width=8, repr.plot.height=7)
    pdf(file.path(path_name$path,paste0("tree_",suffix,"_",path_name$index,'.pdf')))
    #rpart.plot(modello, type=2, fallen.leaves=F,compress = TRUE,ycompress = TRUE, yesno=2, extra = 107,box.palette="-RdGn")


    prp(modello, split.fun=split.fun, yesno=1, extra = 105,type = 1, varlen = 0,#box.palette="RdGn",
            box.col=c("green", "red")[findInterval(modello$frame$yval, v = c(0,0.2))])
    dev.off()
    #print(rpart.rules(modello, cover = TRUE))
    #print('\n ____________________________________________ \n')
    #print(rpart.predict(modello, test_set, rules=TRUE))
    # faccio il predict sul test set
    pred_mod<- predict(modello,test_set)
    pred_mod<- as.data.table(pred_mod)
    dt_appo_pred <- copy(test_set)
    dt_appo_pred$pred_vpc <- as.numeric(pred_mod$DEFAULT)

    summ <- as.data.table(table(dt_appo_pred[,c('pred_vpc','target')]))
    summ <- dcast(summ, formula = pred_vpc ~target)
    summ[, prob := DEFAULT /(OK+DEFAULT) ]
    summ[, tot := OK+DEFAULT ]
    summ



    pmod2 <- pmml(modello, model_name = "VPC Tree Model",
    app_name = NULL,
    description = NULL,
    copyright = NULL)

    write.csv(colnames(master),  row.names = FALSE,file.path(path_name$path,paste0("varstree_",suffix,"_",path_name$index,'.csv')))

    saveXML(pmod2, file=file.path(path_name$path,paste0("tree_",suffix,"_",path_name$index,'.xml')))

    line=paste(paste0("tree_ews",suffix,"_",path_name$index), " - ",as.character(Sys.time()), "\nparametri: ",#EWS =",flag_ews,
        ", split =",perc_split,", cp =",cp,', minbuck = ', minbuck, 'use_surrogate =',use_surr, 'colonne rimosse',cols_2rm,'\n', desc,'\n')
    write(line,file=file.path(path_name$path,"file_versioning.txt"),append=TRUE)
    output<- list(modello,dt_appo_pred,summ,rows)
    names(output)<- c('model','pred','tree_table','rows')
    return(output)
}
