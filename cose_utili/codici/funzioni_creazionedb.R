creazione_indicatori_DSI<- function(master){
    colnames(master)<-tolower(colnames(master))
    colnames(master)<- gsub("\\s+","_",gsub('[^a-zA-Z0-9]', " ",colnames(master)))


    ## FILTRI A MONTE
    master[, protesti_certi := input_protesti_certi]
    master[, fallimenti_certi := input_fallimenti_certi]
    master[,flag_fallimenti_dubbi := colore_fallimenti_dubbi ]
    master[flag_fallimenti_dubbi != "ROSSO",flag_fallimenti_dubbi := FALSE ]
    master[flag_fallimenti_dubbi == "ROSSO",flag_fallimenti_dubbi := TRUE ]

    master[,flag_pregiudizievoli_gravi := colore_pregiudizievoli_gravi ]
    master[flag_pregiudizievoli_gravi == "VERDE",flag_pregiudizievoli_gravi := FALSE ]
    master[flag_pregiudizievoli_gravi == "ROSSO",flag_pregiudizievoli_gravi := TRUE ]
    master[flag_pregiudizievoli_gravi == "ND",flag_pregiudizievoli_gravi := NA ]

    master[, procedure_imprese_collegate := gsub(".* :(.+) STATO.*", "\\1",input_imprese_collegate_proce )]

    master[,flag_procedure_concorsuali := colore_procedure_concorsuali ]
    master[flag_procedure_concorsuali == "ROSSO",flag_procedure_concorsuali := TRUE ]
    master[flag_procedure_concorsuali == "VERDE",flag_procedure_concorsuali := FALSE ]

    master[, flag_presenza_sofferenze := colore_cr_presenza_sofferenze]
    master[is.na(flag_presenza_sofferenze), flag_presenza_sofferenze := 'non censito']

    master[, col_fido_con_causale_negativa := colore_fido_con_causale_negati]
    master[, col_appl_con_causale_negativa := colore_applicazione_con_causal]

    master[,sab :=input_stato_sab]
    master[is.na(sab), sab := "non censito"]

    master[, col_presenza_pratica_pef := colore_presenza_pratica_in_pef]
    master[,parte_correlata := gsub("[^0-9]", "",input_parte_correlata )]
    master[ parte_correlata =='',parte_correlata := 'missing']
    master[ is.na(colore_parte_correlata),parte_correlata := 'non censito']

    master[, stato_societario := input_stato_societario]


    #variabili albero
    master[,rapporti_in_osservazione := esito_rapporti_in_osservazione]
    master[is.na(rapporti_in_osservazione), rapporti_in_osservazione := -100]

    master[,flag_scaduto_20_outstanding := colore_scaduto_20_outstandin ]
    master[flag_scaduto_20_outstanding == "VERDE",flag_scaduto_20_outstanding := FALSE ]
    master[flag_scaduto_20_outstanding == "GIALLO",flag_scaduto_20_outstanding := TRUE ]
    master[is.na(flag_scaduto_20_outstanding), flag_scaduto_20_outstanding := "non censito"]

    master[,flag_fido_ridotto := colore_fido_ridotto ]
    master[flag_fido_ridotto == "VERDE",flag_fido_ridotto := FALSE ]
    master[flag_fido_ridotto == "GIALLO",flag_fido_ridotto := TRUE ]

    master[, presenza_fidi_ced := esito_presenza_di_fidi_cedente]



    master<- master[,which(unlist(lapply(master, function(x)!all(is.na(x))))),with=F]


    col_1_val <- unlist(lapply(colnames(master),function(x){
        if(length(unique(unlist(master[, eval(x), with = FALSE])))<2){
            return(NULL)
        }else return(x)
    }))

    cols_master<- col_1_val[!grepl('esito',col_1_val) & !grepl('colore',col_1_val) & !grepl('input',col_1_val)]

    master <- master[,..cols_master]

    master[,data_delibera_o_annull_fido := NULL]


    return(master)
}
