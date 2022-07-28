cols_modello <- c(
'AT_eta_soc',
'AT_cod_microsett',
'CR_sconf_acc',
'CR_sconf_acc_mlt',
'CR_uti_acc',
'CR_uti_acc_revoca',
'BIL_PFN_Ebitda_adj',
'CRA_andamento',
"CRA_ADJ",
'DSI_flag_scaduto_20_outstanding',
'DSI_presenza_fidi_ced',
'DSI_rapporti_in_osservazione',
'AT_cod_sett_cedente',
'AT_cod_sett',
'DSI_tipo_pratica',
'EWS_ASIS',
'NUM_PRATICA',
'PROGRESS_PRATICA',
'RAT_classe_rating_filt',
'FORMA_GIURIDICA',
'RAT_andamento',
'target',
'macrosegmento_attuale_lc')

 ## soglie per il taglio dei colori
soglia_vg <- 0.07 # verde - giallo
soglia_gr <- 0.095 # giallo - rosso


 # iperparametri dell'albero
cp <- 0.0001 # guadagno
seed <- 1111 #seed per l'estrazione random
minbuck<- 0.0005 # numero minimo di record all'interno di una foglia
perc_split <- 0.80 #percentuale di split tra train e test

path_master <-'../data/master'


suffix = '_retrain'

flag_prune <- FALSE
flag_opt  <- TRUE
