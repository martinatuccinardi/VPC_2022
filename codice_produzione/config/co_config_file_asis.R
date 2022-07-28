cols_modello <- c(
'AT_eta_soc',
'CR_sconf_acc',
'CR_uti_acc',
'CRA_ADJ',
'CRA_andamento',
'BIL_PFN_Ebitda_adj',
'DSI_flag_fido_ridotto',
'DSI_flag_scaduto_20_outstanding',
'DSI_imp_richiesto_lim_autonomia',
'DSI_polizza_diretta_39_',
'DSI_presenza_fidi_ced',
'DSI_rapporti_in_osservazione',
'AT_cod_sett_cedente',
'AT_cod_sett',
'EWS_pred',
'NUM_PRATICA',
'PROGRESS_PRATICA',
'RAT_classe_rating_filt',
'FORMA_GIURIDICA',
'RAT_andamento',
'target',
'macrosegmento_attuale_lc')

 ## soglie per il taglio dei colori
soglia_vg <- 0.02 # verde - giallo
soglia_gr <- 0.05 # giallo - rosso

## iperparametri dell'albero
cp <- 0.0001 # guadagno
seed <- 1011#seed per l'estrazione random
minbuck <- 0.0007# numero minimo di record all'interno di una foglia

# path da cui leggere la master
path_master <-'../data/master'


suffix = '_asis'
