library(openxlsx)
library(data.table)
library(rpart)
library(rpart.plot)
#library(readxl)
library(stringr)
library(pmml)
library(partykit)
library(ROCR)
#require(arules)
require(pracma)
require(ggplot2)



#dt_orig <- readRDS('~/Projects/VPC/VPC_2022/data/DB_perimetro.RDS')


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

rocvalues<-function(predictions, results, type=c("roc","gain")) {
	type<-match.arg(type)
	indici<-order(predictions,decreasing=TRUE)
	vec<-results[indici]
	y<-cumsum(vec)/sum(results) #cumulative positive rate
	if (type=="gain") x<-seq_along(results)/length(results) else x<-cumsum(!vec)/sum(!vec) #percentage of sample
	ritorno<-list(x=x,y=y)
	attr(ritorno,"AUC")<-trapz(x,y)
	ritorno
}



plot_roc <- function(test,segmento,path_name,suffix){
    test[,target2:=ifelse(target=="DEFAULT",1,0)]
    roc <-rocvalues(test$pred_vpc, test$target2)

    rocDT<-data.table(x=roc$x, y=roc$y)
    pdf(file.path(path_name$path,paste0(segmento,"_ROC_",suffix,'.pdf')))
    plot(roc$x, roc$y, cex=.1, col='white',pch=15, xlab = 'FPR', ylab = 'TPR', main = round(attr(roc,"AUC"),2))

    fit5 <- lm(y~poly(x,5,raw=TRUE), data=rocDT)


    x_axis <- seq_along(test$target2)/length(test$target2)
    lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='forestgreen', lwd=2)
    abline(0,1, lty = 2)
    dev.off()
    cat("\n AUC:",attr(roc,"AUC"),'\n')
    return(attr(roc,"AUC"))
}



compute_model <- function(master,minbuck= 0.0005, cp = 1e-4, perc_split =0.75,use_surr = 1, maxsurr = 5,#cols_ews,flag_ews,
                            seed = 1004,segmento = "", flag_opt = TRUE){



    rows <- split_sample(master,segmento = segmento, perc_split,seed)
    train_set <- master[rows,]
    test_set <- master[-rows,]
    cat('\n split', perc_split)
    cat('\n train set - righe', nrow(train_set) )
    cat('\n test set - righe', nrow(test_set) )

    lapply(colnames(test_set), function(x){
        if(class(test_set[,get(x)])%in% c("factor","character")){
            test_set[!(get(x) %in% unique(train_set[,get(x)])),eval(x):= NA]
             test_set[,eval(x):=as.factor(get(x))]

        }
        return(NULL)
    })

    if(flag_opt){
        k <- 0
        j <- 1
        i <- 1
        cp_input <- copy(cp)
        minbuck_input <- copy(minbuck)
        repeat{

            modello <- rpart(target ~. - NUM_PRATICA - PROGRESS_PRATICA ,
                            data=train_set,
                            method="class",#metodo class o anova
                            model=TRUE,
                            control=rpart.control(cp = cp,minbucket =minbuck*length(rows), maxsurrogate = maxsurr, xval = 100,usesurrogate = use_surr)) ##minbucket forza e foglie ad avere almeno quel numero di elementi
            nodes <- as.numeric(rownames(modello$frame))
            print(table(rpart:::tree.depth(nodes)))
            depth <- mean(rpart:::tree.depth(nodes))   #max(rpart:::tree.depth(nodes))

            cat('\n profondità',depth,'\n')
            k <- k+1
            if(k < 15 & depth >7){
                cat('\n minbuck',minbuck,'\n')
                minbuck <- minbuck*(1+i*0.5)
                i <- i+1
           }else if(k < 15 & depth <5){
                cat('\n minbuck',minbuck,'\n')
                minbuck <- minbuck*(1-j*0.5)
                j <- j+1
           }else if(depth <= 7 & depth >=5){
               break
           }else if(k >= 15){
               minbuck <- minbuck_input
               modello <- rpart(target ~. - NUM_PRATICA - PROGRESS_PRATICA ,
                               data=train_set,
                               method="class",#metodo class o anova
                               model=TRUE,
                               control=rpart.control(cp = cp,minbucket =minbuck*length(rows), maxsurrogate = maxsurr, xval = 100,usesurrogate = use_surr)) ##minbucket forza e foglie ad avere almeno quel numero di elementi
               nodes <- as.numeric(rownames(modello$frame))
               print(table(rpart:::tree.depth(nodes)))
               depth <- max(rpart:::tree.depth(nodes))
               cat('\n profondità',depth,'\n')
               break
           }
       }
       print('ok depth')
        seeds <- rbindlist(lapply(sample(1:10^4,10), function(seed){
            print(seed)

            rows <- split_sample(master,segmento = segmento, perc_split,seed)
            train_set <- master[rows,]
            test_set <- master[-rows,]
            modello <- rpart(target ~. - NUM_PRATICA - PROGRESS_PRATICA ,
                            data=train_set,
                            method="class",#metodo class o anova
                            model=TRUE,
                            control=rpart.control(cp = cp,minbucket =minbuck*length(rows), maxsurrogate = maxsurr, xval = 100,usesurrogate = use_surr))

            pred_mod<- predict(modello,test_set)
            pred_mod<- as.data.table(pred_mod)

            test_set$pred_vpc <- as.numeric(pred_mod$DEFAULT)
            test_set[,target2:=ifelse(target=="DEFAULT",1,0)]
            roc <-rocvalues(test_set$pred_vpc, test_set$target2)
            #print(attr(roc,"AUC"))
            data.table(auc = attr(roc,"AUC"), seed = seed)
        }))

        seed<- seeds[which(auc == max(auc[auc < mean(auc)+sd(auc)])),seed]
    }

    rows <- split_sample(master,segmento = segmento, perc_split,seed)
    train_set <- master[rows,]
    test_set <- master[-rows,]

    cat('\n mean',mean(seeds$auc))
    cat('\n sd',sd(seeds$auc))
    cat('\n', seeds[which(auc == max(auc[auc < mean(auc)+sd(auc)])),auc])
    print(paste0('parametri utilizzati: cp=',cp, " minbuck = ", minbuck, " seed = ",seed))
    modello <- rpart(target ~. - NUM_PRATICA - PROGRESS_PRATICA ,
                    data=train_set,
                    method="class",#metodo class o anova
                    model=TRUE,
                    control=rpart.control(cp = cp,minbucket =minbuck*length(rows), maxsurrogate = maxsurr, xval = 100,usesurrogate = use_surr))

    output<- list(modello,rows)
    names(output)<- c('model','rows')
    return(output)
}


split_sample <- function(master,segmento, perc_split,seed){
    set.seed(seed)

    if(segmento !='sme'){

        cod_pochi_set <- master[, .(count = .N), by = "AT_cod_sett"][count <4,AT_cod_sett]

        rows_pochi <- which(master$AT_cod_sett %in% cod_pochi_set)

        rows <- c(sample(setdiff(1:nrow(master),rows_pochi), perc_split*nrow(master), replace=F),rows_pochi)
        train_set <- master[rows,]
        test_set <- master[-rows,]

        k <- 0

        repeat{
            rows <- c(sample(setdiff(1:nrow(master),rows_pochi), perc_split*nrow(master), replace=F),rows_pochi)
            train_set <- master[rows,]
            test_set <- master[-rows,]

            k <- k+1
            if(k>1000 | ( length(setdiff(unique(test_set$AT_cod_sett),unique(train_set$AT_cod_sett)))==0)){
                #print(k)
            break
            }
        }

    }else{
        cod_pochi_microsett <- master[, .(count = .N), by = "AT_cod_microsett"][count <5,AT_cod_microsett]
        cod_pochi_set <- master[, .(count = .N), by = "AT_cod_sett"][count <3,AT_cod_sett]

        rows_pochi <- which(master$AT_cod_sett %in% cod_pochi_set |master$AT_cod_microsett %in% cod_pochi_microsett)

        rows <- c(sample(setdiff(1:nrow(master),rows_pochi), perc_split*nrow(master), replace=F),rows_pochi)
        train_set <- master[rows,]
        test_set <- master[-rows,]

        k <- 0

        repeat{
            rows <- c(sample(setdiff(1:nrow(master),rows_pochi), perc_split*nrow(master), replace=F),rows_pochi)
            train_set <- master[rows,]
            test_set <- master[-rows,]

            k <- k+1
            if(k>1000 | (length(setdiff(unique(test_set$AT_cod_microsett),unique(train_set$AT_cod_microsett)))==0 & length(setdiff(unique(test_set$AT_cod_sett),unique(train_set$AT_cod_sett)))==0)){
                #print(k)
            break
            }
        }

    }
    # output <- list(train_set,test_set)
    # names(output) <- c('train','test')
    return(rows)

}

feature_imp <- function(modello,segmento,suffix,path_name){
    print('calcolo feature importance')
    feat_imp <- setDT(data.frame(imp = round(modello$variable.importance,4)), keep.rownames = TRUE)
    colnames(feat_imp) <- c('nome_var','F_imp')

    tab_variabili <- feat_imp[order(-F_imp)]

    p <- ggplot(tab_variabili[!is.na(F_imp),], aes(x=reorder(nome_var,F_imp), y=F_imp,fill=F_imp))+
            geom_bar(stat="identity", position="dodge")+ coord_flip()+
            ylab("Variable Importance")+
            xlab("")+
            ggtitle("Information Value Summary")+
            guides(fill='none')+
            scale_fill_gradient(low="cyan", high="blue")+
            theme(axis.text=element_text(size=16))

    ggsave(
        filename=file.path(path_name$path,paste0("featImp_",segmento,suffix,'.png')),
        width = 300, height = 450,
        units = "mm",
        dpi = 320,
        p
    )
}


compute_tab_perf <- function(modello, test,foglie,master_modello,dt_orig){

    model_frame <- as.data.table(modello$frame)
    model_frame[,type := 'leaf']
    model_frame[var != '<leaf>',type := 'other']

    foglie[,round_pred := round(as.numeric(pred_vpc),7) ]
    model_frame[,round_pred := round(yval2.V4,7) ]

    dt_tree <-unique(merge(foglie,
                           model_frame[,c('var','type','round_pred')],
                           by = 'round_pred',
                           all.x = TRUE))


    # dt_sample <-merge(test,
    #                  master_modello[,.(NUM_PRATICA, PROGRESS_PRATICA,Classificazione_Cliente)],
    #                  by = c('NUM_PRATICA', 'PROGRESS_PRATICA'),
    #                  all.x = TRUE)

    table_performance <- dt_tree[,.(pred_vpc,DEFAULT,OK,tot)]
    table_performance[, tot_verdi := round(100*cumsum(tot)/sum(tot),2)]
    table_performance[, lift_verde := round((cumsum(DEFAULT)/cumsum(tot))/(sum(DEFAULT)/sum(tot)),2)]
    table_performance <- table_performance[order(-pred_vpc)]

    table_performance[,td_foglia := round(100*DEFAULT/tot,2)]
    table_performance[,def_cum := cumsum(DEFAULT)]
    table_performance[,ok_cum := cumsum(OK)]
    table_performance[,tot_cum := cumsum(tot)]

    table_performance[,recall := round(100*(def_cum-DEFAULT)/sum(DEFAULT),2)]

    dt_results <- copy(test)
    dt_results[pred_vpc > soglia_vg & pred_vpc <=soglia_gr, col_new_vpc := "giallo"]
    dt_results[pred_vpc <= soglia_vg, col_new_vpc := "verde"]
    dt_results[pred_vpc > soglia_gr, col_new_vpc := "rosso"]

    dt_results[col_new_vpc == "verde", tree_col_num := 3]
    dt_results[col_new_vpc == "giallo" , tree_col_num := 4]
    dt_results[col_new_vpc == "rosso" , tree_col_num := 5]

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
   output <- list(table_performance,table_results)
   names(output)<- list('perf','results')
   return(output)
}


check_overfit <- function(sample, modello){
    pred_mod<- predict(modello,sample)
    pred_mod<- as.data.table(pred_mod)

    sample$pred_vpc <- as.numeric(pred_mod$DEFAULT)
    sample[,target2:=ifelse(target=="DEFAULT",1,0)]
    roc <-rocvalues(sample$pred_vpc, sample$target2)
   # print(attr(roc,"AUC"))
    return(attr(roc,"AUC"))
}




compute_model_asis <- function(master, perc_split =0.75,segmento ='',#cols_ews,flag_ews,
                            suffix = '',desc ="", seed = 1004,folder = 'output'){

    set.seed(seed)

    rows <- sample(1:nrow(master), perc_split*nrow(master), replace=F)
    train_set <- master[rows,]
    test_set <- master[-rows,]
    if(segmento =='sme'){
        modello <-readRDS('modelli/modello_sme_trim_asis.RDS')
    }else{
        modello <-readRDS('modelli/modello_co_trim_asis.RDS')
    }


    output<- list(modello,rows)
    names(output)<- c('model','rows')
    return(output)
}
