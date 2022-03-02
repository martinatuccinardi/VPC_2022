

source('model_functions.R')

#lettura dati con perimetro corretto
dt_orig <- readRDS('../data/DB_perimetro.RDS')

master_drop3 <- readRDS('../data/master_drop3.RDS')

model <- compute_model(master_drop3,dt_orig,FALSE)

## calcolo le percentuali di Default rispetto alle classi assegnate dalla vecchia vpc e stampo le numeriche associate

dt_sample <- dt_orig[-model$rows,]

print(performance_oldvpc(dt_sample))
## calcolo le percentuali di default con le nuove classi, a soglie fissatee

print(table_soglie(model$tree_table))
