require(rpart)
library(rpart.plot)
require(pmml)

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

modello <- readRDS('modello_sme.RDS')

trimmed.tree <- rpart.plot(modello, snip=TRUE,split.fun=split.fun, yesno=1, extra = 105,type = 1, varlen = 0)$obj   # manually trim the tree
#rpart.plot(trimmed.tree)

saveRDS(trimmed.tree,'modello_sme_trimmed.RDS')
