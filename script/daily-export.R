#!/usr/bin/Rscript
require(rcoc)
require(rmarkdown)

clan <- load_clan(tag = "#282UJRU28", auth_key = token('/home/andrea/dev/rcoc/tokens/token-34'))
export(clan , path =  '/home/andrea/dev/rcoc/export')
