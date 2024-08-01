library(tidyverse)

setwd("~/Documents/Minerva/Thesis/NewResults1000")

pre.treat.lengths = c(10,15)
total.unit.counts = c(6,10)
sp.unit.ratios = c(0.11, 0.33, 0.67, 0.9)
treat.effects = c(5,3,1.8)
sp.effect.ratios = c(0.1, 0.3, 0.6, 0.9)
modes = c("Stationary", "I(1)")

full.length <- length(total.unit.counts) *
  length(pre.treat.lengths) *
  length(sp.unit.ratios) *
  length(treat.effects) *
  length(sp.effect.ratios) *
  length(modes) 

cur.file <- 1

file.list <- c()

for (pre.treat.length in pre.treat.lengths) {
  for (total.unit.count in total.unit.counts){
    for (sp.unit.ratio in sp.unit.ratios) {
      for (treat.effect in treat.effects) {
        for (sp.effect.ratio in sp.effect.ratios){
          for (mode in modes){
            
            file.name <-  paste(as.character(cur.file), ".txt", sep = "")
            
            if (pre.treat.length != 50 & 
                total.unit.count != 30 ) {
              file.list = append(file.list, file.name)
            }
            
            cur.file = cur.file + 1
            
          }
        }
      }
    }
  }
}

resListImport <- setNames(as.list(1:full.length), paste0("permutation", 1:full.length))

for (file.cur in file.list){
  cur.res.imp <- yaml.load_file(file.cur)
  
  resListImport[[paste0("permutation",parse_number(file.cur))]] = cur.res.imp
}

resListImport = resListImport[parse_number(file.list)]

cat(as.yaml(resListImport), file="~/Documents/Minerva/Thesis/newruns1000results.txt")
