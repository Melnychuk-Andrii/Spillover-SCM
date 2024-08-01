set.seed(2042024) 

setwd("~/Documents/Minerva/Thesis/NewResults1000")

plan(multisession, workers = 20)

runMultipleConfigs(
  nRuns = 1000, 
  pre.treat.lengths = c(10,15),
  total.unit.counts = c(6,10),
  sp.unit.ratios = c(0.11, 0.33, 0.67, 0.9),
  treat.effects = c(5,3,1.8),
  sp.effect.ratios = c(0.1, 0.3, 0.6, 0.9),
  modes = c("Stationary", "I(1)")
)


