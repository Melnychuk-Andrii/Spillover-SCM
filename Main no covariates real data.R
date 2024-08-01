
#---------------------------------------------
# CALIFORNIA
#---------------------------------------------

setwd("~/Documents/Minerva/Thesis/public_code/")

load("cigs.RData")

ind = c(1,0,0,0,1,1,0,0,1,0,0,0,0,0,1,0,0,0,0,
        0,0,0,1,1,1,0,0,0,1,0,0,0,0,1,1,0,0,0,0)

indx = colnames(cigs)[which(!!ind)]
cigs = cbind(cigs[,indx],cigs[,-which(colnames(cigs) %in% indx)])

ind = c(1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

A = diag(n)
A = A[,which(!!ind)]

Y0 = cigs[1:t,]
Y1 = cigs[(t+1):(s + t),]

dataprep <- list(
  foo = cigs,
  pre.treat.period = 1:t,
  full.period = 1:(s + t),
  sp.count = sum(ind) - 1,
  total.ctrl.count = length(ind) - 1,
  Y0 = Y0, Y1 = Y1, A = A, y0 = 0
)

plan(multisession, workers = 6)

runAllCalifornia <- runAllMethods(dataprep)

cat(as.yaml(runAllCalifornia), file="~/Documents/Minerva/Thesis/Empirical Results/noCovarCalifornia.txt")
runAllCalifornia <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/noCovarCalifornia.txt")


timeline <- as.numeric(rownames(cigs))
treat.time <- max(as.numeric(rownames(Y0)))

plotAllMethodsBothPlots(runAllCalifornia, dataprep$foo[,1], timeline, treat.time, 
                        "year",
                        "per-capita cig. sales (in packs)",
                        "gap in per-capita cig. sales (in packs)",
                        "Outcome plot", "Gap plot")

#---------------------------------------------
# POLAND
#---------------------------------------------

dataPoland <- read.csv("~/Documents/Minerva/Thesis/capstone_R_project--main/CP Dataset  - total_dataset - Copy of CP Datasets  - total_dataset.csv", 
                       header=TRUE)

treated_unit <- 7 

donor_pool <- unique(dataPoland$region[dataPoland$region != treated_unit])

pretend_treated_year <- 2014

covariate_matrix <- dataPoland[dataPoland$year < pretend_treated_year, c("gdp", "inf", "pop", "unemp")]
column_datatype <- class(dataPoland$region)

dataprep_out <- dataprep(
  foo = dataPoland,
  predictors = c("migration", "gdp", "inf", "pop", "unemp"),  
  dependent = "migration",  
  unit.variable = "region", 
  time.variable = "year",
  treatment.identifier = treated_unit,
  controls.identifier = donor_pool,
  time.predictors.prior = 2000:2014,  
  time.optimize.ssr = 2000:2014,  
  unit.names.variable = "country",
  time.plot = 2000:2020, 
)

t <- length(dataprep_out$tag$time.optimize.ssr)
s <- length(dataprep_out$tag$time.plot) - t

Yall <- cbind(dataprep_out$Y1plot, dataprep_out$Y0plot)

n <- ncol(Yall)

ind = c(1,1,1,0,1,0,1,0,1,1,
        1,0,0,0,1,1,1,1,1,0)

indx = colnames(Yall)[which(!!ind)]
Yall = cbind(Yall[,indx],Yall[,-which(colnames(Yall) %in% indx)])

ind = c(1,1,1,1,1,1,1,1,1,1,
        1,1,1,0,0,0,0,0,0,0)

A = diag(n)
A = A[,which(!!ind)]

Y0 = Yall[1:t,]
Y1 = Yall[(t+1):(s + t),]

dataprep <- list(
  foo = Yall,
  pre.treat.period = 1:t,
  full.period = 1:(s + t),
  sp.count = sum(ind) - 1,
  total.ctrl.count = length(ind) - 1,
  Y0 = Y0, Y1 = Y1, A = A, y0 = 0
)

plan(multisession, workers = 6)

runAllPoland <- runAllMethods(dataprep)

cat(as.yaml(runAllPoland), file="~/Documents/Minerva/Thesis/Empirical Results/noCovarPoland.txt")
runAllPoland <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/noCovarPoland.txt")

timeline <- as.numeric(rownames(Yall))
treat.time <- max(as.numeric(rownames(Y0)))

plotAllMethodsBothPlots(runAllPoland, dataprep$foo[,1], timeline, treat.time, 
                        "year",
                        "migration",
                        "gap in migration",
                        "Outcome plot", "Gap plot")

#---------------------------------------------
# WEST GERMANY
#---------------------------------------------


library(foreign)
d <- read.dta("~/Documents/Minerva/Thesis/repgermany.dta")

dataprep2 <-
  dataprep(
    foo = d,
    predictors    = c("trade","infrate"),
    dependent     = "gdp",
    unit.variable = "index",
    time.variable = "year",
    special.predictors = list(
      list("industry" ,1981:1990, c("mean")),
      list("schooling",c(1980,1985), c("mean")),
      list("invest80" ,1980, c("mean"))
    ),
    treatment.identifier = 7,
    controls.identifier = setdiff(unique(d$index), c(7)),
    #spillover.controls.identifier = 3,
    time.predictors.prior = 1981:1990,
    time.optimize.ssr = 1960:1989,
    unit.names.variable = "country",
    time.plot = 1960:2003
  )



t <- length(dataprep2$tag$time.optimize.ssr)
s <- length(dataprep2$tag$time.plot) - t

Yall <- cbind(dataprep2$Y1plot, dataprep2$Y0plot)

n <- ncol(Yall)

ind = c(1,0,0,1,0,0,0,0,0,0,
        0,0,0,0,0,0,0)

indx = colnames(Yall)[which(!!ind)]
Yall = cbind(Yall[,indx],Yall[,-which(colnames(Yall) %in% indx)])

ind = c(1,1,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0)

A = diag(n)
A = A[,which(!!ind)]

Y0 = Yall[1:t,]
Y1 = Yall[(t+1):(s + t),]

dataprep <- list(
  foo = Yall,
  pre.treat.period = 1:t,
  full.period = 1:(s + t),
  sp.count = sum(ind) - 1,
  total.ctrl.count = length(ind) - 1,
  Y0 = Y0, Y1 = Y1, A = A, y0 = 0
)

plan(multisession, workers = 6)

runAllGermany <- runAllMethods(dataprep)

cat(as.yaml(runAllGermany), file="~/Documents/Minerva/Thesis/Empirical Results/noCovarGermany.txt")
runAllGermany <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/noCovarGermany.txt")

timeline <- as.numeric(rownames(Yall))
treat.time <- max(as.numeric(rownames(Y0)))

plotAllMethodsBothPlots(runAllGermany, dataprep$foo[,1], timeline, treat.time, 
                        "year",
                        "migration",
                        "gap in migration",
                        "Outcome plot", "Gap plot")



#---------------------------------------------
# BASQUE
#---------------------------------------------


library(Synth)
data(basque)
# dataprep: prepare data for synth

basque1 = basque[basque$year > 1959,]

dataprep.out <-
  dataprep(
    foo = basque1
    ,predictors= c("school.illit",
                   "school.prim",
                   "school.med",
                   "school.high",
                   "school.post.high"
                   ,"invest"
    )
    ,predictors.op = c("mean")
    ,dependent = c("gdpcap")
    ,unit.variable = c("regionno")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("gdpcap",1960:1969,c("mean")),
      list("sec.agriculture",seq(1961,1969,2),c("mean")),
      list("sec.energy",seq(1961,1969,2),c("mean")),
      list("sec.industry",seq(1961,1969,2),c("mean")),
      list("sec.construction",seq(1961,1969,2),c("mean")),
      list("sec.services.venta",seq(1961,1969,2),c("mean")),
      list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
      list("popdens",1969,c("mean")))
    ,treatment.identifier = 17
    ,controls.identifier = c(2:16, 18)
    ,time.predictors.prior = c(1964:1969)
    ,time.optimize.ssr = c(1960:1969)
    ,unit.names.variable = c("regionname")
    ,time.plot = c(1960:1997)
  )
# # 1. combine highest and second highest
# # schooling category and eliminate highest category
# dataprep.out$X1["school.high",] <-
#   dataprep.out$X1["school.high",] +
#   dataprep.out$X1["school.post.high",]
# dataprep.out$X1 <-
#   as.matrix(dataprep.out$X1[
#     -which(rownames(dataprep.out$X1)=="school.post.high"),])
# dataprep.out$X0["school.high",] <-
#   dataprep.out$X0["school.high",] +
#   dataprep.out$X0["school.post.high",]
# dataprep.out$X0 <-
#   dataprep.out$X0[
#     -which(rownames(dataprep.out$X0)=="school.post.high"),]
# # 2. make total and compute shares for the schooling catgeories
# lowest <- which(rownames(dataprep.out$X0)=="school.illit")
# highest <- which(rownames(dataprep.out$X0)=="school.high")
# dataprep.out$X1[lowest:highest,] <-
#   (100 * dataprep.out$X1[lowest:highest,]) /
#   sum(dataprep.out$X1[lowest:highest,])
# dataprep.out$X0[lowest:highest,] <-
#   100 * scale(dataprep.out$X0[lowest:highest,],
#               center=FALSE,
#               scale=colSums(dataprep.out$X0[lowest:highest,])
#   )
# run synth



t <- length(dataprep.out$tag$time.optimize.ssr)
s <- length(dataprep.out$tag$time.plot) - t

Yall <- cbind(dataprep.out$Y1plot, dataprep.out$Y0plot)

n <- ncol(Yall)

ind = c(1,0,0,0,0,0,1,1,0,0,
        0,0,0,0,0,1,1)

indx = colnames(Yall)[which(!!ind)]
Yall = cbind(Yall[,indx],Yall[,-which(colnames(Yall) %in% indx)])

ind = c(1,1,1,1,1,0,0,0,0,0,
        0,0,0,0,0,0,0)

A = diag(n)
A = A[,which(!!ind)]

Y0 = Yall[1:t,]
Y1 = Yall[(t+1):(s + t),]

dataprep <- list(
  foo = Yall,
  pre.treat.period = 1:t,
  full.period = 1:(s + t),
  sp.count = sum(ind) - 1,
  total.ctrl.count = length(ind) - 1,
  Y0 = Y0, Y1 = Y1, A = A, y0 = 0
)

plan(multisession, workers = 6)

runAllBasque <- runAllMethods(dataprep)

cat(as.yaml(runAllBasque), file="~/Documents/Minerva/Thesis/Empirical Results/noCovarBasque.txt")
runAllBasque <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/noCovarBasque.txt")

timeline <- as.numeric(rownames(Yall))
treat.time <- max(as.numeric(rownames(Y0)))

plotAllMethodsBothPlots(runAllBasque, dataprep$foo[,1], timeline, treat.time, 
                        "year",
                        "migration",
                        "gap in migration",
                        "Outcome plot", "Gap plot")



