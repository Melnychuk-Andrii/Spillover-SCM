

#---------------------------------------------
# CALIFORNIA
#---------------------------------------------

california_data_path <- "~/Documents/Minerva/Thesis/california-tobacco-control-program/data-abadie-diamond-hainmueller-california-proposition-99-tobacco-control-program-synthetic-control.csv"

california_dataframe <- read.csv(california_data_path)

restricted <- c(3, 5, 6, 8, 15, 21, 22, 23, 29, 34, 36)

dataprep <- list(
  foo = california.dataframe,
  predictors = c("lnincome", "age15to24", "beer", "retprice"),
  special.predictors = list(
    list("cigsale", 1975, c("mean")),
    list("cigsale", 1980, c("mean")),
    list("cigsale", 1988, c("mean"))
  ),
  dependent = "cigsale",
  unit.variable = "state",
  time.variable = "year",
  treatment.identifier = 3,
  controls.identifier = 
    unique(california.dataframe$state[
      -which(california.dataframe$state %in% restricted)]),
  spillover.controls.identifier = setdiff(restricted, 3),
  time.predictors.prior = 1970:1988,
  time.optimize.ssr = 1970:1988,
  time.plot = 1970:2000,
  unit.names.variable = "state_name"
) 

plan(multisession, workers = 6)

runAllCalifornia <- runAllMethodsWithCov(dataprep)

cat(as.yaml(runAllCalifornia), file="~/Documents/Minerva/Thesis/Empirical Results/California.txt")
runAllCalifornia <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/California.txt")

timeline <- dataprep$time.plot
treat.time <- max(dataprep$time.optimize.ssr)

counterfactual = as.vector(na.omit(
  dataprep$foo[dataprep$foo$state == dataprep$treatment.identifier, dataprep$dependent]))

plotAllMethodsBothPlots(runAllCalifornia, counterfactual, timeline, treat.time, 
                        "year",
                        "per-capita cig. sales (in packs)",
                        "gap in per-capita cig. sales (in packs)",
                        "Outcome plot", "Gap plot")



# runAllCalifornia$regular$gap
# runAllCalifornia$sp.model$gap
# runAllCalifornia$restrictive$gap
# runAllCalifornia$inclusive$gap
# runAllCalifornia$iterative.TT$gap


#---------------------------------------------
# POLAND
#---------------------------------------------

dataPoland <- read.csv("~/Documents/Minerva/Thesis/capstone_R_project--main/CP Dataset  - total_dataset - Copy of CP Datasets  - total_dataset.csv", 
                       header=TRUE)

dataPoland = dataPoland[dataPoland$year != 2021,]

# Define the treated unit, donor pool, and treated year
treated_unit <- 7  # Replace with the name/code of Poland in your dataset
# Review and adjust any country_removed variables if necessary

# Create the donor pool while excluding the country to be removed
donor_pool <- unique(dataPoland$region[dataPoland$region != treated_unit])

sp_pool <- c(1, 2, 4, 6, 9, 10, 11, 15, 16, 17, 18, 19)

donor_pool = setdiff(donor_pool, sp_pool)

# Pretend the intervention happened in 2014 (placebo test)
pretend_treated_year <- 2014

# Review and adjust the selection of covariate variables
covariate_matrix <- dataPoland[dataPoland$year < pretend_treated_year, c("gdp", "inf", "pop", "unemp")]
column_datatype <- class(dataPoland$region)

dataprep <- list(
  foo = dataPoland,
  predictors = c("migration", "gdp", "inf", "pop", "unemp"),  # Review and adjust the predictors
  dependent = "migration",  # Verify that the outcome variable is correctly specified
  unit.variable = "region",  # Verify the variable representing countries/regions
  time.variable = "year",
  treatment.identifier = treated_unit,
  controls.identifier = donor_pool,
  spillover.controls.identifier = sp_pool,
  time.predictors.prior = 2000:2014,  # Adjust the time range for the placebo test
  time.optimize.ssr = 2000:2014,  # Adjust the time range for the placebo test
  unit.names.variable = "country",
  time.plot = 2000:2020  # Adjust the time range for the placebo test
)


plan(multisession, workers = 6)

runAllPoland <- runAllMethodsWithCov(dataprep)

cat(as.yaml(runAllPoland), file="~/Documents/Minerva/Thesis/Empirical Results/Poland.txt")
runAllPoland <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/Poland.txt")


timeline <- dataprep$time.plot
treat.time <- max(dataprep$time.optimize.ssr)

counterfactual = as.vector(na.omit(
  dataprep$foo[dataprep$foo$region == dataprep$treatment.identifier, dataprep$dependent]))

plotAllMethodsBothPlots(runAllPoland, counterfactual, timeline, treat.time, 
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
  list(
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
    controls.identifier = setdiff(unique(d$index), c(3,7)),
    spillover.controls.identifier = 3,
    time.predictors.prior = 1981:1990,
    time.optimize.ssr = 1960:1989,
    unit.names.variable = "country",
    time.plot = 1960:2003
  )


plan(multisession, workers = 6)

runAllGermany <- runAllMethodsWithCov(dataprep2)

cat(as.yaml(runAllGermany), file="~/Documents/Minerva/Thesis/Empirical Results/Germany.txt")
runAllGermany <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/Germany.txt")

timeline <- dataprep2$time.plot
treat.time <- max(dataprep2$time.optimize.ssr)

counterfactual = as.vector(na.omit(
  dataprep2$foo[dataprep2$foo$index == dataprep2$treatment.identifier, dataprep2$dependent]))

plotAllMethodsBothPlots(runAllGermany, counterfactual, timeline, treat.time, 
                        "year",
                        "GDP",
                        "gap in GDP",
                        "Outcome plot", "Gap plot")

#---------------------------------------------
# BASQUE
#---------------------------------------------

library(Synth)
data(basque)
# dataprep: prepare data for synth

basque1 = basque[basque$year > 1959,]

dataprep.out <-
  list(
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
    ,controls.identifier = c(2:6, 9:15)
    ,spillover.controls.identifier = c(7, 8, 16, 18)
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


plan(multisession, workers = 6)

runAllBasque <- runAllMethodsWithCov(dataprep.out)

cat(as.yaml(runAllBasque), file="~/Documents/Minerva/Thesis/Empirical Results/Basque.txt")
runAllBasque <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/Basque.txt")

timeline <- dataprep.out$time.plot
treat.time <- max(dataprep.out$time.optimize.ssr)

counterfactual = as.vector(na.omit(
  dataprep.out$foo[dataprep.out$foo$regionno == dataprep.out$treatment.identifier, 
                   dataprep.out$dependent]))

plotAllMethodsBothPlots(runAllBasque, counterfactual, timeline, treat.time, 
                        "year",
                        "GDP",
                        "gap in GDP",
                        "Outcome plot", "Gap plot")


#---------------------------------------------
# FINAL PLOTTING
#---------------------------------------------

california_data_path <- "~/Documents/Minerva/Thesis/california-tobacco-control-program/data-abadie-diamond-hainmueller-california-proposition-99-tobacco-control-program-synthetic-control.csv"

california_dataframe <- read.csv(california_data_path)

restricted <- c(3, 5, 6, 8, 15, 21, 22, 23, 29, 34, 36)

dataprep <- list(
  foo = california.dataframe,
  predictors = c("lnincome", "age15to24", "beer", "retprice"),
  special.predictors = list(
    list("cigsale", 1975, c("mean")),
    list("cigsale", 1980, c("mean")),
    list("cigsale", 1988, c("mean"))
  ),
  dependent = "cigsale",
  unit.variable = "state",
  time.variable = "year",
  treatment.identifier = 3,
  controls.identifier = 
    unique(california.dataframe$state[
      -which(california.dataframe$state %in% restricted)]),
  spillover.controls.identifier = setdiff(restricted, 3),
  time.predictors.prior = 1970:1988,
  time.optimize.ssr = 1970:1988,
  time.plot = 1970:2000,
  unit.names.variable = "state_name"
) 

runAllCaliforniaCov <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/California.txt")
runAllCaliforniaNoCov <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/noCovarCalifornia.txt")

timelineCal <- dataprep$time.plot
treat.timeCal <- max(dataprep$time.optimize.ssr)

counterfactualCal = as.vector(na.omit(
  dataprep$foo[dataprep$foo$state == dataprep$treatment.identifier, dataprep$dependent]))




library(foreign)
d <- read.dta("~/Documents/Minerva/Thesis/repgermany.dta")

dataprep2 <-
  list(
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
    controls.identifier = setdiff(unique(d$index), c(3,7)),
    spillover.controls.identifier = 3,
    time.predictors.prior = 1981:1990,
    time.optimize.ssr = 1960:1989,
    unit.names.variable = "country",
    time.plot = 1960:2003
  )

runAllGermanyCov <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/Germany.txt")
runAllGermanyNoCov <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/noCovarGermany.txt")

timelineGer <- dataprep2$time.plot
treat.timeGer <- max(dataprep2$time.optimize.ssr)

counterfactualGer = as.vector(na.omit(
  dataprep2$foo[dataprep2$foo$index == dataprep2$treatment.identifier, dataprep2$dependent]))





library(Synth)
data(basque)
# dataprep: prepare data for synth

basque1 = basque[basque$year > 1959,]

dataprep.out <-
  list(
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
    ,controls.identifier = c(2:6, 9:15)
    ,spillover.controls.identifier = c(7, 8, 16, 18)
    ,time.predictors.prior = c(1964:1969)
    ,time.optimize.ssr = c(1960:1969)
    ,unit.names.variable = c("regionname")
    ,time.plot = c(1960:1997)
  )

runAllBasqueCov <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/Basque.txt")
runAllBasqueNoCov <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/noCovarBasque.txt")


timelineBas <- dataprep.out$time.plot
treat.timeBas <- max(dataprep.out$time.optimize.ssr)

counterfactualBas = as.vector(na.omit(
  dataprep.out$foo[dataprep.out$foo$regionno == dataprep.out$treatment.identifier, 
                   dataprep.out$dependent]))






dataPoland <- read.csv("~/Documents/Minerva/Thesis/capstone_R_project--main/CP Dataset  - total_dataset - Copy of CP Datasets  - total_dataset.csv", 
                       header=TRUE)

dataPoland = dataPoland[dataPoland$year != 2021,]

# Define the treated unit, donor pool, and treated year
treated_unit <- 7  # Replace with the name/code of Poland in your dataset
# Review and adjust any country_removed variables if necessary

# Create the donor pool while excluding the country to be removed
donor_pool <- unique(dataPoland$region[dataPoland$region != treated_unit])

sp_pool <- c(1, 2, 4, 6, 9, 10, 11, 15, 16, 17, 18, 19)

donor_pool = setdiff(donor_pool, sp_pool)

# Pretend the intervention happened in 2014 (placebo test)
pretend_treated_year <- 2014

# Review and adjust the selection of covariate variables
covariate_matrix <- dataPoland[dataPoland$year < pretend_treated_year, c("gdp", "inf", "pop", "unemp")]
column_datatype <- class(dataPoland$region)

dataprep <- list(
  foo = dataPoland,
  predictors = c("migration", "gdp", "inf", "pop", "unemp"),  # Review and adjust the predictors
  dependent = "migration",  # Verify that the outcome variable is correctly specified
  unit.variable = "region",  # Verify the variable representing countries/regions
  time.variable = "year",
  treatment.identifier = treated_unit,
  controls.identifier = donor_pool,
  spillover.controls.identifier = sp_pool,
  time.predictors.prior = 2000:2014,  # Adjust the time range for the placebo test
  time.optimize.ssr = 2000:2014,  # Adjust the time range for the placebo test
  unit.names.variable = "country",
  time.plot = 2000:2020  # Adjust the time range for the placebo test
)



runAllPolandCov <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/Poland.txt")
runAllPolandNoCov <- yaml.load_file("~/Documents/Minerva/Thesis/Empirical Results/noCovarPoland.txt")


timelinePol <- dataprep$time.plot
treat.timePol <- max(dataprep$time.optimize.ssr)

counterfactualPol = as.vector(na.omit(
  dataprep$foo[dataprep$foo$region == dataprep$treatment.identifier, dataprep$dependent]))



quadPlot(runAllCaliforniaNoCov, runAllCaliforniaCov, counterfactualCal, 
         timelineCal, treat.timeCal, 
         "year",
         "per-capita cig. sales (in packs)",
         "gap in per-capita cig. sales (in packs)",
         "Trends (No Covariates)", "Gaps (No Covariates)",
         "Trends (With Covariates)", "Gaps (With Covariates)",
         "bottomleft")



quadPlot(runAllGermanyNoCov, runAllGermanyCov, counterfactualGer, 
         timelineGer, treat.timeGer, 
         "year",
         "Per Capita GDP (PPP, 2002 USD)",
         "Gap in Per Capita GDP (PPP, 2002 USD)",
         "Trends (No Covariates)", "Gaps (No Covariates)",
         "Trends (With Covariates)", "Gaps (With Covariates)",
         "left")


quadPlot(runAllBasqueNoCov, runAllBasqueCov, counterfactualBas, 
         timelineBas, treat.timeBas, 
         "year",
         "Per Capita GDP (1986 USD, thousand)",
         "Gap in Per Capita GDP (1986 USD, thousand)",
         "Trends (No Covariates)", "Gaps (No Covariates)",
         "Trends (With Covariates)", "Gaps (With Covariates)",
         "topleft", "bottomleft")


quadPlot(runAllPolandNoCov, runAllPolandCov, counterfactualPol, 
         timelinePol, treat.timePol, 
         "year",
         "Annual Migration",
         "Gap in Annual Migration",
         "Trends (No Covariates)", "Gaps (No Covariates)",
         "Trends (With Covariates)", "Gaps (With Covariates)",
         "topleft")
