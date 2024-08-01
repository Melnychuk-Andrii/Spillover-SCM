library(MASS) 
library("future")
library(furrr)
library(yaml)
library(kernlab)

runIterativeSCM = function(dataprepDf, replacePreTreatData = FALSE, waterfallMode = TRUE) {
  
  pre.treatment.period <- dataprepDf$pre.treat.period
  
  dataprep.final <- dataprepDf
  
  cleaned.controls <- c()
  
  if (dataprepDf$sp.count >= 1)
  {
    for (i in 1:dataprepDf$sp.count)
    {
      current.sp.control <- i + 1
  
      if (waterfallMode == FALSE)
      {
        
        dataprep.current <- replaceSpAndTreated(dataprepDf, current.sp.control)
        dataprep.current = removeSpUnits(dataprep.current, 2:(dataprep.current$sp.count + 1))
        
      } else
      {
        
        dataprep.current <- replaceSpAndTreated(dataprep.final, current.sp.control)
        
        to.be.removed <- setdiff(2:(dataprep.current$sp.count + 1), cleaned.controls)
        dataprep.current = removeSpUnits(dataprep.current, to.be.removed)
    
      }
  
      synth.current <- runSCM(dataprep.current)
  
      cur.control.estimates <- synth.current$actual
  
      if (replacePreTreatData == FALSE)
      {
        cur.control.estimates[pre.treatment.period] = dataprep.current$Y0[,1]
      } 
      
      dataprep.final$foo[,current.sp.control] = cur.control.estimates
      dataprep.final$Y0[,current.sp.control] = cur.control.estimates[pre.treatment.period]
      
      if (length(setdiff(dataprepDf$full.period, dataprepDf$pre.treat.period)) == 1)
      {
        dataprep.final$Y1[current.sp.control] = cur.control.estimates[-pre.treatment.period]
      } else {
        dataprep.final$Y1[,current.sp.control] = cur.control.estimates[-pre.treatment.period]
      }
  
      cleaned.controls[i] = current.sp.control
    }
  }
  
  return(runSCM(dataprep.final))
}

runInclusiveSCM = function(dataprepDf) {
  
  number.of.time.units <- length(dataprepDf$full.period)
  
  main.control.estimates <- runSCM(dataprepDf)
  
  Omega <- matrix(nrow = dataprepDf$sp.count + 1, ncol = dataprepDf$sp.count + 1)
  B <- matrix(nrow = dataprepDf$sp.count + 1, ncol = number.of.time.units)
  
  Omega[1,] = append(1, - main.control.estimates$weights[2:(dataprepDf$sp.count + 1)])
  
  B[1,] = main.control.estimates$gap
  
  for (i in 1:dataprepDf$sp.count)
  {
    current.sp.control <- i + 1
    
    dataprep.current <- replaceSpAndTreated(dataprepDf, current.sp.control)
    
    cur.control.estimates <- runSCM(dataprep.current)
    
    B[i+1,] = cur.control.estimates$gap
    
    Omega[i+1, 1] = - cur.control.estimates$weights[current.sp.control]
    
    k <- 2
    
    for (j in 2:(dataprepDf$sp.count + 1))
    {
      if (j == current.sp.control)
      {
        Omega[i+1, k] = 1
      }else
      {
        Omega[i+1, k] = - cur.control.estimates$weights[j]
      }
      k = k + 1
    }
    
  }
  
  result.gap <- main.control.estimates$gap
  
  det.Omega <- det(Omega)
  
  post.treat.period <- setdiff(dataprepDf$full.period, 
                                   dataprepDf$pre.treat.period)
  first.post.treat.year <- min(post.treat.period)
  
  for (i in first.post.treat.year:number.of.time.units)
  {
    Omega.w.rep <- Omega
    Omega.w.rep[,1] = B[,i]
    
    det.Omega.w.rep <- det(Omega.w.rep)
    
    result.gap[i] = det.Omega.w.rep/det.Omega
  }
  
  result.actual <- dataprepDf$foo[,1] - result.gap
  
  result <- list(gap = result.gap, actual = result.actual)
  
  return(result)
}

runRestrictiveSCM = function(dataprepDf) {
  cur.dataprepDf <- dataprepDf
  
  if (dataprepDf$sp.count > 0) {
    cur.dataprepDf <- removeSpUnits(dataprepDf, 2:(dataprepDf$sp.count + 1))
  }
  
  return(runSCM(cur.dataprepDf))
}

replaceSpAndTreated = function(dataprepDf, current.sp.control) {
  
  updated.dataprepDf = dataprepDf
  
  updated.dataprepDf$foo[,1] = dataprepDf$foo[,current.sp.control]
  updated.dataprepDf$foo[,current.sp.control] = dataprepDf$foo[,1]
  
  updated.dataprepDf$Y0[,1] = dataprepDf$Y0[,current.sp.control]
  updated.dataprepDf$Y0[,current.sp.control] = dataprepDf$Y0[,1]
  
  if (length(setdiff(dataprepDf$full.period, dataprepDf$pre.treat.period)) == 1)
  {
    updated.dataprepDf$Y1[1] = dataprepDf$Y1[current.sp.control]
    updated.dataprepDf$Y1[current.sp.control] = dataprepDf$Y1[1]
  }else {
    updated.dataprepDf$Y1[,1] = dataprepDf$Y1[,current.sp.control]
    updated.dataprepDf$Y1[,current.sp.control] = dataprepDf$Y1[,1]
  }
  
  return(updated.dataprepDf)
}

removeSpUnits = function(dataprepDf, units) {
  
  updated.dataprepDf = dataprepDf
  
  updated.dataprepDf$foo = updated.dataprepDf$foo[,-units]
  updated.dataprepDf$Y0 = updated.dataprepDf$Y0[,-units]
  
  if (length(setdiff(dataprepDf$full.period, dataprepDf$pre.treat.period)) == 1)
  {
    updated.dataprepDf$Y1 = updated.dataprepDf$Y1[-units]
  } else {
    updated.dataprepDf$Y1 = updated.dataprepDf$Y1[,-units]
  }
  
  updated.dataprepDf$sp.count = updated.dataprepDf$sp.count - length(units)
  updated.dataprepDf$total.ctrl.count = updated.dataprepDf$total.ctrl.count - length(units)
  
  n = updated.dataprepDf$total.ctrl.count + 1
  ind = c(rep(1, 1 + updated.dataprepDf$sp.count), 
          rep(0, updated.dataprepDf$total.ctrl.count - updated.dataprepDf$sp.count))
  A = diag(n)
  A = A[,which(!!ind)]
  
  return(updated.dataprepDf)
}


runSCM = function(dataprepDf) {
  
  scm.out.singe.iter = scm_weights(dataprepDf$Y0)
  
  scm.out.actual <- scm.out.singe.iter$a_hat +scm.out.singe.iter$b_hat%*%t(dataprepDf$foo)
  
  scm.out.gap <- dataprepDf$foo[,1] - scm.out.actual
  
  return(list(gap = scm.out.gap, 
              actual = scm.out.actual, 
              weights = scm.out.singe.iter$b_hat))
  
}

runRegularSCM = runSCM


runSP.SCM = function(dataprepDf) {
  
  scm.out = scm_batch_weights(dataprepDf$Y0)
  
  loo = F
  normal = F
  
  return(sp_andrews_te(dataprepDf$foo, dataprepDf$Y0,dataprepDf$Y1,dataprepDf$A,
                       loo,normal, 
                       0.05,
                       length(setdiff(dataprepDf$full.period, dataprepDf$pre.treat.period)),
                       scm.out$A_hat, 
                       scm.out$B_hat))
}

runAllMethods = function(dataprepDf, 
                         params = list(replacePreTreatData = FALSE, 
                                       waterfallMode = TRUE)) {
  
  #iterative.FT <- runIterativeSCM(dataprepDf, FALSE, TRUE)
  #iterative.FF <- runIterativeSCM(dataprepDf, FALSE, FALSE)
  iterative.TT %<-% runIterativeSCM(dataprepDf, TRUE, TRUE)
  #iterative.TF <- runIterativeSCM(dataprepDf, TRUE, FALSE)
  
  inclusive %<-% runInclusiveSCM(dataprepDf)
  restrictive %<-% runRestrictiveSCM(dataprepDf)
  regular %<-% runRegularSCM(dataprepDf)
  sp.model %<-% runSP.SCM(dataprepDf)
  
  result <- list(#iterative.FT = iterative.FT,
                 #iterative.FF = iterative.FF,
                 iterative.TT = iterative.TT,
                 #iterative.TF = iterative.TF,
                 inclusive = inclusive,
                 restrictive = restrictive,
                 regular = regular,
                 sp.model = sp.model)
  
  return(result)
}

raw.post.prediction.gaps = function(nRuns, dataprepDfList, resultList) {
  
  gaps.all <- list()

  gaps.all$iterative.FT <- c()
  gaps.all$iterative.FF <- c()
  gaps.all$iterative.TT <- c()
  gaps.all$iterative.TF <- c()
  
  gaps.all$inclusive <- c()
  gaps.all$restrictive <- c()
  gaps.all$sp.model <- c()
  gaps.all$regular <- c()
  
  for (i in 1:nRuns)
  {
    cur.df <- dataprepDfList[[paste0("run",i)]]
    cur.res <- resultList[[paste0("run",i)]]
    
    real.y0 <- cur.df$y0[-cur.df$pre.treat.period, 1]
    
    gaps.all$iterative.FT = 
      append(gaps.all$iterative.FT, 
             cur.res$iterative.FT$actual[-cur.df$pre.treat.period] - real.y0)
    gaps.all$iterative.FF = 
      append(gaps.all$iterative.FF, 
             cur.res$iterative.FF$actual[-cur.df$pre.treat.period] - real.y0)
    gaps.all$iterative.TT = 
      append(gaps.all$iterative.TT, 
             cur.res$iterative.TT$actual[-cur.df$pre.treat.period] - real.y0)
    gaps.all$iterative.TF = 
      append(gaps.all$iterative.TF, 
             cur.res$iterative.TF$actual[-cur.df$pre.treat.period] - real.y0)
    
    gaps.all$regular = 
      append(gaps.all$regular, 
             cur.res$regular$actual[-cur.df$pre.treat.period] - real.y0)
    gaps.all$inclusive = 
      append(gaps.all$inclusive, 
             cur.res$inclusive$actual[-cur.df$pre.treat.period] - real.y0)
    gaps.all$restrictive = 
      append(gaps.all$restrictive, 
             cur.res$restrictive$actual[-cur.df$pre.treat.period] - real.y0)
    gaps.all$sp.model = 
      append(gaps.all$sp.model, 
             cur.res$sp.model$actual[-cur.df$pre.treat.period] - real.y0)
  }
  
  return(gaps.all)
}

runMultipleConfigs = function(nRuns, 
                              pre.treat.lengths,
                              total.unit.counts,
                              sp.unit.ratios,
                              treat.effects,
                              sp.effect.ratios,
                              modes)
{
  
  cur.file <- 1
  full.length <- length(total.unit.counts) *
                  length(pre.treat.lengths) *
                  length(sp.unit.ratios) *
                  length(treat.effects) *
                  length(sp.effect.ratios) *
                  length(modes) 
  
  configList <- setNames(as.list(1:full.length), paste0("permutation", 1:full.length))
  
  for (pre.treat.length in pre.treat.lengths) {
    for (total.unit.count in total.unit.counts){
      for (sp.unit.ratio in sp.unit.ratios) {
        for (treat.effect in treat.effects) {
          for (sp.effect.ratio in sp.effect.ratios){
            for (mode in modes){
              
              sp.count <- round( (total.unit.count - 1) * sp.unit.ratio)
              file.name <-  paste(as.character(cur.file), ".txt", sep = "")
              
              if (!file.exists(file.name) & 
                  pre.treat.length != 50 & 
                  total.unit.count != 30)
              {
                
                cat("Will do ", file.name, "\n")
                
                params.cur <- list(
                  nRuns = nRuns,
                  pre.treat.period = 1:pre.treat.length,
                  full.period = 1:(pre.treat.length + 1),
                  sp.count = sp.count, 
                  total.ctrl.count = total.unit.count - 1,
                  treat.eff = treat.effect,
                  sp.eff = treat.effect * sp.effect.ratio, 
                  mode = mode,
                  file.name = file.name
                )
                
                configList[[paste0("permutation",cur.file)]] = params.cur
              }
              
              cur.file = cur.file + 1
              
            }
          }
        }
      }
    }
  }
  

  runSingleToFileOnConfigList(configList)
  
}


runSingleToFileOnConfigList = function(configList) {
  
  future_map(configList, runSingleConfigToFile, .options = furrr_options(seed = TRUE))
  
}

runSingleConfigToFile = function(params) {
  
  if ("file.name" %in% names(params)) {
      
    result <- runSingleConfig(params)
    cat(as.yaml(result), file=params$file.name)
    
  }
  
}

runSingleConfig = function(params = list(nRuns, 
                                         pre.treat.period,
                                         full.period,
                                         sp.count, 
                                         total.ctrl.count,
                                         treat.eff,
                                         sp.eff, 
                                         mode))
{
  dataList <- generateDataprepDfList(
    nRuns = params$nRuns,
    pre.treat.period = params$pre.treat.period,
    full.period = params$full.period,
    sp.count = params$sp.count,
    total.ctrl.count = params$total.ctrl.count,
    treat.eff = params$treat.eff,
    sp.eff = params$sp.eff,
    mode = params$mode
  )
  
  resList <- runAllMethodsOnList(params$nRuns, dataList)
  
  predictionGaps <- raw.post.prediction.gaps(params$nRuns, dataList, resList)
  
  return(list(setup = params, gaps = predictionGaps))
}

runAllMethodsOnList = function(nRuns, dataprepDfList, 
                               params = list(replacePreTreatData = FALSE, 
                                             waterfallMode = TRUE)) {
  
  # result <- future_map(dataprepDfList, runAllMethods)
  
  result <- lapply(dataprepDfList, runAllMethods)
  
  
  return(result)
}

generateRandomDataprepDfList = function(nRuns = 10, 
                                  pre.treat.period = 10, 
                                  full.period = 10, 
                                  sp.count = 5, 
                                  total.ctrl.count = 5, 
                                  predictor.count = 0, 
                                  treat.eff = 2,
                                  sp.eff = 3, 
                                  mode = "I(1)") {
  
  dataprepDfList <- setNames(as.list(1:nRuns), paste0("run", 1:nRuns))
  
  for (i in 1:nRuns)
  {
    cur.sp <- 4 + round(sp.count*runif(1))
    last.pre.treat <- 2 + round(runif(1)*pre.treat.period)
    cur.sp.eff <- runif(1)*sp.eff
    
    currentDf <- generateDataprepDf(1:last.pre.treat,
                                    1:(last.pre.treat + 1 + round(runif(1)*full.period)),
                                    cur.sp,
                                    cur.sp + 1 + round(total.ctrl.count*runif(1)),
                                    predictor.count,
                                    cur.sp.eff + runif(1)*treat.eff,
                                    cur.sp.eff,
                                    mode)
    
    dataprepDfList[[paste0("run",i)]] = currentDf
  }
  
  return(dataprepDfList)
}

generateDataprepDfList = function(nRuns = 10, 
                                  pre.treat.period = 1:2, 
                                  full.period = 1:4, 
                                  sp.count = 2, 
                                  total.ctrl.count = 4, 
                                  predictor.count = 0, 
                                  treat.eff = 5,
                                  sp.eff = 3, 
                                  mode = "I(1)") {
  
  dataprepDfList <- setNames(as.list(1:nRuns), paste0("run", 1:nRuns))
  
  for (i in 1:nRuns)
  {
    currentDf <- generateDataprepDf(pre.treat.period,
                                    full.period,
                                    sp.count,
                                    total.ctrl.count,
                                    predictor.count,
                                    treat.eff,
                                    sp.eff,
                                    mode)
    
    dataprepDfList[[paste0("run",i)]] = currentDf
  }
  
  return(dataprepDfList)
}

generateDataprepDf = function(pre.treat.period = 1:2, 
                              full.period = 1:4, 
                              sp.count = 2, 
                              total.ctrl.count = 4, 
                              predictor.count = 0, 
                              treat.eff = 5,
                              sp.eff = 3, 
                              mode = "I(1)") {
  
  dep.data.all <- generateDependentData(pre.treat.period, full.period, sp.count,
                                 total.ctrl.count, predictor.count, treat.eff,
                                 sp.eff, mode)
  
  dep.data <- dep.data.all$dependentData
  
  n = total.ctrl.count + 1
  ind = c(rep(1, 1 + sp.count), rep(0,total.ctrl.count - sp.count))
  A = diag(n)
  A = A[,which(!!ind)]
  
  Y0 = dep.data[pre.treat.period,]
  Y1 = dep.data[setdiff(full.period, pre.treat.period),]
  
  dataprep <- list(
    foo = dep.data,
    pre.treat.period = pre.treat.period,
    full.period = full.period,
    sp.count = sp.count,
    total.ctrl.count = total.ctrl.count,
    Y0 = Y0, Y1 = Y1, A = A, y0 = dep.data.all$y0
  ) 
  
  return(dataprep)
}


generateDependentData = function(pre.treat.period = 1:2, 
                                 full.period = 1:4, 
                                 sp.count = 2, 
                                 total.ctrl.count = 4, 
                                 predictor.count = 0, 
                                 treat.eff = 5,
                                 sp.eff = 3, 
                                 mode = "I(1)") {
  
  if (mode == "Stationary")
  {
  
    m <- matrix(runif((total.ctrl.count + 1)*3), 
                nrow = total.ctrl.count + 1, ncol = 3)
    
    e <- matrix(rnorm((total.ctrl.count + 1)*length(full.period)), 
                      nrow = total.ctrl.count + 1, ncol = length(full.period))
    
    v <- matrix(rnorm(4*length(full.period)), 
                nrow = 4, ncol = length(full.period))
    
    y <- matrix(nrow = total.ctrl.count + 1, ncol = length(full.period))
    y0 <- matrix(nrow = total.ctrl.count + 1, ncol = length(full.period))
    
    lambda <- matrix(nrow = 3, ncol = length(full.period))
    
    n <- c()
    
    for (t in 1:length(full.period))
    {
      
      n[t] = 1 + 0.5*ifelse(t==1, 0, n[t-1]) + v[1,t]
      
      lambda[1,t] = 0.5*ifelse(t==1, 0, lambda[1,t-1]) + v[2,t]
      
      lambda[2,t] = 1 + 0.5*ifelse(t==1, 0,  v[3,t-1]) + v[3,t]
      
      lambda[3,t] = 0.5*ifelse(t==1, 0, lambda[3,t-1]) + v[4,t] + 0.5*ifelse(t==1, 0, v[4,t-1])
      
      for (i in 1:(total.ctrl.count + 1))
      {
        
        y[i,t] = n[t] + sum(lambda[,t]*m[i,]) + e[i,t]
        y0[i,t] = n[t] + sum(lambda[,t]*m[i,]) + e[i,t]
        
        if (t > max(pre.treat.period)) {
          
          if (i == 1) {
            
            y[i,t] = y[i,t] + treat.eff
              
          } else if (i %in% 2:(sp.count + 1)) {
            
            y[i,t] = y[i,t] + sp.eff
            
          }
          
        }
        
      }
    }
    
  } else if(mode == "I(1)") {
    
    m <- matrix(runif((total.ctrl.count + 1)*3), 
                nrow = total.ctrl.count + 1, ncol = 3)
    
    m[1,] = c(1,0,0)
    m[2,] = c(0,1,0)
    m[3,] = c(1,0,0)
    m[4,] = c(0,1,0)
    
    for (i in 5:(total.ctrl.count + 1))
    {
      m[i,] = m[i,]/sum(m[i,])
    }
    
    e <- matrix(rnorm((total.ctrl.count + 1)*length(full.period)), 
                nrow = total.ctrl.count + 1, ncol = length(full.period))
    
    v <- matrix(rnorm(3*length(full.period)), 
                nrow = 3, ncol = length(full.period))
    
    
    y <- matrix(nrow = total.ctrl.count + 1, ncol = length(full.period))
    y0 <- matrix(nrow = total.ctrl.count + 1, ncol = length(full.period))
    
    lambda <- matrix(nrow = 3, ncol = length(full.period))
    
    for (t in 1:length(full.period))
    {
      
      lambda[1,t] = ifelse(t==1, 0, lambda[1,t-1]) + 0.5*v[1,t]
      
      lambda[2,t] = ifelse(t==1, 0, lambda[2,t-1]) + 0.5*v[2,t]
      
      lambda[3,t] = 0.5*ifelse(t==1, 0, lambda[3,t-1]) + v[3,t]
      
      for (i in 1:(total.ctrl.count + 1))
      {
        
        y[i,t] = sum(lambda[,t]*m[i,]) + e[i,t]
        y0[i,t] = sum(lambda[,t]*m[i,]) + e[i,t]
        
        if (t > max(pre.treat.period)) {
          
          if (i == 1) {
            
            y[i,t] = y[i,t] + treat.eff
            
          } else if (i %in% 2:(sp.count + 1)) {
            
            y[i,t] = y[i,t] + sp.eff
            
          }
          
        }
          
      }
    }
    
  } else if(mode == "Custom") {
    
    m <- matrix(runif((total.ctrl.count + 1)*3), 
                nrow = total.ctrl.count + 1, ncol = 3)
    
    for (i in 1:(total.ctrl.count + 1))
    {
      m[i,] = m[i,]/sum(m[i,])
    }
    
    e <- matrix(rnorm((total.ctrl.count + 1)*length(full.period)), 
                nrow = total.ctrl.count + 1, ncol = length(full.period))
    
    v <- matrix(rnorm(3*length(full.period)), 
                nrow = 3, ncol = length(full.period))
    
    n.effects <- 5 
    
    my.effects <- ifelse(runif(1, min = 0, max = 1) >= 0.5, 1, -1)* 
      rnorm(n.effects, mean = sp.eff, sd = treat.eff - sp.eff)
    
    my.effects.time <- matrix(rep(0, length(full.period)*n.effects), 
                                  nrow = n.effects, ncol = length(full.period))
    
    for (i in 1:n.effects) {
      
      duration <- round(runif(1, min = 1, max = max(full.period)))
      start <- round(runif(1, min = 1, max = max(full.period) - duration + 1))
      
      k = start
      for (j in 1:duration) {
        my.effects.time[i,k] = 1
        k = k + 1
      }
    }
    
    my.effects.space <- matrix(round(runif(n.effects*(total.ctrl.count + 1))), 
                               nrow = n.effects, ncol = (total.ctrl.count + 1))
    
    y <- matrix(nrow = total.ctrl.count + 1, ncol = length(full.period))
    y0 <- matrix(nrow = total.ctrl.count + 1, ncol = length(full.period))
    
    lambda <- matrix(nrow = 3, ncol = length(full.period))
    
    for (t in 1:length(full.period))
    {
      
      lambda[1,t] = ifelse(t==1, 0, lambda[1,t-1]) + 0.5*v[1,t]
      
      lambda[2,t] = ifelse(t==1, 0, lambda[2,t-1]) + 0.5*v[2,t]
      
      lambda[3,t] = 0.5*ifelse(t==1, 0, lambda[3,t-1]) + v[3,t]
      
      for (i in 1:(total.ctrl.count + 1))
      {
        
        y[i,t] = sum(lambda[,t]*m[i,]) + e[i,t]
        
        my.effects.current <- 0
        
        for (j in 1:n.effects) {
          my.effects.current = my.effects.current + 
            my.effects[j] * my.effects.time[j,t] * my.effects.space[j,i]
        }
        
        y[i,t] = y[i,t] + my.effects.current
        
        y0[i,t] = y[i,t]
        
        if (t > max(pre.treat.period)) {
          
          if (i == 1) {
            
            y[i,t] = y[i,t] + treat.eff
            
          } else if (i %in% 2:(sp.count + 1)) {
            
            y[i,t] = y[i,t] + sp.eff
            
          }
          
        }
        
      }
    }
    
  }
  
  dependentData <- t(y)
  colnames(dependentData) = as.character(1:(total.ctrl.count + 1))
  rownames(dependentData) = as.character(full.period)
  
  return(list(dependentData = dependentData, y0 = t(y0)))
}

#TODO: test ipop with 1 unit

scm_weights = function(Y,treat.col=1) {
  # Takes a Matrix of outcomes, Y,
  # with columns representing units and rows representing times
  # Where (by default) the first column is the treated unit,
  # and (by necessity), none of the rows represents treatment yet.
  # Returns weights and intercept.
  
  #Checking input
  if (!is.matrix(Y)) Y = as.matrix(Y)
  #Grabbing dimensions
  t = nrow(Y)
  n = ncol(Y)
  
  #Splitting into treatment, control, as well as demeaning
  Y_treated = Y[,treat.col]
  Y_untreated = Y[,-treat.col]
  
  means = colMeans(Y)
  demeaned = t(apply(Y,1,function(x) x-means))
  Y_demeaned = demeaned[,treat.col]
  X_demeaned = demeaned[,-treat.col]
  
  res.weights <- c()
  
  if ((n - 1) == 1) {
    
    Q = function(b) sum((Y_demeaned - (X_demeaned*b))^2)
    
    b_init1 = rep(1,n-1)/(n-1)
    #Criterion function
    A_eq1 = rbind(rbind(matrix(1,ncol=n-1), matrix(-1,ncol=n-1)), diag(n-1))
    B_eq1 = c(c(0.999, -1.001), rep(0,n-1))
    
    opt = constrOptim(b_init1,Q,grad=NULL,ui=A_eq1,ci=B_eq1,
                      outer.iterations = 1000, outer.eps = 1e-10,
                      method = "Nelder-Mead",
                      control = list(maxit = 20000, reltol = 1e-13)) 
    
    res.weights = opt$par
    
  } else {
    
    Margin.ipop = 0.0005
    Sigf.ipop = 5
    Bound.ipop = 10
    
    H <- 2 * (t(X_demeaned) %*% X_demeaned)
    c <-  -2 * (t(Y_demeaned) %*% X_demeaned)
    
    A <- t(rep(1, length(c)))
    b <- 1
    l <- rep(0, length(c))
    u <- rep(1, length(c))
    r <- 0
    
    res.weights <- c()
    
    tryCatch( 
      { res.weights = as.matrix(primal(ipop(c = c, H = H, A = A, b = b, l = l, u = u, r = r,
                                    margin = Margin.ipop, maxiter = 1000, sigf = Sigf.ipop, bound = Bound.ipop)))[,1] }
      , error = function(e) {})
    
    if(length(res.weights) == 0){
      
      tryCatch( 
        { 
          H =  H + (diag(n-1)*0.0000001)
          res.weights = as.matrix(primal(ipop(c = c, H = H, A = A, b = b, l = l, u = u, r = r,
                                              margin = Margin.ipop, maxiter = 1000, sigf = Sigf.ipop, bound = Bound.ipop)))[,1] }
        , error = function(e) {})
      
    }
    
    if(length(res.weights) == 0){
      
      #Optimization
      #
      #Initial guess -- diff-in-diff weights (ish)
      b_init = rep(1,n-1)/n
      #Criterion function
      Q = function(b) sum((Y_demeaned - (X_demeaned%*%b)[,1])^2)
      #Constraints
      A_eq = rbind(matrix(-1,ncol=n-1),diag(n-1)) #ui
      B_eq = c(-1,rep(0,n-1)) #ci
      
      res.weights = constrOptim(b_init,Q,grad=NULL,ui=A_eq,ci=B_eq,
                        outer.iterations = 1000, outer.eps = 1e-10,
                        method = "Nelder-Mead",
                        control = list(maxit = 20000, reltol = 1e-13))$par
    }
    
  }
  
  b_hat = rep(0,n)
  b_hat[-treat.col] = res.weights
  
  if ((n - 1) == 1) {
    a_hat = means[treat.col]-means*b_hat
    return(list(a_hat=a_hat[1],b_hat=b_hat))
  } else {
    a_hat = means[treat.col]-means%*%b_hat
    return(list(a_hat=a_hat[1,1],b_hat=b_hat))
  }
}

scm_batch_weights = function(Y) {
  #Takes a matrix Y
  #Where columns are units
  #and rows are time periods
  #Runs SCM, taking each column as the column to be predicted in turn.
  #Returns vector of intercepts and matrix of weights (0 on diag).
  n = ncol(Y)
  #Apply scm to each column.
  listed = lapply(1:n,
                  function(i) scm_weights(Y,treat.col=i))
  #Unlist outputs. Make each row the weights for one unit.
  B_hat = t(sapply(listed,function(x) x$b_hat))
  A_hat = sapply(listed,function(x) x$a_hat)
  return(list(A_hat=A_hat,B_hat=B_hat))
}

sp_andrews_te = function(Y,Y0,Y1,A,loo=F,Normal=F,alpha_sig=0.05, s, A_hat, B_hat) {

  n = ncol(Y0)
  t = nrow(Y0)
  
  nA <- ncol(A)
  
  i_less_B = diag(n)-B_hat
  M_hat = t(i_less_B)%*%(i_less_B)
  
  AMA_inv = 0
  
  tryCatch( 
    { 
      AMA_inv = solve(t(A)%*%M_hat%*%A)
    }, error = function(e) {})
  
  if (length(AMA_inv) == 1){
    tryCatch( 
      { 
        AMA_inv = solve(t(A)%*%M_hat%*%A + (diag(nA)*0.0000001))
      }, error = function(e) {})
  }
  
  if (length(AMA_inv) == 1){
    return(list(actual = rep(0, t + s), gap = rep(0, t + s)))
  }
  
  synthetic_control_sp_alt <- A_hat[1]+B_hat[1,]%*%t(Y)

  gamma_hat_vector <- Y[,1] - synthetic_control_sp_alt
  
  if (s == 1) {
    Y_Ts = Y1

    gamma_hat = AMA_inv%*%(t(A)%*%t(i_less_B)%*%(i_less_B%*%Y_Ts-A_hat))

    synthetic_control_sp_alt[t+1] = Y[1 + t,1] - gamma_hat[1]

    gamma_hat_vector[1] = gamma_hat[1]

  } else {
  for (i in 1:s) {
    Y_Ts = Y1[i,]

    gamma_hat = AMA_inv%*%(t(A)%*%t(i_less_B)%*%(i_less_B%*%Y_Ts-A_hat))

    synthetic_control_sp_alt[t+i] = Y[i + t,1] - gamma_hat[1]

    gamma_hat_vector[i + t] = gamma_hat[1]
  }
  }
  
  return(list(actual = synthetic_control_sp_alt, gap = gamma_hat_vector))
}