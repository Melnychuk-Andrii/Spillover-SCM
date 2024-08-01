
#---------------------------------------------
# PLOT FUNCTIONS
#---------------------------------------------

quadPlot <- function(results1, results2, counterfactual, timeline, 
                     treat.time, xlab, ylab1, ylab2, main1, main2,
                     main3, main4, leg.pos, leg.pos2 = NA)
{
  par(mfrow=c(2,2))
  
  plotAllMethodsBothPlots(results1, counterfactual, timeline, 
                          treat.time, xlab, ylab1, ylab2, main1, main2, 
                          leg.pos, leg.pos2)
  
  plotAllMethodsBothPlots(results2, counterfactual, timeline, 
                          treat.time, xlab, ylab1, ylab2, main3, main4, 
                          leg.pos, leg.pos2, TRUE)
  
}


plotAllMethodsBothPlots <- function(results, counterfactual, timeline, 
                                    treat.time, xlab, ylab1, ylab2, 
                                    main1, main2, 
                                    leg.pos, leg.pos2, is.cov = FALSE) {
  
  #par(mfrow=c(1,2))
  
  if (is.na(leg.pos2)) {
    leg.pos2 = leg.pos
  }
  
  plotAllMethods(results, counterfactual, timeline, treat.time, 
                 xlab, ylab1, main1, leg.pos, is.cov)
  plotAllMethodsGaps(results, counterfactual, timeline, treat.time, 
                     xlab, ylab2, main2, leg.pos2, is.cov)
}

plotAllMethods <- function(results, counterfactual, timeline, treat.time,
                           xlab, ylab, main, leg.pos, is.cov) {
  
  all.data.plotted <- c(results$iterative.TT$actual, results$regular$actual, 
                        results$sp.model$actual, results$restrictive$actual,
                        results$inclusive$actual, counterfactual)
  
  ylim <- c(min(all.data.plotted), max(all.data.plotted))
  lwd <- 1.5
  
  if (!is.cov) {
    lineCols = c("black", "green", "red", "blue", "purple", "orange")
    lineNames = c("Real data",
                  "Iterative SCM",
                  "Unrestricted SCM",
                  "SP Model",
                  "Restricted SCM",
                  "Inclusive SCM")
    ltys = c(1,2,3,2,4,3) #c(1, 2, 3, 4)
  } else {
    lineCols = c("black", "green", "red", "purple")
    lineNames = c("Real data",
                  "Iterative SCM",
                  "Unrestricted SCM",
                  "Restricted SCM")
    ltys = c(1,2,3,4) #c(1, 2, 3, 4)
  }
  
  plot(timeline, counterfactual, type = "l", lty = 1,lwd = lwd, ylim = ylim, col = "black",
       xlab = xlab, ylab = ylab, main = main)
  abline(v = treat.time, lty=2)
  abline(h = 0, lty=2)
  
  
  lines(timeline, results$iterative.TT$actual, type = "l", lty = 2, lwd = lwd,col = "green")
  
  lines(timeline, results$regular$actual, type = "l", lty = 3,lwd = lwd, col = "red")
  if (!is.cov) {
    lines(timeline, results$sp.model$actual, type = "l", lty = 2,lwd = lwd, col = "blue")
  }

  lines(timeline, results$restrictive$actual, type = "l", lty = 4,lwd = lwd, col = "purple")
  if (!is.cov) {
    lines(timeline, results$inclusive$actual, type = "l", lty = 3, lwd = lwd, col = "orange")
  }
  
  
  legend(leg.pos, 
         legend=lineNames,
         col=lineCols, lwd = lwd,
         lty = ltys, cex=0.8,
         box.lwd = 0,box.col = "white")
  
}

plotAllMethodsGaps <- function(results, counterfactual, timeline, treat.time,
                               xlab, ylab, main, leg.pos, is.cov) {
  
  all.data.plotted <- c(counterfactual - results$iterative.TT$actual, 
                        counterfactual - results$regular$actual, 
                        counterfactual - results$sp.model$actual, 
                        counterfactual - results$restrictive$actual,
                        counterfactual - results$inclusive$actual)
  
  ylim <- c(min(all.data.plotted), max(all.data.plotted))
  lwd <- 1.5
  if (!is.cov) {
    lineCols = c("green", "red", "blue", "purple", "orange")
    lineNames = c("Iterative SCM",
                  "Unrestricted SCM",
                  "SP Model",
                  "Restricted SCM",
                  "Inclusive SCM")
    ltys = c(2,3,2,4,3) #c(1, 2, 3, 4)
  } else {
    lineCols = c("green", "red", "purple")
    lineNames = c("Iterative SCM",
                  "Unrestricted SCM",
                  "Restricted SCM")
    ltys = c(2,3,4) #c(1, 2, 3, 4)
  }
  
  plot(timeline, counterfactual - counterfactual, type = "l", lty = 0, ylim = ylim,
       xlab = xlab, ylab = ylab, main = main)
  abline(v = treat.time, lty=2)
  abline(h = 0, lty=2)
  
  lines(timeline, results$iterative.TT$gap, type = "l", lty = 2,lwd = lwd, col = "green")
  
  lines(timeline, results$regular$gap, type = "l", lty = 3, lwd = lwd,col = "red")
  if (!is.cov) {
    lines(timeline, results$sp.model$gap, type = "l", lty = 2,lwd = lwd, col = "blue")
  }

  lines(timeline, results$restrictive$gap, type = "l", lty = 4,lwd = lwd, col = "purple")
  if (!is.cov) {
    lines(timeline, results$inclusive$gap, type = "l", lty = 3,lwd = lwd, col = "orange")
  }
  
  
  
  legend(leg.pos, 
         legend=lineNames,
         col=lineCols, lwd = lwd,
         lty = ltys, cex=0.8,
         box.lwd = 0,box.col = "white")
}




#---------------------------------------------
# COVARIATE FUNCTIONS
#---------------------------------------------


runAllMethodsWithCov = function(dataprepDf, 
                         params = list(replacePreTreatData = FALSE, 
                                       waterfallMode = TRUE)) {
  
  #iterative.FT <- runIterativeSCM(dataprepDf, FALSE, TRUE)
  #iterative.FF <- runIterativeSCM(dataprepDf, FALSE, FALSE)
  #iterative.TF <- runIterativeSCM(dataprepDf, TRUE, FALSE)
  
  iterative.TT %<-% runIterativeSCMwithCov(dataprepDf, TRUE, TRUE)

  inclusive %<-% runInclusiveSCMwithCov(dataprepDf)
  restrictive %<-% runRestrictiveSCMwithCov(dataprepDf)
  regular %<-% runRegularSCMwithCov(dataprepDf)
  sp.model %<-% runSP.SCMwithCov(dataprepDf)
  
  # iterative.TT <- runIterativeSCMwithCov(dataprepDf, TRUE, TRUE)
  # 
  # inclusive <- runInclusiveSCMwithCov(dataprepDf)
  # restrictive <- runRestrictiveSCMwithCov(dataprepDf)
  # regular <- runRegularSCMwithCov(dataprepDf)
  # sp.model <- runSP.SCMwithCov(dataprepDf)
  
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

runIterativeSCMwithCov = function(dataprepDf, replacePreTreatData = FALSE, waterfallMode = TRUE) {
  
  number.of.spillover.controls <- length(dataprepDf$spillover.controls.identifier)
  
  post.treatment.period <- setdiff(dataprepDf$time.plot, dataprepDf$time.optimize.ssr)
  pre.treatment.period <- setdiff(dataprepDf$time.plot, post.treatment.period)
  
  final.foo <- dataprepDf$foo
  
  cleaned.controls <- c()
  
  for (i in 1:number.of.spillover.controls)
  {
    current.sp.control <- dataprepDf$spillover.controls.identifier[i]
    
    if (waterfallMode == FALSE)
    {
      dataprep.current <- dataprep(
        foo = dataprepDf$foo,
        predictors = dataprepDf$predictors,
        special.predictors = dataprepDf$special.predictors,
        dependent = dataprepDf$dependent,
        unit.variable = dataprepDf$unit.variable,
        time.variable = dataprepDf$time.variable,
        treatment.identifier = current.sp.control,
        controls.identifier = sort(dataprepDf$controls.identifier),
        time.predictors.prior = dataprepDf$time.predictors.prior,
        time.optimize.ssr = dataprepDf$time.optimize.ssr,
        time.plot = dataprepDf$time.plot,
        unit.names.variable = dataprepDf$unit.names.variable
      )
    } else
    {
      dataprep.current <- dataprep(
        foo = final.foo,
        predictors = dataprepDf$predictors,
        special.predictors = dataprepDf$special.predictors,
        dependent = dataprepDf$dependent,
        unit.variable = dataprepDf$unit.variable,
        time.variable = dataprepDf$time.variable,
        treatment.identifier = current.sp.control,
        controls.identifier = sort(append(dataprepDf$controls.identifier,
                                          cleaned.controls)),
        time.predictors.prior = dataprepDf$time.predictors.prior,
        time.optimize.ssr = dataprepDf$time.optimize.ssr,
        time.plot = dataprepDf$time.plot,
        unit.names.variable = dataprepDf$unit.names.variable
      )
    }
    
    synth.current <- synth(
      data.prep.obj = dataprep.current
    )
    
    cur.control.estimates <- dataprep.current$Y0 %*% synth.current$solution.w
    names(cur.control.estimates) <- dataprepDf$time.plot
    
    if (replacePreTreatData == FALSE)
    {
      cur.control.estimates[as.character(pre.treatment.period)] = final.foo[
        final.foo[dataprepDf$unit.variable] == current.sp.control &
          final.foo[[dataprepDf$time.variable]] %in% pre.treatment.period,][[dataprepDf$dependent]]
    } 
    
    final.foo[final.foo[dataprepDf$unit.variable] == current.sp.control, 
    ][dataprepDf$dependent] = cur.control.estimates
    
    cleaned.controls[i] = current.sp.control
  }
  
  dataprep.final <- dataprep(
    foo = final.foo,
    predictors = dataprepDf$predictors,
    special.predictors = dataprepDf$special.predictors,
    dependent = dataprepDf$dependent,
    unit.variable = dataprepDf$unit.variable,
    time.variable = dataprepDf$time.variable,
    treatment.identifier = dataprepDf$treatment.identifier,
    controls.identifier = sort(append(dataprepDf$controls.identifier,
                                      dataprepDf$spillover.controls.identifier)),
    time.predictors.prior = dataprepDf$time.predictors.prior,
    time.optimize.ssr = dataprepDf$time.optimize.ssr,
    time.plot = dataprepDf$time.plot,
    unit.names.variable = dataprepDf$unit.names.variable
  )
  
  synth.final <- synth(
    data.prep.obj = dataprep.final
  )
  
  main.control.estimates <- dataprep.final$Y0 %*% synth.final$solution.w
  main.gap <- t(dataprepDf$foo[
    dataprepDf$foo[dataprepDf$unit.variable] == dataprepDf$treatment.identifier, 
  ][dataprepDf$dependent] - main.control.estimates)
  
  result <- list(dataprep = dataprep.final,
                 synth = synth.final,
                 actual = as.vector(t(main.control.estimates)),
                 gap = as.vector(main.gap))
  
  return(result)
}

runInclusiveSCMwithCov = function(dataprepDf) {
  
  number.of.spillover.controls <- length(dataprepDf$spillover.controls.identifier)
  
  number.of.time.units <- length(dataprepDf$time.plot)
  
  dataprep.main <- dataprep(
    foo = dataprepDf$foo,
    predictors = dataprepDf$predictors,
    special.predictors = dataprepDf$special.predictors,
    dependent = dataprepDf$dependent,
    unit.variable = dataprepDf$unit.variable,
    time.variable = dataprepDf$time.variable,
    treatment.identifier = dataprepDf$treatment.identifier,
    controls.identifier = sort(append(dataprepDf$controls.identifier,
                                      dataprepDf$spillover.controls.identifier)),
    time.predictors.prior = dataprepDf$time.predictors.prior,
    time.optimize.ssr = dataprepDf$time.optimize.ssr,
    time.plot = dataprepDf$time.plot,
    unit.names.variable = dataprepDf$unit.names.variable
  )
  
  synth.main <- synth(
    data.prep.obj = dataprep.main
  )
  
  main.control.estimates <- as.vector(dataprep.main$Y0 %*% synth.main$solution.w)
  
  Omega.cov <- matrix(nrow = number.of.spillover.controls + 1, ncol = number.of.spillover.controls + 1)
  B.cov <- matrix(nrow = number.of.spillover.controls + 1, ncol = number.of.time.units)
  
  Omega.cov[1,] = append(1, 
                         as.vector( 
                           - synth.main$solution.w[as.character(dataprepDf$spillover.controls.identifier),]
                           )
                         )
  
  B.cov[1,] = as.vector(dataprep.main$Y1plot) - main.control.estimates
  
  for (i in 1:number.of.spillover.controls)
  {
    current.sp.control = dataprepDf$spillover.controls.identifier[i]
    
    dataprep.current = dataprep(
      foo = dataprepDf$foo,
      predictors = dataprepDf$predictors,
      special.predictors = dataprepDf$special.predictors,
      dependent = dataprepDf$dependent,
      unit.variable = dataprepDf$unit.variable,
      time.variable = dataprepDf$time.variable,
      treatment.identifier = current.sp.control,
      controls.identifier = sort(append(dataprepDf$controls.identifier,
                                        append(
                                          dataprepDf$spillover.controls.identifier[-i],
                                          dataprepDf$treatment.identifier))),
      time.predictors.prior = dataprepDf$time.predictors.prior,
      time.optimize.ssr = dataprepDf$time.optimize.ssr,
      time.plot = dataprepDf$time.plot,
      unit.names.variable = dataprepDf$unit.names.variable
    )
    
    synth.current = synth(
      data.prep.obj = dataprep.current
    )
    
    cur.control.estimates = as.vector(dataprep.current$Y0 %*% synth.current$solution.w)
    
    B.cov[i+1,] = as.vector(dataprep.current$Y1) - cur.control.estimates
    
    Omega.cov[i+1, 1] = - synth.current$solution.w[as.character(dataprepDf$treatment.identifier),]
    
    k <- 2
    
    for (j in dataprepDf$spillover.controls.identifier)
    {
      if (j == current.sp.control)
      {
        Omega.cov[i+1, k] = 1
      }else
      {
        Omega.cov[i+1, k] = - synth.current$solution.w[as.character(j),]
      }
      k = k + 1
    }
    
  }
  
  result.gap.cov <- B.cov[1,]
  
  det.Omega.cov <- det(Omega.cov)
  
  first.post.treat.year <- min(
    setdiff(dataprepDf$time.plot, 
      dataprepDf$time.optimize.ssr) - min(dataprepDf$time.plot) + 1)
  
  for (i in first.post.treat.year:number.of.time.units)
  {
    Omega.cov.w.rep = Omega.cov
    Omega.cov.w.rep[,1] = B.cov[,i]
    
    det.Omega.cov.w.rep = det(Omega.cov.w.rep)
    
    result.gap.cov[i] = det.Omega.cov.w.rep/det.Omega.cov
  }
  
  result.actual.cov <- as.vector(dataprep.main$Y1) - result.gap.cov
  
  return(list(gap = result.gap.cov, actual = result.actual.cov))
}

runRestrictiveSCMwithCov = function(dataprepDf) {
  
  dataprep.main <- dataprep(
    foo = dataprepDf$foo,
    predictors = dataprepDf$predictors,
    special.predictors = dataprepDf$special.predictors,
    dependent = dataprepDf$dependent,
    unit.variable = dataprepDf$unit.variable,
    time.variable = dataprepDf$time.variable,
    treatment.identifier = dataprepDf$treatment.identifier,
    controls.identifier = sort(dataprepDf$controls.identifier),
    time.predictors.prior = dataprepDf$time.predictors.prior,
    time.optimize.ssr = dataprepDf$time.optimize.ssr,
    time.plot = dataprepDf$time.plot,
    unit.names.variable = dataprepDf$unit.names.variable
  )
  
  synth.main <- synth(
    data.prep.obj = dataprep.main
  )
  
  main.control.estimates <- dataprep.main$Y0 %*% synth.main$solution.w
  main.gap <- t(dataprepDf$foo[
    dataprepDf$foo[dataprepDf$unit.variable] == dataprepDf$treatment.identifier, 
  ][dataprepDf$dependent] - main.control.estimates)
  
  result <- list(dataprep = dataprep.main,
                 synth = synth.main,
                 actual = as.vector(t(main.control.estimates)),
                 gap = as.vector(main.gap))
  
  return(result)
}

runRegularSCMwithCov = function(dataprepDf) {
  
  dataprep.main <- dataprep(
    foo = dataprepDf$foo,
    predictors = dataprepDf$predictors,
    special.predictors = dataprepDf$special.predictors,
    dependent = dataprepDf$dependent,
    unit.variable = dataprepDf$unit.variable,
    time.variable = dataprepDf$time.variable,
    treatment.identifier = dataprepDf$treatment.identifier,
    controls.identifier = sort(append(dataprepDf$controls.identifier,
                                      dataprepDf$spillover.controls.identifier)),
    time.predictors.prior = dataprepDf$time.predictors.prior,
    time.optimize.ssr = dataprepDf$time.optimize.ssr,
    time.plot = dataprepDf$time.plot,
    unit.names.variable = dataprepDf$unit.names.variable
  )
  
  synth.main <- synth(
    data.prep.obj = dataprep.main
  )
  
  main.control.estimates <- dataprep.main$Y0 %*% synth.main$solution.w
  main.gap <- t(dataprepDf$foo[
    dataprepDf$foo[dataprepDf$unit.variable] == dataprepDf$treatment.identifier, 
  ][dataprepDf$dependent] - main.control.estimates)
  
  result <- list(dataprep = dataprep.main,
                 synth = synth.main,
                 actual = as.vector(t(main.control.estimates)),
                 gap = as.vector(main.gap))
  
  return(result)
}

runSP.SCMwithCov = function(dataprepDf) {
  
  dataprep.main <- dataprep(
    foo = dataprepDf$foo,
    predictors = dataprepDf$predictors,
    special.predictors = dataprepDf$special.predictors,
    dependent = dataprepDf$dependent,
    unit.variable = dataprepDf$unit.variable,
    time.variable = dataprepDf$time.variable,
    treatment.identifier = dataprepDf$treatment.identifier,
    controls.identifier = sort(append(dataprepDf$controls.identifier,
                                      dataprepDf$spillover.controls.identifier)),
    time.predictors.prior = dataprepDf$time.predictors.prior,
    time.optimize.ssr = dataprepDf$time.optimize.ssr,
    time.plot = dataprepDf$time.plot,
    unit.names.variable = dataprepDf$unit.names.variable
  )
  
  Yall <- cbind(dataprep.main$Y1plot, dataprep.main$Y0plot)
  
  scm.out = scm_batch_weightsWithCov(dataprepDf, Yall)
  
  t = length(dataprep.main$tag$time.optimize.ssr)
  s = length(dataprep.main$tag$time.plot) - length(dataprep.main$tag$time.optimize.ssr)
  
  n = ncol(Yall)
  
  Y0 = Yall[1:t,]
  Y1 = Yall[(t+1):(s + t),]
  
  
  affected.states = append(dataprepDf$spillover.controls.identifier, 
                           dataprepDf$treatment.identifier)
  
  ind = ifelse(dataprep.main$names.and.numbers$unit.numbers %in% affected.states,
               1, 0)

  A = diag(n)
  A = A[,which(!!ind)]
  
  loo = F
  normal = F
  
  return(sp_andrews_teWithCov(Yall, Y0, Y1,A,
                               loo,normal, 
                               0.05,
                               s,
                               scm.out))
}

sp_andrews_teWithCov = function(Y,Y0,Y1,A,loo=F,Normal=F,alpha_sig=0.05, s, scm.batch) {
  
  n = ncol(Y0)
  t = nrow(Y0)
  
  A_hat = scm.batch$A_hat
  B_hat = scm.batch$B_hat
  
  i_less_B = diag(n)-B_hat
  M_hat = t(i_less_B)%*%(i_less_B)
  AMA_inv = solve(t(A)%*%M_hat%*%A)
  
  synthetic_control_sp_alt <- scm.batch$synth.units[1,] 
  
  A_hat_bias <- rowMeans(A_hat + B_hat%*%t(Y) - scm.batch$synth.units)
  
  A_hat = A_hat - A_hat_bias
  
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
  
  return(list(actual = as.vector(synthetic_control_sp_alt), gap = as.vector(gamma_hat_vector)))
}

scm_batch_weightsWithCov = function(dataprepDf, Yall) {
  
  all.units.list <- append(dataprepDf$treatment.identifier, 
                           sort(append(dataprepDf$controls.identifier,
                           dataprepDf$spillover.controls.identifier)))
  n.units <- length(all.units.list)
  
  
  B_hat <- matrix(0, nrow = n.units, ncol = n.units)
  A_hat <- rep(0, n.units)
  
  synth.units <-  matrix(0, nrow = n.units, 
                         ncol = length(dataprepDf$time.plot))
  
  means = colMeans(Yall)
  
  i = 1
  
  for (cur.unit in all.units.list) {
    
    dataprepDf.current = list(
      foo = dataprepDf$foo,
      predictors = dataprepDf$predictors,
      special.predictors = dataprepDf$special.predictors,
      dependent = dataprepDf$dependent,
      unit.variable = dataprepDf$unit.variable,
      time.variable = dataprepDf$time.variable,
      treatment.identifier = cur.unit,
      controls.identifier = setdiff(all.units.list, cur.unit),
      spillover.controls.identifier = c(),
      time.predictors.prior = dataprepDf$time.predictors.prior,
      time.optimize.ssr = dataprepDf$time.optimize.ssr,
      time.plot = dataprepDf$time.plot,
      unit.names.variable = dataprepDf$unit.names.variable
    )
    
    current.result = runRegularSCMwithCov(dataprepDf.current)
    
    synth.units[i,] = current.result$dataprep$Y0 %*% current.result$synth$solution.w
    
    b_hat = rep(0, n.units)
    b_hat[-i] = current.result$synth$solution.w
    
    B_hat[i,] = b_hat
    
    
    if ((n.units - 1) == 1) {
      A_hat[i] =  (means[as.character(cur.unit)] - means*b_hat)[1]
    } else {
      A_hat[i] =  (means[as.character(cur.unit)] - means%*%b_hat)[1,1]
    }
    
    i = i + 1
  }
  
  return(list(A_hat=A_hat,B_hat=B_hat, synth.units = synth.units))
}
