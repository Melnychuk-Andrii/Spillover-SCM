

library(ggplot2)
library(ggpubr)


makeTwoLinePlots <- function(data1, data2, dataX, methodList, lineNames, 
                             xlab, ylab, main1, main2, legendPos = "topleft") {
  
  par(mfrow=c(1,2))
  
  makeLinePlot(data1, dataX, methodList, lineNames, 
               xlab, ylab, main1,legendPos)
  makeLinePlot(data2, dataX, methodList, lineNames, 
               xlab, ylab, main2,legendPos)
  
}


makeLinePlot <- function(data, dataX, methodList, lineNames, 
                         xlab, ylab, main, legendPos = "topleft")
{
  
  ylim <- c(min(data$MSE), max(data$MSE))
  lwd <- 1
  lineCols = c("green", "red", "blue", "purple", "orange")
  ltys = c(1,1,1,1,1) #c(1, 2, 3, 4)
  pchs = c(15,16,17,18,25) #c(0,1,2,5)
  
  plot(dataX, rep(0, length(unique(dataX))), type = "l", lty = 0, ylim = ylim,
       xlab = xlab, ylab = ylab, main = main)
  
  i <- 1
  
  for (methodCur in methodList) {
    
    lines(dataX, data[data$method == methodCur,]$MSE,  
          type = "l", lty = ltys[i], col = lineCols[i], lwd = lwd)
    
    points(dataX, data[data$method == methodCur,]$MSE, 
           pch = pchs[i], col = lineCols[i], bg="orange")
    
    i = i + 1
    
  }
  
  if (length(legendPos) == 2) {
    legend(legendPos[1],legendPos[2], 
           legend=lineNames,
           col=lineCols, lwd = lwd, pch = pchs,pt.bg="orange",
           lty = ltys, cex=0.8,
           box.lwd = 0,box.col = "white",bg = "white")
    
  } else {
    
    legend(legendPos, 
           legend=lineNames,
           col=lineCols, lwd = lwd, pch = pchs,pt.bg="orange",
           lty = ltys, cex=0.8,
           box.lwd = 0,box.col = "white",bg="transparent")
  }
  
}

tidyData[tidyData$sp.count == 4,]$total.ctrl.count
tidyData <- read.csv("~/Documents/Minerva/Thesis/newtidyDf100.csv", header = TRUE, sep = ",") 

tidyData$method = factor(tidyData$method , levels = methods)

tidyData$mode = factor(tidyData$mode , levels = c("Stationary", "I(1)"))

tidyData$sp.ratio = tidyData$sp.count / tidyData$total.ctrl.count
tidyData$sp.eff.ratio = tidyData$sp.eff / tidyData$treat.eff

#View(tidyData)

sp.ratio.df.table1 <- tidyData %>% 
  filter(mode == "Stationary" & 
           nRuns == 100 & 
           total.ctrl.count > 6 &
           method %in% c("iterative.FT", "iterative.FF", "iterative.TT", "iterative.TF")) %>%
  group_by(method) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))

sp.ratio.df.table2<- tidyData %>% 
  filter(mode == "I(1)" & 
           nRuns == 100 & 
           total.ctrl.count > 6 &
           method %in% c("iterative.FT", "iterative.FF", "iterative.TT", "iterative.TF")) %>%
  group_by(method) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))

# View(sp.ratio.df.table1)
# View(sp.ratio.df.table2)

sp.ratio.df <- tidyData %>% 
                filter(mode == "I(1)" & total.ctrl.count > 6 &
                         nRuns == 100 & method %in% c("iterative.FT", "iterative.FF", "iterative.TT", "iterative.TF")) %>%
                group_by(sp.ratio, method) %>%
                summarise(MSE = mean(MSE.gap, na.rm = TRUE))

sp.ratio.df1 <- tidyData %>% 
  filter(mode == "Stationary" & total.ctrl.count > 6 &
           nRuns == 100 & method %in% c("iterative.FT", "iterative.FF", "iterative.TT", "iterative.TF")) %>%
  group_by(sp.ratio, method) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))

makeTwoLinePlots(sp.ratio.df, sp.ratio.df1, unique(sp.ratio.df$sp.ratio), 
             c("iterative.FT", "iterative.FF", "iterative.TT", "iterative.TF"),
             c("No pre-treat repl.; Use prev. cleaned units",
               "No pre-treat repl.; Don't use prev. cleaned units",
               "Pre-treat repl.; Use prev. cleaned units",
               "Pre-treat repl.; Don't use prev. cleaned units"),
             xlab = "Ratio of Spillover Units to all Control Units",
             ylab = "MSPE",
             main1 = "I(1) case",
             main2 = "Stationary case")




sp.ratio.df2 <- tidyData %>% 
  filter(mode == "Stationary" & nRuns == 100  & total.ctrl.count > 6 & method %in% c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive") ) %>%
  group_by(sp.ratio, method) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))

sp.ratio.df3 <- tidyData %>% 
  filter(mode == "I(1)" & nRuns == 100  & total.ctrl.count > 6 & method %in% c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive") ) %>%
  group_by(sp.ratio, method) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))

makeTwoLinePlots(sp.ratio.df3, sp.ratio.df2, unique(sp.ratio.df2$sp.ratio), 
                 c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive"),
                 c("Iterative SCM",
                   "Unrestricted SCM",
                   "SP Model",
                   "Restricted SCM",
                   "Inclusive SCM"),
                 xlab = "Ratio of Spillover Units to all Control Units",
                 ylab = "MSPE",
                 main1 = "I(1) case",
                 main2 = "Stationary case")


#tidyData$sp.eff.ratio.rounded <- round(tidyData$sp.eff.ratio, 1)

sp.eff.ratio.df <- tidyData %>% 
  filter(mode == "Stationary" & total.ctrl.count > 6 & nRuns == 100 & method %in% c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive")) %>%
  group_by(sp.eff.ratio.rounded, method) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))


sp.eff.ratio.df1 <- tidyData %>% 
  filter(mode == "I(1)" & total.ctrl.count > 6 & nRuns == 100 & method %in% c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive")) %>%
  group_by(sp.eff.ratio.rounded, method) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))


makeTwoLinePlots(sp.eff.ratio.df1, sp.eff.ratio.df, 
                 unique(sp.eff.ratio.df$sp.eff.ratio.rounded), 
                 c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive"),
                 c("Iterative SCM",
                   "Unrestricted SCM",
                   "SP Model",
                   "Restricted SCM",
                   "Inclusive SCM"),
                 xlab = "Ratio of Spillover Effect to Treatment Effect",
                 ylab = "MSPE",
                 main1 = "I(1) case",
                 main2 = "Stationary case")


total.treat.df <- tidyData %>% 
  filter(mode == "Stationary"  & total.ctrl.count > 6 & nRuns == 100 & method %in% c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive")) %>%
  group_by(treat.eff, method) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))

total.treat.df1 <- tidyData %>% 
  filter(mode == "I(1)" & total.ctrl.count > 6 & nRuns == 100 & method %in% c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive")) %>%
  group_by(treat.eff, method) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))


makeTwoLinePlots(total.treat.df1, total.treat.df, 
                 unique(total.treat.df$treat.eff), 
                 c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive"),
                 c("Iterative SCM",
                   "Unrestricted SCM",
                   "SP Model",
                   "Restricted SCM",
                   "Inclusive SCM"),
                 xlab = "Treatment Effect size",
                 ylab = "MSPE",
                 main1 = "I(1) case",
                 main2 = "Stationary case")



n.units.df1 <- tidyData %>% 
  filter(mode == "I(1)" &  nRuns == 100 & method %in% c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive")) %>%
  group_by(method, total.ctrl.count) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))

n.units.df <- tidyData %>% 
  filter(mode == "Stationary" & nRuns == 100 & method %in% c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive")) %>%
  group_by(method, total.ctrl.count) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))

makeTwoLinePlots(n.units.df1, n.units.df, 
                 unique(n.units.df$total.ctrl.count), 
                 c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive"),
                 c("Iterative SCM",
                   "Unrestricted SCM",
                   "SP Model",
                   "Restricted SCM",
                   "Inclusive SCM"),
                 xlab = "Total number of Control Units in the dataset",
                 ylab = "MSPE",
                 main1 = "I(1) case",
                 main2 = "Stationary case",
                 legendPos = "left")



pre.treat.df <- tidyData %>% 
  filter(mode == "Stationary" & nRuns == 100 & method %in% c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive")) %>%
  group_by(method, pre.treat.period) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))

pre.treat.df1 <- tidyData %>% 
  filter(mode == "I(1)" & nRuns == 100 & method %in% c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive")) %>%
  group_by(method, pre.treat.period) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))


makeTwoLinePlots(pre.treat.df1, pre.treat.df, 
                 unique(pre.treat.df$pre.treat.period), 
                 c("iterative.TT", "regular", "sp.model", "restrictive", "inclusive"),
                 c("Iterative SCM",
                   "Unrestricted SCM",
                   "SP Model",
                   "Restrictive SCM",
                   "Inclusive SCM"),
                 xlab = "Pre-treatment period length",
                 ylab = "MSPE",
                 main1 = "I(1) case",
                 main2 = "Stationary case",
                 legendPos = c(10,3.2))





DGP.df <- tidyData %>% 
  filter(nRuns == 100 & method %in% c("regular", "restrictive", "sp.model", "iterative.TT", "inclusive")) %>%
  group_by(method, mode) %>%
  summarise(MSE = mean(MSE.gap, na.rm = TRUE))

DGP.df$method = factor(DGP.df$method , levels = c("regular", "restrictive", "sp.model", "iterative.TT", "inclusive"))

#View(DGP.df)

bartable <- table(DGP.df$mode, DGP.df$method)

colnames(bartable) = c("Iterative", "Unrestricted", "SP Model", "Restricted", "Inclusive")

bartable[1,] = DGP.df[DGP.df$mode == "Stationary",]$MSE
bartable[2,] = DGP.df[DGP.df$mode == "I(1)",]$MSE


par(mfrow=c(1,1))
barplot(bartable[,c(2, 4, 3, 1, 5)], main="Comparison of DGP cases",
        xlab="Method", ylab = "MSPE", col=c("gray26","lightgrey"),
        legend = rownames(bartable), beside=TRUE, 
        ylim = c(0, max(bartable)*1.1))
grid()



tidyerData <- tidyData %>% 
  filter(nRuns == 100 & 
           method %in% c("regular", "restrictive"))


ggplot(tidyerData[tidyerData$mode == "Stationary" & 
                    tidyerData$sp.count > tidyerData$total.ctrl.count * 0.7,], 
       aes(x=method, y=MSE.gap, color=method)) +
  geom_violin()
