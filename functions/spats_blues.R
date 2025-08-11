spats_blues <- function(data){
  require("SpATS")
  data$row_f <-   as.factor(data$row)
  data$range_f <-   as.factor(data$range)
  
  data$range <-   as.numeric(as.character(data$range))
  data$row <-   as.numeric(as.character(data$row))
  data$genotype.id <-   as.factor(data$genotype.id )
  data$value.cleaned <- data$value
  NAbefore <- length(data$value.cleaned[is.na(data$value.cleaned)])
  
  w <- 1                                         # Starter 
  k <- 3    
  print(data$date2[1])
  cat(data$date2[1])
  
  # Number of standard deviations to consider extreme outliers 
  while (w>=1) {
    #genotype not random
    fit.SpATS <- SpATS(response = "value.cleaned", random= ~
                         row_f +range_f,
                       spatial = ~PSANOVA(row, range, nseg = c(4,4),
                                          nest.div=c(2,2)),
                       genotype = "genotype.id", 
                       genotype.as.random = F, data = data
    )
    
    #genotype random
    fit.SpATS_h2 <- SpATS(response = "value.cleaned", random= ~
                            row_f +range_f,
                          spatial = ~PSANOVA(row, range, nseg = c(4,4),
                                             nest.div=c(2,2)),
                          genotype = "genotype.id",
                          genotype.as.random = T, data = data
    )
    h2 <- getHeritability(fit.SpATS_h2)
    print(h2)
    plot(fit.SpATS)
    
    Obs <- fit.SpATS$nobs
    Eff.dim <- sum(c(fit.SpATS$eff.dim))
    Var_resi <- sum( na.omit(residuals(fit.SpATS))^2 ) / (Obs - Eff.dim) # Sum(Errores^ 2)/ED_e
    vect_res <- residuals(fit.SpATS)
    
    # Number of extreme residuals (above k standard deviations) in this iteration
    w <- length( which( abs(vect_res) > abs(k * sqrt(Var_resi)) ) )
    print(w)
    # What is the most extreme residual ?
    p <- which( abs(vect_res) > abs(k * sqrt(Var_resi)) )[which.max( abs( vect_res[which(abs(vect_res) > abs(k * sqrt(Var_resi)))] ) )]
    
    
    data$value.cleaned[p] <- NA
    
    fit.SpATS <- SpATS(response = "value.cleaned", random= ~
                         row_f +range_f,
                       spatial = ~PSANOVA(row, range, nseg = c(4,4),
                                          nest.div=c(2,2)),
                       genotype = "genotype.id", 
                       genotype.as.random = F, data = data
    )
    
    fit.SpATS_h2 <- SpATS(response = "value.cleaned", random= ~
                            row_f +range_f,
                          spatial = ~PSANOVA(row, range, nseg = c(4,4),
                                             nest.div=c(2,2)),
                          genotype = "genotype.id",
                          genotype.as.random = T, data = data
    )
    h2 <- getHeritability(fit.SpATS_h2)  
    gc()
    cat("\n year_site.UID:",  levels(data$year_site.UID) , "\tN_xtreme_residuals:" , w, "\tHeritability in this iteration:", h2 )
    
  }
  
  NAafter <- length(data$value.cleaned[is.na(data$value.cleaned)])
  N_Datapt_removed=NAafter-NAbefore
  
  #dev.off()
  BLUEs <- predict(object = fit.SpATS, which = c('genotype.id'))
  BLUEs$h2 <- h2
  BLUEs$N_Datapt_removed <- N_Datapt_removed
  
  return(BLUEs)
}



getSpats <- function(dat){   
  # dat <-subset(p1, variable=="End.of.maturity")
  
  require(SpATS)
  require(tidyr)
  require(plyr)
  
  print("Start with")
  print(paste(dat$variable[1], dat$year_site.UID[1]  ))
  
  
  dat<-dat  %>%  arrange(year_site.UID, Rep, Range, Row) 
  
  
  dat<- dat[!is.na(dat$value),]
  dat<-droplevels(dat)
  
  #Make a Row and Range block with unique numers for each Lot
  dat<-data.table(dat)
  # str(dat)
  
  
  ### set fields from different year_site.UIDs apart
  # levels(dat$year_site.UID)
  maxRowRange <- setDT(dat)[, list(maxRow=max(as.numeric(as.character(Row))),maxRange=max(as.numeric(as.character(Range))),minRow=min(as.numeric(as.character(Row))),minRange=min(as.numeric(as.character(Range))) ), by=year_site.UID]
  
  maxRowRange$RowAbove <- cumsum(maxRowRange$maxRow)
  maxRowRange$RangeAbove <- cumsum(maxRowRange$maxRange)
  maxRowRange$Distance <- 20
  maxRowRange$Distance <- cumsum(maxRowRange$Distance)
  
  maxRowRange$DistanceRow <- 20
  maxRowRange$DistanceRow <- cumsum(maxRowRange$DistanceRow)
  
  maxRowRange$RowAbove <- c(0,   maxRowRange$RowAbove[-length( maxRowRange$RowAbove)])
  maxRowRange$RangeAbove <- c(0,   maxRowRange$RangeAbove[-length( maxRowRange$RangeAbove)])
  
  dat <- merge(dat, maxRowRange, by="year_site.UID")
  # # make the factors
  dat$RowBL <- dat$Row+dat$RowAbove+dat$DistanceRow
  dat$RangeBL <- dat$Range+dat$RangeAbove+dat$Distance
  
  
  #It is important to have the right sorting range, row
  dat<-dat  %>%  arrange(year_site.UID, RangeBL,  RowBL)
  # dat$RangeBL
  dat$row_f = as.factor(dat$Row) 
  dat$col_f = as.factor(dat$Range)
  # dat$genotype.id = as.factor(dat$Genotype)
  
  
  dat$value.cleaned <- dat$value
  NAbefore <- length(dat$value.cleaned[is.na(dat$value.cleaned)])
  
  testSp<-dat
  head(testSp)
  testSp$year_site.UID <- as.factor(testSp$year_site.UID)
  ##### return cleaned data
  w <- 1                                         # Starter 
  k <- 5    
  
  # Number of standard deviations to consider extreme outliers 
  while (w>=1) {
    if(nlevels(testSp$year_site.UID)==1){
      fit.SpATS <- SpATS(response = "value.cleaned", random= ~ row_f+col_f,
                         spatial = ~PSANOVA(RangeBL, RowBL, nseg = c(4,4), nest.div=c(2,2)),
                         genotype = "genotype.id", genotype.as.random = F, data = testSp)
    }else{
      fit.SpATS <- SpATS(response = "value.cleaned", random= ~ row_f+col_f+year_site.UID,
                         spatial = ~PSANOVA(RangeBL, RowBL, nseg = c(4,4), nest.div=c(2,2)),
                         genotype = "genotype.id", genotype.as.random = F, data = testSp
      )   }
    
    
    Obs <- fit.SpATS$nobs
    Eff.dim <- sum(c(fit.SpATS$eff.dim))
    Var_resi <- sum( na.omit(residuals(fit.SpATS))^2 ) / (Obs - Eff.dim) # Sum(Errores^ 2)/ED_e
    vect_res <- residuals(fit.SpATS)
    
    # Number of extreme residuals (above k standard deviations) in this iteration
    w <- length( which( abs(vect_res) > abs(k * sqrt(Var_resi)) ) )
    print(w)
    # What is the most extreme residual ?
    p <- which( abs(vect_res) > abs(k * sqrt(Var_resi)) )[which.max( abs( vect_res[which(abs(vect_res) > abs(k * sqrt(Var_resi)))] ) )]
    
    
    testSp$value.cleaned[p] <- NA
    
    if(nlevels(testSp$year_site.UID)==1){
      fit.SpATS_H2 <- SpATS(response = "value.cleaned", random= ~ row_f+col_f, 
                            spatial = ~PSANOVA(RangeBL, RowBL, nseg = c(4,4), nest.div=c(2,2)),
                            genotype = "genotype.id", genotype.as.random = T, data = testSp)
    }else{
      fit.SpATS_H2 <- SpATS(response = "value.cleaned", random= ~ row_f+col_f+year_site.UID,
                            spatial = ~PSANOVA(RangeBL, RowBL, nseg = c(4,4), nest.div=c(2,2)),
                            genotype = "genotype.id", genotype.as.random = T, data = testSp
      )   }
    
    h2_SpATS <- getHeritability(fit.SpATS_H2)  
    gc()
    cat("\n year_site.UID:",  levels(testSp$year_site.UID) , "\tN_xtreme_residuals:" , w, "\tHeritability in this iteration:", h2_SpATS )
    
  }
  
  NAafter <- length(testSp$value.cleaned[is.na(testSp$value.cleaned)])
  N_Datapt_removed=NAafter-NAbefore
  
  
  if(nlevels(testSp$year_site.UID)==1){
    png(paste0("FigSpATS_",paste(dat$variable[1], dat$year_site.UID[1]),round(h2_SpATS[1],2),".png"),  width = 168, height = 170, units = "mm", res = 100)
    plot(fit.SpATS, main=paste(paste(dat$variable[1]),"h2=",round(h2_SpATS[1],2)) )
    dev.off()
  }
  summary(fit.SpATS)
  
  
  #This part was taken from the plot.SpATS function
  ##############################################
  
  BLUPs <- predict(object = fit.SpATS, which = 'genotype.id')
  BLUPs$year_site.UID <- dat$year_site.UID[1]
  if(nlevels(testSp$year_site.UID)!=1){BLUPs$year_site.UID <- "AcrossEnvironments"}
  BLUPs$variable <- dat$variable[1]
  BLUPs$Date <- dat$Date[1]
  BLUPs$h2 <- h2_SpATS
  BLUPs$N_Datapt_removed <- N_Datapt_removed
  BLUPs <- as.data.frame(BLUPs)
  gc()
  
  return(BLUPs)
  # save dataframes
  # write.csv(dat, file = paste0("SpATScorr-20190130_",Z,"_",Y,".csv"))
  # save(VarPlt, file = paste0(Z,"_",Y,".rda"))
}

