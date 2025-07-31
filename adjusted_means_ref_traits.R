require(data.table)
require(ggplot2)

################

tol14rainbow=c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C")
tol15rainbow=c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA")
tol18rainbow=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
#####

RefTraits_2015_22 <- read.csv("data/RefTraits_2015_22.csv")

###
getSpats <- function(dat){   
  # dat <-subset(p, Month==5)
  
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
  BLUPs$variable <- dat$variable[1]
  BLUPs$Date <- dat$Date[1]
  BLUPs$h2 <- h2_SpATS
  BLUPs$N_Datapt_removed <- N_Datapt_removed
  BLUPs <- as.data.table(BLUPs)
  gc()
  print("done")
  return(BLUPs)
  # save dataframes
  # write.csv(dat, file = paste0("SpATScorr-20190130_",Z,"_",Y,".csv"))
  # save(VarPlt, file = paste0(Z,"_",Y,".rda"))
}



###


##
p <- setDT(RefTraits_2015_22)
# oo <- subset(p, year_site.UID=="FPSB015")
unique(p$year_site.UID)

p <- droplevels(p)
p <- p[,value_dup:=duplicated(value), by=.(variable,year_site.UID,Date,genotype.id)]
p <- subset(p, value_dup==FALSE)
More_than_2_Reps <- setDT(p)[, length(unique(plot.UID)), by=.(variable, year_site.UID, Date, genotype.id)]

More_than_2_Reps <- More_than_2_Reps[,mean(V1),by=.(variable, year_site.UID, Date)]
More_than_2_Reps <- More_than_2_Reps[More_than_2_Reps$V1>1.5,]

# p <- subset(dat, variable%in%c(traits)&value!=0&!(variable%in%c("Protein.content")&year_site.UID=="FPSB012"))
Protein_traits <- subset(p, !paste(p$year_site.UID, p$variable, p$Date, p$genotype.id)%in%paste(More_than_2_Reps$year_site.UID, More_than_2_Reps$variable, More_than_2_Reps$Date))
Protein_traits <- Protein_traits[!duplicated(paste(Protein_traits$year_site.UID, Protein_traits$genotype.id, Protein_traits$value)),]
p <- subset(p, paste(p$year_site.UID, p$variable, p$Date)%in%paste(More_than_2_Reps$year_site.UID, More_than_2_Reps$variable, More_than_2_Reps$Date))

p$variable2 <- p$variable


# p <- na.omit(p)
p <- p[!is.na(p$value),]
p <- droplevels(p)
p <- setDT(p)[, NperTrial:=nrow(na.omit(.SD)), by=.(variable2,year_site.UID,Date)]
p <- subset(p, NperTrial>10)
hist(p$NperTrial)
unique(p$year_site.UID)
p$year_site.UID2 <- p$year_site.UID
p$Date2 <- p$Date

setDT(p)[, nrow(na.omit(.SD)), by=.(variable2, year_site.UID2, Date2)]
o <- setDT(p)[, nlevels(as.factor(Row)), by=.(variable2, year_site.UID2, Date2)]
min(o$V1)
o <- setDT(p)[, nlevels(as.factor(Range)), by=.(variable2, year_site.UID2, Date2)]
min(o$V1)

ggplot(p,aes(x=value, fill=year_site.UID, color=year_site.UID))+ ylab("Value")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
  geom_density(alpha=0.5)+
  scale_color_manual(values = tol18rainbow)+
  scale_fill_manual(values = tol18rainbow)+
  facet_wrap(variable~., scales = "free", switch="both", labeller = label_parsed)



setDT(p)[, list( mean(value) ), by=.(variable2, year_site.UID2, Date2)]

# p <- subset(p, variable%in%c("Yield"))
spats <- setDT(p)[, ( getSpats(.SD) ), by=.(variable2, year_site.UID2, Date2)]

BLUE <- spats
BLUE
BLUE <- merge(add_gen_id, BLUE, by=c("genotype.id"))
BLUE$Genotype_name <- BLUE$genotype.name

ggplot(BLUE,aes(x=year_site.UID, y=predicted.values, color=year_site.UID))+ ylab("Value")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
  geom_boxplot(alpha=0.5)+
  geom_point()+
  scale_color_manual(values = tol14rainbow)+
  scale_fill_manual(values = tol14rainbow)+
  facet_wrap(variable~., scales = "free", switch="both", labeller = label_parsed)


p <- BLUE
# p <- subset(p, variable=="Days_to_Max_Plant.height")
ggplot(p,aes(x=predicted.values, fill=year_site.UID, color=year_site.UID))+ ylab("Value")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
  geom_density(alpha=0.5)+
  scale_color_manual(values = tol18rainbow)+
  scale_fill_manual(values = tol18rainbow)+
  facet_wrap(variable~., scales = "free", switch="both", labeller = label_parsed)



# ggplot(dat,aes(x=year_site.UID, y=value, color=year_site.UID))+ ylab("Value")+
#   theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
#   geom_boxplot(alpha=0.5)+
#   geom_point()+
#   # scale_color_manual(values = tol4qualitative)+
#   # scale_fill_manual(values = tol4qualitative)+
#   facet_wrap(variable~., scales = "free", switch="both", labeller = label_parsed)
# 

p_h2_all <-  unique(BLUE[,c("h2","year_site.UID","Date","N_Datapt_removed","variable")])
hist(p_h2_all$h2)

p <- subset(p_h2_all)
ggplot(data=p, aes(x=year_site.UID, y=h2) ) +
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=0.6,color="white"),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 0, hjust = 0.5),text = element_text(size=12),strip.text = element_text(size = 12), axis.title = element_text(size = 12))+
  geom_point()+
  # geom_boxplot( outlier.colour = NA, alpha=0.75)+
  facet_grid(variable~.)

# write.csv(BLUE, "Soybean/201x/SpATScorr-20250227-BLUE_Soybean.csv", row.names = F)

# SpATsBLUE <- read.csv("data/SpATS_BLUEs_Soybean_2015_22.csv")
###########




######
p1 <- subset(dat, variable%in%c("Yield","Protein.content","Protein.yield","End.of.maturity")&year_site.UID%in%year_site.UIDs[1:15])
p1$year_site.UID <- "AcrossAllEnv"
p1 <- droplevels(p1)
p1$variable2 <- p1$variable
setDT(p1)[, nrow(na.omit(.SD)), by=.(variable2)]
# p1 <- subset(p1, NperTrial>35)
p1$Treatment <- as.factor(p1$Treatment)
# 
SpatsAcrossAllEnv <- setDT(p1)[, ( getSpats(.SD) ), by=.(variable2)] ## fix me: cannot plot figure. 

SpATsBLUE_overall <- SpatsAcrossAllEnv
SpATsBLUE_overall <- merge(SpATsBLUE_overall, add_gen_id, by="genotype.id")
SpATsBLUE_overall$Genotype_name <- SpATsBLUE_overall$genotype.name

# write.csv(SpATsBLUE_overall, "data/SpATS_BLUEsOverall_Soybean_2015_22.csv", row.names = F)

p_h2_all <-  unique(SpATsBLUE_overall[,c("h2","year_site.UID","Date","N_Datapt_removed","variable")])
hist(p_h2_all$h2)

p <- subset(p_h2_all)
ggplot(data=p, aes(x=year_site.UID, y=h2) ) +
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=0.6,color="white"),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 0, hjust = 0.5),text = element_text(size=12),strip.text = element_text(size = 12), axis.title = element_text(size = 12))+
  geom_jitter()+
  # geom_boxplot( outlier.colour = NA, alpha=0.75)+
  facet_grid(variable~.)