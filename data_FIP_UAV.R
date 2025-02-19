setwd("~/public/Evaluation/Projects/KP0023_legumes/Soybean/2022/")

noMask <- T

require(data.table)
require(ggplot2)

#### better color schemes
tol1qualitative=c("#4477AA")
tol2qualitative=c("#4477AA", "#CC6677")
tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")

tol14rainbow=c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C")
tol15rainbow=c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA")
tol18rainbow=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
#####

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
  BLUPs$year_site.UID <- year_site.UID[1]
  BLUPs$variable <- variable
  BLUPs$Date <- Date
  BLUPs$h2 <- h2_SpATS
  BLUPs$N_Datapt_removed <- N_Datapt_removed
  BLUPs <- as.data.frame(BLUPs)
  gc()
  
  return(BLUPs)
  # save dataframes
  # write.csv(dat, file = paste0("SpATScorr-20190130_",Z,"_",Y,".csv"))
  # save(VarPlt, file = paste0(Z,"_",Y,".rda"))
}

######

## instead of source data from drives, load:
# load("~/public/Evaluation/Projects/KP0023_legumes/Scripts/stats-lab-crops/data/Soybean_FIP_UAV.RData")

###############
###############

fnames = list.files(path="~/public/Evaluation/Projects/KP0023_legumes/CroPyDB_export/", pattern="design_",recursive=T, full.names = T)
# fnames<-fnames[grepl("CroPyDB_export",fnames) ]
fnames
require(plyr)
CroPyDB_design = ldply(fnames, function(filename) {
  dum = fread(filename,header=T, sep="," , na.strings=c("NA"))
  #If you want to add the filename as well on the column
  return(dum) })
detach("package:plyr", unload=TRUE)
CroPyDB_design

CroPyDB_design$plot.spatial_x <- NULL
CroPyDB_design$plot.spatial_y <- NULL
CroPyDB_design <- unique(CroPyDB_design)
########NULL
add_gen_id_all <- read.csv("~/public/Evaluation/Projects/KP0023_legumes/Design/2024/GenotypeID_all.csv")
add_gen_id <- subset(add_gen_id_all, crop=="soybean")
add_gen_id <- add_gen_id[!is.na(add_gen_id$name),]
add_gen_id <- add_gen_id[!duplicated(paste(add_gen_id$id,add_gen_id$name)),]

add_gen_old <- read.csv("~/public/Evaluation/Projects/KP0023_legumes/Soybean/2022/Design/Genotypes_2022_40_05.csv")
add_gen_id$dup <- "no"
add_gen_id$dup[duplicated(add_gen_id$id)] <- "yes"
remove <- subset(add_gen_id, breeder=="Agroscope")
remove <- subset(remove, id%in%add_gen_id$id[add_gen_id$dup=="yes"])
remove <- remove[!duplicated(remove$id),]
add_gen_id <- subset(add_gen_id, !paste(id,name)%in%paste(remove$id,remove$name))
add_gen_id$dup <- "no"
add_gen_id$dup[duplicated(add_gen_id$name)] <- "yes"
add_gen_id <- subset(add_gen_id, !(!name%in%add_gen_old$name&dup=="yes"))


add_gen_id$name[!is.na(add_gen_id$characteristics)] # exchange id x id with line names x line names
add_gen_id$name[!is.na(add_gen_id$characteristics)] <- add_gen_id$characteristics[!is.na(add_gen_id$characteristics)]
add_gen_id <- add_gen_id[!duplicated(add_gen_id$id),]
add_gen_id$name <- gsub("T_","", add_gen_id$name )
add_gen_id$dup <- NULL
# write.csv(add_gen_id, "~/public/Evaluation/Projects/KP0023_legumes/Design/2024/ids_soybean_cleaned.csv", row.names = F, quote = F)

add_gen_id <- add_gen_id[,c("id","name")]
add_gen_id$genotype.name <- add_gen_id$name
add_gen_id$genotype.id <- add_gen_id$id
add_gen_id$name <- NULL
add_gen_id$id <- NULL


design22 <- read.csv("~/public/Evaluation/Projects/KP0023_legumes/Soybean/2022/Design/20220513_Design_Measurements_FPSB016.csv")
design22 <- unique(design22)
# design22$Sowing_date <- as.Date("2022-04-21")
design22$genotype_name <- design22$Genotype_name
design22$Genotype_name <- NULL
names(design22)[names(design22) == "plot_s"] <- "plot"
design22$gen_id[design22$genotype_name=="Gallec_Tourmaline"] <- subset(add_gen_id_all, characteristics=="Gallec_Tourmaline")$id
design22$gen_id[design22$genotype_name=="Tourmaline_Opaline"] <- subset(add_gen_id_all, characteristics=="Tourmaline_Opaline")$id
design22$gen_id[design22$genotype_name=="Gallec_Opaline"] <- subset(add_gen_id_all, characteristics=="Gallec_Opaline")$id

design21 <- read.csv("~/public/Evaluation/FIP/RW/2021/Feldbuch/SB/210408_Design_FPSB015_Fieldbook.csv")
design21$genotype_name <- iconv(design21$genotype_name, "ISO-8859-1", "UTF-8")

require(readxl)
design15 <- readxl::read_excel("~/public/Evaluation/FIP/2015/Feldbuch/SB/2015_FPSB004_20150512.xlsx",sheet=6)
names(design15) <- tolower(names(design15))
names(design15)[names(design15) == "col"] <- "range"
names(design15)[names(design15) == "exp"] <- "exp_UID"
names(design15)[names(design15) == "plot_id"] <- "plot_UID"
names(design15)[names(design15) == "genotype"] <- "genotype.name"
design15$exp_UID <- paste0("FP",design15$exp_UID )
design15$plot_UID <- gsub("FSB","FPSB",design15$plot_UID )
design15$genotype.name[duplicated(design15$plot_UID, fromLast=T)]

p <- subset(design15)
p$col <- p$range
p$Treatment <- "no"
p$Treatment[p$genotype.name=="Opaline"] <- "Opa"
p$row <- p$row-1
require(ggplot2)
ggplot(data=p, aes(col,row, fill = Treatment, color=Treatment))+
  labs(title="Brütten and peas on top, <- Forest , Scheune -> ")+
  geom_tile(color = "white")+
  theme(text=element_text(size=5),legend.title=element_text(size=7),legend.key.size = unit(0.6, "lines"),
        strip.background = element_blank(),
        # axis.ticks = element_blank() 
  )+
  scale_x_continuous(breaks = seq(2, 5, by=1)) +
  scale_y_continuous(breaks = seq(0, 23, by=1)) +
  scale_color_manual(values=c("black","grey90","grey10"))+
  geom_text(aes(col,row, label= paste(genotype.name, plot_UID)),size=1.75)

plot_UID.Corr <- design15$plot_UID
design15corr <- design15[,c("genotype.name","plot_UID")]

design15 <- design15[order(design15$range, design15$row),]
design15$row_corr <- rep(c(2:23,23:2),times=2) # reverse rows for IDs
design15 <- design15[order(design15$range, design15$row_corr),]
design15$plot_UID.old <- design15$plot_UID
design15$plot_UID <- paste0("FPSB00400",1:88)#plot_UID.Corr
design15$plot_UID[1:9] <- paste0("FPSB004000",1:9)#plot_UID.Corr



## corrected design
p <- subset(design15)
p$col <- p$range
p$Treatment <- "no"
p$Treatment[p$genotype.name=="Opaline"] <- "Opa"
p$row <- p$row-1
require(ggplot2)
ggplot(data=p, aes(col,row, fill = Treatment, color=Treatment))+
  labs(title="Brütten and peas on top, <- Forest , Scheune -> ")+
  geom_tile(color = "white")+
  theme(text=element_text(size=5),legend.title=element_text(size=7),legend.key.size = unit(0.6, "lines"),
        strip.background = element_blank(),
        # axis.ticks = element_blank() 
  )+
  scale_x_continuous(breaks = seq(2, 5, by=1)) +
  scale_y_continuous(breaks = seq(0, 23, by=1)) +
  scale_color_manual(values=c("black","grey90","grey10"))+
  geom_text(aes(col,row, label= paste(genotype.name, plot_UID)),size=1.75)


design15 <- merge(design15, add_gen_id[,c("genotype.name","genotype.id")], by="genotype.name")

# write.csv(design15, "~/public/Evaluation/Projects/KP0023_legumes/Soybean/2015/design2015_FPSB004_corrected.csv", row.names = F, quote = F)

#####

design16 <- read_excel("/home/kellebea/public/Evaluation/FIP/2016/Feldbuch/SB/2016_FPSB005_FPSB006_FINAL_LK.xlsx",sheet = 4)
names(design16)[names(design16) == "Genotype"] <- "genotype.name"

design16 <- subset(design16, Exp=="FPSB005")


design16 <- merge(design16, add_gen_id[,c("genotype.name","genotype.id")], by="genotype.name")

names(design16) <- tolower(names(design16))
names(design16)[names(design16) == "row"] <- "range" ## switched!!
names(design16)[names(design16) == "col"] <- "row"
names(design16)[names(design16) == "exp"] <- "exp_UID"
names(design16)[names(design16) == "plot_id"] <- "plot_UID"
# names(design16)[names(design16) == "genotype"] <- "genotype.name"
# design16$exp_UID <- paste0("FP",design16$exp_UID )
# design16$plot_UID <- gsub("FSB","FPSB",design16$plot_UID )
design16$genotype.name[duplicated(design16$plot_UID, fromLast=T)]
p <- subset(design16)
p$col <- p$range
p$Treatment <- "no"
p$Treatment[p$genotype.name=="Opaline"] <- "Opa"
p$row <- p$row-1
require(ggplot2)
ggplot(data=p, aes(col,row, fill = Treatment, color=Treatment))+
  labs(title="Brütten and peas on top, <- Forest , Scheune -> ")+
  geom_tile(color = "white")+
  theme(text=element_text(size=5),legend.title=element_text(size=7),legend.key.size = unit(0.6, "lines"),
        strip.background = element_blank(),
        # axis.ticks = element_blank() 
  )+
  scale_x_continuous(breaks = seq(1, 6, by=1)) +
  scale_y_continuous(breaks = seq(1, 19, by=1)) +
  scale_color_manual(values=c("black","grey90","grey10"))+
  geom_text(aes(col,row, label= paste(genotype.name, plot_UID)),size=1.75)


design16 <- design16[order(design16$plot_UID),]
design16$plot_UID.old <- design16$plot_UID
design16$plot_UID <- paste0("FPSB00500",6:83)
design16$plot_UID[6:9] <- paste0("FPSB005000",6:9)

## corrected design
p <- subset(design16)
p$col <- p$range
p$Treatment <- "no"
p$Treatment[p$genotype.name=="Opaline"] <- "Opa"
p$row <- p$row-1
require(ggplot2)
ggplot(data=p, aes(col,row, fill = Treatment, color=Treatment))+
  labs(title="Brütten and peas on top, <- Forest , Scheune -> ")+
  geom_tile(color = "white")+
  theme(text=element_text(size=5),legend.title=element_text(size=7),legend.key.size = unit(0.6, "lines"),
        strip.background = element_blank(),
        # axis.ticks = element_blank() 
  )+
  scale_x_continuous(breaks = seq(2, 5, by=1)) +
  scale_y_continuous(breaks = seq(0, 23, by=1)) +
  scale_color_manual(values=c("black","grey90","grey10"))+
  geom_text(aes(col,row, label= paste(genotype.name, plot_UID)),size=1.75)



# write.csv(design16, "~/public/Evaluation/Projects/KP0023_legumes/Soybean/2016/design2016_FPSB005_corrected.csv", row.names = F, quote = F)

###



# design21$Sowing_date <- as.Date("2021-04-21")
require(plyr)
design_21_22 <- rbind.fill(design21,design22)
names(CroPyDB_design)
names(design_21_22)
# names(design_21_22)[names(design_21_22) == "genotype_name"] <- "genotype.name"
names(design_21_22)[names(design_21_22) == "gen_id"] <- "genotype.id"

design15_21_22 <- rbind.fill(design_21_22,design15,design16)

names(design15_21_22)[names(design15_21_22) == "exp_UID"] <- "year_site.UID"
names(design15_21_22)[names(design15_21_22) == "plot_UID"] <- "plot.UID"
names(design15_21_22)[names(design15_21_22) == "rep"] <- "replication"
names(design15_21_22)[names(design15_21_22) == "range"] <- "plot.range"
names(design15_21_22)[names(design15_21_22) == "row"] <- "plot.row"
p_design <- subset(CroPyDB_design, !year_site.UID%in%design15_21_22$year_site.UID)
design_all <- rbind.fill(design15_21_22,p_design)
design_all <- design_all[,c("year_site.UID","plot.UID","genotype.id","plot.range","plot.row")] #,"replication"
design_all <- unique(design_all)

design_all[duplicated(design_all$plot.UID),]
design_all <- design_all[!is.na(design_all$plot.UID),]
design_all <- merge(design_all, add_gen_id, by="genotype.id",all.x = T)

## SB008 is omitted due to small repetition in other fields and bad yield quality data

#######
#######


setwd("~/public/Evaluation/Projects/KP0023_legumes/Soybean/201x/")

########
# list.dirs(path=getwd())
fnames_all <- NULL
fnames <- NULL
for (year in 2015:2022) {
  for (experiment in 1:16) {
    
    
    fnames1 = list.files(path= paste0("~/public/Evaluation/Projects/KP0023_legumes/Soybean/",as.character(year),"/SB0",as.character(experiment),"/RGB1/Segmentation") ,pattern="_data_plot.csv",recursive=F, full.names = T)
    fnames2 = list.files(path= paste0("~/public/Evaluation/Projects/KP0023_legumes/Soybean/",as.character(year),"/SB00",as.character(experiment),"/RGB1/Segmentation") ,pattern="_data_plot.csv",recursive=F, full.names = T)
    fnames <- c(fnames1,fnames2)
    # fnames<-fnames[ !grepl("Exp2",fnames) ]
    fnames_all <- c(fnames_all,fnames)
  }
}
fnames_all

require(plyr)
Data_plots_all = ldply(fnames_all, function(filename) {
  dum = fread(filename,header=T, sep="," , na.strings=c("NA","","x"))
  #If you want to add the filename as well on the column
  return(dum) })
detach("package:plyr", unload=TRUE)




fnames_all <- NULL
fnames <- NULL
for (year in 2015:2022) {
  for (experiment in 1:16) {
    
    
    fnames1 = list.files(path= paste0("~/public/Evaluation/Projects/KP0023_legumes/Soybean/",as.character(year),"/SB0",as.character(experiment),"/RGB1/Segmentation") ,pattern="_data_rows.csv",recursive=F, full.names = T)
    fnames2 = list.files(path= paste0("~/public/Evaluation/Projects/KP0023_legumes/Soybean/",as.character(year),"/SB00",as.character(experiment),"/RGB1/Segmentation") ,pattern="_data_rows.csv",recursive=F, full.names = T)
    fnames3 = list.files(path= paste0("~/public/Evaluation/Projects/KP0023_legumes/Soybean/",as.character(year),"/SB0",as.character(experiment),"/RGB1/Segmentation") ,pattern="_data_MaskRows.csv",recursive=F, full.names = T)
    fnames4 = list.files(path= paste0("~/public/Evaluation/Projects/KP0023_legumes/Soybean/",as.character(year),"/SB00",as.character(experiment),"/RGB1/Segmentation") ,pattern="_data_MaskRows.csv",recursive=F, full.names = T)
    
    fnames <- c(fnames1,fnames2,fnames3,fnames4)
    # fnames<-fnames[ !grepl("Exp2",fnames) ]
    fnames_all <- c(fnames_all,fnames)
  }
}
fnames_all

if(noMask==T){ fnames_all <-    fnames_all[!grepl("_data_MaskRows",fnames_all)]
}else{
  fnames <- gsub("data_MaskRows.csv","data_rows.csv",fnames_all)
  dup_fnames <- fnames[duplicated(fnames)]
  dup_fnames_mask <- gsub("data_rows.csv","data_MaskRows.csv",dup_fnames) ## if masked rows exists prefer it
  NoDup_fnames <- fnames[!duplicated(fnames)]
  NoDup_fnames <-   NoDup_fnames[!NoDup_fnames%in%dup_fnames]
  fnames_all <- c(NoDup_fnames,dup_fnames_mask)
}
fnames_all

require(plyr)
Data_rows_all = ldply(fnames_all, function(filename) {
  dum = fread(filename,header=T, sep="," , na.strings=c("NA","","x"))
  #If you want to add the filename as well on the column
  return(dum) })
detach("package:plyr", unload=TRUE)
##########


########## process the data


Data_rows <- rbind(setDT(Data_rows_all), setDT(Data_plots_all),fill=T)

Data_rows$Filename_org <- Data_rows$Filename
Data_rows$Filename <- gsub("\\./","", Data_rows$Filename)
Data_rows$Filename <- gsub("\\.png","", Data_rows$Filename)


Data_rows <- setDT(Data_rows)[, c("Exp","Cam","Seg","Date","Filename") := tstrsplit(Filename, "/", fixed=TRUE)]
Data_rows$Filename <-    gsub("_05_","05",Data_rows$Filename)
Data_rows$Filename <-    gsub("_06_","06",Data_rows$Filename)
Data_rows$Filename <-    gsub("_07_","07",Data_rows$Filename)
Data_rows$Filename <-    gsub("_08_","08",Data_rows$Filename)
Data_rows$Filename <-    gsub("_09_","09",Data_rows$Filename)

Data_rows2015 <-    Data_rows[grepl("2015_0",Data_rows$Date),]
Data_rows2015$Filename <-    gsub("FPNK","FPSB",Data_rows2015$Filename)

# Data_rows2015_2 <-    Data_rows2015[grepl("_RGB1_",Data_rows2015$Date),]
Data_rows2015 <-    Data_rows2015[!grepl("_RGB1_",Data_rows2015$Filename),]

Data_rows201x <-    Data_rows[grepl("_RGB1_",Data_rows$Filename),]

Data_rows2015 <- setDT(Data_rows2015)[, c("Date","plot.UID","Segmentation") := tstrsplit(Filename, "_", fixed=TRUE)]
# Data_rows2015$Date <- paste0(Data_rows2015$Year, Data_rows2015$Month,Data_rows2015$Day)
# Data_rows2015$Month <- NULL
# Data_rows2015$Day <- NULL
Data_rows2015$Time <- NA
Data_rows2015$Mask <- NA


Data_rows201x <- setDT(Data_rows201x)[, c("plot.UID","Cam","Date","Time","Segmentation","Mask") := tstrsplit(Filename, "_", fixed=TRUE)]
Data_rows <- rbind(Data_rows2015,Data_rows201x,fill=T)

Data_rows$Date <- as.Date(Data_rows$Date, format = '%Y%m%d')
Data_rows$Year <- strftime(Data_rows$Date , format = "%Y")


Data_rows[duplicated(paste(Date, plot.UID)),]
####
p <- design_all[!is.na(design_all$genotype.id)&duplicated(design_all$plot.UID),]
p #have different genotype_name
unique(Data_rows$plot.UID[!Data_rows$plot.UID%in%design_all$plot.UID]) ## SB008 is omitted due to small repetition in other fields and bad yield quality data

Data_rows_exp <- merge(Data_rows, design_all[!is.na(design_all$genotype.id)&!duplicated(design_all$plot.UID),], by = c("plot.UID"))
# Data_rows_exp$genotype.id <- Data_rows_exp$genotype_name
Data_rows_exp[duplicated(paste(Date, plot.UID)),]
Data_rows_exp$Sum_Pixel_plot <- as.numeric(Data_rows_exp$Sum_Pixel_plot)
Data_rows_exp$Sum_Pixel_row <- as.numeric(Data_rows_exp$Sum_Pixel_row)

ggplot(Data_rows_exp, aes(color=genotype.id))+
  geom_point(aes(x=0, y=min_y_at_x0))+geom_point(aes(x=0, y=max_y_at_x0))+
  geom_point(aes(x=5000, y=min_y_at_xmax))+geom_point(aes(x=5000, y=max_y_at_xmax))+
  facet_wrap(.~Date)

ggplot(Data_rows_exp, aes(color=genotype.id))+
  geom_point(aes(x=0, y=Plot_width))+
  facet_wrap(.~Date)


make_loop_plots_with_plot_edges <- function(investigate, Pattern = "_segmentation.png", ncol_plot = 3) {
  library(magick)
  library(grid)
  library(cowplot)
  library(ggplot2)
  library(data.table)
  library(gridExtra)
  
  # Ensure year is properly set
  year <- as.character(investigate$Year)[1]
  
  make_plots <- function(files_to_plot, ncol_plot) {
    if (nrow(files_to_plot) > 0) {
      plots <- lapply(files_to_plot$File, function(file) {
        if (is.na(file)) {
          grid::textGrob("No Data", gp = gpar(fontsize = 10, col = "grey"))
        } else {
          print(paste("Read", file))
          img <- image_read(file)
          # Convert image to a data array
          
          
          # img_data <- image_data(img)
          # 
          # img_matrix <- as.numeric(img_data)   # Normalize to range 0-1
          # 
          # if(length(img_matrix[round(img_matrix, 1) == 0.6] )<10){
            img <- image_convert(img, colorspace = "Gray")
            img_data <- image_data(img)
            img_matrix <- as.numeric(img_data)   # Normalize to range 0-1
            img_matrix[round(img_matrix, 1) == 0] <- 1 
            img_matrix[round(img_matrix, 1) == 0.7] <- 0  # Set 0.5555 to 1
            img_matrix[round(img_matrix, 1) == 0.8] <- 0  # Set 0.5555 to 1
            
            # }else{
            # # hist(img_matrix)
            # # Apply transformations
            # # img_matrix[round(img_matrix, 1) == 1] <- 0  # Set 1 to 0
            # img_matrix[round(img_matrix, 1) == 0.6] <- 1  # Set 0.5555 to 1
            # img_matrix[round(img_matrix, 1) == 0] <- 0 
            # # Convert back to integer scale (0-255)
            # }
            # 

          # Convert back to magick image
          img <- image_read(img_matrix)

          imagePNG <- image_scale(img, "50%")
          
          image_info_df <- image_info(img)  # Extract image metadata
          
          maxY <- image_info_df$height  # Correctly extract image height
          maxX <- image_info_df$width   # Correctly extract image width
          
          g <- rasterGrob(imagePNG, interpolate = T)
          
          points_data <- data.frame(
            x = c(0, 0, maxX, maxX),
            y = c(
              maxY - investigate$min_y_at_x0[1], 
              maxY - investigate$max_y_at_x0[1], 
              maxY - investigate$min_y_at_xmax[1], 
              maxY - investigate$max_y_at_xmax[1]
            )
          )
          
          ggimage_plot <- ggplot(points_data, aes(x,y),geom="blank") +
            scale_y_continuous(limits=c(0,maxY)) +
            scale_x_continuous(limits=c(0,maxX)) +
            theme(legend.position="none",axis.text = element_blank(), axis.ticks = element_blank(), axis.title= element_blank(),
                  plot.margin=unit(c(0,0,0,0),"line")) +
            annotation_custom(g, xmin=0, xmax=maxX, ymin=0, ymax=maxY) +
            # geom_image(image=file)+
            geom_point(aes(x,y), color="darkred", size=3,shape=13)+
            coord_fixed()
          
          return(ggimage_plot)
        }
      })
      
      gg <- plot_grid(plotlist = plots, ncol = ncol_plot, labels = as.character(files_to_plot$TitleNr), label_size = 16, label_colour = "#77AADD")
      title <- ggdraw() + draw_label(paste("Plot:", files_to_plot$UID[1]), fontface = 'bold', size = 16)
      final_plot <- plot_grid(title, gg, ncol = 1, rel_heights = c(0.1, 1))
      return(final_plot)
    }
  }
  
  ToPlot <- data.table(
    File = paste0("~/public/Evaluation/Projects/KP0023_legumes/Soybean/",year, gsub("\\.\\/","\\/",investigate$Filename_org)), 
    TitleNr = investigate$Date, 
    UID = investigate$plot.UID, 
    genotype.name = investigate$genotype.name
  )
  
  ToPlot <- ToPlot[order(UID, TitleNr)]
  
  if (nrow(ToPlot) == 0) {
    print(paste("Skip", year))
  } else {
    gg_list <- lapply(unique(ToPlot$UID), function(xx) make_plots(ToPlot[UID == xx], ncol_plot))
    
    NperPlot <- ToPlot[, .(N = .N), by = .(UID)]
    NperPlot[, N_ceiling := ceiling(N / ncol_plot)]
    nrow_plot_overall <- sum(NperPlot$N_ceiling)
    
    final_plot <- arrangeGrob(grobs = gg_list, ncol = 1)
    title <- ggdraw() + draw_label(paste("Breeding line:", ToPlot$genotype.name[1]), fontface = 'bold', size = 18)
    final_plot <- plot_grid(title, final_plot, ncol = 1, rel_heights = c(0.05, 1))
    
    ggsave(
      filename = paste0(year, Pattern, ToPlot$genotype.name[1], "_checks.png"),
      plot = final_plot,
      dpi = 150,
      units = "mm",
      height = nrow_plot_overall * 60 + 1,
      width = 90 * ncol_plot
    )
  }
}


investigate <- subset(Data_rows_exp, Year==2021&genotype.name=="Protéix")
investigate <- subset(investigate, plot.UID==investigate$plot.UID[1])

# make_loop_plots_with_plot_edges(investigate, Pattern = "_segmentation.png", ncol_plot = 6) 


######


p <- Data_rows_exp[!is.na(Data_rows_exp$Sum_Pixel_plot),]
p <- p[duplicated(paste(p$Sum_Pixel_plot,p$plot.UID)),]

o <- subset(Data_rows_exp, Sum_Pixel_plot%in%p$Sum_Pixel_plot)

Data_rows_exp$below_delta_y <- Data_rows_exp$min_y_at_x0-Data_rows_exp$min_y_at_xmax
Data_rows_exp$upper_delta_y <- Data_rows_exp$max_y_at_x0-Data_rows_exp$max_y_at_xmax

Data_rows_exp$delta_y_at_x0 <- Data_rows_exp$max_y_at_x0-Data_rows_exp$min_y_at_x0
# Data_rows_exp$delta_y_at_xmax <- Data_rows_exp$min_y_at_xmax-Data_rows_exp$max_y_at_xmax

Data_rows_exp$delta_y_at_x500 <- Data_rows_exp$delta_y_at_x0 + Data_rows_exp$below_delta_y*0.1-Data_rows_exp$upper_delta_y*0.1
Data_rows_exp$delta_y_at_x1500 <- Data_rows_exp$delta_y_at_x0 + Data_rows_exp$below_delta_y*0.3-Data_rows_exp$upper_delta_y*0.3
Data_rows_exp$delta_y_at_x2500 <- Data_rows_exp$delta_y_at_x0 + Data_rows_exp$below_delta_y*0.5-Data_rows_exp$upper_delta_y*0.5
Data_rows_exp$delta_y_at_x3500 <- Data_rows_exp$delta_y_at_x0 + Data_rows_exp$below_delta_y*0.7-Data_rows_exp$upper_delta_y*0.7
Data_rows_exp$delta_y_at_x4500 <- Data_rows_exp$delta_y_at_x0 + Data_rows_exp$below_delta_y*0.9-Data_rows_exp$upper_delta_y*0.9

Data_rows_exp$Area_0 <- Data_rows_exp$delta_y_at_x500 *1000
Data_rows_exp$Area_1000 <- Data_rows_exp$delta_y_at_x1500 *1000
Data_rows_exp$Area_2000 <- Data_rows_exp$delta_y_at_x2500 *1000
Data_rows_exp$Area_3000 <- Data_rows_exp$delta_y_at_x3500 *1000
Data_rows_exp$Area_4000 <- Data_rows_exp$delta_y_at_x4500 *1000

Data_rows_exp$meanArea <- Data_rows_exp$delta_y_at_x2500 *5000

p <- subset(Data_rows_exp,Year%in%2020&genotype.id%in%unique(Data_rows_exp$genotype.id)[1])
ggplot(p, aes(color=as.factor(plot.UID)))+
  # # geom_point(aes(x=0, y=min_y_at_x0))+geom_point(aes(x=0, y=max_y_at_x0))+
  # geom_point(aes(x=5000, y=min_y_at_xmax))+geom_point(aes(x=5000, y=max_y_at_xmax))+
  geom_point(aes(x=500, y=delta_y_at_x500))+geom_point(aes(x=500, y=delta_y_at_x500))+
  geom_point(aes(x=1500, y=delta_y_at_x1500))+geom_point(aes(x=1500, y=delta_y_at_x1500))+
  geom_point(aes(x=2500, y=delta_y_at_x2500))+geom_point(aes(x=2500, y=delta_y_at_x2500))+
  geom_point(aes(x=3500, y=delta_y_at_x3500))+geom_point(aes(x=3500, y=delta_y_at_x3500))+
  geom_point(aes(x=4500, y=delta_y_at_x4500))+geom_point(aes(x=4500, y=delta_y_at_x4500))+
  geom_point(aes(x=3000, y=Plot_width),shape=2)+
  facet_wrap(.~Date)



middle_row_plot_variables <- names(Data_rows_exp)[names(Data_rows_exp)%in%c(paste0("Row_middle_",1:4), paste0("Plot_begin_Row_middle_",1:4), paste0("Plot_end_Row_middle_",1:4))]


remove_outliers <- function(x,IQR_times, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25,0.5, .75), na.rm = na.rm, ...)
  H <- IQR_times #* IQR(x, na.rm = na.rm)
  y <- x
  NAbefore <- length(y[is.na(y)])
  y[x < (qnt[1] - H* (qnt[3]-qnt[1]) )] <- NA
  y[x > (qnt[3] +  H* (qnt[3]-qnt[1]) )] <- NA
  y <- as.numeric(y)
  NAafter <- length(y[is.na(y)])
  print(paste("removed",NAafter-NAbefore))
  
  return(y)
}
hist(Data_rows_exp$meanArea)
# Data_rows_exp$meanArea[Data_rows_exp$meanArea<0.25] <- NA
# Data_rows_exp$meanArea[Data_rows_exp$meanArea>0.4] <- NA

# Data_rows_exp[,meanArea:=remove_outliers(meanArea,1.5),by=.(year_site.UID) ]
Data_rows_exp <- Data_rows_exp[order(Data_rows_exp$Date),]
Data_rows_exp[,meanArea_roll:=frollmean(meanArea,6, na.rm = T,align = "left"),by=.(plot.UID ,year_site.UID) ]
Data_rows_exp$meanArea[is.na(Data_rows_exp$meanArea)] <- Data_rows_exp$meanArea_roll[is.na(Data_rows_exp$meanArea)] 
Data_rows_exp$meanArea[is.na(Data_rows_exp$meanArea)] <- median(Data_rows_exp$meanArea,na.rm = T)

p <- subset(Data_rows_exp,Year%in%2020)
ggplot(p, aes(color=(genotype.id)))+
  geom_point(aes(x=0, y=meanArea))+facet_wrap(.~Date)
# Data_rows_exp$meanArea <- Data_rows_exp$meanArea*5000*5000
Data_rows_melt1 <- melt.data.table(Data_rows_exp, id.vars = c("Filename","genotype.name","genotype.id","plot.UID","year_site.UID","plot.row","plot.range","Year","Time","Date","Canopy_cover"), measure.vars = paste0("Sum_pixel_",(0:4)*1000)) #,"SD_Pixel_row","Sum_Pixel_row","Canopy_cover"
Data_rows_melt1$variable_area <- "value"
Data_rows_melt2 <- melt.data.table(Data_rows_exp, id.vars = c("Filename","genotype.name","genotype.id","plot.UID","year_site.UID","plot.row","plot.range","Year","Time","Date","Canopy_cover"), measure.vars = paste0("Area_",(0:4)*1000)) #,"SD_Pixel_row","Sum_Pixel_row","Canopy_cover"
Data_rows_melt2$variable_area <- "Area"

Data_rows_melt <- rbind(Data_rows_melt1,Data_rows_melt2)

Data_rows_melt[,value:=remove_outliers(value,1.5),by=.(plot.UID,genotype.id,year_site.UID,Date,variable_area) ]
Data_rows_melt <- Data_rows_melt[!is.na(Data_rows_melt$value),]
# Data_rows_melt[,value:=remove_outliers(value,1.5),by=.(plot.UID,year_site.UID,Time,Date,Filename,variable_area) ]
# Data_rows_melt <- Data_rows_melt[!is.na(Data_rows_melt$value),]

Means_pixels1 <- Data_rows_melt[, list(Sum_Pixel_plot=mean(value,na.rm=T), SD_Pixel_row=sd(value,na.rm=T)), by=.( Year, Date,Time, Filename, genotype.id, genotype.name,plot.UID, year_site.UID,plot.row,plot.range,variable_area)]
Means_pixels2 <- Data_rows_melt
Means_pixels2$variable <- gsub("Sum_pixel","",Means_pixels2$variable)
Means_pixels2$variable <- gsub("Area","",Means_pixels2$variable)
Means_pixels2 <- dcast.data.table(Means_pixels2, ...~variable_area, value.var = c("value"))
Means_pixels2 <- Means_pixels2[is.na(value), value:=median(value,na.rm=T), by=.( Year, Date,Time, Filename, genotype.id, genotype.name,plot.UID, year_site.UID,plot.row,plot.range)]
Means_pixels2 <- Means_pixels2[is.na(Area), Area:=median(Area,na.rm=T), by=.( Year, Date,Time, Filename, genotype.id, genotype.name,plot.UID, year_site.UID,plot.row,plot.range)]
Means_pixels2 <- Means_pixels2[, list(Sum_Pixel_plot=mean(value/Area,na.rm=T), SD_Pixel_row=sd(value/Area,na.rm=T)), by=.( Year, Date,Time, Filename, genotype.id, genotype.name,plot.UID, year_site.UID,plot.row,plot.range)]
Means_pixels2 <- subset(Means_pixels2, Sum_Pixel_plot<1)
Means_pixels2[,Sum_Pixel_plot:=remove_outliers(Sum_Pixel_plot,1.5),by=.(year_site.UID,Date) ]
Means_pixels2$variable_area <- "MeanValue"

###
Means_pixels1 <- dcast.data.table(Means_pixels1, ...~variable_area, value.var = c("Sum_Pixel_plot","SD_Pixel_row"))
names(Means_pixels1)[names(Means_pixels1)=="Sum_Pixel_plot_value"] <- "Mean_Sum_Pixel_plot"
names(Means_pixels1)[names(Means_pixels1)=="Sum_Pixel_plot_Area"] <- "meanArea"
names(Means_pixels1)[names(Means_pixels1)=="SD_Pixel_row_value"] <- "Mean_SD_Pixel_row"
Means_pixels1$Mean_Sum_Pixel_plot <- Means_pixels1$Mean_Sum_Pixel_plot
# 
Means_pixels2 <- dcast.data.table(Means_pixels2, ...~variable_area, value.var = c("Sum_Pixel_plot","SD_Pixel_row"))
names(Means_pixels2)[names(Means_pixels2)=="Sum_Pixel_plot_MeanValue"] <- "Mean_canopy_cover"

# hist(Mean_pixels$Sum_Pixel_plot)
# hist(Mean_pixels$meanArea)
# Mean_pixels <- melt.data.table(Mean_pixels, id.vars = c("Filename","genotype.name","genotype.id","plot.UID","year_site.UID","plot.row","plot.range","Year","Time","Date","meanArea"), measure.vars = c("Sum_Pixel_plot","Canopy_cover","SD_Pixel_row")) #,"SD_Pixel_row","Sum_Pixel_row","Canopy_cover"
Means_pixels1$meanArea <- NULL
names_both <- c(names(Means_pixels1)[names(Means_pixels1)%in%names(Data_rows_exp)])
Data_rows_mean <- merge(Means_pixels1,Data_rows_exp, by=names_both, all = T)

Data_rows_vars <- rbind(Means_pixels2,Data_rows_mean,fill=T)
Data_rows_vars <- melt.data.table(Data_rows_vars, id.vars = c("Filename","genotype.name","genotype.id","plot.UID","year_site.UID","plot.row","plot.range","Year","Time","Date","meanArea",middle_row_plot_variables), measure.vars =c("SD_Pixel_row","Sum_Pixel_plot","Canopy_cover","Mean_canopy_cover","Mean_Sum_Pixel_plot") )
Data_rows_vars <- Data_rows_vars[!is.na(Data_rows_vars$value),]

Data_rows_vars$genotype.name[duplicated(paste(Data_rows_vars$value, Data_rows_vars$genotype.name,Data_rows_vars$variable))]

p <- subset(Data_rows_vars, plot.UID%in%c("FPSB0040086","FPSB0040002")) # badly grown or sown

ggplot(data=p, aes(Date, value, color=plot.UID))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(size=1.5, alpha=1)+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F)+
  # guides(color = guide_legend(nrow=3))+
  facet_grid(variable~Year+year_site.UID,scale="free",switch="both", labeller = label_parsed)

# Data_rows_vars <- subset(Data_rows_vars, plot.UID!="FPSB0040086") # badly grown or sown
# 
# ggplot(data=Data_rows_vars, aes(Date, value, color=genotype.id))+ ylab("Canopy cover (%)")+
#   theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
#   # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
#   geom_point(size=1.5, alpha=1)+
#   geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F)+
#   # guides(color = guide_legend(nrow=3))+
#   facet_grid(variable~Year+year_site.UID,scale="free",switch="both", labeller = label_parsed)

# ggsave("Data_FIP_soybean.png",  width = 240, height = 160, units = "mm")




###

Data_rows_melt_RowNr <- Data_rows_vars#[!is.na(Data_rows_melt$Loess_residuals),]
unique(Data_rows_melt_RowNr$variable)
# Data_rows_melt_RowNr <- Data_rows_melt_RowNr[1,]


#### calculate canopy cover for row values

Data_rows_melt_CC <- subset(Data_rows_melt_RowNr, variable%in%c("Canopy_cover","Mean_canopy_cover"))

Data_rows_melt_RowNr <- melt.data.table(subset(Data_rows_melt_RowNr, variable%in%c("Sum_Pixel_plot","Mean_Sum_Pixel_plot")), measure.vars = middle_row_plot_variables, variable.name = "Row_middle", value.name= "Row_middle_value")
Data_rows_melt_RowNr <- Data_rows_melt_RowNr[order(Data_rows_melt_RowNr$Row_middle_value, decreasing = T),]
Data_rows_melt_RowNr <- Data_rows_melt_RowNr[!is.na(Data_rows_melt_RowNr$Row_middle_value),]
unique(Data_rows_melt_RowNr$variable)

Data_rows_melt_RowNr <- Data_rows_melt_RowNr[Data_rows_melt_RowNr$Row_middle_value>0,]
Data_rows_melt_RowNr <- Data_rows_melt_RowNr[order(Data_rows_melt_RowNr$plot.UID,Data_rows_melt_RowNr$variable,Data_rows_melt_RowNr$Date)]
Data_rows_melt_RowNr

Data_rows_melt_RowNr[, Row_middle:=paste0("Row_middle_",nrow(.SD):1), by=.(Year, Date,Time, Filename, genotype.id, plot.UID, year_site.UID,variable)] # fix me in pipline

p <- subset(Data_rows_melt_RowNr,variable=="Sum_Pixel_plot")

ggplot(data=p, aes(Date, Row_middle_value, color=genotype.id, shape=Row_middle))+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9),axis.title = element_blank())+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(aes(color=genotype.id, shape=Row_middle),size=2.5, alpha=0.6)+
  geom_line(aes(group=paste(genotype.id,Row_middle), color=genotype.id))+
  facet_grid(.~Year+year_site.UID,scale="free",switch="both", labeller = label_parsed)


Data_rows_melt_RowNr$value[duplicated(paste(Data_rows_melt_RowNr$value, Data_rows_melt_RowNr$Row_middle))]

Data_rows_melt_cleaned <- dcast.data.table(Data_rows_melt_RowNr, ...~Row_middle, value.var = "Row_middle_value")




Data_rows_melt_cleaned$Distance_row1_2 <- Data_rows_melt_cleaned$Row_middle_2-Data_rows_melt_cleaned$Row_middle_1
Data_rows_melt_cleaned$Distance_row2_3 <- Data_rows_melt_cleaned$Row_middle_3-Data_rows_melt_cleaned$Row_middle_2

Row_distance <- melt.data.table(Data_rows_melt_cleaned, measure.vars = c("Distance_row1_2","Distance_row2_3"), variable.name = "Distance", value.name= "Row_distance")
Row_distance$Row_distance[Row_distance$Row_distance<500] <- NA 
Row_distance[,Row_distance:=remove_outliers(Row_distance,1.5),by=.(Year,Date,year_site.UID, variable) ]
hist(Row_distance$Row_distance)
# hist(Row_distance$Row_distance[Row_distance$Date=="2020-06-23"])

Row_distance[,Row_distance:=mean(Row_distance,na.rm = T),by=.(Year,genotype.id,year_site.UID, variable,plot.UID) ]
Row_distance[duplicated(paste(Date, plot.UID,Filename,variable,Distance)),]
Row_distance <- as.data.frame(Row_distance)
Row_distance <- Row_distance[order(Row_distance$Date),]
Row_distance <- setDT(Row_distance)
Row_distance[,Mean_Row_distance:=frollmean(Row_distance,3, na.rm = T,align = "right"),by=.(plot.UID ,year_site.UID) ]

require(zoo)
# Row_distance[,Mean_Row_distance:=na.locf(Row_distance),by=.(plot.UID ,year_site.UID) ]
Row_distance$Mean_Row_distance[is.na(Row_distance$Mean_Row_distance)]
Row_distance[,Median:=median(Mean_Row_distance,na.rm=T),by=.(Year,year_site.UID, Date) ]
Row_distance$Mean_Row_distance[is.na(Row_distance$Mean_Row_distance)] <- Row_distance$Median[is.na(Row_distance$Mean_Row_distance)]

Row_distance <- Row_distance[,list(Mean_Row_distance=mean(Mean_Row_distance,na.rm=T)),by=.(Filename, genotype.name, genotype.id, plot.UID ,year_site.UID,plot.row,plot.range,Year,Time,Date,meanArea,variable,value) ]
Row_distance$Area <- 5616*Row_distance$Mean_Row_distance*2
Row_distance$Area[Row_distance$Mean_Row_distance>1250] <- 5616*Row_distance$Mean_Row_distance[Row_distance$Mean_Row_distance>1250]

Row_distance$value_relative <- Row_distance$value/Row_distance$Area
Row_distance$value_relative_Area <- Row_distance$value/Row_distance$meanArea

plot(Row_distance$value_relative,Row_distance$value_relative_Area )

###

# 

###
Data_rows_melt_CC$value_relative <- Data_rows_melt_CC$value#/Data_rows_melt_CC$meanArea
##
Data_relative <- rbind(Data_rows_melt_CC, Row_distance,fill=T)
# Data_relative$value_relative[Data_relative$value_relative>1] <- NA
Data_relative[,value_relative:=remove_outliers(value_relative,2.5),by=.(year_site.UID,Date,variable) ]

# Dates_canopy_cover <- unique(Data_relative$Date[Data_relative$variable=="Canopy_cover"])
# Data_relative <- subset(Data_relative, !(variable=="Mean_canopy_cover"&!Date%in%Dates_canopy_cover)) # exclude values because there are too small if canopy is not fully covered
Data_relative <- subset(Data_relative, variable%in%c("Sum_Pixel_plot", "Canopy_cover")) # exclude because the other because the values do not match

unique(Data_relative$variable)
double_analyzed <- unique(Data_relative[,c("Date","variable")])
double_analyzed <-  subset(double_analyzed, variable%in%c("Sum_Pixel_plot", "Canopy_cover", "Mean_Sum_Pixel_plot", "Mean_canopy_cover"))
double_analyzed <- double_analyzed[duplicated(double_analyzed$Date),]
double_analyzed

double_analyzed_data <- subset(Data_relative, Date%in%  double_analyzed$Date)


ggplot(data=double_analyzed_data, aes(Date, value_relative))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(size=1.5, alpha=1,aes(color=genotype.id,shape=variable))+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(group=variable))+
  # guides(color = guide_legend(nrow=3))+
  facet_grid(.~Year+year_site.UID,scale="free",switch="both", labeller = label_parsed)


double_analyzed_data <- double_analyzed_data[,list(genetic_SD=sd(value_relative),population_mean=mean(value_relative)),by=.(variable,Date)]

##### check for higher h2 when canopy cover was extracted with two methods
p <-  subset(Data_relative, Date%in%  double_analyzed$Date & variable%in%c("Sum_Pixel_plot", "Canopy_cover", "Mean_Sum_Pixel_plot", "Mean_canopy_cover")) #
p$row <- p$plot.row
p$range <- p$plot.range
p$date <- p$Date
p <- p[!is.na(p$value),]
p <- p[!is.na(p$row),]
p <- p[!is.na(p$range),]
p <- p[!is.na(p$genotype.id),]

p <- droplevels(p)
p <- setDT(p)[, NperTrial:=nrow(.SD), by=.(variable,year_site.UID,date)]
p <- subset(p, NperTrial>40)

p[,nrow(.SD),by=.(Date,variable,year_site.UID)]
p$date2 <- p$date

BLUEs_Traits_vars <- setDT(p)[,spats_blues(.SD), by=.(year_site.UID,date,date,variable)]
BLUEs_Traits_vars[,nrow(.SD),by=.(genotype.id,variable)]

p_h2 <-  unique(BLUEs_Traits_vars[,c("h2","year_site.UID","date","variable","N_Datapt_removed")])
p_h2[order(p_h2$date),]
p_h2$month <- as.numeric(format(as.Date(p_h2$date),"%m"))
p <- p_h2#[,mean(h2), by = .(year_site.UID, variable, month)]
# p[order(p$year_site.UID,p$month,p$variable),]
ggplot(data=p, aes(x=variable, y=h2) ) +
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=0.6,color="white"),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 0, hjust = 0.5),text = element_text(size=12),strip.text = element_text(size = 12), axis.title = element_text(size = 12))+
  geom_jitter(aes(color=date))+
  geom_boxplot( outlier.colour = NA, alpha=0.75)+
  facet_grid(year_site.UID~month)

p_h2_max <- p_h2[, list(h2=h2[which.max(h2)][1],variable=variable[which.max(h2)][1]), by = .(year_site.UID, date)]
p_h2_max

p_h2_max[,nrow(.SD),by=.(variable)]
mean(p_h2_max$h2)
p_h2_2vars <- p_h2[variable%in%c("Canopy_cover","Sum_Pixel_plot","Mean_Sum_Pixel_plot"), list(h2=h2[which.max(h2)][1],variable=variable[which.max(h2)][1]), by = .(year_site.UID, date)]
mean(p_h2_2vars$h2)
p_h2_2vars <- p_h2[variable%in%c("Canopy_cover","Sum_Pixel_plot"), list(h2=h2[which.max(h2)][1],variable=variable[which.max(h2)][1]), by = .(year_site.UID, date)]
mean(p_h2_2vars$h2)

double_analyzed_exclude <- subset(p_h2, !paste(variable,date)%in%paste(p_h2_max$variable,p_h2_max$date))


Data_relative <- subset(Data_relative, !paste(variable,Date)%in% paste(double_analyzed_exclude$variable,double_analyzed_exclude$date))
Data_relative$variable_org <- Data_relative$variable
Data_relative$variable <- "Canopy_cover"

hist(Data_relative$value_relative)
unique(Data_relative$Year)
Data_relative <- unique(Data_relative)
Data_relative <- Data_relative[!duplicated(paste(Date, plot.UID,Filename,variable,value,Time,value_relative)),]


ggplot(data=Data_relative, aes(Date, value_relative))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(size=1.5, alpha=1,aes(color=genotype.id))+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(group=1))+
  # guides(color = guide_legend(nrow=3))+
  facet_grid(.~Year+year_site.UID,scale="free",switch="both", labeller = label_parsed)

###
Data_relative_all <- Data_relative
Data_relative_all$value <- Data_relative_all$value_relative
# p <- subset(Data_rows_melt_RowNr, !variable%in%c("Canopy_cover","Sum_Pixel_plot"))
# p <- p[!duplicated(paste(plot.UID,Date,value)),]# 2 row middle position values
# Data_relative_all <- rbind(Data_relative_all, p,fill=TRUE)
Data_relative_all <- Data_relative_all[,c(names(Data_relative_all)[1:10],"variable","value"),with=F]
Data_relative_all$location = 'Eschikon'
Data_relative_all <- unique(Data_relative_all)
p <- Data_relative_all[duplicated(paste(Date, plot.UID,Filename,variable,Time)),]
oo <- subset(Data_relative, Filename%in%p$Filename[1])
oo <- subset(Data_rows_melt, Filename%in%p$Filename[1])

Data_relative_all <- Data_relative_all[!duplicated(paste(Date, plot.UID,Filename,variable,Time)),] ## fix me

####
setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/stats-lab-crops/")
soybeans_UAV <- fread("data/Phenotype_genotype_variables_20221123.csv")
soybeans_UAV <- soybeans_UAV[, -1]
soybeans_UAV$location = 'Eschikon'
soybeans_UAV[stringr::str_starts(soybeans_UAV$UID, 'D'), 'location'] = 'Delley'
unique(soybeans_UAV$variable)
soybeans_UAV <- subset(soybeans_UAV, variable=="Canopy.coverage")
soybeans_UAV$variable <- "Canopy_cover"
hist(soybeans_UAV$value)
colnames(soybeans_UAV)[colnames(soybeans_UAV) == "UID"] <- "plot.UID"
colnames(soybeans_UAV)[colnames(soybeans_UAV) == "range"] <- "plot.range"
colnames(soybeans_UAV)[colnames(soybeans_UAV) == "row"] <- "plot.row"
soybeans_UAV$Date <- as.Date(soybeans_UAV$Date)
soybeans_UAV$plot_number <- NULL
soybeans_UAV$Year <- strftime(soybeans_UAV$Date, format = "%Y")
soybeans_UAV$platform <- "UAV"

soybeans_UAV$genotype.name <- NULL
soybeans_UAV <- merge(soybeans_UAV, add_gen_id,by="genotype.id")

Data_relative_all$platform <- "FIP"
Data_relative_all <- rbind(Data_relative_all, soybeans_UAV,fill=TRUE)
Data_relative_all <- unique(Data_relative_all)

###
Data_rows_distance_relative <- dcast.data.table(Data_relative_all, ...~variable)
Data_rows_distance_relative$value_relative <- Data_rows_distance_relative$Canopy_cover
Data_rows_distance_relative$variable <- "Canopy_cover"

Data_rows_distance_relative <- Data_rows_distance_relative[!is.na(value_relative),]
unique(Data_rows_distance_relative$Year)
Data_rows_distance_relative <- droplevels(Data_rows_distance_relative)
require(mgcv)
# Data_rows_distance_relative[,Loess_fit:= gam(value_relative~ s(as.numeric(Date)),data=.SD)$fit,by=.(plot.UID,genotype.id, Year,year_site.UID, variable)]
# Data_rows_distance_relative[,max_height:= max(Loess_fit,na.rm = T),by=.(plot.UID,genotype.id,Year,year_site.UID, variable)]
# 
######

Data_rows_distance_relative$date_of_sowing = as.Date('2016-04-21', format = '%Y-%m-%d')
Data_rows_distance_relative[Data_rows_distance_relative$location == 'Eschikon' & Data_rows_distance_relative$Year == 2015, 'date_of_sowing'] = as.Date('2015-04-10', format = '%Y-%m-%d')

Data_rows_distance_relative[Data_rows_distance_relative$location == 'Eschikon' & Data_rows_distance_relative$Year == 2016, 'date_of_sowing'] = as.Date('2016-04-21', format = '%Y-%m-%d')
Data_rows_distance_relative[Data_rows_distance_relative$location == 'Eschikon' & Data_rows_distance_relative$Year == 2017, 'date_of_sowing'] = as.Date('2017-04-12', format = '%Y-%m-%d')
Data_rows_distance_relative[Data_rows_distance_relative$location == 'Eschikon' & Data_rows_distance_relative$Year == 2018, 'date_of_sowing'] = as.Date('2018-04-19', format = '%Y-%m-%d')
Data_rows_distance_relative[Data_rows_distance_relative$location == 'Eschikon' & Data_rows_distance_relative$Year == 2019, 'date_of_sowing'] = as.Date('2019-04-23', format = '%Y-%m-%d')
Data_rows_distance_relative[Data_rows_distance_relative$location == 'Eschikon' & Data_rows_distance_relative$Year == 2020, 'date_of_sowing'] = as.Date('2020-04-15', format = '%Y-%m-%d')
Data_rows_distance_relative[Data_rows_distance_relative$location == 'Eschikon' & Data_rows_distance_relative$Year == 2021, 'date_of_sowing'] = as.Date('2021-04-16', format = '%Y-%m-%d')
Data_rows_distance_relative[Data_rows_distance_relative$location == 'Eschikon' & Data_rows_distance_relative$Year == 2022, 'date_of_sowing'] = as.Date('2022-04-21', format = '%Y-%m-%d')

Data_rows_distance_relative[Data_rows_distance_relative$location == 'Delley' & Data_rows_distance_relative$Year == 2019, 'date_of_sowing'] = as.Date('2019-05-01', format = '%Y-%m-%d')
Data_rows_distance_relative[Data_rows_distance_relative$location == 'Delley' & Data_rows_distance_relative$Year == 2020, 'date_of_sowing'] = as.Date('2020-04-27', format = '%Y-%m-%d')

table(Data_rows_distance_relative$date_of_sowing)


Data_rows_distance_relative$time_since_sowing = as.numeric(Data_rows_distance_relative$Date - Data_rows_distance_relative$date_of_sowing)
sort(unique(Data_rows_distance_relative$time_since_sowing))
######
# #### fit max date
new_data <- Data_rows_distance_relative[!duplicated(Data_rows_distance_relative$plot.UID),]
new_data$Date <- NULL
new_data$Canopy_cover <- NULL

SelectedDates <- Data_rows_distance_relative[,list(StartMeas=min(Date),EndMeas=max(Date)),by=.(year_site.UID)]
PeriodMeasured<- SelectedDates[,list(Period=seq(StartMeas, EndMeas, by="day")),by=.(year_site.UID)]


AllHours <- data.frame(Date=seq(min(Data_rows_distance_relative$Date), max(Data_rows_distance_relative$Date), by="day"))
AllHours$variable <- "Canopy_cover"
# AllHours <- rbind(AllHours[grepl("-07-0",AllHours$Date),],AllHours[grepl("-07-1",AllHours$Date),],AllHours[grepl("-07-2",AllHours$Date),],AllHours[grepl("-06-2",AllHours$Date),],AllHours[grepl("-06-3",AllHours$Date),],AllHours[grepl("-07-3",AllHours$Date),],AllHours[grepl("-08-0",AllHours$Date),],AllHours[grepl("-08-1",AllHours$Date),],AllHours[grepl("-08-2",AllHours$Date),])
AllHours <- AllHours[!grepl("-09-2",AllHours$Date),]
AllHours <- AllHours[!grepl("-09-3",AllHours$Date),]

AllHours <- AllHours[!grepl("-05-0",AllHours$Date),]
AllHours <- AllHours[!grepl("-05-1",AllHours$Date),]
AllHours <- AllHours[!grepl("-05-2",AllHours$Date),]
AllHours <- AllHours[!grepl("-05-3",AllHours$Date),]

AllHours <- subset(AllHours, Date%in%PeriodMeasured$Period)
AllHours$Year <- as.factor(format(AllHours$Date,"%Y"))


new_data <- merge(new_data,AllHours, by=c("variable","Year"),allow.cartesian = T )
new_data <- new_data[!duplicated(paste(new_data$genotype.id,new_data$Date,new_data$plot.UID))]
new_data <- subset(new_data, Date%in%PeriodMeasured$Period)

myData_fit <- Data_rows_distance_relative[,list(Date=new_data$Date[new_data$year_site.UID==.SD$year_site.UID[1]], year_site.UID=.SD$year_site.UID[1], fit=predict(lm(value_relative~ poly(as.Date(Date),3),data=.SD), newdata=subset(new_data, year_site.UID==.SD$year_site.UID[1]))),by=.(plot.UID,genotype.id, genotype.name, plot.row, plot.range, Year, variable,date_of_sowing)]
myData_fit <- na.omit(myData_fit)
myData_fit <- subset(myData_fit, fit>0)
myData_fit <- subset(myData_fit, fit<2)

p_maxHeight_fit <- myData_fit[,list(Max_Date_fit= Date[fit==max(fit,na.rm = T)][1], value_relative=max(fit,na.rm = T)),by=.(plot.UID,genotype.id, genotype.name, plot.row, plot.range, Year,year_site.UID, date_of_sowing,variable)]
p_maxHeight_fit <- p_maxHeight_fit[!is.na(p_maxHeight_fit$Max_Date_fit),]
p_maxHeight_fit <- unique((p_maxHeight_fit))

min(p_maxHeight_fit$Max_Date_fit)
max(p_maxHeight_fit$Max_Date_fit)

# 
# 
# ##

p <- subset(myData_fit, variable%in%c("Canopy_cover")&plot.UID%in%p_maxHeight_fit$plot.UID[1])
p <- na.omit(p)
ggplot(data=p, aes(Date, fit))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(size=1.5, alpha=1,aes(shape=variable))+
  # geom_point(data=o,size=2.5, alpha=1,aes(shape=variable))+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(group=1))+
  # geom_smooth(method = lm, formula = y ~ splines::bs(x, 4), se = FALSE)+  # guides(color = guide_legend(nrow=3))+
  facet_grid(.~Year+year_site.UID,scale="free",switch="both", labeller = label_parsed)
# 
# 
# ###
# ###

#### just extract max date
Data_canopy_cover_Max <- Data_rows_distance_relative
p_maxHeight <- Data_canopy_cover_Max[,list(Max_variable_date= Date[value_relative==max(value_relative,na.rm = T)][1],MaxHeight_start_date= min(Date[value_relative>max(value_relative,na.rm = T)-0.05]),MaxHeight_end_date= max(Date[value_relative>max(value_relative,na.rm = T)-0.15]), Max_height=max(value_relative,na.rm = T)),by=.(date_of_sowing,plot.UID,genotype.id, genotype.name, plot.row, plot.range, Year,year_site.UID, variable)]
p_maxHeight$Max_variable_date_7 <- p_maxHeight$MaxHeight_start_date+10
p_maxHeight$End_variable_date_7 <- p_maxHeight$MaxHeight_end_date-15

p_maxHeight$time_since_sowing = as.numeric(p_maxHeight$Max_variable_date - p_maxHeight$date_of_sowing)

p_maxHeight[duplicated(p_maxHeight$plot.UID),]
p_maxHeight <- merge(p_maxHeight, p_maxHeight_fit[,c("plot.UID","Max_Date_fit")],by="plot.UID" )
######



Data_canopy_cover_Max <- merge(Data_canopy_cover_Max, p_maxHeight[,c("plot.UID","Max_variable_date","MaxHeight_start_date","Max_variable_date_7","End_variable_date_7","Max_height","Max_Date_fit","year_site.UID")], by=c("plot.UID","year_site.UID"))
unique(Data_canopy_cover_Max$year_site.UID)





### Growth
Data_canopy_cover_Max$id <- 1:nrow(Data_canopy_cover_Max)
Data_growth_curve <- subset(Data_canopy_cover_Max, Date<=Max_variable_date_7)
Data_growth_curve <- subset(Data_growth_curve, !(Date>Max_variable_date&value_relative<Max_height-0.05))
Data_growth_curve <- subset(Data_growth_curve, Date<=Max_Date_fit)

Data_growth_curve <- Data_growth_curve[!is.na(Data_growth_curve$value_relative),]

length(unique(Data_growth_curve$plot.UID))


##### Senescence
Data_senescence_curve <- subset(Data_canopy_cover_Max, Date>=End_variable_date_7)
# Data_senescence_curve <- subset(Data_senescence_curve, !(Date<End_variable_date_7+15&value_relative<Max_height-0.05))
Data_senescence_curve <- subset(Data_senescence_curve, Date>Max_Date_fit)


#####

unique(p_maxHeight$Max_variable_date)
unique(Data_canopy_cover_Max$Max_variable_date)



overlap <- Data_growth_curve$id[Data_growth_curve$id%in%Data_senescence_curve$id] # check for overlap

Data_canopy_cover_Max$Period <- "Reproductive"
Data_canopy_cover_Max$Period[Data_canopy_cover_Max$id%in%Data_senescence_curve$id] <- "Senescence"
Data_canopy_cover_Max$Period[Data_canopy_cover_Max$id%in%Data_growth_curve$id] <- "Growth"
Data_canopy_cover_Max$Period[Data_canopy_cover_Max$id%in%overlap] <- "Both"


###


p <- subset(Data_canopy_cover_Max, variable%in%c("Canopy_cover")&Period=="Growth")
p[,length(unique(genotype.id)),by=year_site.UID]

ggplot(data=p, aes(Date, value_relative))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(size=1.5, alpha=1,aes(color=genotype.id,shape=variable))+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(group=1))+
  # guides(color = guide_legend(nrow=3))+
  facet_grid(platform~Year+year_site.UID,scale="free",switch="both", labeller = label_parsed)

p <- subset(Data_canopy_cover_Max, variable%in%c("Canopy_cover")&Period=="Growth"&year_site.UID=="FPSB016")

ggplot(data=p, aes(Date, value_relative))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(size=1.5, alpha=1,aes(color=genotype.id,shape=variable))+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(group=1))+
  # guides(color = guide_legend(nrow=3))+
  facet_grid(.~Year+year_site.UID,scale="free",switch="both", labeller = label_parsed)

p <- subset(Data_canopy_cover_Max, variable%in%c("Canopy_cover")&Period=="Growth")
p <- subset(p, Year%in%c("2017"))
p <- subset(p, genotype.id%in%unique(p$genotype.id)[5:7])
# oo <- subset(myData_fit, variable%in%c("Canopy_cover")&plot.UID%in%p$plot.UID)

ggplot(data=p, aes(Date, value_relative))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(data=p,size=2.5, alpha=1,aes(color=plot.UID))+
  geom_vline(data=p,aes(xintercept=Max_variable_date), alpha=1)+
  # geom_point(data=oo, size=1, alpha=1,aes(Date, fit, color=plot.UID),shape=2)+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(group=plot.UID,color=plot.UID))+
  # guides(color = guide_legend(nrow=3))+
  scale_color_manual(values = tol18rainbow)+
  facet_grid(genotype.id~year_site.UID,scale="free",switch="both", labeller = label_parsed)


p <- subset(Data_canopy_cover_Max, variable%in%c("Canopy_cover")&plot.UID%in%unique(Data_canopy_cover_Max$plot.UID)[1:12]&Period=="Growth")

ggplot(data=p, aes(Date, value_relative))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(data=p,size=2.5, alpha=1,aes(color=plot.UID))+
  geom_vline(data=p,aes(xintercept=Max_variable_date), alpha=1)+
  # geom_point(data=oo, size=1, alpha=1,aes(Date, fit, color=plot.UID),shape=2)+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(group=plot.UID,color=plot.UID))+
  # guides(color = guide_legend(nrow=3))+
  scale_color_manual(values = tol18rainbow)+
  facet_grid(genotype.id~year_site.UID,scale="free",switch="both", labeller = label_parsed)



MaxGenos <- Data_canopy_cover_Max[,list(Max_variable_date= Max_variable_date[Max_variable_date==max(Max_variable_date)][1],plot.UID=plot.UID[Max_variable_date==max(Max_variable_date)][1]),by=.(year_site.UID,Year)]


o <- subset(p_maxHeight, variable%in%c("Canopy_cover")&plot.UID%in%MaxGenos$plot.UID)
p <- subset(Data_canopy_cover_Max, variable%in%c("Canopy_cover")&plot.UID%in%MaxGenos$plot.UID)

ggplot(data=p, aes(Date, value_relative))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(data=p,size=1.5, alpha=1,aes(color=Period,shape=variable))+
  # geom_point(data=o,size=2.5, alpha=1,aes(shape=variable))+
  geom_vline(data=p,size=0.5, color="grey", alpha=1,aes(xintercept = Max_variable_date))+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(data=p,method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(group=1))+
  scale_color_manual(values = tol4qualitative)+
  facet_grid(.~Year+year_site.UID,scale="free",switch="both", labeller = label_parsed)


p <- subset(Data_canopy_cover_Max, variable%in%c("Canopy_cover")&plot.UID%in%MaxGenos$plot.UID)

ggplot(data=p, aes(Date, value_relative))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(data=p,size=1.5, alpha=1,aes(color=Period,shape=variable))+
  # geom_point(data=o,size=2.5, alpha=1,aes(shape=variable))+
  geom_vline(data=p,size=0.5, color="grey", alpha=1,aes(xintercept = End_variable_date_7))+
  geom_vline(data=p,size=0.5, color="grey", linetype="dashed", alpha=1,aes(xintercept = Max_variable_date_7))+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(data=p,method="lm",formula = y ~poly(x,3),  alpha=0.25, show.legend = F, aes(group=1))+
  scale_color_manual(values = tol4qualitative)+
  facet_wrap(plot.UID~Year+year_site.UID,scale="free",switch="both", labeller = label_parsed)




library(dplyr)
library(ggplot2)

# Filter data
p <- subset(Data_canopy_cover_Max, variable %in% c("Canopy_cover"))


genotype_order <- p[,list(mean_value = mean(value_relative, na.rm = TRUE)),by=genotype.id]
genotype_order <- genotype_order[order(genotype_order$mean_value,decreasing =T),]
# Convert genotype.id to a factor with the desired order
p$genotype.id <- factor(p$genotype.id, levels = genotype_order$genotype.id)

# Ensure Year and year_site.UID are also ordered factors
p$Year <- factor(p$Year, levels = sort(unique(p$Year)))
p$year_site.UID <- factor(p$year_site.UID, levels = sort(unique(p$year_site.UID)))

p$genotype.id <- as.numeric(as.character(p$genotype.id))  # Ensure genotype.id is numeric

ggCC <- ggplot(data=p, aes(Date, value_relative, color=genotype.id)) +
  ylab("Canopy cover (%)") +
  theme_bw() +
  theme(strip.placement = "outside",
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        legend.key.size = unit(0.9, "lines"),
        legend.position="top",
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        text = element_text(size=9)) +
  geom_point(size=1.5, alpha=0.5, aes(shape=Period)) +
  geom_vline(size=0.5, alpha=1, aes(xintercept = Max_variable_date)) +
  scale_color_gradientn(name="Breeding lines", colors=c(tol4qualitative)) +  # Customize colors
  geom_smooth(method="loess", formula = y ~ x, alpha=0.1, size=0.5, show.legend = F, aes(group=genotype.id)) +
  facet_wrap(location ~ paste(Year, year_site.UID), scale="free", switch="both")

# ggCC
# ggsave("Datapoints_CC_loess_soybean.pdf",  width = 170, height = 180, units = "mm")


unique_lines <- unique(Data_canopy_cover_Max[,c("genotype.id","year_site.UID","Year","location")])
# unique_lines$year_loc <- paste(unique_lines$Year, unique_lines$location)
unique_lines[,nrow(.SD),by=Year]
unique_lines$Year_grouped <- unique_lines$Year

unique_lines$Year_grouped[unique_lines$Year=="2015"] <- "2015-16"
unique_lines$Year_grouped[unique_lines$Year=="2016"] <- "2015-16"


unique_lines$Year_grouped[unique_lines$Year=="2017"] <- "2017-20"
unique_lines$Year_grouped[unique_lines$Year=="2018"] <- "2017-20"

unique_lines$Year_grouped[unique_lines$Year=="2019"] <- "2017-20"
unique_lines$Year_grouped[unique_lines$Year=="2020"] <- "2017-20"

unique_lines$Year_grouped[unique_lines$Year=="2022"] <- "2021-22"
unique_lines$Year_grouped[unique_lines$Year=="2021"] <- "2021-22"

unique_lines$Year_grouped <- paste(unique_lines$Year_grouped,unique_lines$location)
unique(unique_lines$Year_grouped)

library(ggVennDiagram)


# Convert data.table into a list of sets for Venn diagram
venn_data <- unique_lines[, .(genotype_list = list(unique(genotype.id))), by = Year_grouped]
venn_list <- setNames(venn_data$genotype_list, venn_data$Year_grouped)

# Plot the Venn diagram
ggVenn <- ggVennDiagram(
  venn_list, 
  label_alpha = 0, set_size=2,
  label_size =2  # Decrease label text inside the Venn diagram
) +  
  theme_bw() +
  theme(
    strip.placement = "outside",
    axis.title = element_blank(),
    axis.text = element_blank(),
    strip.background = element_blank(),
    legend.key.size = unit(0.9, "lines"),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    text = element_text(size = 1),  # Decrease global text size
    plot.title = element_text(size = 1),  # Decrease title size
    legend.text = element_text(size = 1),  # Decrease legend text size
    legend.title = element_text(size = 1),  # Decrease legend title size
    strip.text = element_text(size = 1)  # Decrease facet strip text size
  ) +
  scale_fill_gradient(low = "white", high = tol5qualitative[1]) 

ggVenn


require(cowplot)
second_row <- plot_grid(ggVenn, NULL,  ncol = 2, rel_widths = c(0.5,1), labels = c(""))  #,vjust=0.5+

first_row <- plot_grid(ggCC, second_row,  ncol = 1, rel_heights = c(1,0.5), labels = c("A","B"))  #,vjust=0.5+


# ggsave("Datapoints_CC_loess_Venn_soybean.pdf", width = 180, height = 200, units = "mm", dpi = 100, first_row)


########

p <- subset(Data_senescence_curve, variable%in%c("Canopy_cover"))

ggplot(data=p, aes(Date, value_relative))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(size=1.5, alpha=1,aes(color=genotype.id,shape=variable))+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(group=1))+
  # guides(color = guide_legend(nrow=3))+
  facet_grid(.~Year+year_site.UID,scale="free",switch="both", labeller = label_parsed)



p <- setDT(Data_canopy_cover_Max)[,list(N=nrow(.SD)),by=.(year_site.UID,Year,plot.row,plot.range,plot.UID,variable,Period,location)]
p$plot.range[p$Period=="Senescence"] <- p$plot.range[p$Period=="Senescence"]+0.4
p$plot.range[p$Period=="Reproductive"] <- p$plot.range[p$Period=="Reproductive"]+0.2

ggplot(data=p, aes(x= plot.range, y=plot.row, alpha=N, color=Period)) +
  theme_bw() +
  theme(
    strip.placement = "outside",
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    legend.key.size = unit(0.9, "lines"),
    legend.position = "right",
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    # panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    text = element_text(size = 9)
  ) +
  geom_point() +
  scale_color_manual(values = tol4qualitative) +
  facet_wrap(location ~ Year + year_site.UID, scale = "free", switch = "both", labeller = label_parsed) +
  scale_x_continuous(breaks = seq(floor(min(p$plot.range)), ceiling(max(p$plot.range)), by = 1)) +
  scale_y_continuous(breaks = seq(floor(min(p$plot.row)), ceiling(max(p$plot.row)), by = 1))




if(noMask==T){ 
  Data_canopy_cover_Max$Weed_removed <- "No"
  write.csv(Data_canopy_cover_Max, "~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/data/soybean_pixels_NoMask_data.csv", row.names = F, quote = F)
}else{
  Data_canopy_cover_Max$Weed_removed <- "Yes"
  write.csv(Data_canopy_cover_Max, "~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/data/soybean_pixels_Mask_data.csv", row.names = F, quote = F)}

#################################
setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/")

require(data.table)
soybeans_FIP_UAV_Mask <- fread("data/soybean_pixels_Mask_data.csv")
soybeans_FIP_UAV_noMask <- fread("data/soybean_pixels_NoMask_data.csv")
soybeans_FIP_UAV <- rbind(soybeans_FIP_UAV_Mask,soybeans_FIP_UAV_noMask)
soybeans_FIP <- subset(soybeans_FIP_UAV, platform=="FIP")
soybeans_UAV <- subset(soybeans_FIP_UAV_noMask, platform!="FIP")

soybeans_FIP_unique <- unique(soybeans_FIP[,c("year_site.UID","Date","Weed_removed")])
soybeans_FIP_doubled <- soybeans_FIP_unique[duplicated(paste(Date)),]
soybeans_FIP_doubled$Weed_removed <- NULL
soybeans_FIP_doubled

soybean_date_masked <- unique(soybeans_FIP$Date[grepl("_mask",soybeans_FIP$Filename)])

soybeans_doubled <- subset(soybeans_FIP, platform=="FIP"&Date%in%soybeans_FIP_doubled$Date&Date%in%soybean_date_masked)

p <- soybeans_doubled
p$row <- p$plot.row
p$range <- p$plot.range
p$date <- p$Date
p <- p[!is.na(p$value),]
p <- p[!is.na(p$row),]
p <- p[!is.na(p$range),]
p <- p[!is.na(p$genotype.id),]

p <- droplevels(p)
p <- setDT(p)[, NperTrial:=nrow(.SD), by=.(platform,year_site.UID,date)]
p <- subset(p, NperTrial>40)

p[,nrow(.SD),by=.(Date,platform,year_site.UID,Weed_removed)]
p$date2 <- p$date

BLUEs_Traits <- setDT(p)[,spats_blues(.SD), by=.(year_site.UID,date,platform,date,Weed_removed)]
BLUEs_Traits[,nrow(.SD),by=.(genotype.id,platform,Weed_removed)]

p_h2 <-  unique(BLUEs_Traits[,c("h2","platform","year_site.UID","date","Weed_removed","N_Datapt_removed")])
p_h2[order(p_h2$date),]
p_h2$month <- as.numeric(format(as.Date(p_h2$date),"%m"))
months <- unique(p_h2$month)
p_h2$Month <- month.name[as.integer(p_h2$month)]
p_h2$Month <- as.factor(p_h2$Month)
p_h2$Month <- factor(p_h2$Month, levels=month.name[months])

# p <- subset(p_h2, month<9)
# ggplot(data=p, aes(x=Weed_removed, y=h2) ) +
#   theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=0.6,color="white"),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 0, hjust = 0.5),text = element_text(size=12),strip.text = element_text(size = 12), axis.title = element_text(size = 12))+
#   geom_jitter(aes(color=date))+
#   geom_boxplot( outlier.colour = NA, alpha=0.75)+
#   facet_grid(year_site.UID~Month)

p_h2_max <- p_h2[, list(h2=h2[which.max(h2)],Weed_removed=Weed_removed[which.max(h2)]), by = .(year_site.UID, date, platform)]
p_h2_max
p_h2_max[,nrow(.SD),by=.(Weed_removed)]

date_mask <- subset(p_h2_max, Weed_removed=="Yes")

soybeans_FIP_Mask <- subset(soybeans_FIP_UAV_Mask, platform=="FIP"&Date%in%date_mask$date)
soybeans_FIP_NoMask <- subset(soybeans_FIP_UAV_noMask, platform=="FIP"&!Date%in%date_mask$date)
nrow(rbind(soybeans_FIP_Mask,soybeans_FIP_NoMask))
nrow(soybeans_FIP)/2
nrow(soybeans_UAV)

nrow(soybeans_FIP_UAV_Mask)
nrow(soybeans_FIP_UAV_noMask)

soybean <- rbind(soybeans_UAV , soybeans_FIP_NoMask, soybeans_FIP_Mask)
nrow(soybean)
nrow(soybean)-nrow(soybeans_FIP_UAV_Mask)
nrow(soybean)-nrow(soybeans_FIP_UAV_noMask)
setDT(soybean)[,length(unique(platform)),by="year_site.UID"]
setDT(soybean)[,(unique(Date)),by="year_site.UID"]

soybean <- soybean[order(plot.UID,Date,Time,year_site.UID,platform,Weed_removed)]
setDT(soybean)[,Run:=1:nrow(.SD),by=.(plot.UID,Date,year_site.UID,platform,Weed_removed)]
hist(soybean$Run)
setDT(soybean)[is.na(replication),c:=1:nrow(.SD),by=.(plot.UID,Date,Run,year_site.UID,platform,Weed_removed)]

p <- (setDT(soybean)[,nrow(.SD),by=.(plot.UID,Date,Run,year_site.UID,platform,Weed_removed)])
subset(p,V1>1)

p <- soybean
p$row <- p$plot.row
p$range <- p$plot.range
p$date <- p$Date
p <- p[!is.na(p$value),]
p <- p[!is.na(p$row),]
p <- p[!is.na(p$range),]
p <- p[!is.na(p$genotype.id),]

p <- droplevels(p)
p <- setDT(p)[, NperTrial:=nrow(.SD), by=.(platform,year_site.UID,date,Run)]
p <- subset(p, NperTrial>40)
p[,length(unique(plot.UID)),by=.(platform,year_site.UID)]

p[,nrow(.SD),by=.(Date,platform,year_site.UID,Weed_removed,Run)]
p$date2 <- p$date

BLUEs_Traits_platfrom <- setDT(p)[,spats_blues(.SD), by=.(year_site.UID,date,Run,platform,date,Weed_removed)]
BLUEs_Traits_platfrom[,nrow(.SD),by=.(genotype.id,platform,Weed_removed)]

write.csv(BLUEs_Traits_platfrom,"BLUEs_Traits_Canopy_Cover.csv", row.names = F, quote = F)
# BLUEs_Traits_platfrom <- fread("BLUEs_Traits_Canopy_Cover.csv")

p_h2_platform <-  unique(BLUEs_Traits_platfrom[,c("h2","platform","year_site.UID","Run","date","Weed_removed","N_Datapt_removed")])
hist(p_h2_platform$h2)
removed_due_to_low_h2 <- p_h2_platform$date[p_h2_platform$h2<0.1]
removed_due_to_low_h2
soybean <- subset(soybean, !Date%in%removed_due_to_low_h2)
nrow(soybean)


p <- (setDT(soybean)[,nrow(.SD),by=.(plot.UID,Date,Run,year_site.UID,platform,Weed_removed)])
subset(p,V1>1)
# p <- subset(soybean, Date=="2022-07-25")

write.csv(soybean, "~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/data/soybean_pixels_data.csv", row.names = F, quote = F)

p_h2_platform$treatment <- "No"
p_h2_platform$variable <- "Platform"

p_h2$platform <- "FIP weed"
p_h2$platform[p_h2$Weed_removed=="No"] <- "FIP ctrl"
p_h2$treatment <- p_h2$Weed_removed
p_h2$variable <- "Segmentation"

# p <- subset(p_h2, !date%in%p_h2_platform$date&platform=="FIP")
p <- subset(p_h2, date%in%p_h2$date[p_h2$Weed_removed=="Yes"])

p_h2_all <- rbind(p_h2_platform,p,fill=T)
p_h2_all[order(p_h2_all$date),]
p_h2_all[,nrow(.SD),by=.(treatment)]
p_h2_all$month <- as.numeric(format(as.Date(p_h2_all$date),"%m"))
p_h2_all$Month <- month.name[as.integer(p_h2_all$month)]
p_h2_all$Month <- as.factor(p_h2_all$Month)
months <- unique(as.integer(p_h2_all$month))
months <- months[order(months)]
p_h2_all$Month <- factor(p_h2_all$Month, levels=month.name[months], ordered=TRUE)

p_h2_all[duplicated(paste(p_h2_all$date,p_h2_all$h2,p_h2_all$year_site.UID)),]

time_since_sowing <- unique(soybean[,c("time_since_sowing","Date")])

p <-  subset(p_h2_all,year_site.UID%in%c("FPSB012","FPSB013","FPSB014","FPSB015","FPSB016")&month<9)
p <- merge(p, time_since_sowing, by.x="date",by.y = "Date")

p$platform <- as.factor(p$platform)
p$platform <- factor(p$platform, levels=levels(p$platform)[c(2,3,1,4)])
levels(p$platform)
p$xintercept <- 2.5
p$xintercept[p$Month%in%c("July","August")] <- NA
ggplot(data=p, aes(x=platform, y=h2, fill=platform) ) + ylab("Heritability")+ xlab("Method")+
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=0.6,color="white"),legend.key.size = unit(0.6, "lines"),legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_blank(),text = element_text(size=9),strip.text = element_text(size = 8), axis.title = element_text(size = 11))+
  geom_jitter(aes(color=time_since_sowing),width=0.1,size=0.5)+
  # geom_point(data=subset(p, Weed_removed=="Yes"&platform=="FIP weed"),aes(color=time_since_sowing),x=1.2,size=0.5)+
  # geom_point(data=subset(p, Weed_removed=="No"&platform=="FIP weed"),aes(color=time_since_sowing),x=0.8,size=0.5)+
  geom_boxplot( outlier.colour = NA, alpha=0.6)+
  scale_color_gradientn(name="Days after \n sowing",colors=c("grey","black"))+
  scale_fill_manual(name='Method',values = tol4qualitative) +
  geom_vline(aes(xintercept = xintercept),color="grey40")+
  guides(
    fill = guide_legend(nrow = 2, title.position = "top"), # Arrange fill legend in 2 rows
    color = guide_colorbar(title.position = "top"))+
  facet_grid(year_site.UID~Month,scales = "free",space="free")

# ggsave("H2_soybean.pdf",  width = 95, height = 160, units = "mm")
p <- setDT(soybean)[,list(Datapoints=nrow(.SD)),by=.(year_site.UID,Year,plot.row,plot.range,plot.UID,variable,Period,location)]
subset(p,Datapoints>50)
oo <- subset(soybean, plot.UID=="FPSB0140085")
p$plot.range[p$Period=="Senescence"] <- p$plot.range[p$Period=="Senescence"]+0.4
p$plot.range[p$Period=="Reproductive"] <- p$plot.range[p$Period=="Reproductive"]+0.2

p$location_year <- paste(p$location, p$Year, sep=", ")

ggplot(data=p, aes(x= plot.range, y=plot.row, alpha=Datapoints, color=Period)) + xlab("Range")+ylab("Row")+
  theme_bw() +
  theme(
    strip.placement = "outside",
    # axis.title.x = element_blank(),
    strip.background = element_blank(),
    legend.key.size = unit(0.9, "lines"),
    legend.position = "right",
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    # panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    text = element_text(size = 9)
  ) +
  geom_point() +
  scale_color_manual(values = tol4qualitative) +
  facet_wrap(location_year ~  year_site.UID, scale = "free", switch = "both") +
  scale_x_continuous(breaks = seq(floor(min(p$plot.range)), ceiling(max(p$plot.range)), by = 1)) +
  scale_y_continuous(breaks = seq(floor(min(p$plot.row)), ceiling(max(p$plot.row)), by = 1))

# ggsave("Datapoints_CC_soybean.pdf",  width = 170, height = 160, units = "mm")



p_h2_all[,mean(h2),by=.(platform,year_site.UID)]

p_h2_max_FIP <- p_h2_all[, list(h2=h2[which.max(h2)],Date=date[which.max(h2)],Weed_removed=Weed_removed[which.max(h2)]), by = .(year_site.UID, Month, month, platform)]
p_h2_max_FIP <- subset(p_h2_max_FIP, platform=="FIP"&month<9)

Selected_measuring_days <- subset(soybean, paste(platform,Date)%in%paste(p_h2_max_FIP$platform, p_h2_max_FIP$Date ))
Selected_measuring_days$Month <- as.numeric(format(as.Date(Selected_measuring_days$Date),"%m"))

write.csv(Selected_measuring_days, "Selected_measuring_days.csv", row.names = F, quote = F)
