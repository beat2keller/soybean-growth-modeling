require(data.table)
require(ggplot2)

################

tol14rainbow=c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C")
tol15rainbow=c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA")
tol18rainbow=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
#####

setwd("~/public/Evaluation/Projects/KP0023_legumes/CroPyDB_export/export_legumes/")


fnames_design = list.files(pattern="design_",recursive=T)
# fnames<-fnames[grepl("CroPyDB_export",fnames) ]
fnames_design
require(plyr)
CroPyDB_design1 = ldply(fnames_design[-13], function(filename) {
  dum = fread(filename,header=T, sep="," , na.strings=c("NA"))
  names(dum) <- gsub("plot_","plot\\.",names(dum))
  #If you want to add the filename as well on the column
  return(dum) })
detach("package:plyr", unload=TRUE)

require(plyr)
CroPyDB_design2 = ldply(fnames_design[13], function(filename) {
  dum = fread(filename,header=T, sep="," , na.strings=c("NA"))
  names(dum) <- gsub("plot_","plot\\.",names(dum))
  #If you want to add the filename as well on the column
  return(dum) })
detach("package:plyr", unload=TRUE)

names(CroPyDB_design1)
names(CroPyDB_design2)

names(CroPyDB_design2)[1] <- "year_site.UID"
names(CroPyDB_design2)[3] <- "genotype.id"
names(CroPyDB_design2)[7:10] <- paste0("plot.",names(CroPyDB_design2)[7:10] ) 
setDT(CroPyDB_design2)[,plot.replication:=1:nrow(.SD),by=.(plot.UID)]

CroPyDB_design <- rbind(setDT(CroPyDB_design1),setDT(CroPyDB_design2),fill=T)
####

fnames = list.files(pattern="trait_data",recursive=T)
fnames<-fnames[ !grepl("genotype_trait_data_",fnames) ]
fnames

require(plyr)
CroPyDB_export = ldply(fnames, function(filename) {
  dum = fread(filename,header=T, sep="," , na.strings=c("NA"))
  #If you want to add the filename as well on the column
  return(dum) })
detach("package:plyr", unload=TRUE)
CroPyDB_export

#####
fnames_add = list.files(pattern="_data",recursive=T,path = "~/public/Evaluation/FIP/RW/2021/SB/RefTraits/db/",full.names = T)
fnames_add <- fnames_add[3:5]
# moisture<- read.csv("~/public/Evaluation/FIP/RW/2021/SB/RefTraits/db/moisture/FPSB015_moisture_data.csv")
# oil<- read.csv("~/public/Evaluation/FIP/RW/2021/SB/RefTraits/db/oil/FPSB015_oil_data.csv")
require(plyr)
CroPyDB_export_add = ldply(fnames_add, function(filename) {
  dum = read.csv(filename)
  #If you want to add the filename as well on the column
  dum$filename <- filename
  return(dum) })
detach("package:plyr", unload=TRUE)
# write.csv(moisture, "~/public/Evaluation/FIP/RW/2021/SB/RefTraits/db/moisture/FPSB015_moisture_data-copy.csv", row.names = F, quote = F)
names(CroPyDB_export_add) <- gsub("_","\\.",names(CroPyDB_export_add))
names(CroPyDB_export_add)[7] <- "trait.label"
CroPyDB_export_add <- setDT(CroPyDB_export_add)
unique(CroPyDB_export_add$filename)
CroPyDB_export_add$trait.name <- NA
CroPyDB_export_add$trait.name[grepl("yield",CroPyDB_export_add$filename)] <- "Yield"
CroPyDB_export_add$trait.name[grepl("tkw",CroPyDB_export_add$filename)] <- "TKW"
CroPyDB_export_add$trait.name[grepl("protein",CroPyDB_export_add$filename)] <- "Protein content"

CroPyDB_export_add$timestamp <- "20211001T0100+0100"
  
CroPyDB_export <- setDT(CroPyDB_export)

CroPyDB_export <- rbind(CroPyDB_export,CroPyDB_export_add ,fill=T)
#####


CroPyDB_export$variable <- CroPyDB_export$trait.name
CroPyDB_export$variable <- gsub("Thousand kernel weight","TKW",CroPyDB_export$variable)

levels(as.factor(CroPyDB_export$variable))

setwd("~/public/Evaluation/Projects/KP0023_legumes/")

# nrow(p)
p <- subset(CroPyDB_export, variable=="Protein content")
p$value[duplicated(p$value)]

CroPyDB_export_all <- merge(CroPyDB_export, CroPyDB_design[,c("plot.UID","plot.replication","genotype.id","plot.range_global","plot.row_global","plot.range","plot.row")], by=c("plot.UID"))

# CroPyDB_design$plot.UID[grepl("FPSB007",CroPyDB_design$plot.UID)]
# unique(CroPyDB_export$variable[grepl("FPSB007",CroPyDB_export$plot.UID)])

# CroPyDB_export_all[grepl("FPSB007",CroPyDB_export_all$plot.UID),]

levels(as.factor(CroPyDB_export_all$variable))
names(CroPyDB_export_all) <- gsub("plot\\.", "", names(CroPyDB_export_all) )
names(CroPyDB_export_all) 


add_gen_id <- read.csv("~/public/Evaluation/Projects/KP0023_legumes/Design/2024/GenotypeID_all.csv")
add_gen_id <- subset(add_gen_id, crop=="soybean")
add_gen_id <- add_gen_id[,c("id","name")]
add_gen_id$Genotype_name <- add_gen_id$name
add_gen_id$genotype.id <- add_gen_id$id
add_gen_id <- add_gen_id[!duplicated(add_gen_id$Genotype_name),]

###
add_gen_id <- add_gen_id[!duplicated(add_gen_id$genotype.id),]
add_gen_id$name <- NULL
add_gen_id$id <- NULL
CroPyDB_export_all <- merge(CroPyDB_export_all, add_gen_id, by="genotype.id")
CroPyDB_export_all$variable <- gsub(" ", ".", CroPyDB_export_all$variable  )
unique(CroPyDB_export_all$variable )

Growth_traits_melt <- subset(CroPyDB_export_all, variable %in% c("Yield","Protein.content","TKW"))
Growth_traits_melt$timestamp <- gsub("T"," ",Growth_traits_melt$timestamp)
Growth_traits_melt$Datetime <- as.POSIXct(Growth_traits_melt$timestamp,format = "%Y%m%d %H%M%z")
Growth_traits_melt$Datetime
Growth_traits_melt$Date <- as.Date(Growth_traits_melt$Datetime)
Growth_traits_melt$year_site.UID <- substr(Growth_traits_melt$UID,1,7)
unique(Growth_traits_melt$year_site.UID)
names(Growth_traits_melt)
Growth_traits_melt <- setDT(Growth_traits_melt)
Growth_traits_melt$plot.UID <- Growth_traits_melt$UID

Growth_traits_melt$Rep <- Growth_traits_melt$replication
Growth_traits_melt$Row <- Growth_traits_melt$row
Growth_traits_melt$Range <- Growth_traits_melt$range

#########


############# 2016 Ref traits
# design_2016 <- read_excel("/home/kellebea/public/Evaluation/FIP/2016/Feldbuch/SB/2016_FPSB005_FPSB006_FINAL_LK.xlsx",sheet = 4)
design_2016 <- read.csv("/home/kellebea/public/Evaluation/Projects/KP0023_legumes/Soybean/2016/design2016_FPSB005_corrected.csv")

require(readxl)
yield_SB006_2016 <- read_excel("/home/kellebea/public/Evaluation/FIP/2016/Feldbuch/SB/Kornertrag_SB005_SB006_St (003).xlsx",sheet = 2)
yield_SB006_2016 $`TKG/100` <- NULL
names(yield_SB006_2016)[10] <- "Plot_ID"
names(yield_SB006_2016)[12] <- "TKW"
names(yield_SB006_2016)[13] <- "Yield"
yield_SB006_2016$Yield <- yield_SB006_2016$Yield/10
yield_SB006_2016$Date <- as.POSIXct("2016-08-31", tz="Europe/Berlin") # not true
yield_SB006_2016 <- as.data.table(yield_SB006_2016)
yield_SB006_2016 <- melt.data.table(yield_SB006_2016, id.vars = c("Plot_ID","Plot","Date"), measure.vars = c("Yield","TKW"))

yield_SB005_2016 <- read_excel("/home/kellebea/public/Evaluation/FIP/2016/SB005/Ref_Traits/single_row_harvest.xls",sheet = 1)
yield_SB005_2016 <- as.data.table(yield_SB005_2016)
names(yield_SB005_2016)[8] <- "PlantNo1"
names(yield_SB005_2016)[9] <- "PlantNo"
# yield_SB005_2016$Plot_ID <- paste0("FPSB00500",yield_SB005_2016$Plot)
yield_SB005_2016 <- merge(yield_SB005_2016,design_2016[,c("row","range","plot_UID")], by.x=c("Row","Col"),by.y=c("row","range"))
yield_SB005_2016$Plot_ID <- yield_SB005_2016$plot_UID
# names(yield_SB005_2016)[11] <- "Yield" # approx
yield_SB005_2016$Yield <- yield_SB005_2016$Pod_FM/((yield_SB005_2016$PlantNo+yield_SB005_2016$PlantNo1)*0.5)
yield_SB005_2016$Date_harvested <- yield_SB005_2016$Date
yield_SB005_2016$Date <- as.POSIXct("2016-08-31", tz="Europe/Berlin") # not true

yield_SB005_2016$Yield <- yield_SB005_2016$Yield/10# approx
yield_SB005_2016 <- melt.data.table(yield_SB005_2016, id.vars = c("Plot_ID","Plot","Date"), measure.vars = c("Plant.FM","Yield","PlantNo1","PlantNo"))

SPAD_SB005_2016 <- read_excel("/home/kellebea/public/Evaluation/FIP/2016/SB005/Ref_Traits/SPAD_SB005.xls",sheet = 1)
SPAD_SB005_2016 <- as.data.table(SPAD_SB005_2016)
SPAD_SB005_2016 <- melt.data.table(SPAD_SB005_2016, id.vars = c("Plot_ID","Plot","Date"), measure.vars = c("SPAD"))

Vigour_SB005_2016 <- read_excel("/home/kellebea/public/Evaluation/FIP/2016/SB005/Ref_Traits/Vigour_SB005.xls",sheet = 1)
Vigour_SB005_2016 <- as.data.table(Vigour_SB005_2016)
names(Vigour_SB005_2016) <- gsub(" ","_",names(Vigour_SB005_2016) )
Vigour_SB005_2016 <- melt.data.table(Vigour_SB005_2016, id.vars = c("Plot_ID","Plot","Date"), measure.vars = c("Vigour_LK","Vigour_FL"))

BBCH_SB005_2016 <- read_excel("/home/kellebea/public/Evaluation/FIP/2016/SB005/Ref_Traits/BBCH_SB005.xls",sheet = 1)
BBCH_SB005_2016 <- as.data.table(BBCH_SB005_2016)
BBCH_SB005_2016 <- melt.data.table(BBCH_SB005_2016, id.vars = c("Plot_ID","Plot","Date"), measure.vars = c("BBCH"))

Traits_2016 <- rbind(yield_SB006_2016, yield_SB005_2016, SPAD_SB005_2016, Vigour_SB005_2016, BBCH_SB005_2016)

names(design_2016)
names(Traits_2016)

Traits_2016 <- merge(Traits_2016, design_2016, by.x=c("Plot_ID"),by.y=c("plot_UID"))
# Traits_2016 <- merge(add_gen_id,Traits_2016, by.x="Genotype_name", by.y=c("genotype.name"))
Traits_2016$Row <- Traits_2016$row
Traits_2016$Range <- Traits_2016$range
Traits_2016$Genotype_name <- Traits_2016$genotype.name


######
#Traits_2016$Date <- as.POSIXct("2016-06-01", tz="Europe/Berlin")  # not true
####
Traits_2016_melt <- melt.data.table(Traits_2016, id.vars = c("Plot_ID","Plot","Date","Row","Range","variable","genotype.id","Genotype_name"), measure.vars = c("value"))
Traits_2016_melt$variable.1 <- NULL
Traits_2016_melt <- Traits_2016_melt[!is.na(Traits_2016_melt$value),]

###
#2017 is in data.base; but not yield
############# 2017 Ref traits
yield_SB008_2017 <- read_excel(skip=1,"/home/kellebea/public/Evaluation/FIP/2017/SB007/Ref_Traits/FPSB007_Ernterohdaten.xlsx",sheet = 1)
yield_SB008_2017 <- as.data.table(yield_SB008_2017)
names(yield_SB008_2017)
names(yield_SB008_2017)[4] <- "Yield" 
names(yield_SB008_2017)[1] <- "Plot_ID" 
yield_SB008_2017$Yield <- yield_SB008_2017$Yield/10
yield_SB008_2017$Date <- as.POSIXct("2017-06-01", tz="Europe/Berlin") # not true

design_2017 <- read_excel("/home/kellebea/public/Evaluation/FIP/2017/Feldbuch/SB/Design_FPSB2017.xlsx", sheet = 4)
Traits_2017 <- merge(yield_SB008_2017, design_2017, by="Plot_ID")
Traits_2017 <- merge(add_gen_id,Traits_2017, by.x="Genotype_name", by.y=c("Gen"))
Traits_2017 <- setDT(Traits_2017)

Traits_2017_melt <- melt.data.table(Traits_2017, id.vars = c("Plot_ID","Plot","Date","Row","Range","genotype.id","Genotype_name"), measure.vars = c("Yield"))

#############
Traits_2016_17_melt <-  rbind(Traits_2016_melt,Traits_2017_melt)

###################
################
##########
Traits_2016_17_melt$plot.UID <- Traits_2016_17_melt$Plot_ID
Traits_2016_17_melt$year_site.UID <- substr(Traits_2016_17_melt$plot.UID,1,7)


Traits_2016_17_melt <- Traits_2016_17_melt[order(Traits_2016_17_melt$year_site.UID, Traits_2016_17_melt$variable, Traits_2016_17_melt$Range,  Traits_2016_17_melt$Row)]
Traits_2016_17_melt[,Rep:=1:nrow(.SD),by=.(genotype.id, variable, year_site.UID)]


# Traits_2016_17_melt$genotype.id <- as.character(Traits_2016_17_melt$genotype.id)
names(Traits_2016_17_melt)[names(Traits_2016_17_melt)=="genotype.name"] <- "Genotype_name"
Traits_2016_17_melt <- merge(Traits_2016_17_melt, add_gen_id, by="genotype.id")



###
traits_cast1 <- Growth_traits_melt[,c("plot.UID","value","variable","Genotype_name","genotype.id","year_site.UID","Row","Range","Rep","Date")]
traits_cast <- dcast.data.table(traits_cast1, ...~variable)
traits_cast$value <- traits_cast$Protein.content*traits_cast$Yield
traits_cast$variable <- "Protein.yield"
cc <- c(names(traits_cast)[names(traits_cast)%in%names(traits_cast1)])
traits_cast <- as.data.frame(traits_cast)
traits_cast <- traits_cast[,cc]


###
Traits_2016_17_melt_2 <- Traits_2016_17_melt[,names(Traits_2016_17_melt)%in%names(traits_cast1),with=F]
Traits_2016_17_melt_2$Date <- as.Date(Traits_2016_17_melt_2$Date)

##
yield_data <- rbind(traits_cast1,traits_cast,Traits_2016_17_melt_2,fill=T)
yield_data$Treatment <- "Control" # fix me

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


######
traits <- c("Yield","Protein.content","Protein.yield","TKW" )
yield_data$Location <- "Eschikon"
yield_data$Location[grepl("DSSB",yield_data$year_site.UID)] <- "Lindau"

dat <- yield_data[,c("value","variable","Location","year_site.UID","Row","Range","Rep","plot.UID","genotype.id","Date","Treatment")]
year_site.UIDs <- unique(dat$year_site.UID)[order(unique(dat$year_site.UID))]

###


### exclude newer data
year_site.UIDs
p <- subset(dat, year_site.UID%in%year_site.UIDs[1:15])
p <- subset(p, variable%in%c(traits)&value!=0)

###

p$value[p$value==Inf] <- NA
p$value[p$variable=="Yield"&p$value>7] <- NA

###
p <- setDT(p)
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
  geom_jitter()+
  # geom_boxplot( outlier.colour = NA, alpha=0.75)+
  facet_grid(variable~.)

# write.csv(BLUE, "Soybean/201x/SpATScorr-20241108-BLUE_Soybean.csv", row.names = F)
# setwd("/home/kellebea/public/Evaluation/Projects/KP0023_legumes/Soybean/201x/")
# SpATsBLUE <- read.csv("Soybean/201x/SpATScorr-20241108-BLUE_Soybean.csv")
###########




######
p1 <- subset(dat, variable%in%c("Yield","Protein.content","Protein.yield")&year_site.UID%in%year_site.UIDs[1:15])

p1$value[p1$variable=="Yield"&p1$value>7] <- NA


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

# write.csv(SpATsBLUE_overall, "Soybean/201x/SpATScorr-20241108-BLUE_overall_Soybean.csv", row.names = F)

p_h2_all <-  unique(SpATsBLUE_overall[,c("h2","year_site.UID","Date","N_Datapt_removed","variable")])
hist(p_h2_all$h2)

p <- subset(p_h2_all)
ggplot(data=p, aes(x=year_site.UID, y=h2) ) +
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=0.6,color="white"),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 0, hjust = 0.5),text = element_text(size=12),strip.text = element_text(size = 12), axis.title = element_text(size = 12))+
  geom_jitter()+
  # geom_boxplot( outlier.colour = NA, alpha=0.75)+
  facet_grid(variable~.)


#####
Selected_measuring_days <- fread("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/Selected_measuring_days.csv")
p <- setDT(Selected_measuring_days)
p$value <- p$value_relative
p <- subset(p, value!=0)
p$value[p$value==Inf] <- NA
p <- p[!is.na(p$value),]
p$Rep <- p$replication
p$Rep[!is.na(p$Rep )]
p$Range <- p$plot.range
p$Range[is.na(p$Range )]
p$Row <- p$plot.row
p$Row[is.na(p$Row )]
setDT(p)[, nrow(.SD), by=.(Month,platform)]
setDT(p)[, length(unique(year_site.UID)), by=.(Month,platform)]

BLUEs_selected <- setDT(p)[,getSpats(.SD), by=.(Month,platform)]

# setDT(BLUEs_selected)[, nrow(.SD), by=.(Month,platform)]
# write.csv(BLUEs_selected, "Soybean/201x/BLUEs_CanopyCover_overall_Soybean.csv", row.names = F)
