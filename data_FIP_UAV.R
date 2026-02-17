setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/")

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


source("~/public/Evaluation/Projects/KP0023_legumes/Scripts/fip-soybean-canopycover/functions/spats_blues.R")

######

design_all <- read.csv("data/design_2015_2022.csv")
add_gen_id <- unique(design_all[,c("genotype.id","genotype.name")])
add_gen_id_UID <- unique(design_all[,c("genotype.id","genotype.name","plot.UID")])

###############

#######
#######

if(noMask==T){ 
Data_relative_FIP <- fread("~/public/Evaluation/Projects/KP0023_legumes/Soybean/201x/Get_raw_data_soybean/data/Soybean_CanopyCover_data.csv")
  
}else{
Data_relative_FIP <- fread( "data/Soybean_CanopyCover_Mask_data.csv")}
Data_relative_FIP <- merge(Data_relative_FIP, design_all, by="plot.UID")

####
soybeans_UAV <- fread("~/public/Evaluation/Projects/KP0023_legumes/Scripts/stats-lab-crops/data/Phenotype_genotype_variables_20221123.csv")
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
soybeans_UAV$genotype.id <- NULL
soybeans_UAV <- merge(soybeans_UAV, add_gen_id_UID, by="plot.UID")



Data_relative_FIP$platform <- "FIP"
Data_relative_all <- rbind(Data_relative_FIP, soybeans_UAV,fill=TRUE)
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
        legend.key.size = unit(1.2, "lines"),
        legend.position="top",
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        text = element_text(size=9)) +
  geom_point(size=0.5, alpha=0.5, aes(shape=Period)) +
  guides(alpha = guide_legend(override.aes = list(alpha=1)),shape = guide_legend(override.aes = list(size=1.5)))+
  geom_vline(size=0.25, alpha=0.5, linetype="dotted", aes(xintercept = Max_variable_date, color=genotype.id)) +
  scale_color_gradientn(name="Breeding lines", colors=c(tol4qualitative)) +  # Customize colors
  geom_smooth(method="loess", formula = y ~ x, fill=NA, alpha=0.05, size=0.25, show.legend = F, aes(group=genotype.id)) +
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

p <- subset(Data_canopy_cover_Max, variable%in%c("Canopy_cover"))

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
  write.csv(Data_canopy_cover_Max, "~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/data/Soybean_pixels_NoMask_data.csv", row.names = F, quote = F)
}else{
  Data_canopy_cover_Max$Weed_removed <- "Yes"
  write.csv(Data_canopy_cover_Max, "~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/data/Soybean_pixels_Mask_data.csv", row.names = F, quote = F)}

#################################
setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/")

require(data.table)
soybeans_FIP_UAV_Mask <- fread("data/Soybean_pixels_Mask_data.csv")
soybeans_FIP_UAV_noMask <- fread("data/Soybean_pixels_NoMask_data.csv")

names(soybeans_FIP_UAV_Mask)[!names(soybeans_FIP_UAV_Mask)%in%names(soybeans_FIP_UAV_noMask)]
names(soybeans_FIP_UAV_noMask)[!names(soybeans_FIP_UAV_noMask)%in%names(soybeans_FIP_UAV_Mask)]

soybeans_FIP_UAV <- rbind(soybeans_FIP_UAV_Mask,soybeans_FIP_UAV_noMask)
uniqueN(soybeans_FIP_UAV$Date)
uniqueN(soybeans_FIP_UAV$genotype.id)

soybeans_FIP <- subset(soybeans_FIP_UAV, platform=="FIP")
soybeans_UAV <- subset(soybeans_FIP_UAV_noMask, platform!="FIP")

soybeans_UAV[,uniqueN(.SD$Date),by=.(platform,year_site.UID)]

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
setDT(soybean)
soybean[is.na(replication), 
        replication := seq_len(.N), 
        by = .(plot.UID, Date, Run, year_site.UID, platform, Weed_removed)]

library(data.table)

dt <- as.data.table(soybean)
# dt[, date_run := paste(Date, Run)]

result <- dt[, .(
  Plots = uniqueN(plot.UID),
  Genotypes = uniqueN(genotype.id),
  Platform = paste(unique(platform), collapse = ","),
  Measurements = uniqueN(Date)
), by = .(year_site.UID, location, Year)]
result$Platform <- gsub("UAV,FIP","FIP,UAV",result$Platform)
result$Trial <- 1:16
names(result)[1:2] <- c("Year_Site.UID","Location")
result

library(xtable)
latex_table <- xtable(result)
print(latex_table, file = "soybean_summary_table.tex", include.rownames = FALSE)

###

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
removed_due_to_low_h2 <- p_h2_platform[p_h2_platform$h2<0.05]
removed_due_to_low_h2
soybean <- subset(soybean, !paste(year_site.UID, Date)%in%paste(removed_due_to_low_h2$year_site.UID, removed_due_to_low_h2$date))
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
p <- subset(p_h2, date%in%p_h2$date[p_h2$Weed_removed=="Yes"]&h2>0.2)

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
  ylim(c(0.2,1))+
  guides(
    fill = guide_legend(nrow = 2, title.position = "top"), # Arrange fill legend in 2 rows
    color = guide_colorbar(title.position = "top"))+
  facet_grid(year_site.UID~Month,scales = "free",space="free")

# ggsave("H2_soybean.pdf",  width = 95, height = 160, units = "mm")
p <- setDT(soybean)[,list(Datapoints=nrow(.SD)),by=.(year_site.UID,Year,plot.row,plot.range,plot.UID,variable,Period,location)]
nrow(soybean)
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
