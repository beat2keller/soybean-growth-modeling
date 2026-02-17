# setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/")

Period <- "Growth"
# Period <- "Senescence"

# this code fits the model

# Packages
library(nlme)
library(ggplot2)
library(data.table)

# data "plot_grouped_global" is used for the grouping of the data
df = read.csv("data/model_data.csv")
nrow(subset(df, period%in%c("Reproductive")))
df <- subset(df, period%in%c(Period,"Both"))

unique(df$date)
setDT(df)[,length(unique(platform)),by="year_site.UID"]
df$date <- as.Date(df$date)
hist(df$value)

# restore factor variables lost due to saving 
df$genotype.id   <- as.factor(df$genotype.id) 
df$plot_grouped_global   <- ordered(as.factor(df$plot_grouped_global))
#prepare grouping
df <- as.data.frame(df)
df <- droplevels(df)
df$Filename <- NULL
# df$gdd_temperature <- NULL

df$time[is.na(df$time)] <- "one_measurement_per_day"

#transform outcome  
df$value        <- asin(sqrt(df$value))
df <- df[!is.na(df$value),]
setDT(df)[,N:=nrow(.SD),by=UID]
# setDT(df)[,length(unique(genotype.id)),by=year_site.UID]

# df <- subset(df, N>4) 
if(Period=="Senescence"){
  df$time_since_sowing <- df$time_since_sowing* (-1)
}

p <- subset(df, genotype.id%in%unique(genotype.id)[1:10])
ggplot(data=p, aes(date, value))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(size=1.5, alpha=1,aes(color=genotype.id, shape=platform))+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(group=1),color="black")+
  # guides(color = guide_legend(nrow=3))+
  facet_grid(genotype.id~Location+year,scale="free",switch="both", labeller = label_parsed)


# 
p <- df
p <- setDT(p)[,Rep:=c(1:nrow(.SD)),by=.(genotype.id,date,year_site.UID)]
p$Rep <- paste("Rep",p$Rep )

p$Date <- p$date
ggplot(data=p, aes(Date, value, color=genotype.id))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_point(size=1.5, alpha=1)+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F)+
  facet_grid(platform~year_site.UID,scale="free",switch="both", labeller = label_parsed)

oo <- subset(p, year_site.UID=="FPSB016"&genotype.id==p$genotype.id[year_site.UID=="FPSB016"][1])
ggplot(data=oo, aes(Date, value, color=Rep))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_point(size=3, aes(shape=(Rep)))+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F)+
  facet_grid(.~year_site.UID,scale="free",switch="both", labeller = label_parsed)



# lapply(unique(p$year_site.UID), function(exp_UID) {
#   gg1 <- ggplot(data=subset(p, year_site.UID==exp_UID), aes(Date, value, color=genotype.id,shape=Rep))+ ylab("Canopy cover (%)")+xlab(exp_UID)+
#     theme_bw()+theme(strip.placement = "outside", strip.text = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
#     geom_point(size=1.5)+
#     geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(linetype=Rep),size=0.75)+
#     facet_wrap(genotype.id~year_site.UID,scale="free",switch="both", labeller = label_parsed)
#   print(gg1)
#   ggsave(paste0("Lines_",exp_UID,".png"),  width = 170, height = 160, units = "mm", dpi = 100, bg="white",gg1)
# }
# )

df <- as.data.frame(df)
df$platform_num <- as.numeric(as.factor(df$platform))
# grouped DF
df <- groupedData(value ~ time_since_sowing | plot_grouped_global, data = df)
# list object for later modelling
fm1Soy.lis <- nlsList( value ~ SSlogis(time_since_sowing, Asym, xmid, scal), data = df, control=list(lower=c(0.0,0.0,0.0,0),maxiter=1000))


# remove outliers suggested by last year. Needed for convergence
fm1Soy.table <- as.data.frame(summary(fm1Soy.lis)$coef)
fm1Soy.table$plot_grouped_global <- names(fm1Soy.lis)
fm1Soy.table <- setDT(fm1Soy.table)
# fm1Soy.table$plot_grouped_global <-1:nrow(fm1Soy.table)

p <- melt.data.table(fm1Soy.table, measure.vars = c("Estimate.Asym","Estimate.xmid","Estimate.scal"))
ggplot(p,aes(x=variable,y=value))+geom_boxplot()+
  facet_wrap(variable~.,scales = "free")


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

outlier_plots1 <- fm1Soy.table$plot_grouped_global[is.na(fm1Soy.table$Estimate.Asym)]
outlier_plots2 <- fm1Soy.table$plot_grouped_global[is.na(fm1Soy.table$Estimate.xmid)]
outlier_plots3 <- fm1Soy.table$plot_grouped_global[is.na(fm1Soy.table$Estimate.scal)]
outlier_plots <- c(outlier_plots1,outlier_plots2, outlier_plots3)
outlier_plots <- outlier_plots[!duplicated(outlier_plots)]
outlier_plots

for (ii in 1:length(outlier_plots)) {
  exclude <- outlier_plots[ii]
  df_plot <- subset(df, plot_grouped_global==exclude)
  # df_plot <- na.omit(df_plot)
  # plot(as.Date(df_plot$date), df_plot$value,main=df_plot$year_site.UID[1])
  gg <- ggplot(df_plot, aes(x=date, y=value, color=UID,shape=platform)) +
    geom_point() +
    labs(title=paste("Plot for", df_plot$year_site.UID[1]),
         x="Date", y="Value") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
    # facet_grid()
  print(gg)
}



######


df <- subset(df, !plot_grouped_global %in%outlier_plots)
setDT(df)[,length(genotype.id[!duplicated(genotype.id)])]
setDT(df)[,N:=nrow(.SD),by=genotype.id]

if(Period=="Senescence"){
  df <- subset(df, N>21)
}

df <- droplevels(df)
setDT(df)[,length(genotype.id[!duplicated(genotype.id)])]
df <- as.data.frame(df)

# repeat process
df <- groupedData(value ~ time_since_sowing | plot_grouped_global, data = df)
fm1Soy.lis <- nlsList( value ~ SSlogis(time_since_sowing, Asym, xmid, scal), data = df, control=list(lower=c(0.0,0.0,0.0,0),maxiter=1000))

# set controls s.t. method converges
nlmeControl(msMaxIter = 5000, msVerbose = TRUE)
#update as nlme model with random effects
cc_rf_scal <- nlme( fm1Soy.lis , random = Asym+ xmid ~ 1, weights = varPower())
summary(cc_rf_scal)
# vectors for starting values
soyFix <- fixef(cc_rf_scal)

df$platform <- as.factor(df$platform)
unique(df$year_site.UID)[order(unique(df$year_site.UID))]
uniqueN(df$year_site.UID)
uniqueN(df$genotype.id)

##
save.image(paste0("data/",Period,"_data.RData") )
###
###
require(nlme)




### final model

###
###
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
dynamic_xmid = c(soyFix[2], rep(0,3))
dynamic_scal = c(soyFix[3], rep(0,2*no_genotypes+1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

##
start_time <- Sys.time()

Growth_E.GxPTxP <- update(cc_rf_scal,
                           fixed = list(Asym ~ genotype.id+platform,
                                        xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
                                        scal ~ genotype.id:(avg_photothermal_14+avg_precipitation_14)+platform),
                           start = dynamic_vector, control = list (msVerbose = TRUE,
                                                                   maxIter = 100,
                                                                   msMaxIter = 100))
end_time <- Sys.time()
print(end_time - start_time)

save(Growth_E.GxPTxP, file=paste0("model/", Period, "_E.GxPTxP.RData"))


#########################

### modelling comparison

####

no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
dynamic_xmid = c(soyFix[2], rep(0,0))
dynamic_scal = c(soyFix[3], rep(0,1*no_genotypes))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
#final model, takes a while 20 min +

##
start_time <- Sys.time()

Growth1_G <- update(cc_rf_scal,
                    fixed = list(Asym ~ genotype.id+platform,
                                 xmid ~ 1 ,
                                 scal ~ genotype.id+platform),
                    start = dynamic_vector, control = list (msVerbose = TRUE,
                                                            maxIter = 500,
                                                            msMaxIter = 500))
end_time <- Sys.time()
print(end_time - start_time)

save(Growth1_G, file=paste0("model/", Period, "1_G.RData"))
# ###

####
# no_genotypes = length(levels(df$genotype.id))
# dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
# dynamic_xmid = c(soyFix[2], rep(0,3))
# dynamic_scal = c(soyFix[3], rep(0,1*no_genotypes))
# 
# dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
# 
# ##
# start_time <- Sys.time()
# 
# Growth1_E.G <- update(cc_rf_scal,
#                       fixed = list(Asym ~ genotype.id+platform,
#                                    xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
#                                    scal ~ genotype.id+platform),
#                       start = dynamic_vector, control = list (msVerbose = TRUE,
#                                                               maxIter = 100,
#                                                               msMaxIter = 100))
# end_time <- Sys.time()
# print(end_time - start_time)
# 
# save(Growth1_E.G, file=paste0("model/", Period, "1_E.G.RData"))

########
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
dynamic_xmid = c(soyFix[2], rep(0,3))
dynamic_scal = c(soyFix[3], rep(0,1*no_genotypes+1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

##
start_time <- Sys.time()

Growth2_E.GxT <- update(cc_rf_scal,
                        fixed = list(Asym ~ genotype.id+platform,
                                     xmid ~ avg_temperature_14+avg_precipitation_14+avg_radiation_14 ,
                                     scal ~ genotype.id:(avg_temperature_14)+platform),
                        start = dynamic_vector, control = list (msVerbose = TRUE,
                                                                maxIter = 100,
                                                                msMaxIter = 100))
end_time <- Sys.time()
print(end_time - start_time)

save(Growth2_E.GxT, file=paste0("model/", Period, "2_E.GxT.RData"))

##
start_time <- Sys.time()

Growth3_E.GxPT <- update(cc_rf_scal,
                        fixed = list(Asym ~ genotype.id+platform,
                                     xmid ~ avg_temperature_14+avg_precipitation_14+avg_radiation_14 ,
                                     scal ~ genotype.id:(avg_photothermal_14)+platform),
                        start = dynamic_vector, control = list (msVerbose = TRUE,
                                                                maxIter = 100,
                                                                msMaxIter = 100))
end_time <- Sys.time()
print(end_time - start_time)

save(Growth3_E.GxPT, file=paste0("model/", Period, "3_E.GxPT.RData"))

##
start_time <- Sys.time()

Growth4_E.GxR <- update(cc_rf_scal,
                        fixed = list(Asym ~ genotype.id+platform,
                                     xmid ~ avg_temperature_14+avg_precipitation_14+avg_radiation_14 ,
                                     scal ~ genotype.id:(avg_radiation_14)+platform),
                        start = dynamic_vector, control = list (msVerbose = TRUE,
                                                                maxIter = 100,
                                                                msMaxIter = 100))
end_time <- Sys.time()
print(end_time - start_time)

save(Growth4_E.GxR, file=paste0("model/", Period, "4_E.GxR.RData"))
##
start_time <- Sys.time()

Growth5_E.GxP <- update(cc_rf_scal,
                          fixed = list(Asym ~ genotype.id+platform,
                                       xmid ~ avg_temperature_14+avg_precipitation_14+avg_radiation_14 ,
                                       scal ~ genotype.id:(avg_precipitation_14)+platform),
                          start = dynamic_vector, control = list (msVerbose = TRUE,
                                                                  maxIter = 100,
                                                                  msMaxIter = 100))
end_time <- Sys.time()
print(end_time - start_time)

save(Growth5_E.GxP, file=paste0("model/", Period, "5_E.GxP.RData"))


###
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
dynamic_xmid = c(soyFix[2], rep(0,3))
dynamic_scal = c(soyFix[3], rep(0,2*no_genotypes))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

##
start_time <- Sys.time()

Growth6_E.GxPT <- update(cc_rf_scal,
                         fixed = list(Asym ~ genotype.id+platform,
                                      xmid ~ avg_temperature_14+avg_precipitation_14+avg_radiation_14 ,
                                      scal ~ genotype.id*(avg_photothermal_14)+platform),
                         start = dynamic_vector, control = list (msVerbose = TRUE,
                                                                 maxIter = 100,
                                                                 msMaxIter = 100))
end_time <- Sys.time()
print(end_time - start_time)

save(Growth6_E.GxPT, file=paste0("model/", Period, "6_E.GxPT.RData"))

####
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
dynamic_xmid = c(soyFix[2], rep(0,3))
dynamic_scal = c(soyFix[3], rep(0,3*no_genotypes))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))


######
start_time <- Sys.time()

Growth7_E.GxRxP <- update(cc_rf_scal,
                            fixed = list(Asym ~ genotype.id+platform,
                                         xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
                                         scal ~ genotype.id*(avg_radiation_14+avg_precipitation_14)+platform),
                            start = dynamic_vector, control = list (msVerbose = TRUE,
                                                                    maxIter = 200,
                                                                    msMaxIter = 200))
end_time <- Sys.time()
print(end_time - start_time)

save(Growth7_E.GxRxP, file=paste0("model/", Period, "7_E.GxRxP.RData"))


##
start_time <- Sys.time()

Growth8_E.GxPTxP <- update(cc_rf_scal,
                            fixed = list(Asym ~ genotype.id+platform,
                                         xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
                                         scal ~ genotype.id*(avg_photothermal_14+avg_precipitation_14)+platform),
                            start = dynamic_vector, control = list (msVerbose = TRUE,
                                                                    maxIter = 200,
                                                                    msMaxIter = 200))
end_time <- Sys.time()
print(end_time - start_time)

save(Growth8_E.GxPTxP, file=paste0("model/", Period, "8_E.GxPTxP.RData"))
######

#######


if(Period=="Senescence"){
  Senescence1_G <- Growth1_G
  save(Senescence1_G, file=paste0("model/", Period, "1_G.RData"))
  Senescence2_E.GxT <- Growth1_E.G
  save(Senescence2_E.GxT, file=paste0("model/", Period, "2_E.GxT.RData"))
  Senescence_E.GxPT <- Growth3_E.GxPT
  save(Senescence_E.GxPT, file=paste0("model/", Period, "_E.GxPT.RData"))
  # Senescence4_E.GxP <- Growth4_E.GxP
  # save(Senescence4_E.GxP, file=paste0("model/", Period, "4_E.GxP.RData"))
  # Senescence5_E.GxPre <- Growth5_E.GxPre
  # save(Senescence5_E.GxPre, file=paste0("model/", Period, "5_E.GxPre.RData"))
}