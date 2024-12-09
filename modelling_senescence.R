setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/")

Period <- "Senescence"

# this file fits the model
# Packages
library(nlme)
library(stringr)
library(ggplot2)
library(dplyr)
library(broom.mixed)
library(data.table)
# data "plot_grouped_global" is used for the grouping of the data
df = read.csv("data/model_data.csv")
df <- subset(df, period%in%c(Period,"Both"))


unique(df$date)
# setDT(df)[,length(unique(platform)),by="year_site.UID"]
df$date <- as.Date(df$date)
hist(df$value)

# restore factor variables lost due to saving 
df$genotype.id   <- as.factor(df$genotype.id) 
df$plot_grouped_global   <- ordered(as.factor(df$plot_grouped_global))
#prepare grouping
df <- as.data.frame(df)
df <- droplevels(df)
df$Filename <- NULL
df$gdd_temperature <- NULL
df$time[is.na(df$time)] <- "one_measurement_per_day"


# setDT(df)[,N:=nrow(.SD),by=.(UID)]
# df <- subset(df, N>5)
# 
setDT(df)[,N:=nrow(.SD),by=.(date,genotype.id)]
df <- subset(df, N>1)
#transform outcome  
df$value        <- asin(sqrt(df$value)) 
df <- df[!is.na(df$value),]

df$time_since_sowing <- df$time_since_sowing* (-1)

p <- subset(df, genotype.id%in%unique(genotype.id)[1:10])
ggplot(data=p, aes(date, value))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(size=1.5, alpha=1,aes(color=genotype.id))+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(group=1),color="black")+
  # guides(color = guide_legend(nrow=3))+
  facet_grid(genotype.id~Location+year+platform,scale="free",switch="both", labeller = label_parsed)


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
# df$res <- residuals(fm1Soy.lis)
# hist(df$res)


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

# fm1Soy.table$Estimate.Asym <- remove_outliers(fm1Soy.table$Estimate.Asym, 1.5)
# fm1Soy.table$Estimate.xmid <- remove_outliers(fm1Soy.table$Estimate.xmid, 1.5)
# fm1Soy.table$Estimate.scal <- remove_outliers(fm1Soy.table$Estimate.scal, 1.5)
# 
# outlier_plots1 <- fm1Soy.table$plot_grouped_global[is.na(fm1Soy.table$Estimate.Asym)]
# outlier_plots2 <- fm1Soy.table$plot_grouped_global[is.na(fm1Soy.table$Estimate.xmid)]
# outlier_plots3 <- fm1Soy.table$plot_grouped_global[is.na(fm1Soy.table$Estimate.scal)]

outlier_plots1 <- fm1Soy.table$plot_grouped_global[(fm1Soy.table$Estimate.Asym> 2.5)]
outlier_plots2 <- c(fm1Soy.table$plot_grouped_global[(fm1Soy.table$Estimate.xmid>    150)],fm1Soy.table$plot_grouped_global[(fm1Soy.table$Estimate.xmid< -170)])
outlier_plots3 <- fm1Soy.table$plot_grouped_global[(fm1Soy.table$Estimate.scal>20)]
outlier_plots4 <- fm1Soy.table$plot_grouped_global[is.na(fm1Soy.table$Estimate.scal)]
outlier_plots <- c(outlier_plots1,outlier_plots2, outlier_plots3, outlier_plots4)
outlier_plots <- outlier_plots[!is.na(outlier_plots)]

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

####### investigate outlier plots
# investigate <- subset(df, plot_grouped_global %in%outlier_plots&platform=="FIP")
# setDT(investigate)[,length(unique(UID)),by=plot_grouped_global]
# 
# p <- (setDT(investigate)[,nrow(.SD),by=.(UID,date,time,year_site.UID,platform)])
# subset(p,V1>1)
# # investigate <- subset(investigate, year_site.UID=="FPSB007")
# 
# lapply(unique(investigate$year), function(x) {
#   files <- list.files(path = paste0("~/public/Evaluation/Projects/KP0023_legumes/Soybean/", x),
#                       pattern = "_rows.png", recursive = TRUE, full.names = TRUE) #_canopy_plot.png
#   files <- files[!grepl("mask_rows.png", files)]
#   
#   investigate$UID2 <- investigate$UID
#   # Ensure 'TitleNr' is in Date format for proper ordering
#   investigate$date2 <- as.Date(investigate$date) # Assuming 'date2' is already in a format like "YYYY-MM-DD"
#   
#   
#   setDT(investigate)[,Filename_row:=files[grepl(UID2,files)&grepl(gsub("-","_",date2), files)][1],by=.(UID,date,year_site.UID)]
#   
#   # Creating the ToPlot table and arranging by date
#   ToPlot <- data.table(File = investigate$Filename_row, TitleNr = investigate$date2, UID = investigate$UID)
#   ToPlot <- ToPlot[order(UID, TitleNr)]  # Arrange by UID and then by date
#   ToPlot <- na.omit(ToPlot)
#   # Defining the make_plots function with NULL check and UID in filename
#   make_plots <- function(png_files_exclude, nrow_plot , UID) {
#     library(png)
#     library(grid)
#     library(cowplot)
#     
#     # Filter out any NULL or empty files
#     png_files_exclude <- png_files_exclude[!is.na(png_files_exclude$File),]
#     
#     if (nrow(png_files_exclude) > 0) {  # Only proceed if there are files to plot
#       plots <- lapply(png_files_exclude$File, function(file) {
#         img <- readPNG(file)
#         rasterGrob(img, interpolate = TRUE)
#       })
#       ncol_plot <- ceiling(length(plots) / nrow_plot)
#       # Arrange plots with date labels
#       gg <- plot_grid(plotlist = plots, nrow=nrow_plot, labels = as.character(png_files_exclude$TitleNr),label_size = 4)
#       
#       # Save plot with UID in the filename
#       output_file <- paste0("out_", UID, ".png")
#       ggsave(output_file, plot = gg, width = ncol_plot * 260, height = nrow_plot * 560, units = "px")
#       return(output_file)  # Return the file path as a placeholder result
#     } else {
#       message("No valid images found for plotting in this UID.")
#       return(NA)  # Return NA if no plots were created
#     }
#   }
#   
#   # Filter and apply plotting function by UID, ordered by date
#   ToPlot <- na.omit(ToPlot)
#   ToPlot[, list(Result = make_plots(.SD,  nrow_plot=2, UID = UID)), by = UID]
# })


######


df <- subset(df, !plot_grouped_global %in%outlier_plots)
setDT(df)[,length(genotype.id[!duplicated(genotype.id)])]
setDT(df)[,N:=nrow(.SD),by=genotype.id]
df <- subset(df, N>30)
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
# vectors for starting values
soyFix <- fixef(cc_rf_scal)

##
# save.image(paste0("/home/kellebea/cluster/SoySeg/Senescence/",Period,"_data_nlme_v5.1.RData") )
###

df$platform <- as.factor(df$platform)

require(nlme)


no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes-1))
dynamic_xmid = c(soyFix[2], rep(0,0))
dynamic_scal = c(soyFix[3], rep(0,1*no_genotypes-1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
#final model, takes a while 20 min +

##
start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ genotype.id,
                             xmid ~ 1 ,
                             scal ~ genotype.id),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="baseline_nlme_v5.1.RData")
###
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,0))
dynamic_xmid = c(soyFix[2], rep(0,2))
dynamic_scal = c(soyFix[3], rep(0,0))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
#final model, takes a while 20 min +


##
start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ 1,
                             xmid ~ avg_temperature_14 + avg_precipitation_14,
                             scal ~ 1),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="baseline2_nlme_v5.1.RData")

no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,0))
dynamic_xmid = c(soyFix[2], rep(0,3))
dynamic_scal = c(soyFix[3], rep(0,0))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
#final model, takes a while 20 min +


##
start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ 1,
                             xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
                             scal ~ 1),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="baseline2.1_nlme_v5.1.RData")
####
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes-1))
dynamic_xmid = c(soyFix[2], rep(0,2))
dynamic_scal = c(soyFix[3], rep(0,1*no_genotypes-1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
#final model, takes a while 20 min +


##
start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ genotype.id,
                             xmid ~ avg_temperature_14 + avg_precipitation_14,
                             scal ~ genotype.id),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="baseline3_nlme_v5.1.RData")


###



no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes-1))
dynamic_xmid = c(soyFix[2], rep(0,0))
dynamic_scal = c(soyFix[3], rep(0,1*no_genotypes-1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
#final model, takes a while 20 min +


##
start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ genotype.id,
                             xmid ~ 1 ,
                             scal ~ genotype.id),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="baseline4_nlme_v5.1.RData")

####



no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
dynamic_xmid = c(soyFix[2], rep(0,3))
dynamic_scal = c(soyFix[3], rep(0,1*no_genotypes-1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
#final model, takes a while 20 min +


##
start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ genotype.id+platform,
                             xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
                             scal ~ genotype.id),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="baseline5_nlme_v5.1.RData")

####
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes-1))
dynamic_xmid = c(soyFix[2], rep(0,3))
dynamic_scal = c(soyFix[3], rep(0,1*no_genotypes-1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

##
start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ genotype.id,
                             xmid ~ avg_temperature_14 + avg_precipitation_14 +platform,
                             scal ~ genotype.id),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="baseline6_nlme_v5.1.RData")

####
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
dynamic_xmid = c(soyFix[2], rep(0,3))
dynamic_scal = c(soyFix[3], rep(0,1*no_genotypes-1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

##
start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ genotype.id+platform,
                             xmid ~ avg_temperature_14 + avg_precipitation_14 +platform,
                             scal ~ genotype.id),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="baseline7_nlme_v5.1.RData")

####
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
dynamic_xmid = c(soyFix[2], rep(0,2))
dynamic_scal = c(soyFix[3], rep(0,1*no_genotypes))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

##
start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ genotype.id+platform,
                             xmid ~ avg_temperature_14 + avg_precipitation_14,
                             scal ~ genotype.id+platform),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="baseline8_nlme_v5.1.RData")
###

###
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
dynamic_xmid = c(soyFix[2], rep(0,3))
dynamic_scal = c(soyFix[3], rep(0,1*no_genotypes))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

##
start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ genotype.id+platform,
                             xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
                             scal ~ genotype.id+platform),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="baseline9_nlme_v5.1.RData")

# 
# 
# 
# 

# ###
# ##
# 
# 
# no_genotypes = length(levels(df$genotype.id))
# dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
# dynamic_xmid = c(soyFix[2], rep(0,2))
# dynamic_scal = c(soyFix[3], rep(0,3*no_genotypes))
# 
# dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
# 
# 
# ##
# start_time <- Sys.time()
# 
# model <- update(cc_rf_scal,
#                 fixed = list(Asym ~ genotype.id+platform,
#                              xmid ~ avg_temperature_14 + avg_precipitation_14 ,
#                              scal ~ genotype.id*(avg_precipitation_14+avg_radiation_14)+platform),
#                 start = dynamic_vector, control = list (msVerbose = TRUE,
#                                                         maxIter = 100,
#                                                         msMaxIter = 100))
# end_time <- Sys.time()
# print(end_time-start_time)
# 
# 
# save(model, file="avg_precipitation_radiation_14_nlme_v5.1.RData")
# ##
# 
# start_time <- Sys.time()
# 
# model <- update(cc_rf_scal,
#                 fixed = list(Asym ~ genotype.id+platform,
#                              xmid ~ avg_temperature_14 + avg_precipitation_14 ,
#                              scal ~ genotype.id*(avg_precipitation_14+avg_temperature_14)+platform),
#                 start = dynamic_vector, control = list (msVerbose = TRUE,
#                                                         maxIter = 100,
#                                                         msMaxIter = 100))
# end_time <- Sys.time()
# print(end_time-start_time)
# 
# 
# save(model, file="avg_precipitation_temperature_14_nlme_v5.1.RData")
# 
# ##
# 
# start_time <- Sys.time()
# 
# model <- update(cc_rf_scal,
#                 fixed = list(Asym ~ genotype.id+platform,
#                              xmid ~ avg_temperature_14 + avg_precipitation_14 ,
#                              scal ~ genotype.id*(avg_radiation_14+avg_temperature_14)+platform),
#                 start = dynamic_vector, control = list (msVerbose = TRUE,
#                                                         maxIter = 100,
#                                                         msMaxIter = 100))
# end_time <- Sys.time()
# print(end_time-start_time)
# 
# 
# save(model, file="avg_radiation_14_temperature_14_nlme_v5.1.RData")
##

# ###
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
dynamic_xmid = c(soyFix[2], rep(0,3))
dynamic_scal = c(soyFix[3], rep(0,1*no_genotypes+1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

##

start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ genotype.id+platform,
                             xmid ~ avg_temperature_14+avg_precipitation_14+avg_radiation_14 ,
                             scal ~ genotype.id:(avg_temperature_14)+platform),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="avg_temperatureMinus_14_nlme_v5.1.RData")

#

start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ genotype.id+platform,
                             xmid ~ avg_temperature_14+avg_precipitation_14+avg_radiation_14 ,
                             scal ~ genotype.id:(avg_photothermal_14)+platform),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="avg_photothermalMinus_14_nlme_v5.1.RData")

#

start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ genotype.id+platform,
                             xmid ~ avg_temperature_14+avg_precipitation_14+avg_radiation_14 ,
                             scal ~ genotype.id:(avg_vpd_14)+platform),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="avg_vpdMinus_14_nlme_v5.1.RData")
# 
# 
# ###
no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes))
dynamic_xmid = c(soyFix[2], rep(0,2))
dynamic_scal = c(soyFix[3], rep(0,2*no_genotypes))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

##
start_time <- Sys.time()

model <- update(cc_rf_scal,
                fixed = list(Asym ~ genotype.id+platform,
                             xmid ~ avg_temperature_14 + avg_precipitation_14 ,
                             scal ~ genotype.id*(avg_precipitation_14)+platform),
                start = dynamic_vector, control = list (msVerbose = TRUE,
                                                        maxIter = 100,
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)


save(model, file="avg_precipitation_14_nlme_v5.1.RData")

# ##
# start_time <- Sys.time()
# 
# model <- update(cc_rf_scal,
#                 fixed = list(Asym ~ genotype.id+platform,
#                              xmid ~ avg_temperature_14 + avg_precipitation_14 ,
#                              scal ~ genotype.id*(avg_temperature_14)+platform),
#                 start = dynamic_vector, control = list (msVerbose = TRUE,
#                                                         maxIter = 100,
#                                                         msMaxIter = 100))
# end_time <- Sys.time()
# print(end_time-start_time)
# 
# 
# save(model, file="avg_temperature_14_nlme_v5.1.RData")
# 
# #
# start_time <- Sys.time()
# 
# model <- update(cc_rf_scal,
#                 fixed = list(Asym ~ genotype.id+platform,
#                              xmid ~ avg_temperature_14 + avg_precipitation_14 ,
#                              scal ~ genotype.id*(avg_radiation_14)+platform),
#                 start = dynamic_vector, control = list (msVerbose = TRUE,
#                                                         maxIter = 100,
#                                                         msMaxIter = 100))
# end_time <- Sys.time()
# print(end_time-start_time)
# 
# 
# save(model, file="avg_radiation_14_nlme_v5.1.RData")
# 
# ##
# start_time <- Sys.time()
# 
# model <- update(cc_rf_scal,
#                 fixed = list(Asym ~ genotype.id+platform,
#                              xmid ~ avg_temperature_14 + avg_precipitation_14 ,
#                              scal ~ genotype.id*(avg_photothermal_14)+platform),
#                 start = dynamic_vector, control = list (msVerbose = TRUE,
#                                                         maxIter = 100,
#                                                         msMaxIter = 100))
# end_time <- Sys.time()
# print(end_time-start_time)
# 
# 
# save(model, file="avg_photothermal_14_nlme_v5.1.RData")
# 
# 
# ##
# start_time <- Sys.time()
# 
# model <- update(cc_rf_scal,
#                 fixed = list(Asym ~ genotype.id+platform,
#                              xmid ~ avg_temperature_14 + avg_precipitation_14 ,
#                              scal ~ genotype.id*(avg_vpd_14)+platform),
#                 start = dynamic_vector, control = list (msVerbose = TRUE,
#                                                         maxIter = 100,
#                                                         msMaxIter = 100))
# end_time <- Sys.time()
# print(end_time-start_time)
# 
# 
# save(model, file="avg_vpd_14_nlme_v5.1.RData")
# 
# ##
# start_time <- Sys.time()
# 
# model <- update(cc_rf_scal,
#                 fixed = list(Asym ~ genotype.id+platform,
#                              xmid ~ avg_temperature_14 + avg_precipitation_14 ,
#                              scal ~ genotype.id*(gdd_temperature)+platform),
#                 start = dynamic_vector, control = list (msVerbose = TRUE,
#                                                         maxIter = 100,
#                                                         msMaxIter = 100))
# end_time <- Sys.time()
# print(end_time-start_time)
# 
# 
# save(model, file="gdd_temperature_nlme_v5.1.RData")