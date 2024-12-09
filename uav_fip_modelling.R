

# this file fits the UAV-FIP model and compares it with the simple version

# Packages
library(nlme)
library(stringr)
library(ggplot2)
library(dplyr)
library(broom.mixed)
library(merTools)
library(data.table)
# data "plot_grouped_global" is used for the grouping of the data
data_all = fread("data/model_data.csv",fill=T)
p <- unique(data_all[,c("year_site.UID","platform","year")])
p <- p[duplicated(paste(p$year_site.UID,p$year)),]
unique(p$year_site.UID)
df <- subset(data_all, year_site.UID%in%unique(p$year_site.UID))
p <- dcast.data.table(df, year_site.UID+UID+date+genotype.id+range+row~platform, value.var = "value")
p <- setDT(p)[, NperTrial:=nrow(.SD), by=.(year_site.UID,date)]
p <- subset(p, NperTrial>40)
select <- p[,cor(FIP,UAV, use = "pairwise.complete.obs"),by=.(date)]
select
select <- subset(select, V1<0.5)
p <- subset(p, date%in%select$date)


###########################################################
#### DATA PREPARATION; COPIED FROM MAIN MODEL <- START
###########################################################
# restore factor variables lost due to saving 
data_prep <- function(df){df$genotype.id   <- as.factor(df$genotype.id)
df$plot_grouped_global   <- ordered(as.factor(df$plot_grouped_global))
#prepear grouping
df <- as.data.frame(df)
df <- droplevels(df)
df$Filename <- NULL
#transform outcome  
df$value        <- asin(sqrt(df$value))

# grouped DF
df <- groupedData(value ~ time_since_sowing | plot_grouped_global, data = df)
# list object for later modelling
fm1Soy.lis <- nlsList( value ~ SSlogis(time_since_sowing, Asym, xmid, scal), data = df, control=list(lower=c(0.0,0.0,0.0,0),maxiter=1000))


# remove outliers suggested by last year. Needed for convergence
fm1Soy.table <- as.data.frame(summary(fm1Soy.lis)$coef)
fm1Soy.table <- setDT(fm1Soy.table)
fm1Soy.table$plot_grouped_global <-1:nrow(fm1Soy.table)

outlier_plots1 <- fm1Soy.table$plot_grouped_global[(fm1Soy.table$Estimate.Asym>4)]
outlier_plots2 <- fm1Soy.table$plot_grouped_global[(fm1Soy.table$Estimate.xmid>100)]
outlier_plots3 <- fm1Soy.table$plot_grouped_global[(fm1Soy.table$Estimate.scal>20)]
outlier_plots4 <- fm1Soy.table$plot_grouped_global[is.na(fm1Soy.table$Estimate.scal)]
outlier_plots <- c(outlier_plots1,outlier_plots2, outlier_plots3, outlier_plots4)
outlier_plots <- outlier_plots[!is.na(outlier_plots)]

df <- subset(df, !plot_grouped_global %in%outlier_plots)
setDT(df)[,length(genotype.id[!duplicated(genotype.id)])]
setDT(df)[,N:=nrow(.SD),by=genotype.id]
df <- subset(df, N>10)
df <- droplevels(df)
setDT(df)[,length(genotype.id[!duplicated(genotype.id)])]
df <- as.data.frame(df)
return(df)
}

df <- data_prep(df)


ggplot(data=df, aes(date, value))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  # geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(size=1.5, alpha=1,aes(color=genotype.id))+
  # geom_line(aes(y=Loess_fit, x=Date))+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, aes(group=1),color="black")+
  # guides(color = guide_legend(nrow=3))+
  facet_grid(.~Location+year+platform,scale="free",switch="both", labeller = label_parsed)


# repeat process
df <- groupedData(value ~ time_since_sowing | plot_grouped_global, data = df)
fm1Soy.lis <- nlsList( value ~ SSlogis(time_since_sowing, Asym, xmid, scal), data = df, control=list(lower=c(0.0,0.0,0.0,0),maxiter=1000))

# set controls s.t. method converges
nlmeControl(msMaxIter = 5000, msVerbose = TRUE)

###########################################################
#### DATA PREPARATION; COPIED FROM MAIN MODEL <- END
###########################################################

### Fitting the main UAV-FIP model <- START

uav_fip_model <- nlme( fm1Soy.lis , random = Asym + xmid ~ 1, control = nlmeControl(maxIter = 100))
fix_ef <- fixed.effects(uav_fip_model)

# length 2 as we have 2 intercepts
dynamic_Asym <- c(fix_ef[1], 0) 
dynamic_xmid <- c(fix_ef[2], 0)
dynamic_scal <- c(fix_ef[3], 0)
dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

# change platform to factor
df$platform = as.factor(df$platform)

start_time <- Sys.time()

# main UAV-FIP model
uav_fip_model <- update(uav_fip_model, 
                        fixed = list(Asym ~ platform,
                                     xmid ~ platform,
                                     scal ~  platform), 
                        start = dynamic_vector, control = list (msVerbose = TRUE,  
                                                                maxIter = 100, 
                                                                msMaxIter =100))
end_time <- Sys.time()
print(end_time-start_time)
summary(uav_fip_model)

save(uav_fip_model, file=paste0("model/platform/UAV_FIP_model.RData"))


### Simple model <- START
# length 1 as only 1 intercept
dynamic_Asym <- c(fix_ef[1]) 
dynamic_xmid <- c(fix_ef[2])
dynamic_scal <- c(fix_ef[3])
dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))

# fitting simple model <- same intercept for the two platforms
uav_fip_model_simple <- update(uav_fip_model, 
                               fixed = list(Asym ~ 1,
                                            xmid ~ 1,
                                            scal ~ 1), 
                               start = dynamic_vector, control = list (msVerbose = TRUE,
                                                                       maxIter = 100,
                                                                       msMaxIter =100))
summary(uav_fip_model_simple)

save(uav_fip_model, file=paste0("model/platform/UAV_FIP_model-simple.RData"))

# anova for the two models
anova(uav_fip_model, uav_fip_model_simple)
### Comparison with the simple model <- END








### Comparison of error variance between both platforms with selected model


# data "plot_grouped_global" is used for the grouping of the data

p <- unique(data_all[,c("year_site.UID","platform","year")])
p <- p[duplicated(paste(p$year_site.UID,p$year)),]
unique(p$year_site.UID)
df <- subset(data_all, year_site.UID%in%unique(p$year_site.UID)&platform=="FIP")
##


df <- data_prep(df)
df <- groupedData(value ~ time_since_sowing | plot_grouped_global, data = df)
fm1Soy.lis <- nlsList( value ~ SSlogis(time_since_sowing, Asym, xmid, scal), data = df, control=list(lower=c(0.0,0.0,0.0,0),maxiter=1000))

# set controls s.t. method converges
nlmeControl(msMaxIter = 5000, msVerbose = TRUE)
#update as nlme model with random effects
cc_rf_scal <- nlme( fm1Soy.lis , random = Asym+ xmid ~ 1, weights = varPower())
# vectors for starting values
soyFix <- fixef(cc_rf_scal)


no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes-1)) 
dynamic_xmid = c(soyFix[2], rep(0,2))
dynamic_scal <- c(soyFix[3], rep(0,3*no_genotypes-1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
#final model, takes a while 20 min +

start_time <- Sys.time()

model_fip <- update(cc_rf_scal, 
                fixed = list(Asym ~ genotype.id,
                             xmid ~avg_Temperature_14 + avg_precipitation_14 ,
                             scal ~  genotype.id*(avg_precipitation_14 + avg_radiation_14)), 
                start = dynamic_vector, control = list (msVerbose = TRUE,  
                                                        maxIter = 100, 
                                                        msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)

FIP_error_var <- model_fip$sigma^2


####

p <- unique(data_all[,c("year_site.UID","platform","year")])
p <- p[duplicated(paste(p$year_site.UID,p$year)),]
unique(p$year_site.UID)
df <- subset(data_all, year_site.UID%in%unique(p$year_site.UID)&platform=="UAV")
##


df <- data_prep(df)
df <- groupedData(value ~ time_since_sowing | plot_grouped_global, data = df)
fm1Soy.lis <- nlsList( value ~ SSlogis(time_since_sowing, Asym, xmid, scal), data = df, control=list(lower=c(0.0,0.0,0.0,0),maxiter=1000))

# set controls s.t. method converges
nlmeControl(msMaxIter = 5000, msVerbose = TRUE)
#update as nlme model with random effects
cc_rf_scal <- nlme( fm1Soy.lis , random = Asym+ xmid ~ 1, weights = varPower())
# vectors for starting values
soyFix <- fixef(cc_rf_scal)


no_genotypes = length(levels(df$genotype.id))
dynamic_Asym = c(soyFix[1], rep(0,1*no_genotypes-1)) 
dynamic_xmid = c(soyFix[2], rep(0,2))
dynamic_scal <- c(soyFix[3], rep(0,3*no_genotypes-1))

dynamic_vector <- append(dynamic_Asym, c(dynamic_xmid, dynamic_scal))
#final model, takes a while 20 min +

start_time <- Sys.time()

model_uav <- update(cc_rf_scal, 
                    fixed = list(Asym ~ genotype.id,
                                 xmid ~avg_Temperature_14 + avg_precipitation_14 ,
                                 scal ~  genotype.id*(avg_precipitation_14 + avg_radiation_14)), 
                    start = dynamic_vector, control = list (msVerbose = TRUE,  
                                                            maxIter = 100, 
                                                            msMaxIter = 100))
end_time <- Sys.time()
print(end_time-start_time)

UAV_error_var <- model_uav$sigma^2

#######
UAV_error_var
FIP_error_var


###
p <- unique(data_all[,c("year_site.UID","platform","date")])
p <- p[duplicated(paste(p$year_site.UID,p$date)),]
unique(p$date)
df <- subset(data_all, date%in%unique(p$date))
unique(df$year_site.UID)

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

p <- subset(df, value!=0)
p <- setDT(p)
p <- droplevels(p)
p <- p[,value_dup:=duplicated(value), by=.(platform,year_site.UID,date,genotype.id)]
p <- subset(p, value_dup==FALSE)
p <- p[!is.na(p$value),]
p <- droplevels(p)
p <- setDT(p)[, NperTrial:=nrow(.SD), by=.(platform,year_site.UID,date)]
p <- subset(p, NperTrial>40)
hist(p$NperTrial)


BLUEs_Traits <- setDT(p)[,spats_blues(.SD), by=.(year_site.UID,platform,date)]
BLUEs_Traits[,nrow(.SD),by=.(genotype.id,platform)]

p_h2 <-  unique(BLUEs_Traits[,c("h2","platform","year_site.UID","date","N_Datapt_removed")])
p_h2[order(p_h2$date),]
p_h2[,nrow(.SD),by=platform]
p_h2$Month <- as.numeric(format(as.Date(p_h2$date),"%m"))

ggplot(data=p_h2, aes(x=platform, y=h2) ) +
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=0.6,color="white"),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 0, hjust = 0.5),text = element_text(size=12),strip.text = element_text(size = 12), axis.title = element_text(size = 12))+
  geom_jitter(aes(color=date))+
  geom_boxplot( outlier.colour = NA, alpha=0.75)+
  facet_grid(year_site.UID~Month)

ggplot(data=p_h2, aes(x=date, y=h2,color=platform) ) +
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=0.6,color="white"),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 0, hjust = 0.5),text = element_text(size=12),strip.text = element_text(size = 12), axis.title = element_text(size = 12))+
  geom_point()+geom_line()
