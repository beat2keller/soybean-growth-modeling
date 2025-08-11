# setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/")

####

library(nlme)
library(ggplot2)
require(data.table)

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

vis4net7 <- c('#e8ed6b', '#baca56', '#9ea347', '#957744', '#904351', '#6f0e66', '#03027b')
vis4net8 <- c('#00429d', '#674bb7', '#9a59cc', '#c26fdb', '#e18ce5', '#f6aee9', '#ffd6e8', '#ffffe0')
vis4net10 <- c('#00758d', '#5c8f84', '#8aa97a', '#b1c56d', '#d7e25b', '#feb5cd', '#db86a1', '#b45b76', '#8a3352', '#5e092e')

###


###
# main model
load("model/Growth_E.GxPTxP.RData")

# model comparison
load("model/Growth1_G.RData")
load("model/Growth2_E.GxT.RData")
load("model/Growth3_E.GxPT.RData")
load("model/Growth4_E.GxR.RData")
load("model/Growth5_E.GxP.RData")
load("model/Growth6_E.GxPT.RData")
load("model/Growth7_E.GxRxP.RData")
# load("model/Growth8_E.GxPTxP.RData") # did not converge

# load("model/Growth_E.GxPTxP_Contr.Sum.RData") # did not converge

anova_result_Gro <-anova(Growth1_G, Growth2_E.GxT,  Growth3_E.GxPT,  Growth4_E.GxR, Growth5_E.GxP, Growth6_E.GxPT, Growth7_E.GxRxP, Growth_E.GxPTxP)
anova_result_Gro
# require(knitr)
# kable(anova_result_Gro, digits = 4, file = "table/anova_table_growth.tex")
anova(Growth2_E.GxT, Growth_E.GxPTxP)
anova(Growth2_E.GxT, Growth6_E.GxPT)

###
# main model
load("model/Senescence_E.GxPT.RData") 

# model comparison
load("model/Senescence1_G.RData")
load("model/Senescence2_E.GxT.RData")
# load("model/Senescence4_E.GxV.RData")
# load("model/Senescence4_E.GxR.RData") did not converge
# load("model/Senescence5_E.GxPre.RData") did not converge
# load("model/Senescence6_E.GxPxPre.RData") did not converge

# Perform the ANOVA
anova_result_Sen <-anova(Senescence1_G, Senescence2_E.GxT, Senescence_E.GxPT)
anova_result_Sen
####

printCoefmat(anova_result_Gro)
printCoefmat(anova_result_Sen)

# library(xtable)
# 
# latex_table <- xtable(as.data.frame(anova_result_Sen))
# print(latex_table, file = "anova_result_Sen.tex", type = "latex",
#       include.rownames = TRUE, floating = FALSE, tabular.environment = "tabular",
#       hline.after = c(-1, 0, nrow(anova_result_Sen)))


###

candidate_genotypes <- read.csv("model/candidates/candidate_genotypes.csv", 
                                colClasses = "character",
                                header = TRUE)

load("data/Growth_data.RData") 

df$date <- as.Date(df$date)
df <- setDT(df)
df$Model0 <- fitted(Growth_E.GxPTxP)
df$Model1 <- fitted(Growth1_G)
df$Model2 <- fitted(Growth2_E.GxT)
df$Model3 <- fitted(Growth3_E.GxPT)
df$Model4 <- fitted(Growth4_E.GxR)
df$Model5 <- fitted(Growth5_E.GxP)
df$Model6 <- fitted(Growth6_E.GxPT)
df$Model7 <- fitted(Growth7_E.GxRxP)

Model_names <- data.frame(Model=paste0("Model",c(0:1,3,10:11)),Name=c("Growth_E.GxPTxP","Growth1_G","Growth3_E.GxPT","Senescence_E.GxPT","Senescence1_G"))
# p <- subset(df, Location=="Delley")
df[,nrow(na.omit(.SD)[!duplicated(genotype.id),]),by=year_site.UID]

p_Growth <- melt.data.table(df, measure.vars = c(paste0("Model",c(0:7))),variable.name = "Model", value.name = "Fit")
p_Growth$Period <- "Growth"
p_Growth$period <- NULL
p_Growth$Max <- NULL
p_Growth$Rep<- NULL


load("data/Senescence_data.RData") 
df$date <- as.Date(df$date)
df <- setDT(df)
df$Model10 <- fitted(Senescence_E.GxPT)
df$Model11 <- fitted(Senescence1_G)
df$Model12 <- fitted(Senescence2_E.GxT)



df[,nrow(na.omit(.SD)[!duplicated(genotype.id),]),by=year_site.UID]

p_Sen <- melt.data.table(df, measure.vars = c(paste0("Model",c(10,11,12))),variable.name = "Model", value.name = "Fit")
p_Sen$Period <- "Senescence"
p_Sen$period <- NULL
p_Sen$Rep <- NULL
p_Sen$time_since_sowing <- p_Sen$time_since_sowing * (-1)

names(p_Sen)
names(p_Growth)
df_all <- rbind(p_Growth,p_Sen,fill=T)
df_all$value <- sin(df_all$value)^2 # backtransform
df_all$Fit <- sin(df_all$Fit)^2 # backtransform


design_all <- read.csv("data/design_2015_2022.csv")
add_gen_id <- unique(design_all[,c("genotype.id","genotype.name")])
add_gen_id$genotype.id <- as.character(add_gen_id$genotype.id)

df_all <- merge(df_all, add_gen_id, by="genotype.id")

compute_r2 <- function(actual, predicted) {
  # Check if lengths match
  if (length(actual) != length(predicted)) {
    stop("Error: The length of actual and predicted values must be the same.")
  }
  
  # Total Sum of Squares (SST)
  SST <- sum((actual - mean(actual))^2)
  
  # Residual Sum of Squares (SSE)
  SSE <- sum((actual - predicted)^2)
  
  # Compute R-squared
  r2 <- 1 - (SSE / SST)
  
  return(r2)
}
r2_models <- df_all[,compute_r2(value, Fit),by=Model]
r2_models$r2 <- round(r2_models$V1,digits = 3)


df_selected <- subset(df_all, Model%in%paste0("Model",c(0, 1, 3, 10, 11)))
##### inspect fits
p <- p_Growth
p$Date <- p$date
p <- setDT(p)[,Rep:=as.numeric(as.factor(UID)),by=.(genotype.id,date,year_site.UID)]
p$Rep <- paste("Rep",p$Rep )



p[,compute_r2(value, Fit),by=Model]



ggFit <- ggplot(data=p,aes(Date, value, color=genotype.id,shape=Rep))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(aes(Date, Fit,group=genotype.id),size=0.1)+
  facet_grid(.~year_site.UID,scale="free",switch="both", labeller = label_parsed)
ggFit

# pairs(df[,c(paste0("Fit",2:4))])

# plots <- lapply(unique(p_Growth$year_site.UID), function(x){
#   
#   p <- subset(p_Growth, year_site.UID==x)
# 
#   p <- subset(p, genotype.id%in%unique(p$genotype.id)[4:6])
#   p$Date <- p$date
#   p$plot_grouped_global <- paste(p$plot_grouped_global, p$genotype.id)
#   p <- setDT(p)[,Rep:=as.numeric(as.factor(UID)),by=.(genotype.id,date,year_site.UID)]
#   p$Rep <- paste("Rep",p$Rep )
#   
#   gg1 <- ggplot(data=p,aes(Date, value, shape=Rep))+ ylab("Canopy cover (%)")+
#     theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
#     geom_point(size=1.5, alpha=1)+
#     geom_line(aes(Date, Fit,group=paste(plot_grouped_global,platform),linetype=platform, color=Model))+
#     facet_grid(genotype.id~year_site.UID+Model,scale="free",switch="both", labeller = label_parsed)
#   
#   print(gg1)
# })

len_geno <- df_selected[,length(unique(year_site.UID)),by=genotype.id]
len_geno <- len_geno[order(len_geno$V1,decreasing = T),]

p <- subset(df_selected, genotype.id%in%len_geno$genotype.id[1:2]&Model%in%c("Model0","Model1","Model3","Model10","Model11")&Location=="Eschikon"&platform=="FIP")
p$Date <- p$date
p$plot_grouped_global <- paste(p$plot_grouped_global, p$genotype.id)
p <- setDT(p)[,Rep:=as.numeric(as.factor(UID)),by=.(genotype.id,date,year_site.UID)]
p <- subset(p, Rep<5)
p$Rep <- paste("Rep",p$Rep )

p <- merge(p, Model_names, by="Model")

ggFit_idealLines <- ggplot(data=p,aes(Date, value, shape=Rep))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_point(size=1.5, alpha=1)+
  geom_line(data=subset(p, Period=="Growth"),aes(Date, Fit,group=paste(plot_grouped_global,platform,Model),color=Name))+
  geom_line(data=subset(p, Period=="Senescence"),aes(Date, Fit,group=paste(plot_grouped_global,platform,Model),color=Name))+
  scale_color_manual(values = rev(tol5qualitative))+
  guides(color = guide_legend(nrow=2))+
  guides(shape = guide_legend(nrow=2))+
  facet_grid(genotype.id+platform~year_site.UID,scale="free",switch="both", labeller = label_parsed)

print(ggFit_idealLines)

# ggsave("Soybean_IdealLines_Fit.pdf", width = 170, height = 140, units = "mm", dpi = 100, ggFit_idealLines)
# ggsave("Soybean_IdealLines_Fit.png", width = 170, height = 140, units = "mm", dpi = 300, ggFit_idealLines)


p <- subset(df_selected, genotype.id%in%candidate_genotypes[2,]&Model%in%c("Model0","Model3","Model10")&Location=="Eschikon")
p$Date <- p$date
p$plot_grouped_global <- paste(p$plot_grouped_global, p$genotype.id)
p <- setDT(p)[,Rep:=as.numeric(as.factor(UID)),by=.(genotype.id,date,year_site.UID)]
p$Rep <- paste(p$Rep )

ggplot(data=p,aes(Date, value, shape=Rep))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_point(size=1.5, alpha=1)+
  geom_line(data=subset(p, Period=="Growth"),aes(Date, Fit,group=paste(plot_grouped_global,platform,Model),color=Model))+
  geom_line(data=subset(p, Period=="Senescence"),aes(Date, Fit,group=paste(plot_grouped_global,platform,Model),color=Model))+
  scale_color_manual(values = tol4qualitative)+
  guides(color = guide_legend(nrow=2))+
  facet_grid(genotype.id+platform~year_site.UID,scale="free",switch="both", labeller = label_parsed)

# ggsave("Example_Soybean_IdealLine_Fit_UAV_FIP.pdf", width = 120, height = 140, units = "mm", dpi = 100)

################################


################################
# Extract the coefficients
require(nlme)

# Function to extract fixed effects with standard errors and confidence intervals
extract_fixed_effects <- function(model, model_name, period) {
  coefs <- fixed.effects(model)
  se <- sqrt(diag(vcov(model)))  # Extract standard errors
  ci <- intervals(model, which = "fixed")$fixed  # Get confidence intervals
  
  df <- data.frame(
    estimate = coefs,
    StdError = se,
    CI_Lower = ci[, "lower"],
    CI_Upper = ci[, "upper"],
    Model = model_name,
    Period = period,
    term = names(coefs)
  )
  
  return(df)
}

# Extract coefficients for all models
coefs0 <- extract_fixed_effects(Growth_E.GxPTxP, "Model0", "Growth")
coefs1 <- extract_fixed_effects(Growth1_G, "Model1", "Growth")
coefs3 <- extract_fixed_effects(Growth3_E.GxPT, "Model4", "Growth")
coefs10 <- extract_fixed_effects(Senescence_E.GxPT, "Model10", "Senescence")
coefs11 <- extract_fixed_effects(Senescence1_G, "Model11", "Senescence")

# Combine all results into one dataframe
coefs <- rbind(coefs0, coefs1, coefs3,  coefs10, coefs11)

# Display results

unique(coefs$Period)

# coefs$Weather <- NA
# coefs$Weather[grepl("emper",coefs$term)] <- "Temperature"
# coefs$Weather[grepl("recipi",coefs$term)] <- "Precipitation"

# coefs$variable <- NA
coefs$variable[grepl("Asym",coefs$term)] <- "Asym"
coefs$variable[grepl("scal",coefs$term)] <- "scal"
coefs$variable[grepl("mid",coefs$term)] <- "mid"
coefs$variable[grepl(":avg_",coefs$term)] <- "Interaction"
coefs$variable[grepl(":avg_rad",coefs$term)] <- "InteractionRad"
coefs$variable[grepl(":avg_temp",coefs$term)] <- "InteractionTemp"
coefs$variable[grepl(":avg_hum",coefs$term)] <- "InteractionHum"
coefs$variable[grepl(":avg_prec",coefs$term)] <- "InteractionPrec"
coefs$variable[grepl(":avg_phot",coefs$term)] <- "InteractionPhot"

coefs$variable <- paste(coefs$variable,coefs$Period,sep=".")

coefs <- setDT(coefs)[,c("ModelVar","genotype.id"):= tstrsplit(term, "genotype.id", fixed=TRUE)[1:2]]
coefs <- setDT(coefs)[,c("genotype.id","Interaction_term"):= tstrsplit(genotype.id, ":avg_", fixed=TRUE)[1:2]]
coefs <- merge(coefs, add_gen_id, by="genotype.id")

coefs$variable[grep("Intercept",coefs$term)]

p <- coefs
p[,estimate_scaled:=scale(estimate),by=.(variable,Period)]
# subset(p,estimate_scaled>-5&estimate_scaled<5)
# p$estimate_scaled[p$variable=="Asym"] <- p$estimate_scaled[p$variable=="Asym"]*-1
ggplot(p, aes(x=variable,y=estimate_scaled,color=genotype.id,group=genotype.id))+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_point()+geom_line()+
  facet_grid(.~Model)
# coefs$year_site.UID <- "overall"
coefs_max <- coefs[,list(max=max(estimate),max_genotype=genotype.id[estimate==max(estimate)],min=min(estimate),min_genotype=genotype.id[estimate==min(estimate)]),by=.(variable,Model,Period)]
coefs_max[is.na(variable),]

genotypes2022 <- unique(df_all$genotype.id[df_all$year==2022])
coefs[Model=="Model0"&genotype.id%in%genotypes2022,list(max=max(estimate),max_genotype=genotype.id[estimate==max(estimate)],min=min(estimate),min_genotype=genotype.id[estimate==min(estimate)]),by=.(variable,Model,Period)]

coefs_max_melt <- melt.data.table(coefs_max, id.vars =c("variable","Model","Period"),measure.vars=c("max_genotype","min_genotype"),value.name = "extreme_genotypes",variable.name = "Extreme")
coefs_max_melt$variable_extreme <- paste(coefs_max_melt$variable,coefs_max_melt$Extreme) #,coefs_max_melt$Model
unique(coefs_max_melt$extreme_genotypes)
coefs_max_melt <- subset(coefs_max_melt, Model%in%c("Model0","Model10"))
coefs_max_overall <- subset(coefs_max_melt, Model%in%c("Model0","Model10"))


df_selected <- rbind(subset(df_selected,Model%in%c(paste0("Model",c(0)))&Period=="Growth"),  subset(df_selected,Model%in%c(paste0("Model",c(10)))&Period=="Senescence"))
p <- merge(df_selected, coefs_max_melt, by.x =c("genotype.id","Model","Period"), by.y=c("extreme_genotypes","Model","Period"),allow.cartesian=TRUE, all.x = T, all.y = F )

p <- subset(p, Model%in%c("Model6","Model13")&genotype.id!="average"&variable_extreme!="average")
p_mean <- p[,list(Fit=mean(Fit,na.rm=T)),by=.(time_since_sowing,Model,Period,variable_extreme,genotype.id,year_site.UID)]



ggplot(data=p,aes(time_since_sowing, value, color=genotype.id,shape=year_site.UID))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_point(size=1.5, alpha=1)+
  # geom_line(aes(time_since_sowing, Fit,group=plot_grouped_global))+
  # ylim(0, 1)+
  scale_shape_manual(values=1:25)+
  geom_line(data=p_mean,aes(time_since_sowing, Fit, color=genotype.id),size=1,alpha=0.5)

# facet_grid(Model~.,scale="free",switch="both", labeller = label_parsed)
# geom_smooth(method = "loess",aes(time_since_sowing, value, color=genotype.id))

p <- subset(df_selected, Model%in%c("Model0","Model10")&genotype.id%in% coefs_max_melt$extreme_genotypes[coefs_max_melt$Model%in%c("Model0","Model10")])

ggExtremGrowthCurvesSummary <- ggplot(data=p,aes(time_since_sowing, value, color=genotype.name,shape=year_site.UID, group=genotype.name))+ ylab("Canopy cover (%)")+xlab("Days after sowing (d)")+
  theme_bw()+theme(strip.placement = "outside", strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_point(size=1.5, alpha=1)+
  geom_smooth(method="loess",formula = y ~x,  alpha=0.25, show.legend = F, size=1.5)+
  scale_shape_manual(values=1:25)+
  scale_color_manual(values=rev(tol12qualitative))+
  guides(color = guide_legend(nrow=3))+
  ylim(0, 1.5)+
  # geom_line(data=p_mean,aes(time_since_sowing, Fit, color=genotype.id),size=1.5)+
  facet_grid(Location~Period,scale="free",switch="both", labeller = label_parsed)

ggExtremGrowthCurvesSummary

# ggsave("Soybean_XtremLines_Summary.png", width = 170, height = 140, units = "mm", dpi = 100, ggExtremGrowthCurvesSummary)
# ggsave("Soybean_XtremLines_Summary.pdf", width = 170, height = 140, units = "mm", dpi = 100, ggExtremGrowthCurvesSummary)


p <- df_selected#subset(df_selected,Model%in%c(paste0("Model",c(2))))

coefs_max_melt <- coefs_max_overall

unique(paste(p$year_site.UID,p$year))
p <-p[!is.na(p$Fit),]

p$Date <- p$date
p$plot_grouped_global <- paste(p$plot_grouped_global, p$genotype.id)
p <- merge(p, coefs_max_melt, by.x =c("genotype.id","Model","Period"), by.y=c("extreme_genotypes","Model","Period"),allow.cartesian=TRUE, all.x = T, all.y = F )
unique(p$year_site.UID[!is.na(p$variable_extreme)] )
unique(p$Period)


ideal_candidate <- candidate_genotypes[,1]# c("10025", "10051")
unique(p$genotype.name[p$genotype.id%in% ideal_candidate] )

p$variable_extreme[is.na(p$variable_extreme)] <- "average"
p$variable_extreme[!p$genotype.id%in% c(coefs_max_melt$extreme_genotype,ideal_candidate)] <- "average"
unique(p$variable_extreme)
p$genotype.id[!p$genotype.id%in% c(coefs_max_melt$extreme_genotype,ideal_candidate)] <- "average"
p$genotype.name[!p$genotype.id%in% c(coefs_max_melt$extreme_genotype,ideal_candidate)] <- "average"


p$year_loc <- paste(p$Location, p$year, p$year_site.UID, sep=", ")
p$Selection <- "Extreme"
p$Selection[p$genotype.id%in%ideal_candidate] <- "Stable"


variable_extreme_growth <- subset(coefs_max_overall, Period=="Growth")$variable_extreme
variable_extreme_sen <- subset(coefs_max_overall, Period=="Senescence")$variable_extreme

p_ideal_candidates <- subset(p, Period=="Growth"&genotype.id%in%ideal_candidate)
p_ideal_candidates[,length(unique(year_site.UID)),by=genotype.name]

R2_year_site <- p[,compute_r2(value, Fit),by=.(Model,year_site.UID,Period)]
# Clean up the R2 table if needed
R2_year_site[, label := paste0("R² = ", round(V1, 2))]

R2_year_site

# Compute dynamic x positions and set y = Inf for top placement
library(data.table)

# Assuming your plot data is `p`
time_bounds <- p[, .(
  x_min = min(time_since_sowing, na.rm = TRUE),
  x_max = max(time_since_sowing, na.rm = TRUE),
  year_loc=year_loc,
  Period=Period,
  year_site.UID=year_site.UID
)]#, by = .(year_site.UID)]

# Join with R² table
R2_plot_data <- merge(R2_year_site, time_bounds, by = c("year_site.UID", "Period"), all.x = TRUE)
R2_plot_data[, label := paste0("R² = ", round(V1, 2))]

# Set x based on period, y to Inf
R2_plot_data[, x := ifelse(Period == "Growth", x_min, x_max)]
R2_plot_data[, y := Inf]


ggGrowthCurves <- ggplot()+ ylab("Canopy cover (%)")+ xlab("Days after sowing (d)")+
  theme_bw()+theme(strip.placement = "outside", strip.background = element_blank(),legend.key.size = unit(0.9, "lines"),
        legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=8))+
  geom_jitter( size=1.5, alpha=1)+
  geom_point(data=subset(p, !variable_extreme%in%variable_extreme_growth&Period=="Growth"),aes(time_since_sowing, value, shape=variable_extreme), size=0.25, alpha=0.5, color="grey", shape=1,show.legend = F)+
  geom_point(data=subset(p, variable_extreme%in%variable_extreme_growth&Period=="Growth"),aes(time_since_sowing, value, shape=variable_extreme, color=genotype.name, group=paste(UID,platform)), size=0.5, alpha=0.5, shape=1)+
  geom_point(data=p_ideal_candidates,aes(time_since_sowing, value, shape=variable_extreme, color=genotype.name, group=paste(UID,platform)), size=0.5, alpha=0.5, shape=1)+
  
  geom_point(data=subset(p, !variable_extreme%in%variable_extreme_sen&Period=="Senescence"),aes(time_since_sowing, value, shape=variable_extreme), size=0.25, alpha=0.5, color="grey", shape=1,show.legend = F)+
  geom_point(data=subset(p, variable_extreme%in%variable_extreme_sen&Period=="Senescence"),aes(time_since_sowing, value, shape=variable_extreme, color=genotype.name, group=paste(UID,platform)), size=0.5, alpha=0.5, shape=1)+
  scale_color_manual(name="Genotype", values=tol12qualitative)+
  scale_shape_manual(values=1:25)+
  # guides(shape = guide_legend(nrow=2))+
  # scale_linetype_manual(values=c(3,1))+
  geom_line(data=subset(p, !variable_extreme%in%variable_extreme_growth&Period=="Growth"),aes(time_since_sowing, Fit,  group=paste(UID,platform)),size=0.1,color="grey80",show.legend = F)+
  geom_line(data=subset(p, variable_extreme%in%variable_extreme_growth&Period=="Growth"),aes(time_since_sowing, Fit, color=genotype.name, linetype = Selection, group=paste(UID,platform)),size=0.5)+ #, linetype=variable_extreme
  geom_line(data=p_ideal_candidates,aes(time_since_sowing, Fit, color=genotype.name, linetype = Selection, group=paste(UID,platform)),size=0.5)+ #, linetype=variable_extreme
  
  geom_line(data=subset(p, !variable_extreme%in%variable_extreme_sen&Period=="Senescence"),aes(time_since_sowing, Fit,  group=paste(UID,platform)),size=0.1,color="grey80",show.legend = F)+
  geom_line(data=subset(p, variable_extreme%in%variable_extreme_sen&Period=="Senescence"),aes(time_since_sowing, Fit, color=genotype.name, linetype = Selection, group=paste(UID,platform)),size=0.5)+ #, linetype=variable_extreme
  
  geom_text(data = R2_plot_data,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE,
              hjust = ifelse(R2_plot_data$Period == "Growth", 0, 1),
              vjust = 1.5,
              size = 2.5, check_overlap = T, color="grey80")+

  facet_wrap(year_loc ~ .,switch="y")
ggGrowthCurves

#####
#####

SpATsBLUE <- read.csv("~/public/Evaluation/Projects/KP0023_legumes/Soybean/201x/Get_raw_data_soybean/data/RefTraits_Soybean_Eschikon_2015_22_BLUEs.csv")
unique(SpATsBLUE$year_site.UID)

### check year_site.UID


SpATsBLUE$value <- SpATsBLUE$predicted.values
#
SpATsBLUE_yield <- subset(SpATsBLUE, variable%in%c("Yield"))#,"Protein.content"
SpATsBLUE_yield$genotype.id <- as.character(SpATsBLUE_yield$genotype.id)
# SpATsBLUE_growth <- subset(SpATsBLUE_growth_overall, variable%in%c("Canopy_cover_fitInt","SD_Pixel_row_Int"))#,"Protein.content"
# SpATsBLUE_growth$estimate <- SpATsBLUE_growth$predicted.values
setDT(SpATsBLUE_yield)[,length(genotype.id[!duplicated(genotype.id)])]
# coefs[,length(genotype.id[!duplicated(genotype.id)])]
# setDT(df_o)[,length(genotype.id[!duplicated(genotype.id)])]


####################
names(df_selected)
WeatherVars <- unique(df_selected[,c(8,11,13,16:30)])
WeatherVars_melt <- melt.data.table(WeatherVars, measure.vars =c("avg_temperature_14","avg_radiation_14","avg_precipitation_14") )
p <- WeatherVars_melt
p$Year <- as.factor(p$year)
ggplot(data=p, aes(x=Year, y=value) ) +
  theme_bw()+theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),strip.placement = "outside", plot.title=element_text(hjust=-0.2),strip.background = element_blank(),legend.key=element_rect(size=0.6,color="white"),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="top", panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x =element_text(angle = 0, hjust = 0.5),text = element_text(size=12),strip.text = element_text(size = 12), axis.title = element_text(size = 12))+
  geom_jitter()+
  geom_boxplot( outlier.colour = NA, alpha=0.75)+
  facet_grid(variable~Location,scales = "free",space="free_x")



###
source("~/public/Evaluation/Projects/KP0023_legumes/Scripts/fip-soybean-canopycover/functions/spats_blues.R")

##########
# SpATsBLUE_overall_CC <- fread("~/public/Evaluation/Projects/KP0023_legumes/Soybean/201x/BLUEs_CanopyCover_overall_Soybean.csv")

soybeans_FIP_UAV <- fread("data/soybean_pixels_data.csv")
unique(soybeans_FIP_UAV$Period)
p0 <- subset(soybeans_FIP_UAV)
p0$Range <- p0$plot.range
p0$Row <- p0$plot.row
p0$Rep <- p0$replication
p0$value <- p0$Canopy_cover

p00 <- subset(p0, Period=="Reproductive")
# p01 <- subset(p0)
# p01$Month <- format(as.Date(p01$Date),"%m")
# p01$variable <- paste(p01$variable, p01$Month)
################# get cc treshold

# treshold_dates <- subset(df_all, Model=="Model0")
# treshold_dates

# --- Load and setup ---
library(data.table)
library(nlme)

# Step 1: Create prediction grid for time_since_sowing
growing_dates <- lapply(2015:2022, function(Year) {
  pred_dates <- seq(as.Date(paste0(Year, "-05-01")),
                    as.Date(paste0(Year, "-07-30")),
                    by = "day")
  data.table(Date = pred_dates, time_since_sowing = 1:length(pred_dates), Year = Year)
})
growing_dates <- rbindlist(growing_dates)
growing_dates_sub <- growing_dates[seq(1, .N, 3)]  # Optional thinning every 3 days

# Step 2: Expand to genotypes
select_geno <- unique(df_all$genotype.id)
new_soybean <- CJ(genotype.id = select_geno, Date = growing_dates_sub$Date)
new_soybean <- merge(new_soybean, growing_dates_sub, by = "Date")
new_soybean[, platform := "FIP"]

# Step 3: Merge weather data
weathter_data_modelling <- fread("data/weather_data_for_modelling.csv")
weathter_data <- melt.data.table(weathter_data_modelling,
                                 id.vars = c("WeatherVariable", "Location", "Date", "Year",
                                             "Measure_7", "Measure_14", "Measure_21"),
                                 measure.vars = c("CumulativeDailyMean"),
                                 variable.name = "WeatherCalcVar", value.name = "WeatherValue")
weathter_data <- weathter_data[!is.na(WeatherValue)]

# Keep necessary subset of weather data
WeatherDataToPreodict <- merge(new_soybean, weathter_data, by = c("Date", "Year"), allow.cartesian = TRUE)

# Pivot to wide format
WeatherDataToPreodict_cast <- dcast.data.table(
  WeatherDataToPreodict,
  time_since_sowing + genotype.id + platform + Location+ Date ~ WeatherVariable,
  value.var = c("Measure_7", "Measure_14", "Measure_21")
)

# Step 4: Build model_df for prediction
model_df <- WeatherDataToPreodict_cast[, .(
  time_since_sowing,
  genotype.id = as.factor(genotype.id),
  platform = as.factor(platform),
  Date,
  
  # avg_temperature_28 = Measure_28_Temperature,
  avg_temperature_14 = Measure_14_Temperature,
  
  # avg_precipitation_28 = Measure_28_PrecipitationCap,
  avg_precipitation_14 = Measure_14_PrecipitationCap,
  
  # avg_radiation_28 = Measure_28_RadiationCap,
  avg_radiation_14 = Measure_14_Radiation,
  
  # avg_photothermal_28 = Measure_28_PhotoThermalCap,
  avg_photothermal_14 = Measure_14_PhotothermalProd,
  
  # avg_vpd_28 = Measure_28_VPD,
  avg_vpd_14 = Measure_14_VPD,
  
  # avg_humidity_28 = Measure_28_Humidity,
  avg_humidity_14 = Measure_14_Humidity,
  
  Location= Location
)]

# Step 5: Predict fitted canopy cover
model_df$fit <- predict(Growth_E.GxPTxP, newdata = model_df, level = 0)

lookup_year_site <- unique(df_all[, .(year, Location,year_site.UID)])
lookup_year_site <- lookup_year_site[!duplicated(paste(year,Location)),]

model_df[, year := as.integer(format(Date, "%Y"))]
# Step 3: Merge back into model_df
model_df <- merge(model_df, lookup_year_site, by = c("year", "Location"), all.x = TRUE)


##
###
library(data.table)
setDT(model_df)

# Make sure data is sorted correctly
setorder(model_df, genotype.id, year_site.UID, time_since_sowing)

# Define threshold extraction function
get_threshold_das <- function(dt, threshold) {
  idx <- which(diff(sign(dt$fit - threshold)) != 0)
  if (length(idx) == 0) return(NA_real_)
  
  i <- idx[1]
  x1 <- dt$time_since_sowing[i]
  x2 <- dt$time_since_sowing[i + 1]
  y1 <- dt$fit[i]
  y2 <- dt$fit[i + 1]
  
  x1 + (threshold - y1) / (y2 - y1) * (x2 - x1)
}

# Apply threshold detection grouped by genotype and year_site.UID
threshold_fits <- model_df[, {
  list(
    DAS_CC_0.25 = round(get_threshold_das(.SD, 0.25), 1),
    DAS_CC_0.5  = round(get_threshold_das(.SD, 0.5), 1),
    DAS_CC_0.75 = round(get_threshold_das(.SD, 0.75), 1)
  )
}, by = .(genotype.id, year_site.UID)]


threshold_fits

##############


RefTraits_2015_22 <- fread("data/RefTraits_2015_22_raw.csv")
unique(paste(RefTraits_2015_22$Date, RefTraits_2015_22$variable))
p1 <- subset(RefTraits_2015_22, variable%in%c("Yield","Protein.content","Protein.yield","End.of.maturity"))
p1$var_data <- (paste(p1$Date, p1$variable))
p1 <- subset(p1, !var_data%in%c("2021-09-17 End.of.maturity","2021-09-20 End.of.maturity", "2021-09-22 End.of.maturity", "2021-09-24 End.of.maturity"))

RefTraitsCC <- rbind(p1,p00, fill=T)

RefTraitsCC$year_site.UID2 <- RefTraitsCC$year_site.UID
RefTraitsCC$year_site.UID <- paste(RefTraitsCC$year_site.UID,RefTraitsCC$Date)

RefTraitsCC <- droplevels(RefTraitsCC)
RefTraitsCC$variable2 <- RefTraitsCC$variable
setDT(RefTraitsCC)[, nrow(na.omit(.SD)), by=.(variable2)]
# RefTraitsCC <- subset(RefTraitsCC, NperTrial>35)
RefTraitsCC$Treatment <- as.factor(RefTraitsCC$Treatment)
# 
SpatsAcrossAllEnv <- setDT(RefTraitsCC)[, ( getSpats(.SD) ), by=.(variable2)] ##

SpATsBLUE_overall <- SpatsAcrossAllEnv
SpATsBLUE_overall$genotype.id <- as.character(SpATsBLUE_overall$genotype.id )
SpATsBLUE_overall <- merge(SpATsBLUE_overall, add_gen_id, by="genotype.id")
SpATsBLUE_overall$Genotype_name <- SpATsBLUE_overall$genotype.name

# SpATsBLUE_overall <- subset(SpATsBLUE_overall, predicted.values>1) #fix me. is for yield
yield_variables <- unique(SpATsBLUE_overall$variable)
# SpATsBLUE_overall <- dcast.data.table(SpATsBLUE_overall, genotype.id+year_site.UID+Date~variable, value.var = "predicted.values")
# SpATsBLUE_overall$Protein.yield_h2 <- SpATsBLUE_overall$Yield*SpATsBLUE_overall$Protein.content
# SpATsBLUE_overall$Yield_moist <- SpATsBLUE_overall$Yield*(SpATsBLUE_overall$Moisture/100+1)
# SpATsBLUE_overall <- melt.data.table(SpATsBLUE_overall, measure.vars = c(yield_variables,"Protein.yield_h2","Yield_moist"))


SpATsBLUE_overall$value <- SpATsBLUE_overall$predicted.values
SpATsBLUE_overall$genotype.id <- as.character(SpATsBLUE_overall$genotype.id)



library(data.table)
library(ggplot2)

setDT(SpATsBLUE_overall)

# Add error bars and selection info
SpATsBLUE_overall[, `:=`(
  lower = predicted.values - standard.errors,
  upper = predicted.values + standard.errors,
  Selection = fifelse(Genotype_name == "CH90137", "CH90137",
                      fifelse(Genotype_name == "Opaline", "Opaline",
                              fifelse(Genotype_name == "Gallec", "Gallec", "Other")))
)]

# Create a temporary ordering column
SpATsBLUE_overall[, temp_order := .I, by = variable]

# Ordered factor by predicted values per variable
SpATsBLUE_overall[, Genotype_ordered := factor(temp_order,
                                               levels = .SD[order(predicted.values)]$temp_order),
                  by = variable]

# Mean estimate per variable
SpATsBLUE_overall[, mean_est := mean(predicted.values, na.rm = TRUE), by = variable]

# Updated colors
custom_colors <- c("CH90137" = "red", "Opaline" = "blue", "Gallec" = "darkgreen", "Other" = "grey")

# Plot
ggplot(SpATsBLUE_overall, aes(x = Genotype_ordered, y = predicted.values, color = Selection)) +
  xlab("Genotype") + ylab("Estimated Value") +
  theme_bw() +
  theme(
    legend.position = "top",
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_blank(),
    text = element_text(size = 8)
  ) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "grey") +
  geom_point(data = SpATsBLUE_overall[Genotype_name %in% c("CH90137", "Opaline", "Gallec")], size = 2) +
  geom_errorbar(data = SpATsBLUE_overall[Genotype_name %in% c("CH90137", "Opaline", "Gallec")],
                aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  scale_color_manual(values = custom_colors) +
  geom_hline(aes(yintercept = mean_est), linetype = "dashed") +
  facet_wrap(~variable, scales = "free")



#######
# CI width and selection logic
SpATsBLUE_overall[, `:=`(
  ci_width = 2 * standard.errors,
  Selection = fifelse(Genotype_name == "CH90137", "CH90137",
                      fifelse(Genotype_name == "Opaline", "Opaline",
                              fifelse(Genotype_name == "Gallec", "Gallec", "Other")))
)]

# Ordering based on CI width
SpATsBLUE_overall[, temp_order_ci := .I, by = variable]
SpATsBLUE_overall[, Genotype_ordered_ci := factor(temp_order_ci,
                                                  levels = .SD[order(ci_width)]$temp_order_ci),
                  by = variable]

# Mean CI width per variable
SpATsBLUE_overall[, mean_ci_width := mean(ci_width, na.rm = TRUE), by = variable]

# Plot
ggplot(SpATsBLUE_overall, aes(x = Genotype_ordered_ci, y = ci_width, color = Selection)) +
  xlab("Genotype (ordered by CI width)") + ylab("CI Width") +
  theme_bw() +
  theme(
    legend.position = "top",
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_blank(),
    text = element_text(size = 8)
  ) +
  geom_point(size = 0.5) +
  geom_point(data = SpATsBLUE_overall[Genotype_name %in% c("CH90137", "Opaline", "Gallec")], size = 2) +
  scale_color_manual(values = custom_colors) +
  geom_hline(aes(yintercept = mean_ci_width), linetype = "dashed") +
  facet_wrap(~variable, scales = "free")

#######






p <- subset(SpatsAcrossAllEnv, variable=="Canopy_cover")#SpATsBLUE_overall_CC
p$estimate <- p$predicted.values
p$StdError <- p$standard.errors
p$standard.errors <- NULL
p$predicted.values <- NULL
p$Model <- as.character(p$Date)
p$Date <- NULL
coefs_CC <- rbind(coefs, p, fill=T)

yield_coefs <- merge(subset(SpATsBLUE_overall, variable!="Canopy_cover"),coefs_CC, by="genotype.id",allow.cartesian=TRUE)
# yield_coefs <- merge(SpATsBLUE_overall,SpATsBLUE_overall_CC, by="genotype.id",allow.cartesian=TRUE)

yield_coefs <- setDT(yield_coefs)
yield_coefs$variable_measured <- yield_coefs$variable.x
yield_coefs$variable_fitted <- yield_coefs$variable.y
# yield_coefs[,estimate:=remove_outliers(estimate, 2.5),by=.(variable_measured,variable_fitted)]

yield_coefs_wide <- dcast.data.table(subset(yield_coefs, Model%in%c("Model0","Model10")), genotype.id+variable_measured+value~variable_fitted, value.var = "estimate")
yield_coefs_wide <- subset(yield_coefs_wide, variable_measured=="Protein.yield")
# plot(yield_coefs_wide$scal.Growth, yield_coefs_wide$InteractionPhot.Growth)

var_names <- names(yield_coefs_wide)[4:ncol(yield_coefs_wide)]
yield_coefs_wide <- na.omit(yield_coefs_wide)
formula_string <- paste(var_names, collapse = "+")
formula_object01 <- as.formula(paste("value ~", formula_string))
lm01 <- lm(formula_object01,data=yield_coefs_wide)
summary(lm01)
drop1(lm01,test = "F")
require(plgraphics)
plregr(lm01)
require(car)
vif(lm01)


yield_coefs_wide <- subset(yield_coefs_wide, variable_measured=="End.of.maturity")
# plot(yield_coefs_wide$scal.Growth, yield_coefs_wide$InteractionPhot.Growth)
var_names <- names(yield_coefs_wide)[4:ncol(yield_coefs_wide)]
var_names <- var_names[grepl("Senescence",var_names)]
yield_coefs_wide <- na.omit(yield_coefs_wide)
formula_string <- paste(var_names, collapse = "+")
formula_object01 <- as.formula(paste("value ~", formula_string))
lm01 <- lm(formula_object01,data=yield_coefs_wide)
summary(lm01)
drop1(lm01,test = "F")
require(plgraphics)
plregr(lm01)
require(car)
vif(lm01)
##########

# p_env <- p_Growth[,list(N_env=length(unique(year_site.UID))),by=genotype.id]
p <- subset(yield_coefs)#, genotype.id%in%p_env$genotype.id[p_env$N_env>1])
p$dataset <- "1 env"
# p$dataset[p$genotype.id%in%p_env$genotype.id[p_env$N_env>1]] <- ">1 env"


p <- p[!is.na(value),]
p <- p[!is.na(estimate),]
r2 <- subset(p)
r2 <- setDT(r2)[, list(r=cor(value, estimate),p_cor= cor.test(value, estimate)$p.value, N=nrow(.SD), xx=min((value),yy=min(estimate),na.rm = T), yy=max(estimate,na.rm = T)), by=.(variable.y,variable.x,dataset,Model)]
r2$Significance <- ""
r2$Significance[r2$p_cor<0.1] <- "."
r2$Significance[r2$p_cor<0.05] <- "*"
r2$Significance[r2$p_cor<0.01] <- "**"
r2$Significance[r2$p_cor<0.001] <- "***"

r2$r <- paste("r=",round(r2$r,digits=2),r2$Significance )
r2 <- setDT(r2)[, xx:=min((xx),na.rm = T), by=.(variable.y,variable.x,dataset,Model)]
r2 <- setDT(r2)[, yy:=min((yy),na.rm = T), by=.(variable.y,variable.x,dataset,Model)]

p <- merge(p,r2, by=c("variable.x","variable.y","dataset","Model"))

ggplot(data=p, aes(y=estimate,x=value)) + #ylab(expression("F"["q"]*"'"/"F"["m"]*"'"))+xlab(expression("Biomass [mg/pot and kg/ha]"))+
  theme_bw()+theme(panel.spacing.x = unit(-0.2, "lines"),plot.title=element_text(hjust=-0.2), strip.placement = "outside",strip.background = element_blank(),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=11))+
  # geom_errorbar(aes(ymin=estimate-SE_trend, ymax=estimate+SE_trend),color="grey90",width=0.000001)  +
  # geom_errorbarh(aes(xmin=value.1-SD_Biomass, xmax=value.1+SD_Biomass),height=0.000001,color="grey90")+
  geom_point(aes(y=estimate,x=value,color=value, fill=estimate, shape=dataset),size=1.75, alpha=1)+
  scale_color_gradientn(colours = c("yellow3","darkblue") )+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  # scale_shape_manual(values = c(19,2,4,15,12,6))+
  guides(shape = guide_legend(override.aes = list(size=2)),color = guide_legend(override.aes = list(size=2)))+
  geom_smooth(method="lm",formula = y ~ x, aes(group=1), fill=NA,  alpha=1, show.legend = F)+
  facet_grid(Period+variable_fitted+Model~variable_measured, scales = "free", labeller = label_parsed,switch = "both")+
  # geom_vline(aes(xintercept = Quantile),linetype="dashed")+  # geom_boxplot(outlier.colour = "grey")+#stat_boxplot(geom = "errorbar", width = 0.2)+
  # geom_text_repel(aes(label = Label), color='grey3',  size=2.5, box.padding = 1 )+
  geom_text(size=8/ (14/5), color="black", show.legend = F, aes(x=xx, y=Inf,label=r ,vjust=1.2, hjust=0), check_overlap = T)


p <- subset(p, Model%in%c("Model0","Model10")&variable_measured%in%c("Protein.yield","End.of.maturity"))
p$variable_measured <- gsub("Protein.yield","Protein yield (t/ha)",p$variable_measured)
p$variable_measured <- gsub("End.of.maturity","Maturity (d)",p$variable_measured)
p$variable_measured <- gsub("Yield","Yield (t/ha)",p$variable_measured)

# p$Model <- gsub("Model13","Model3",p$Model)

# p$Model <- as.factor(p$Model)
# p$Model <- factor(p$Model, levels = levels(p$Model)[2:1])
p$variable_fitted <- gsub("Interaction","Scal.Gen:",p$variable_fitted )
p$variable_fitted <- gsub(".Growth","",p$variable_fitted )
p$variable_fitted <- gsub(".Senescence","",p$variable_fitted )

p$variable_fitted <- as.factor(p$variable_fitted)
p$variable_fitted <- factor(p$variable_fitted, levels = levels(p$variable_fitted)[c(1,3,5,2,4)])

library(scales)

ggBeanCoef <- ggplot(data=p, aes(x=estimate,y=value)) + ylab(expression("Reference trait"))+xlab("Model coefficient")+
  theme_bw()+theme(panel.spacing.x = unit(-0.2, "lines"),plot.title=element_text(hjust=-0.2), strip.placement = "outside",strip.background = element_blank(),
                   legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="none",
                   panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                   axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=8))+
  geom_errorbar(aes(ymin=value-standard.errors, ymax=value+standard.errors),color="grey90",width=0.000001)  +
  geom_errorbarh(aes(xmin=estimate-StdError, xmax=estimate+StdError),height=0.000001,color="grey90")+
  geom_point(aes(x=estimate,y=value,fill=estimate),size=1, alpha=0.75)+
  scale_color_gradientn(colours = c("yellow3","darkblue") )+
  scale_x_continuous(breaks = pretty_breaks(n = 3))+
# scale_x_continuous(labels = function(x) format(x, scientific = TRUE))+
  # scale_shape_manual(values = c(19,2,4,15,12,6))+
  guides(shape = guide_legend(override.aes = list(size=2)),color = guide_legend(override.aes = list(size=2)))+
  geom_smooth(method="lm",formula = y ~ x, aes(group=1),   alpha=0.5, show.legend = F, color="#661100")+
  facet_grid(variable_measured~Period+variable_fitted, scales = "free", switch = "both")+
  # geom_vline(aes(xintercept = Quantile),linetype="dashed")+  # geom_boxplot(outlier.colour = "grey")+#stat_boxplot(geom = "errorbar", width = 0.2)+
  # geom_text_repel(aes(label = Label), color='grey3',  size=2.5, box.padding = 1 )+
  geom_text(size=6/ (14/5), color="black", show.legend = F, aes(x=xx, y=Inf,label=r ,vjust=1.5, hjust=0), check_overlap = T)



# library(ggplot2)
# library(cowplot)
# library(scales)
# 
# 
# # --- UPPER PLOT ---
# p_upper <- ggplot(data = subset(p, variable_measured == "Maturity (d)"), aes(x = estimate, y = value)) +
#   ylab(expression("Reference trait")) +
#   theme_bw() +
#   theme(
#     panel.spacing.x = unit(-0.2, "lines"),
#     plot.title = element_text(hjust = -0.2),
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     strip.text.x = element_blank(),  # <-- Remove strip text
#     legend.key.size = unit(0.6, "lines"),
#     legend.title = element_blank(),
#     legend.position = "none",
#     panel.border = element_rect(colour = "black", fill = NA, size = 1),
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_blank(),
#     axis.text.x = element_blank(),         # <-- Remove x-axis labels
#     axis.ticks.x = element_blank(),        # <-- Remove x-axis ticks
#     axis.title.x = element_blank(),        # <-- Remove x-axis title
#     text = element_text(size = 8)
#   ) +
#   geom_errorbar(aes(ymin = value - standard.errors, ymax = value + standard.errors),
#                 color = "grey90", width = 0.000001) +
#   geom_errorbarh(aes(xmin = estimate - StdError, xmax = estimate + StdError),
#                  height = 0.000001, color = "grey90") +
#   geom_point(aes(fill = estimate), size = 1, alpha = 0.75) +
#   scale_color_gradientn(colours = c("yellow3", "darkblue")) +
#   scale_x_continuous(breaks = pretty_breaks(n = 3)) +
#   guides(
#     shape = guide_legend(override.aes = list(size = 2)),
#     color = guide_legend(override.aes = list(size = 2))
#   ) +
#   geom_smooth(method = "lm", formula = y ~ x, aes(group = 1),
#               alpha = 0.5, show.legend = FALSE, color = "#661100") +
#   facet_grid(variable_measured ~ Period + variable_fitted, scales = "free", switch = "both") +
#   geom_text(size = 6 / (14 / 5), color = "black", show.legend = FALSE,
#             aes(x = xx, y = Inf, label = r, vjust = 1.5, hjust = 0), check_overlap = TRUE) +
#   coord_cartesian(ylim = c(100, 130))
# 
# # --- LOWER PLOT ---
# p_lower <- ggplot(data = subset(p, variable_measured != "Maturity (d)"), aes(x = estimate, y = value)) +
#   ylab(expression("Reference trait")) +
#   xlab("Model coefficient") +
#   theme_bw() +
#   theme(
#     panel.spacing.x = unit(-0.2, "lines"),
#     plot.title = element_text(hjust = -0.2),
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     legend.key.size = unit(0.6, "lines"),
#     legend.title = element_blank(),
#     legend.position = "none",
#     panel.border = element_rect(colour = "black", fill = NA, size = 1),
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_blank(),
#     axis.text.x = element_text(angle = 0, hjust = 0.5),
#     text = element_text(size = 8)
#   ) +
#   geom_errorbar(aes(ymin = value - standard.errors, ymax = value + standard.errors),
#                 color = "grey90", width = 0.000001) +
#   geom_errorbarh(aes(xmin = estimate - StdError, xmax = estimate + StdError),
#                  height = 0.000001, color = "grey90") +
#   geom_point(aes(fill = estimate), size = 1, alpha = 0.75) +
#   scale_color_gradientn(colours = c("yellow3", "darkblue")) +
#   scale_x_continuous(breaks = pretty_breaks(n = 3)) +
#   guides(
#     shape = guide_legend(override.aes = list(size = 2)),
#     color = guide_legend(override.aes = list(size = 2))
#   ) +
#   geom_smooth(method = "lm", formula = y ~ x, aes(group = 1),
#               alpha = 0.5, show.legend = FALSE, color = "#661100") +
#   facet_grid(variable_measured ~ Period + variable_fitted, scales = "free", switch = "both") +
#   geom_text(size = 6 / (14 / 5), color = "black", show.legend = FALSE,
#             aes(x = xx, y = Inf, label = r, vjust = 1.5, hjust = 0), check_overlap = TRUE) +
#   coord_cartesian(ylim = c(1.25, 3))
# 
# 
# # Adjust margins: remove bottom margin of upper, top margin of lower
# p_upper <- p_upper + theme(plot.margin = margin(t = 5, r = 5, b = 0, l = 5))
# p_lower <- p_lower + theme(plot.margin = margin(t = 0, r = 5, b = 5, l = 5),strip.switch.pad.grid = unit(0.01, "lines"))
# 
# Combine plots with minimal vertical spacing
# 
# 
# ggBeanCoef <- cowplot::plot_grid(
#   p_upper,
#   p_lower,
#   ncol = 1,
#   rel_heights = c(0.5, 0.8)
# )

ggBeanCoef


#########
Weather_data_Melt <- data.table::fread( "data/weather_data_for_modelling.csv")
names(Weather_data_Melt)
unique(Weather_data_Melt$WeatherVariable)
WeatherVariableSel <- c("Temperature","Radiation","Precipitation","VPD") #,"Windspeed"

p <- subset(Weather_data_Melt, WeatherVariable%in%WeatherVariableSel) #
p <- subset(p, !(Year%in%c(2015,2016,2017,2018,2021,2022)&Location=="Delley")) #

p$Year <-  as.factor(format(p$Date,"%Y"))


levels(as.factor(p$WeatherVariable))
# p <- subset(Weather_data_Melt, Site=="BEL") #

p1 <- df_selected
p1$Date <- p1$date
SelectedDates <- p1[,list(StartMeas=min(Date),EndMeas=max(Date),Sowing_dates=Date[1]-time_since_sowing[1]),by=.(year, Location)]



ggFluc <- ggplot(p)+ ylab("Value")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="right",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
  geom_errorbar(data=p, aes(x=Date, y=dailymean, ymin=dailymean-dailySD, ymax=dailymean+dailySD),color="grey",width=1)+
  geom_point(data=p, aes(x=Date, y=dailymean, color=Year),size=0.75)+
  # geom_area(aes(x=Date, y=value, fill=variable), alpha=0.4, position = "identity")+
  scale_color_manual(values = tol8qualitative)+
  scale_fill_manual(values = tol8qualitative)+
  scale_shape_manual(values = c(4,16))+
  facet_grid(WeatherVariable~Location, scales = "free", space="free_x", switch="both", labeller = label_parsed)+
  geom_vline(data=SelectedDates, aes(xintercept = StartMeas), linetype="dashed")+
  geom_vline(data=SelectedDates, aes(xintercept = EndMeas), linetype="dashed")+
  # geom_hline(aes(yintercept = FinalEe))+
  # ylim(aes(c(0.2, maxY)))+
  # scale_x_datetime(date_labels="%m", date_breaks = "6 hours")+
  guides(color = guide_legend(nrow = 1),fill = guide_legend(nrow = 1))

# ggsave("WeatherFlucOverYears.png",  width = 190, height = 250, units = "mm", dpi = 300)
legendGeno <- cowplot::get_legend(ggFluc)

p$Date <- as.POSIXct(p$Date)
ggFluc <- ggplot(p)+ ylab("Value")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
  geom_errorbar(data=p, aes(x=Date, y=dailymean, ymin=dailymean, ymax=dailymean+dailySD),color="grey",width=1)+
  geom_point(data=p, aes(x=Date, y=dailymean, color=Year),size=0.75)+
  # geom_area(aes(x=Date, y=value, fill=variable), alpha=0.4, position = "identity")+
  scale_color_manual(values = tol8qualitative)+
  scale_fill_manual(values = tol8qualitative)+
  scale_shape_manual(values = c(4,16))+
  facet_grid(WeatherVariable~Location, scales = "free", space="free_x", switch="both", labeller = label_parsed)+
  geom_vline(data=SelectedDates, aes(xintercept = Sowing_dates), linetype="dotted")+
  geom_vline(data=SelectedDates, aes(xintercept = StartMeas), linetype="dashed")+
  geom_vline(data=SelectedDates, aes(xintercept = EndMeas), linetype="dashed")+
  # geom_hline(aes(yintercept = FinalEe))+
  # ylim(aes(c(0.2, maxY)))+
  scale_x_datetime(date_labels="%Y", date_breaks = "1 year")+
  guides(color = guide_legend(nrow = 1),fill = guide_legend(nrow = 1))


AllDateMeasured<- SelectedDates[,list(AllDateMeasured=seq(StartMeas, EndMeas, by="day")),by=.(year, Location)]
# AllDateMeasured <- rbindlist(AllDateMeasured)

p <- subset(Weather_data_Melt, !(WeatherVariable=="Precipitation"&dailymean >0.1))
p$Year <-  as.factor(format(p$Date,"%Y"))
p <- subset(p, Date %in% AllDateMeasured$AllDateMeasured)
p <- subset(p, WeatherVariable%in%WeatherVariableSel) #

ggDensity <- ggplot(p,aes(x=dailymean, fill=Year, color=Year))+ ylab("Value")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
  geom_density(alpha=0.5)+
  scale_color_manual(values = tol8qualitative)+
  scale_fill_manual(values = tol8qualitative)+
  facet_wrap(WeatherVariable~Location, scales = "free", switch="both", labeller = label_parsed, ncol=2)

p$month <-  as.factor(format(p$Date,"%m"))

p[WeatherVariable=="Precipitation",list(sum(dailymean,na.rm=T)),by=.(Year,Location,month)]
p[WeatherVariable=="Radiation",list(sum(dailymean,na.rm=T)),by=.(Year,Location,month)]

library(cowplot)
first_row <- plot_grid(ggFluc, ggDensity,  rel_widths =   c(1,0.5), ncol = 2, labels = c("AUTO"))  #,vjust=0.5+

o1 <- plot_grid(legendGeno, first_row,  rel_heights  =   c(0.2,2), ncol = 1, labels = c(""))  #,vjust=0.5+
# ggsave("Weather_conditions.png",  width = 280, height = 180, units = "mm", o1, bg="white")

#######
Weather_measured <- subset(Weather_data_Melt, Date %in% AllDateMeasured$AllDateMeasured)
Weather_measured$Year <-  as.factor(format(Weather_measured$Date,"%Y"))

p1 <- subset(Weather_measured, WeatherVariable%in%c("PhotothermalProd"))
p2 <- subset(Weather_measured, WeatherVariable%in%c("Precipitation"))
p2$Week <-  as.factor(format(p2$Date,"%W"))
p2 <- p2[,list(dailymean=sum(dailymean)),by=.(Year,Week,Location,WeatherVariable)]

p <- rbind(p1,p2,fill=T)
p <- subset(p, !(Year%in%c(2015,2016,2017,2018,2021,2022)&Location=="Delley")) #

p$dailymean[p$WeatherVariable=="PhotothermalProd"] <- p$dailymean[p$WeatherVariable=="PhotothermalProd"] /1000 # fix me
p$dailymean[p$WeatherVariable=="Precipitation"] <- p$dailymean[p$WeatherVariable=="Precipitation"] *24 # fix me

# p <- subset(p, WeatherVariable%in%WeatherVariableSel) #

ggDensityModel <- ggplot(p,aes(x=dailymean, fill=Year, color=Year,linetype = Location))+ ylab("Density (Probability)")+xlab("Environment")+
  theme_bw()+theme(strip.placement = "outside", panel.spacing.x = unit(0, "lines"), strip.background = element_blank(),legend.title = element_blank(),
                   legend.key.height=unit(0.1,"cm"),legend.key.size = unit(0.2, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
                   axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=8),
                   legend.margin = margin(t = 0, b = -5, l = 35, r = -22), legend.spacing = unit(-5, 'cm'), legend.text = element_text(size = 5) )+
  geom_density(alpha=0.15,size=1)+
  scale_color_manual(values = tol8qualitative)+
  scale_fill_manual(values = tol8qualitative)+
  scale_linetype_manual(values=c(3,1))+
  guides(color = guide_legend(nrow=2))+
  guides(linetype = guide_legend(nrow=2))+
  facet_wrap(.~paste(WeatherVariable), scales = "free", ncol=2, strip.position = "bottom")
ggDensityModel


require(cowplot)
second_row <- plot_grid(ggDensityModel, ggBeanCoef, ncol = 2, rel_widths = c(0.45,1), labels = c("B","C"))  #,vjust=0.5+
first_row <- plot_grid(ggGrowthCurves, second_row,  ncol = 1, rel_heights = c(1,0.45), labels = c("A",""))  #,vjust=0.5+


# ggsave("GrowthFitCoef_Soybean.png", width = 180, height = 210, units = "mm", dpi = 300, first_row)
# ggsave("GrowthFitCoef_Soybean.pdf", width = 180, height = 210, units = "mm", dpi = 100, first_row)


########






library(ggplot2)
library(grid)

p <- subset(Weather_data_Melt, WeatherVariable%in%WeatherVariableSel) #
p <- subset(p, !(Year%in%c(2015,2016,2017,2018,2021,2022)&Location=="Delley")) #

library(ggplot2)
library(grid)
library(data.table)


# Ensure Date is in numeric format for positioning
p$Date_numeric <- as.numeric(p$Date)
p$Year <- as.factor(p$Year)
# Function to create density plots for each facet
create_density_inset <- function(data_subset) {
  ggplot(data_subset, aes(x = dailymean, fill = Year, color = Year)) +
    geom_density(alpha = 0.5) +
    facet_grid(WeatherVariable ~ Location, scales = "free", space = "free_x", switch = "both", labeller = label_parsed) +
    theme_bw() + 
    theme(
      legend.position = "none",
      strip.text = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 6)
    ) +
    scale_fill_manual(values = tol8qualitative) +
    scale_color_manual(values = tol8qualitative) +
    xlab("Daily Mean") +
    ylab("Density")
}

# Base plot (ggFluc)
ggFluc <- ggplot(p) +
  geom_errorbar(aes(x = Date, y = dailymean, ymin = dailymean - dailySD, ymax = dailymean + dailySD), color = "grey", width = 1) +
  geom_point(aes(x = Date, y = dailymean, color = Year), size = 0.75) +
  facet_grid(WeatherVariable ~ Location, scales = "free", space = "free_x", switch = "both", labeller = label_parsed) +
  theme_bw() +
  theme(
    strip.placement = "outside",
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  ) +
  scale_color_manual(values = tol8qualitative)

# Extract unique facets
facet_combinations <- unique(p[, c("WeatherVariable", "Location")])

for (i in 1:nrow(facet_combinations)) {
  facet <- facet_combinations[i, ]
  
  # Filter data for the current facet
  facet_data <- p[p$WeatherVariable == facet$WeatherVariable & p$Location == facet$Location, ]
  
  # Create the density plot for this facet
  density_plot <- create_density_inset(facet_data)
  
  # Convert to grob
  density_grob <- ggplotGrob(density_plot)
  
  # Calculate inset positions
  xmin <- min(facet_data$Date_numeric)+50
  xmax <- xmin + (max(facet_data$Date_numeric) - xmin) * 0.3  # Adjust width of inset
  ymin <- max(facet_data$dailymean) * 0.6
  ymax <- max(facet_data$dailymean) * 0.9
  
  # Add inset to the plot
  ggFluc <- ggFluc +
    annotation_custom(
      grob = density_grob,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    )
}

# Plot the result
print(ggFluc)

# ggsave("FlucWithInsets.png",  width = 280, height = 180, units = "mm", ggFluc, bg="white")

#######
load("/data/Growth_data_nlme_v2.2.RData") 

# Create a data frame with the predictors
predictors <- df
predictors$Asym <- 1
predictors$xmid <- 1
predictors$scal <- 1

predictors$genotype.id <- as.character(predictors$genotype.id)

# # Create vector with empty elements for starting values
                                            # fixed = list(Asym ~ genotype.id+platform,
             #                              xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
             #                              scal ~ genotype.id*(avg_precipitation_14+avg_radiation_14)+platform),
# Create the design matrix
design_matrix_Asym <- model.matrix(~ genotype.id+platform  , data = predictors)
colnames(design_matrix_Asym) <- paste0("Asym.",colnames(design_matrix_Asym))
dim(design_matrix_Asym)
design_matrix_xmid <- model.matrix(~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14 , data = predictors)
colnames(design_matrix_xmid)<- paste0("xmid.",colnames(design_matrix_xmid))
design_matrix_scal <- model.matrix(~      genotype.id*(avg_precipitation_14+avg_radiation_14)+platform  , data = predictors)
colnames(design_matrix_scal)  <- paste0("scal.",colnames(design_matrix_scal))

design_matrix <- cbind(design_matrix_Asym, design_matrix_xmid, design_matrix_scal)
dim(design_matrix)

# options(contrasts = c("contr.treatment", "contr.poly"))
# options(contrasts = c("contr.sum", "contr.poly"))

fixed_effects <- fixef(Model6)
colnames(design_matrix)==names(fixed_effects) ## temp needs to be true


fixed_effects
fixed_effects_names <- names(fixed_effects)
(fixed_effects_names[grep("Asym",fixed_effects_names)])
fixed_effects_names[grep("xmid",fixed_effects_names)]
(fixed_effects_names[grep("scal",fixed_effects_names)])

# colnames(design_matrix) <- fixed_effects_names

design_matrix%*%fixed_effects

# design_matrix_no_genotypes <- design_matrix[,!grepl("genotype",colnames(design_matrix))]
# select <- grepl("Temperature",colnames(design_matrix_no_genotypes))
# select

select <-grepl("genotype",colnames(design_matrix))& grepl(":avg_prec",colnames(design_matrix))
colnames(design_matrix[,select])
effect <- design_matrix[,select]%*%fixed_effects[select]
effect <- as.data.frame(effect)
names(effect) <- "Genotype:Prec"
GxPrec <- (effect)
#                
select <-grepl("genotype",colnames(design_matrix))& grepl(":avg_rad",colnames(design_matrix))
colnames(design_matrix[,select])
effect <- design_matrix[,select]%*%fixed_effects[select]
effect <- as.data.frame(effect)
names(effect) <- "Genotype:Rad"
GxRad <- (effect)
#
select <- grepl("temperature",colnames(design_matrix))& !grepl("genotype",colnames(design_matrix))& !grepl(":",colnames(design_matrix))
colnames(design_matrix[,select])
fixed_effects_names[select]

vector1 <- c("scal.avg_", "xmid.avg_")
vector2 <- c("temperature", "precipitation", "radiation")

# Combine the vectors
combinations <- expand.grid(vector1, vector2)
combined <- paste0(combinations$Var1, combinations$Var2)


weather_effects <- lapply(combined, function(x){
  print(x)
  select <- grepl(x,colnames(design_matrix))& !grepl("genotype",colnames(design_matrix))& !grepl(":",colnames(design_matrix))
  if(length(fixed_effects[select])==1){effect <- design_matrix[,select]*fixed_effects[select]
  effect <- as.data.frame(effect)
  names(effect) <- x
  }else{
    effect <- design_matrix[,select]%*%fixed_effects[select]
    colnames(effect) <- x
  }
  return(effect)
})
weather_effects <- as.data.frame(weather_effects)
names(weather_effects) <- paste0(names(weather_effects),"_effect")

select <- grepl(":avg_",colnames(design_matrix))#& !grepl("genotype",colnames(design_matrix))
colnames(design_matrix[,select])
fixed_effects_names[select]

df$Interaction_effect <- design_matrix[,select]%*%fixed_effects[select]
df_effects <- setDT(cbind(df, weather_effects,GxPrec,GxRad) )#fixme

df_melt <- melt.data.table(df_effects, measure.vars = c("Genotype:Prec","Genotype:Rad",names(weather_effects)),variable.name = "Effect_variable",value.name = "Effect" )
df_melt <- subset(df_melt, Effect_variable!="scal.avg_temperature_effect")
df_melt$Date <- as.Date(df_melt$date)
df_melt$year <- strftime(df_melt$Date  , format = "%Y")


df_melt[,Effect:=remove_outliers(Effect,2.5),by=Effect_variable]

p <- df_melt[,list(Effect=mean(Effect,na.rm=T),SD=sd(Effect,na.rm=T)),by=.(Effect_variable,Date,Location,year_site.UID,year)]
p$variable <- "scal"
p$variable[grepl("xmid",p$Effect_variable)] <- "xmid"

p$Effect_variable2 <- as.character(p$Effect_variable)
p$Effect_variable2[!grepl("Temperature", p$Effect_variable2)] <- "Not Temp"
p$year_loc <- paste(p$Location, p$year, p$year_site.UID, sep=", ")

ggEffects <- ggplot(p) + ylab("Effect size")+
  theme_bw()+theme(panel.spacing.x = unit(-0.2, "lines"),plot.title=element_text(hjust=-0.2), strip.placement = "outside",strip.background = element_blank(),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=8))+
  geom_bar(aes(x=Date, y=(Effect), fill=Effect_variable), stat='identity', position = "dodge", width=10) +
  scale_fill_manual(values = tol8qualitative)+
  facet_wrap(~year_loc, scales = "free_x",switch = "both")
ggEffects

# ggsave("Effects_Soybean.pdf", width = 180, height = 160, units = "mm", dpi = 100, ggEffects)


df_melt$month <- strftime(df_melt$date , format = "%m")

p <- df_melt[,list(Effect=mean(Effect,na.rm=T),SD=sd(Effect,na.rm=T)),by=.(Effect_variable,Location,year_site.UID,month,year)]
p$variable <- "scal"
p$variable[grepl("xmid",p$Effect_variable)] <- "xmid"

p$Date <- as.Date(p$date)

p$Effect_variable2 <- as.character(p$Effect_variable)
p$year_loc <- paste(p$Location, p$year, p$year_site.UID, sep=", ")
p$Month <- month.name[as.numeric(p$month)]

p$month <- as.numeric(p$month )
months <- unique(p$month)
months <- months[order(months)]
p$Month <- month.name[as.integer(p$month)]
p$Month <- as.factor(p$Month)
p$Month <- factor(p$Month, levels=month.name[months])
p <- subset(p, !Effect_variable%in%c("Genotype:Rad","Genotype:Prec"))
ggplot(p) + ylab("Effect size")+
  theme_bw()+theme(panel.spacing.x = unit(-0.2, "lines"),plot.title=element_text(hjust=-0.2), strip.placement = "outside",strip.background = element_blank(),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=8))+
  geom_bar(aes(x=Month, y=(Effect), fill=Effect_variable), stat='identity', position = "dodge", width=1) +
  scale_fill_manual(values = tol8qualitative)+
  facet_wrap(Effect_variable~year_loc, scales = "free_x",switch = "both")

# ggEffects2 <- ggplot(subset(p, Location!="Eschikon")) + ylab("Effect size")+
#   theme_bw()+theme(panel.spacing.x = unit(-0.2, "lines"),plot.title=element_text(hjust=-0.2), strip.placement = "outside",strip.background = element_blank(),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=8))+
#   geom_bar(aes(x=Date, y=(Effect), fill=Effect_variable), stat='identity', position = "dodge", width=10) +
#   scale_fill_manual(values = tol8qualitative)+
#   facet_grid(Location~year_site.UID, scales = "free",switch = "both")

# ggEffects <- plot_grid(ggEffects1, ggEffects2, rel_heights = c(1.5,1), ncol = 1, labels = c(""))  #,vjust=0.5+
# ggEffects

# require(cowplot)
# 
# first_row <- plot_grid(ggFit, ggEffects,  ncol = 1, labels = c("AUTO"))  #,vjust=0.5+
# first_row

########## singe G:Temp curves
unique(df$year_site.UID)
location <- "FPSB015"

df_melt <- melt.data.table(df_effects, measure.vars = c("Interaction_effect",names(weather_effects),"G:Temp"),variable.name = "Effect_variable",value.name = "Effect" )
p <- df_melt

p <- subset(p, year_site.UID==location)
p <- subset(p, genotype.id%in%unique(p$genotype.id)[1:3])

p$Date <- p$date
p$Effect_variable <- as.character(p$Effect_variable)
p$Effect_variable[grep("G:",p$Effect_variable)] <- paste0(p$genotype.id[grep("G:",p$Effect_variable)] ,p$Effect_variable[grep("G:",p$Effect_variable)] )
p$Effect_variable <- gsub("G:",":",p$Effect_variable)

ggEffects <- ggplot(data=p) + ylab("Effect size")+
  theme_bw()+theme(panel.spacing.x = unit(-0.2, "lines"),plot.title=element_text(hjust=-0.2), strip.placement = "outside",strip.background = element_blank(),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=8))+
  geom_bar(aes(x=Date, y=abs(Effect), fill=Effect_variable), stat='identity', position = "dodge", width=10) +
  scale_fill_manual(values = c(tol3qualitative,tol8qualitative))+
  guides(fill = guide_legend(nrow=3))+
  facet_grid(Location~year_site.UID, scales = "free",switch = "both")

# plots <- lapply(unique(df$year_site.UID), function(x){
p <- subset(df, year_site.UID==location)
p <- setDT(p)
p <- melt.data.table(p, measure.vars = c(paste0("Model",0:1)),variable.name = "Model", value.name = "Fit")


p <- subset(p, genotype.id%in%unique(p$genotype.id)[1:3])
p$Date <- p$date
p$plot_grouped_global <- paste(p$plot_grouped_global, p$genotype.id)
p <- setDT(p)[,Rep:=as.numeric(as.factor(UID)),by=.(genotype.id,date,year_site.UID)]
p$Rep <- paste("Rep",p$Rep )


ggFit <- ggplot(data=p,aes(Date, value, group=plot_grouped_global,shape=Rep))+ ylab("Canopy cover (%)")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_point(size=1.5, alpha=1)+
  scale_color_manual(values = tol3qualitative)+
  geom_line(aes(Date, Fit,group=plot_grouped_global, color=genotype.id))+
  facet_grid(genotype.id~year_site.UID+Model,scale="free",switch="both", labeller = label_parsed)

# first_row <- plot_grid(ggFit, ggEffects,  ncol = 1, labels = c("AUTO"))  #,vjust=0.5+
first_row <- plot_grid(NULL, ggEffects,  ncol = 2, labels = c("","B"))  #,vjust=0.5+
first_row <- plot_grid(ggFit, first_row,  ncol = 1, rel_heights = c(1,0.75), labels = c("A",""))  #,vjust=0.5+
first_row

ggsave(paste0("Effects_Soybean_GxE_",location,".png"), width = 200, height = 180, units = "mm", dpi = 100, first_row)


#########

Fit_effects <- rbind(df_melt,subset(p_Growth, Model=="Model1"),fill=T)
Fit_effects$year_site <- paste(Fit_effects$Year, Fit_effects$Location)
Fit_effects$Date <- format(as.Date(Fit_effects$date), "%m-%d")
Fit_effects$Year <- format(as.Date(Fit_effects$date), "%Y")
Fit_effects$Effect <- abs(Fit_effects$Effect/100)
Fit_effects[,Effect_rel:=Effect/max(Effect),by=Effect_variable]
setDT(Fit_effects)[,Rep:=as.numeric(as.factor(UID)),by=.(genotype.id,date,year_site.UID)]
Fit_effects$Rep <- paste("Rep",Fit_effects$Rep )
# p <- subset(p, year_site.UID=="DSSB001")

p <- Fit_effects

p$genotype.id <- as.numeric(as.character(p$genotype.id))

p_mean <- p[,list(Fit_mean=mean(Fit,na.rm=T)),by=.(Year,Location,time_since_sowing,year_site,Model,Period,Effect_variable,genotype.id,Date)]
p_mean <- p_mean[!is.na(p_mean$Fit_mean)]
p[!is.na(p$Fit_mean)]

ggplot() +
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_bar(data=p,aes(x=Date, y=Effect_rel, fill=Effect_variable), stat='identity', position = "dodge", width=2) +
  geom_point(data=p,aes(Date, value, color=genotype.id,shape=Location),size=0.5, alpha=0.5,show.legend = F)+
  geom_line(data=p_mean,aes(Date, Fit_mean, color=genotype.id,group=paste(genotype.id,Location)),size=0.1)+
  scale_colour_gradientn(colors = c("#88CCEE","#117733", "#DDCC77", "#CC6677"))+
  scale_fill_manual(values = tol6qualitative)+
  facet_grid(Year~., scales = "free_y")


####


p <- merge(Fit_effects, coefs_max_melt, by.x =c("genotype.id","Model","Period","year_site.UID"), by.y=c("extreme_genotypes","Model","Period","year_site"),allow.cartesian=TRUE, all.x = T, all.y = F )
# p <- p[!is.na(p$Fit)]
p$variable_extreme[!p$genotype.id%in% coefs_max_melt$extreme_genotype] <- "average"
p$genotype.id[!p$genotype.id%in% coefs_max_melt$extreme_genotype] <- "average"

p_mean <- p[,list(Fit_mean=mean(Fit,na.rm=T)),by=.(Year,Location,time_since_sowing,year_site,year_site.UID,Model,Period,Effect_variable,genotype.id,Date)]
p_mean <- p_mean[!is.na(p_mean$Fit_mean)]

ggplot() +
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_bar(data=p,aes(x=time_since_sowing, y=Effect_rel, fill=Effect_variable), stat='identity', position = "dodge", width=2) +
  geom_point(data=p,aes(time_since_sowing, value, color=genotype.id,shape=year_site),size=0.5, alpha=0.5,show.legend = F)+
  geom_line(data=p_mean,aes(time_since_sowing, Fit_mean,group=paste(genotype.id,year_site.UID)),color="grey",size=1)+
  geom_line(data=subset(p_mean, genotype.id!="average"),aes(time_since_sowing, Fit_mean, color=genotype.id,group=paste(genotype.id,year_site.UID)),size=1)+
  # scale_colour_gradientn(colors = c("#88CCEE","#117733", "#DDCC77", "#CC6677"))+
  scale_fill_manual(values = tol6qualitative)
# facet_grid(year_site.UID~.)

# ###
# coefs_max <- subset(coefs, Period=="Growth"&Model=="Model1")
# coefs_max <- subset(coefs_max, genotype.id%in%df$genotype.id[df$Location=="Eschikon"]  )
# coefs_max <- coefs_max[grepl("genotype.id",coefs_max$term),]
# 
# coefs_max_melt <- coefs_max[,list(max=max(estimate),max_genotype=genotype.id[estimate==max(estimate)],min=min(estimate),min_genotype=genotype.id[estimate==min(estimate)]),by=.(variable,Model,Period)]
# 
# coefs_max_melt <- melt.data.table(coefs_max_melt, id.vars =c("variable","Model","Period"),measure.vars=c("max_genotype","min_genotype"),value.name = "extreme_genotypes",variable.name = "Extreme")
# coefs_max_melt$variable_extreme <- paste(coefs_max_melt$variable,coefs_max_melt$Extreme) #,coefs_max_melt$Model
# unique(coefs_max_melt$extreme_genotypes)
# 
# df_melt <- melt.data.table(df_effects, measure.vars = c("G:Temp","Temperature_effect"),variable.name = "Effect_variable",value.name = "Effect" )
# Fit_effects <- rbind(df_melt,subset(p_Growth, Model=="Model1"),fill=T)
# Fit_effects$year_site <- paste(Fit_effects$Year, Fit_effects$Location)
# Fit_effects$Date <- format(as.Date(Fit_effects$date), "%m-%d")
# Fit_effects$Year <- format(as.Date(Fit_effects$date), "%Y")
# # Fit_effects$Effect <- abs(Fit_effects$Effect/100)
# Fit_effects[,Effect_rel:=Effect/max(Effect),by=Effect_variable]
# setDT(Fit_effects)[,Rep:=as.numeric(as.factor(UID)),by=.(genotype.id,date,year_site.UID)]
# Fit_effects$Rep <- paste("Rep",Fit_effects$Rep )
# p <- merge(Fit_effects, coefs_max_melt, by.x =c("genotype.id","Model","Period"), by.y=c("extreme_genotypes","Model","Period"),allow.cartesian=TRUE, all.x = T, all.y = F )
# p <- subset(p, year_site.UID%in%df$year_site.UID[df$genotype.id%in%coefs_max_melt$extreme_genotype]  )
# unique(p$year_site.UID)
# 
# p$variable_extreme[!p$genotype.id%in% coefs_max_melt$extreme_genotype] <- "average"
# p$genotype.id[!p$genotype.id%in% coefs_max_melt$extreme_genotype] <- "average"
# p$Effect_rel[!p$genotype.id%in% coefs_max_melt$extreme_genotype] <- NA
# 
# p_mean <- p[,list(Fit_mean=mean(Fit,na.rm=T)),by=.(Year,Location,time_since_sowing,year_site,year_site.UID,Model,Period,Effect_variable,genotype.id,Date)]
# p_mean <- p_mean[!is.na(p_mean$Fit_mean)]
# 
# ggplot() +
#   theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
#   # geom_bar(data=subset(p,Effect_variable=="Temperature_effect" ),aes(x=time_since_sowing, y=Effect_rel, fill=Effect_variable), stat='identity', position = "dodge", width=2) +
#   geom_bar(data=subset(p,Effect_variable=="G:Temp" ),aes(x=time_since_sowing, y=Effect_rel, fill=genotype.id), stat='identity', position = "dodge", width=2) +
#   geom_point(data=p,aes(time_since_sowing, value, color=genotype.id,shape=year_site),size=0.5, alpha=0.5,show.legend = F)+
#   geom_line(data=p_mean,aes(time_since_sowing, Fit_mean,group=paste(genotype.id,year_site.UID)),color="grey",size=1)+
#   geom_line(data=subset(p_mean, genotype.id!="average"),aes(time_since_sowing, Fit_mean, color=genotype.id,group=paste(genotype.id,year_site.UID)),size=1)+
#   # scale_colour_gradientn(colors = c("#88CCEE","#117733", "#DDCC77", "#CC6677"))+
#   scale_color_manual(values = tol6qualitative)+
#   scale_fill_manual(values = tol6qualitative)
#   # facet_grid(year_site.UID~.)



####### investigate outlier plots
# coefs_max_melt <- melt.data.table(coefs_max_year_site, id.vars =c("year_site","variable","Model","Period"),measure.vars=c("max_genotype","min_genotype"),value.name = "extreme_genotypes",variable.name = "Extreme")

# investigate <- subset(df, paste(genotype.id, year_site.UID) %in%paste(coefs_max_melt$extreme_genotype, coefs_max_melt$year_site))
# investigate <- investigate[grepl("FPSB",investigate$year_site.UID),]
# setDT(investigate)[,length(unique(UID)),by=plot_grouped_global]
# setDT(investigate)[,N_env:=as.numeric(as.factor(year_site.UID)),by=genotype.id]
# investigate <- investigate[order(UID, genotype.id,date),]
# investigate <- investigate[!duplicated(paste(date, genotype.id)),]
# setDT(investigate)[,N:=1:nrow(.SD),by=UID]
# investigate <- subset(investigate, N_env==1&N>3)
# setDT(investigate)[,maxN:=max(N),by=.(UID,year_site.UID)]
# setDT(investigate)[,maxN:=min(maxN),by=.(year_site.UID)]
# investigate <- investigate[investigate$N<=investigate$maxN,]
# investigate$year <- as.character(investigate$year)

###
  
  
make_loop_plots  <-  function(year,investigate,Pattern= "_segmentation.png",ncol_plot=3){
  year2 <- as.character(year)
  print(year2)
  files <- list.files(path=paste0("~/public/Evaluation/Projects/KP0023_legumes/Soybean/",year2),pattern=Pattern,recursive = T,full.names = T)
  files <- files[grepl("/RGB1/",files)]
  # investigate <- subset(example, year_site.UID=="FPSB004"&genotype.name=="Opaline")
  investigate$UID2 <- investigate$plot.UID
  # Ensure 'TitleNr' is in Date format for proper ordering
  investigate$date2 <- as.Date(investigate$Date) # Assuming 'date2' is already in a format like "YYYY-MM-DD"
  
  
  setDT(investigate)[,Filename_row:=files[grepl(UID2,files)&grepl(gsub("-","_",date2), files)][1],by=.(plot.UID,Date,year_site.UID)]
  
  make_plots <- function(png_files_exclude, ncol_plot ) {
    library(magick)
    require(grid)
    require(cowplot)
    
    # Filter out any NULL or empty files
    # png_files_exclude <- png_files_exclude[!is.na(png_files_exclude$File),] # not anymore
    
    if (nrow(png_files_exclude) > 0) {  # Only proceed if there are files to plot
      plots <- lapply(png_files_exclude$File, function(file) {
        # Check if file is NA
        if (is.na(file)) {
          grid::textGrob("No Data", gp = gpar(fontsize = 10, col = "grey"))
          }else{
        img <- image_read(file)
        img <- image_scale(img, "25%")
        
       
        # Get the dimensions of the image
        img_height <- image_info(img)$height
        img_width <- image_info(img)$width
        
        # Calculate the number of pixels to crop from the top and bottom
        crop_pixels_top <- floor((10 / 100) * img_height)
        crop_pixels_bottom <- ceiling((10 / 100) * img_height)
        if(Pattern%in%c("pixels.png","MaskPixels.png")){
          crop_pixels_bottom <- 0
          crop_pixels_top <- 0
        }
        # Crop the image from top and bottom
        img <- image_crop(img, geometry = paste(img_width, img_height - crop_pixels_top - crop_pixels_bottom, "+0+", crop_pixels_top, sep = "x"))
        
        rasterGrob(img, interpolate = TRUE)
        }
      })
      
      nrow_plot <- ceiling(length(plots) / ncol_plot)
      
      # Arrange plots with date labels
      gg <- plot_grid(plotlist = plots, ncol=ncol_plot, labels = as.character(png_files_exclude$TitleNr),label_size = 16)
      title <- ggdraw() + draw_label(paste("Plot:",png_files_exclude$UID[1]), fontface='bold',size = 16)
      gg <- plot_grid(title, gg, ncol=1, rel_heights=c(0.1, 1))
      
      return(gg)
    }
  }
  
  # Creating the ToPlot table and arranging by date
  ToPlot <- data.table(File = investigate$Filename_row, TitleNr = investigate$date2, UID = investigate$plot.UID, genotype.name=investigate$genotype.name)
  # ToPlot <- ToPlot[!duplicated(paste(ToPlot$date2, ToPlot$UID)),]
  ToPlot <- ToPlot[order(UID, TitleNr)]  # Arrange by UID and then by date
  # ToPlot <- na.omit(ToPlot)
  print(ToPlot)
  # Defining the make_plots function with NULL check and UID in filename
  
  # ToPlot[, list(Result = make_plots(.SD,  ncol_plot=ncol_plot, UID = UID)), by = UID]
  if(nrow(ToPlot)==0){print(paste("Skip",year2)) }else{
    # gg_list <- ToPlot[,list(make_plots(.SD,ncol=nrow(.SD))),by=UID]
    gg_list <-  lapply(unique(ToPlot$UID), function(xx) make_plots(subset(ToPlot,UID==xx),ncol_plot=ncol_plot))
    
    NperPlot <- ToPlot[,list(N=nrow(.SD)),by=.(UID)]
    NperPlot$N_ceiling <- ceiling(NperPlot$N/ncol_plot)
    nrow_plot_overall <- sum( NperPlot$N_ceiling)
    print(nrow_plot_overall)
    library(gridExtra)
    final_plot <- arrangeGrob(grobs =gg_list, ncol=1)
    title <- ggdraw() + draw_label(paste("Breeding line:",ToPlot$genotype.name[1]), fontface='bold',size = 18)
    final_plot <- plot_grid(title, final_plot, ncol=1, rel_heights=c(0.1, 1))
    ggsave(paste0(year2,Pattern,ToPlot$genotype.name[1],"_checks.png"), plot = final_plot, dpi = 150, units = "mm", height = nrow_plot_overall*60+1, width=90*ncol_plot)
    }
    
}


## check the checks
soybeans_FIP_UAV <- fread("data/soybean_pixels_data.csv")
example <- subset(soybeans_FIP_UAV, platform=="FIP")
N_per_id <- setDT(example)[,list(N=length(unique(year_site.UID))),by=.(genotype.id,genotype.name)]
N_per_id[order(N_per_id$N),]
example <- subset(example, genotype.name%in%c("Gallec","Opaline","Protéix"))
example$Year <-  as.factor(format(example$date,"%Y"))
example <- example[!duplicated(paste(example$Date, example$plot.UID)),]
example <- subset(example, time_since_sowing>50)
unique(example$year_site.UID)


check_dates <- unique(example[,c("Year","Date","year_site.UID")])
check_plots <- unique(example[,c("Year","plot.UID","genotype.id","genotype.name")])
check <- merge(check_dates,check_plots,by="Year",allow.cartesian=TRUE)
add <- subset(check, !paste(Year, Date, plot.UID)%in% paste(example$Year, example$Date, example$plot.UID) )
# lapply(levels(example$Year), function(x) make_loop_plots(x, example, RGB="_q90.jpg",ncol_plot = 6) ) 
example <- rbind(example, add, fill=T)
example <- example[order(example$Date),]
example <- example[,measuremnt:=1:nrow(.SD),by=plot.UID]
example <- subset(example, measuremnt%in%1:4)
example$Year2 <- example$Year

# example[,nrow(.SD),by=.(Year2,genotype.id)]
# example <- subset(example, Year==2021)
# example <- subset(example, plot.UID==example$plot.UID[1])

# example[, make_loop_plots(Year[1], .SD, Pattern="_pixels.png",ncol_plot = 6), by=.(Year2,genotype.id)]
example[, make_loop_plots(Year[1], .SD, Pattern="_q90.jpg",ncol_plot = 6), by=.(Year2,genotype.id)]

#####
library(magick)
library(ggplot2)
library(cowplot)
library(grid)

# Load two images
img1 <- image_read("2021_q90.jpgProtéix_checks.png")
img2 <- image_read("2021_segmentation.pngProtéix_checks.png")

# Convert images to rasterGrob for ggplot
g1 <- rasterGrob(img1, interpolate = TRUE)
g2 <- rasterGrob(img2, interpolate = TRUE)

# Create ggplot objects for each image
plot1 <- ggplot() +
  annotation_custom(g1, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_void()

plot2 <- ggplot() +
  annotation_custom(g2, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_void()

# Arrange the plots underneath each other
final_plot <- plot_grid(plot1, plot2, ncol = 1, align = "v",labels = "AUTO")

ggsave("Example_plots_2021.png" , final_plot,  width = 170, height = 120, units = "mm")
####
####
####
library(ggplot2)
library(data.table)
library(grid)
library(magick)

# Load and prepare the data
files <- list.files(pattern = "_checks.png", recursive = TRUE, full.names = TRUE)

# Extract year and line information from filenames
data <- data.table(File = files)
data[, `:=`(
  Year = as.numeric(gsub("^./|_.*", "", File)),  # Extract year
  Line = gsub(".*_q90.jpg|_checks.png", "", File)  # Extract line
)]

# Define all possible combinations of Year and Line
all_years <- sort(unique(data$Year))
all_lines <- sort(unique(data$Line))
complete_grid <- CJ(Year = all_years, Line = all_lines)

# Merge the actual data with the complete grid
data <- merge(complete_grid, data, by = c("Year", "Line"), all.x = TRUE)

# Read images and calculate their dimensions (only for existing files)
data[, `:=`(
  ImageHeight = sapply(File, function(file) {
    if (!is.na(file)) image_info(image_read(file))$height else NA
  }),
  ImageWidth = sapply(File, function(file) {
    if (!is.na(file)) image_info(image_read(file))$width else NA
  })
)]

# Create rasterGrobs for valid files
data[, ImageGrob := lapply(File, function(file) {
  if (!is.na(file)) {
    img <- image_read(file)
    # img <- image_scale(img, "25%")
    rasterGrob(as.raster(img), interpolate = TRUE)
  } else {
    NULL
  }
})]

# Normalize heights for consistent scaling in the plot
data[, ScaledHeight := ifelse(!is.na(ImageHeight), ImageHeight / max(ImageHeight, na.rm = TRUE), NA)]
data$Line_num <- as.numeric(as.factor(data$Line))
data <- data[order(data$Line_num),]

# Base ggplot with blank geom for faceting
p <- ggplot(data, aes(x = Line_num, y = Year)) +
  geom_blank() +  # Blank layer to establish facets
  facet_grid(rows = vars(Year), cols = vars(Line), space = "free", scales = "free",switch="y") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  )

# Add images dynamically using annotation_custom (only for non-NULL grobs)
for (i in seq_len(nrow(data))) {
  row <- data[i]
  if (!is.null(row$ImageGrob[[1]])) {  # Ensure the grob exists
    p <- p + annotation_custom(
      grob = row$ImageGrob[[1]],  # Use the grob directly
      xmin =  row$Line_num - 0.5,  # Adjust xmin for alignment
      xmax =  row$Line_num + 0.5,   # Adjust xmax for alignment
      ymin = row$Year - row$ScaledHeight/4,  # Adjust ymin for alignment
      ymax =  row$Year + row$ScaledHeight/4  # Adjust ymax for alignment
    )
  }
}

# Save the plot
ggsave("facet_image_plot.png", plot = p, width = 15, height = 20, units = "in", dpi=150)

##############################

# library(data.table)
# library(ggplot2)
# 
# library(data.table)
# library(ggplot2)
# library(grid)
# library(magick)
# library(data.table)
# library(ggplot2)
# library(grid)
# library(magick)
# 
# make_loop_plots <- function(year, investigate, Pattern = "_segmentation.png", ncol = 5) {
#   year2 <- as.character(year)
#   print(year2)
#   
#   # Load files
#   files <- list.files(path = paste0("~/public/Evaluation/Projects/KP0023_legumes/Soybean/", year2),
#                       pattern = Pattern, recursive = TRUE, full.names = TRUE)
#   files <- files[grepl("/RGB1/", files)]
#   
#   # Match files to investigate dataset
#   investigate$UID2 <- investigate$plot.UID
#   investigate$date2 <- as.Date(investigate$Date)
#   setDT(investigate)[, Filename_row := files[grepl(UID2, files) & grepl(gsub("-", "_", date2), files)][1], 
#                      by = .(plot.UID, Date, year_site.UID)]
#   
#   # Ensure every UID has consistent dates
#   all_dates <- unique(investigate$date2)
#   all_uids <- unique(investigate$plot.UID)
#   complete_grid <- CJ(UID = all_uids, Date = all_dates)
#   investigate <- merge(complete_grid, investigate, by.x = c("UID", "Date"), by.y = c("plot.UID", "date2"), all.x = TRUE)
#   
#   # Prepare data for plotting
#   investigate[, DataAvailable := !is.na(Filename_row)]
#   investigate[, Image := lapply(Filename_row, function(file) {
#     if (!is.na(file)) {
#       img <- image_read(file)
#       img <- image_scale(img, "100x100")
#       rasterGrob(as.raster(img))
#     } else {
#       NULL
#     }
#   })]
#   
#   # Prepare ggplot-compatible dataset with an empty column for images
#   image_data <- investigate[!is.na(Filename_row), .(UID, Date, Image)]
#   
#   # Create the base ggplot
#   p <- ggplot() +
#     geom_tile(data = investigate, aes(x = Date, y = UID, fill = DataAvailable), color = "white") +
#     scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "grey")) +
#     facet_wrap(~ UID, ncol = ncol, scales = "free") +
#     theme_minimal() +
#     labs(title = paste("Plots for Year", year2), x = "Date", y = "UID") +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       panel.grid = element_blank(),
#       strip.background = element_rect(fill = "lightblue"),
#       legend.position = "none"
#     )
#   
#   # Overlay images where data is available
#   for (i in seq_len(nrow(image_data))) {
#     row <- image_data[i]
#     p <- p + annotation_custom(row$Image[[1]], xmin = as.numeric(row$Date) - 0.5, 
#                                xmax = as.numeric(row$Date) + 0.5, 
#                                ymin = as.numeric(row$UID) - 0.5, 
#                                ymax = as.numeric(row$UID) + 0.5)
#   }
#   
#   # Save the plot
#   ggsave(paste0(year2, Pattern, "_facet_image_plot.png"), plot = p, width = ncol * 3, height = 5, units = "in")
# }
# 
# # Example usage
# example <- fread("data/soybean_pixels_data.csv")
# example <- subset(example, genotype.name %in% c("Gallec", "Opaline") & platform == "FIP")
# example$Year <- as.factor(format(as.Date(example$date), "%Y"))
# example$Year2 <- example$Year
# example <- example[!duplicated(paste(example$Date, example$plot.UID)), ]
# example <- subset(example, time_since_sowing > 50)
# all_dates <- unique(example$Date)
# all_uids <- unique(example$plot.UID)
# complete_grid <- CJ(plot.UID = all_uids, Date = all_dates)
# example <- merge(example, complete_grid, by = c("plot.UID", "Date"),  all.y = TRUE)
# example <- example[order(example$Date),]
# example <- example[,measuremnt:=1:nrow(.SD),by=plot.UID]
# example <- subset(example, measuremnt%in%1:4)
# example$Year2 <- example$Year
# 
# # Call the function
# example[, make_loop_plots(Year[1], .SD, Pattern = "_q90.jpg", ncol = 4), by = .(Year2, genotype.id)]

########

##
CoefsOfInterest <- subset(coefs_max_melt, Model%in%c("Model1")) #,"Model3"
CoefsOfInterest
coefs_max <- subset(coefs, Model%in%CoefsOfInterest$Model &variable%in%CoefsOfInterest$variable)
coefs_max <- coefs_max[grep(":avg_Temp",coefs_max$term),]
coefs_max <- coefs_max[,max_values:=max(estimate),by=.(variable,Model,Period)]
coefs_max <- coefs_max[,min_values:=min(estimate),by=.(variable,Model,Period)]
# coefs_max$extreme_genotype <- "average"
# coefs_max$extreme_genotype[coefs_max$estimate==coefs_max$max_values]  <- coefs_max$genotype.name[coefs_max$estimate==coefs_max$max_values]
# coefs_max$extreme_genotype[coefs_max$estimate==coefs_max$min_values]  <- coefs_max$genotype.name[coefs_max$estimate==coefs_max$min_values]
coefs_max$extreme_genotype <- as.character(coefs_max$genotype.name)
coefs_max <- coefs_max[!is.na(coefs_max$extreme_genotype),]
coefs_max$extreme_genotype[!coefs_max$genotype.id%in%CoefsOfInterest$extreme_genotypes] <- "x_average"

p <- coefs_max

ggBarExtremeLines<- ggplot(p) + ylab("Line x Temp")+
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1),text = element_text(size=8))+
  geom_bar(aes(x=genotype.name, y=estimate, fill=extreme_genotype), stat='identity', position = "dodge", width=2) +
  scale_fill_manual(values=c(tol21rainbow[c(1,3,5,7,9,11,13,15,17,19,21,2,4,6,8,10,12,14,16,18,20)],"grey"))+
  facet_grid(variable~.,scales = "free",switch = "both")
ggBarExtremeLines

# p <- melt.data.table(p, measure.vars = c("Asym.Growth"))
p <- dcast.data.table(p, genotype.id+Model+Period~variable, value.var = "estimate")
p$extreme_genotype <- p$genotype.id
p$extreme_genotype[!p$genotype.id%in% coefs_max$extreme_genotype] <- "average"
ggplot(p) +
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.size = unit(0.9, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9))+
  geom_point(aes(x=InteractionTemp.Growth, y=mid.Growth, color=extreme_genotype)) +
  scale_color_manual(values = tol6qualitative)+
  facet_grid(Period~Model,scales = "free")

##
ggEffects <- plot_grid(ggEffects1, ggEffects2, rel_heights = c(1.5,1), ncol = 1, labels = c(""))  #,vjust=0.5+
ggEffects
require(cowplot)
first_row <- plot_grid(ggGrowthCurves, ggBarExtremeLines, ggEffects, rel_heights = c(1.5,0.5,0.75), ncol = 1, labels = c("AUTO"))  #,vjust=0.5+
# first_row
# ggsave("Effects_Soybean_GxE.png", width = 200, height = 280, units = "mm", dpi = 100, first_row)
# ggsave("Effects_Soybean_GxE.pdf", width = 200, height = 280, units = "mm", dpi = 100, first_row)


##########

