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
# load("model/Growth7_E.GxRxP.RData")
# load("model/Growth8_E.GxPTxP.RData") # did not converge

# load("model/Growth_E.GxPTxP_Contr.Sum.RData") # did not converge

anova_result_Gro <-anova(Growth1_G, Growth2_E.GxT,  Growth3_E.GxPT,  Growth4_E.GxR, Growth5_E.GxP, Growth6_E.GxPT, Growth_E.GxPTxP)
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
nrow(df)
df$date <- as.Date(df$date)
df <- setDT(df)
df$Model0 <- fitted(Growth_E.GxPTxP)
df$Model1 <- fitted(Growth1_G)
df$Model2 <- fitted(Growth2_E.GxT)
df$Model3 <- fitted(Growth3_E.GxPT)
df$Model4 <- fitted(Growth4_E.GxR)
df$Model5 <- fitted(Growth5_E.GxP)
df$Model6 <- fitted(Growth6_E.GxPT)
# df$Model7 <- fitted(Growth7_E.GxRxP)

Model_names <- data.frame(Model=paste0("Model",c(0:1,3,10:11)),Name=c("Growth_E.GxPTxP","Growth1_G","Growth3_E.GxPT","Senescence_E.GxPT","Senescence1_G"))
# p <- subset(df, Location=="Delley")
df[,nrow(na.omit(.SD)[!duplicated(genotype.id),]),by=year_site.UID]

p_Growth <- melt.data.table(df, measure.vars = c(paste0("Model",c(0:6))),variable.name = "Model", value.name = "Fit")
p_Growth$Period <- "Growth"
p_Growth$period <- NULL
p_Growth$Max <- NULL
p_Growth$Rep<- NULL


load("data/Senescence_data.RData") 
nrow(df)
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
add_gen_id <- unique(design_all[,c("genotype.id","genotype.name","maturity.group")])
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

p <- subset(df_selected, genotype.id%in%len_geno$genotype.id[1:2]&Model%in%c("Model0","Model1","Model3","Model10","Model11")&Location=="Eschikon")
p <- subset(p, !year_site.UID%in%c("FPSB015","FPSB016"))
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
  scale_color_manual(name="Model", values = (tol5qualitative))+
  guides(color = guide_legend(nrow=2))+
  guides(shape = guide_legend(nrow=2))+
  facet_grid(genotype.id~year_site.UID,scale="free",switch="both", labeller = label_parsed)

print(ggFit_idealLines)

# ggsave("Soybean_IdealLines_Fit.pdf", width = 170, height = 140, units = "mm", dpi = 100, ggFit_idealLines)


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
coefs2 <- extract_fixed_effects(Growth2_E.GxT, "Model2", "Growth")
coefs3 <- extract_fixed_effects(Growth3_E.GxPT, "Model3", "Growth")
coefs4 <- extract_fixed_effects(Growth4_E.GxR, "Model4", "Growth")
coefs5 <- extract_fixed_effects(Growth5_E.GxP, "Model5", "Growth")
coefs10 <- extract_fixed_effects(Senescence_E.GxPT, "Model10", "Senescence")
coefs11 <- extract_fixed_effects(Senescence1_G, "Model11", "Senescence")

# Combine all results into one dataframe
coefs <- rbind(coefs0, coefs1, coefs2, coefs3, coefs4, coefs5,  coefs10, coefs11)

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
coefs[,uniqueN(genotype.id),by=Model]
coefs[,uniqueN(genotype.id)]
coefs[,length(estimate[is.na(estimate)]),by=.(Model,Interaction_term)]


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

unique(coefs$maturity.group)
LateMaturityGroups <- c("I","I/II")

coefs_max <- coefs[!maturity.group%in%LateMaturityGroups,list(max=max(estimate),max_genotype=genotype.id[estimate==max(estimate)],min=min(estimate),min_genotype=genotype.id[estimate==min(estimate)]),by=.(variable,Model,Period)]
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
p$Selection <- "Unstable/extreme"
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

tol9qualitativeAdj <- (tol10qualitative[c(2,6,3,1,7:9,4,5,10)]) 
# p$genotype.name <- paste(p$genotype.name, p$Selection)
# p_ideal_candidates$genotype.name <- paste(p_ideal_candidates$genotype.name, p_ideal_candidates$Selection)

ggGrowthCurves <- ggplot()+ ylab("Canopy cover (%)")+ xlab("Days after sowing (d)")+
  theme_bw()+theme(panel.spacing.x = unit(-0.2, "lines"), strip.placement = "outside", strip.background = element_blank(),legend.key.size = unit(0.9, "lines"),
                   legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(), axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=8))+
  geom_jitter( size=1.5, alpha=1)+
  geom_point(data=subset(p, !variable_extreme%in%variable_extreme_growth&Period=="Growth"),aes(time_since_sowing, value, shape=variable_extreme, linetype = Selection), size=0.25, alpha=0.5, color="grey", shape=1,show.legend = F)+
  geom_point(data=subset(p, variable_extreme%in%variable_extreme_growth&Period=="Growth"),aes(time_since_sowing, value, shape=variable_extreme, color=genotype.name, group=paste(UID,platform), linetype = Selection), size=0.5, alpha=0.5, shape=1)+
  geom_point(data=p_ideal_candidates,aes(time_since_sowing, value, shape=variable_extreme, color=genotype.name, group=paste(UID,platform), linetype = Selection), size=0.5, alpha=0.5, shape=1)+
  
  geom_point(data=subset(p, !variable_extreme%in%variable_extreme_sen&Period=="Senescence"),aes(time_since_sowing, value, shape=variable_extreme, linetype = Selection), size=0.25, alpha=0.5, color="grey", shape=1,show.legend = F)+
  geom_point(data=subset(p, variable_extreme%in%variable_extreme_sen&Period=="Senescence"),aes(time_since_sowing, value, shape=variable_extreme, color=genotype.name, group=paste(UID,platform), linetype = Selection), size=0.5, alpha=0.5, shape=1)+
  scale_color_manual(name="Genotype", values=tol9qualitativeAdj)+
  scale_shape_manual(values=1:25)+
  # guides(shape = guide_legend(nrow=2))+
  scale_linetype_manual(values=c(1,3))+
  geom_line(data=subset(p, !variable_extreme%in%variable_extreme_growth&Period=="Growth"),aes(time_since_sowing, Fit,  group=paste(UID,platform), linetype = Selection),size=0.1,color="grey80",show.legend = F)+
  geom_line(data=subset(p, variable_extreme%in%variable_extreme_growth&Period=="Growth"),aes(time_since_sowing, Fit, color=genotype.name, linetype = Selection, group=paste(UID,platform)),size=0.5)+ #, linetype=variable_extreme
  geom_line(data=p_ideal_candidates,aes(time_since_sowing, Fit, color=genotype.name, linetype = Selection, group=paste(UID,platform), linetype = Selection),size=0.5)+ #, linetype=variable_extreme
  
  geom_line(data=subset(p, !variable_extreme%in%variable_extreme_sen&Period=="Senescence"),aes(time_since_sowing, Fit,  group=paste(UID,platform), linetype = Selection),size=0.1,color="grey80",show.legend = F)+
  geom_line(data=subset(p, variable_extreme%in%variable_extreme_sen&Period=="Senescence"),aes(time_since_sowing, Fit, color=genotype.name, linetype = Selection, group=paste(UID,platform)),size=0.5)+ #, linetype=variable_extreme
  
  geom_text(data = R2_plot_data,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            hjust = ifelse(R2_plot_data$Period == "Growth", 0, 1),
            vjust = 1.5,
            size = 2.5, check_overlap = T, color="grey80")+
  
  facet_wrap(year_loc ~ . ,strip.position="top")
ggGrowthCurves

#####
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


p$year_loc <- paste(p$Location, p$year, sep=", ")
p$Selection <- "Unstable/extreme"
p$Selection[p$genotype.id%in%ideal_candidate] <- "Stable"


variable_extreme_growth <- subset(coefs_max_overall, Period=="Growth")$variable_extreme
variable_extreme_sen <- subset(coefs_max_overall, Period=="Senescence")$variable_extreme

p_ideal_candidates <- subset(p, Period=="Growth"&genotype.id%in%ideal_candidate)
p_ideal_candidates[,length(unique(year_loc)),by=genotype.name]

R2_year_site <- p[,compute_r2(value, Fit),by=.(Model,year_loc,Period)]
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
  Period=Period
)]#, by = .(year_site.UID)]

# Join with R² table
R2_plot_data <- merge(R2_year_site, time_bounds, by = c("year_loc", "Period"), all.x = TRUE)
R2_plot_data[, label := paste0("R² = ", round(V1, 2))]

# Set x based on period, y to Inf
R2_plot_data[, x := ifelse(Period == "Growth", x_min, x_max)]
R2_plot_data[, y := Inf]

# p$genotype.name <- paste(p$genotype.name, p$Selection)
# p_ideal_candidates$genotype.name <- paste(p_ideal_candidates$genotype.name, p_ideal_candidates$Selection)

ggGrowthCurvesYear <- ggplot()+ ylab("Canopy cover (%)")+ xlab("Days after sowing (d)")+
  theme_bw()+theme(panel.spacing.x = unit(-0.2, "lines"), strip.placement = "outside", strip.background = element_blank(),legend.key.size = unit(0.9, "lines"),
        legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=8),
        legend.margin = margin(t = 0, b = -5, l = 0, r = 0))+
  geom_jitter( size=1.5, alpha=1)+
  geom_point(data=subset(p, !variable_extreme%in%variable_extreme_growth&Period=="Growth"),aes(time_since_sowing, value, shape=variable_extreme, linetype = Selection), size=0.25, alpha=0.5, color="grey", shape=1,show.legend = F)+
  geom_point(data=subset(p, variable_extreme%in%variable_extreme_growth&Period=="Growth"),aes(time_since_sowing, value, shape=variable_extreme, color=genotype.name, group=paste(UID,platform), linetype = Selection), size=0.5, alpha=0.5, shape=1)+
  geom_point(data=p_ideal_candidates,aes(time_since_sowing, value, shape=variable_extreme, color=genotype.name, group=paste(UID,platform), linetype = Selection), size=0.5, alpha=0.5, shape=1)+
  
  geom_point(data=subset(p, !variable_extreme%in%variable_extreme_sen&Period=="Senescence"),aes(time_since_sowing, value, shape=variable_extreme, linetype = Selection), size=0.25, alpha=0.5, color="grey", shape=1,show.legend = F)+
  geom_point(data=subset(p, variable_extreme%in%variable_extreme_sen&Period=="Senescence"),aes(time_since_sowing, value, shape=variable_extreme, color=genotype.name, group=paste(UID,platform), linetype = Selection), size=0.5, alpha=0.5, shape=1)+
  scale_color_manual(name="Genotype", values=tol9qualitativeAdj)+
  scale_shape_manual(values=1:25)+
  # guides(shape = guide_legend(nrow=2))+
  scale_linetype_manual(values=c(1,3))+
  geom_line(data=subset(p, !variable_extreme%in%variable_extreme_growth&Period=="Growth"),aes(time_since_sowing, Fit,  group=paste(UID,platform), linetype = Selection),size=0.1,color="grey80",show.legend = F)+
  geom_line(data=subset(p, variable_extreme%in%variable_extreme_growth&Period=="Growth"),aes(time_since_sowing, Fit, color=genotype.name, linetype = Selection, group=paste(UID,platform)),size=0.5)+ #, linetype=variable_extreme
  geom_line(data=p_ideal_candidates,aes(time_since_sowing, Fit, color=genotype.name, linetype = Selection, group=paste(UID,platform), linetype = Selection),size=0.5)+ #, linetype=variable_extreme
  
  geom_line(data=subset(p, !variable_extreme%in%variable_extreme_sen&Period=="Senescence"),aes(time_since_sowing, Fit,  group=paste(UID,platform), linetype = Selection),size=0.1,color="grey80",show.legend = F)+
  geom_line(data=subset(p, variable_extreme%in%variable_extreme_sen&Period=="Senescence"),aes(time_since_sowing, Fit, color=genotype.name, linetype = Selection, group=paste(UID,platform)),size=0.5)+ #, linetype=variable_extreme
  
  geom_text(data = R2_plot_data,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE,
              hjust = ifelse(R2_plot_data$Period == "Growth", 0, 1),
              vjust = 1.5,
              size = 2.5, check_overlap = T, color="grey80")+

  facet_wrap(year_loc ~ .,strip.position="top",ncol=5)
ggGrowthCurvesYear

#####
#####



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
uniqueN(RefTraits_2015_22$genotype.id)
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

SpATsBLUE_overall <- copy(SpatsAcrossAllEnv)
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
coefs_CC <- rbind(coefs[,!names(coefs)%in%c("genotype.name","maturity.group"),with=F], p[,names(p)%in%names(coef),with=F], fill=T)

SpATsBLUE_overall_cast <- dcast.data.table(SpatsAcrossAllEnv, genotype.id   ~variable, value.var = "predicted.values")
uniqueN(SpATsBLUE_overall_cast$genotype.id)
uniqueN(SpATsBLUE_overall_cast$genotype.id[!is.na(SpATsBLUE_overall_cast$Protein.yield)])

SpATsBLUE_overall_cast$Yield_per_day <- SpATsBLUE_overall_cast$Yield/SpATsBLUE_overall_cast$End.of.maturity
SpATsBLUE_overall_cast$Protein.yield_per_day <- SpATsBLUE_overall_cast$Protein.yield/SpATsBLUE_overall_cast$End.of.maturity

p <- melt.data.table(SpATsBLUE_overall_cast, measure.vars = c("Yield_per_day","Protein.yield_per_day"))
p$predicted.values <- NA
p <- p[,names(p)%in%names(SpATsBLUE_overall),with=F]
SpATsBLUE_overall <- rbind(SpATsBLUE_overall, p, fill=T)

yield_coefs <- merge(subset(SpATsBLUE_overall, variable!="Canopy_cover"),coefs_CC, by="genotype.id",allow.cartesian=TRUE)
# yield_coefs <- merge(SpATsBLUE_overall,SpATsBLUE_overall_CC, by="genotype.id",allow.cartesian=TRUE)
yield_coefs[,uniqueN(genotype.id),by=variable.x]

yield_coefs <- setDT(yield_coefs)
yield_coefs$variable_measured <- yield_coefs$variable.x
yield_coefs$variable_fitted <- yield_coefs$variable.y
# yield_coefs[,estimate:=remove_outliers(estimate, 2.5),by=.(variable_measured,variable_fitted)]
yield_per_day <- subset(yield_coefs, variable_measured=="Protein.yield"&variable_fitted=="Asym.Senescence")
yield_per_day$predicted.values <- yield_per_day$predicted.values/yield_per_day$estimate
yield_per_day$variable <- "Protein.yield.daily"
yield_per_day$variable_fitted <- NULL
yield_coefs_daily <- merge(yield_per_day[,names(yield_per_day)%in%names(SpATsBLUE_overall),with=F],coefs_CC, by="genotype.id",allow.cartesian=TRUE)

p <- yield_coefs_daily
p$variable_measured <- p$variable.x
p$variable_fitted <- p$variable.y
p$dataset <- "all"
r2 <- setDT(p)[, list(r=cor(value, estimate),p_cor= cor.test(value, estimate)$p.value, N=nrow(.SD), xx=min((value),yy=min(estimate),na.rm = T), yy=max(estimate,na.rm = T)), by=.(variable.y,variable.x,Model,dataset)]
r2
# p <- merge(p,r2, by=c("variable.x","variable.y","dataset","Model"))
# 
# ggplot(data=p, aes(y=estimate,x=value)) + #ylab(expression("F"["q"]*"'"/"F"["m"]*"'"))+xlab(expression("Biomass [mg/pot and kg/ha]"))+
#   theme_bw()+theme(panel.spacing.x = unit(-0.2, "lines"),plot.title=element_text(hjust=-0.2), strip.placement = "outside",strip.background = element_blank(),legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=11))+
#   # geom_errorbar(aes(ymin=estimate-SE_trend, ymax=estimate+SE_trend),color="grey90",width=0.000001)  +
#   # geom_errorbarh(aes(xmin=value.1-SD_Biomass, xmax=value.1+SD_Biomass),height=0.000001,color="grey90")+
#   geom_point(aes(y=estimate,x=value,color=value, fill=estimate, shape=dataset),size=1.75, alpha=1)+
#   scale_color_gradientn(colours = c("yellow3","darkblue") )+
#   scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
#   # scale_shape_manual(values = c(19,2,4,15,12,6))+
#   guides(shape = guide_legend(override.aes = list(size=2)),color = guide_legend(override.aes = list(size=2)))+
#   geom_smooth(method="lm",formula = y ~ x, aes(group=1), fill=NA,  alpha=1, show.legend = F)+
#   facet_grid(Period+variable_fitted+Model~variable_measured, scales = "free", labeller = label_parsed,switch = "both")
#   # geom_vline(aes(xintercept = Quantile),linetype="dashed")+  # geom_boxplot(outlier.colour = "grey")+#stat_boxplot(geom = "errorbar", width = 0.2)+
#   # geom_text_repel(aes(label = Label), color='grey3',  size=2.5, box.padding = 1 )+
#   # geom_text(size=8/ (14/5), color="black", show.legend = F, aes(x=xx, y=Inf,label=r ,vjust=1.2, hjust=0), check_overlap = T)
require(car)

yield_coefs_wide <- dcast.data.table(subset(yield_coefs, Model%in%c("Model2","Model10")), genotype.id+variable_measured+value~variable_fitted, value.var = "estimate")
yield_coefs_wide <- subset(yield_coefs_wide, variable_measured=="Protein.yield")
p <- dcast.data.table(subset(yield_coefs, Model%in%c("Model5")), genotype.id+variable_measured+value~variable_fitted, value.var = "estimate")
yield_coefs_wide_sub <- merge(yield_coefs_wide, p[,c("genotype.id","InteractionPrec.Growth")],by="genotype.id")

# plot(yield_coefs_wide$scal.Growth, yield_coefs_wide$InteractionPhot.Growth)

var_names <- names(yield_coefs_wide_sub)[4:ncol(yield_coefs_wide_sub)]
yield_coefs_wide_sub <- na.omit(yield_coefs_wide_sub)
formula_string <- paste(var_names, collapse = "+")
formula_object00 <- as.formula(paste("value ~", formula_string))
lm00 <- lm(formula_object00,data=yield_coefs_wide_sub)
summary(lm00)
vif(lm00)
#####

yield_coefs_wide <- dcast.data.table(subset(yield_coefs, Model%in%c("Model0","Model10")), genotype.id+variable_measured+value~variable_fitted, value.var = "estimate")
yield_coefs_wide_sub <- subset(yield_coefs_wide, variable_measured=="Protein.yield")
uniqueN(yield_coefs_wide_sub$genotype.id)

var_names <- names(yield_coefs_wide_sub)[4:ncol(yield_coefs_wide_sub)]
yield_coefs_wide_sub <- na.omit(yield_coefs_wide_sub)
formula_string <- paste(var_names, collapse = "+")
formula_object00 <- as.formula(paste("value ~", formula_string))
lm00 <- lm(formula_object00,data=yield_coefs_wide_sub)
summary(lm00)
vif(lm00)

yield_coefs_wide <- dcast.data.table(subset(yield_coefs, Model%in%c("Model0")), genotype.id+variable_measured+value~variable_fitted, value.var = "estimate")
yield_coefs_wide_sub <- subset(yield_coefs_wide, variable_measured=="Protein.yield")
uniqueN(yield_coefs_wide_sub$genotype.id)

var_names <- names(yield_coefs_wide_sub)[4:ncol(yield_coefs_wide_sub)]
yield_coefs_wide_sub <- na.omit(yield_coefs_wide_sub)
formula_string <- paste(var_names, collapse = "+")
formula_object00 <- as.formula(paste("value ~", formula_string))
lm00 <- lm(formula_object00,data=yield_coefs_wide_sub)
summary(lm00)
vif(lm00)
#####
## Stand-alone code: fit the same linear model separately for each maturity group

library(data.table)

# --- 1) Cast to wide ---
dt <- as.data.table(yield_coefs)
dt <- dt[Model %in% c("Model10","Model5")]

yield_coefs_wide <- dcast(
  dt,
  genotype.id + maturity.group + variable_measured + value ~ variable_fitted,
  value.var = "estimate"
)

# --- 2) Subset to the reference trait ---
yield_coefs_wide_sub <- yield_coefs_wide[variable_measured == "Protein.yield"]

# --- 3) Pick predictors (Senescence columns) ---
var_names <- names(yield_coefs_wide_sub)[5:ncol(yield_coefs_wide_sub)]

if (length(var_names) == 0) stop("No predictor columns matched 'Senescence'.")

# --- 4) Keep only needed columns and drop NAs ---
keep_cols <- c("maturity.group", "value", var_names)
dat <- yield_coefs_wide_sub[, ..keep_cols]
dat <- na.omit(dat)

# --- 5) Build formula ---
form <- as.formula(paste("value ~", paste(var_names, collapse = " + ")))

# --- 6) Fit lm per maturity group and print summary + n ---
maturity_groups <- sort(unique(dat$maturity.group))
lm_list <- setNames(vector("list", length(maturity_groups)), maturity_groups)

for (m in maturity_groups) {
  
  dat_m <- dat[maturity.group == m]
  n_obs <- nrow(dat_m)
  
  cat("\n==============================\n")
  cat("Maturity group:", m, "\n")
  cat("Number of observations (n):", n_obs, "\n")
  cat("==============================\n")
  
  # safety check
  if (n_obs < (length(var_names) + 2)) {
    cat("Skipping: not enough observations to fit model\n")
    next
  }
  
  fit <- lm(form, data = as.data.frame(dat_m))
  lm_list[[as.character(m)]] <- fit
  
  print(summary(fit))
}

# --- Optional: compact overview table ---
overview <- data.table(
  maturity.group = names(lm_list),
  n = sapply(lm_list, function(m) if (is.null(m)) NA else nobs(m)),
  r.squared = sapply(lm_list, function(m) if (is.null(m)) NA else summary(m)$r.squared),
  adj.r.squared = sapply(lm_list, function(m) if (is.null(m)) NA else summary(m)$adj.r.squared)
)

cat("\nModel overview:\n")
print(overview)


### summary table

yield_coefs_wide <- dcast.data.table(subset(yield_coefs, Model%in%c("Model0")), genotype.id+variable_measured+value~variable_fitted, value.var = "estimate")
yield_coefs_wide_sub <- subset(yield_coefs_wide, variable_measured=="Protein.yield")

var_names <- names(yield_coefs_wide_sub)[4:ncol(yield_coefs_wide_sub)]
yield_coefs_wide_sub <- na.omit(yield_coefs_wide_sub)
formula_string <- paste(var_names, collapse = "+")
formula_object00 <- as.formula(paste("value ~", formula_string))
lm00 <- lm(formula_object00,data=yield_coefs_wide_sub)
summary(lm00)
vif(lm00)


## assumes:
## - yield_coefs is a data.table (or data.frame) with columns:
##   Model, genotype.id, variable_measured, value, variable_fitted, estimate
## - data.table is available

library(data.table)

# ---------- helpers ----------
sig_stars <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*",
                       ifelse(p < 0.1, ".", "")
                )
         )
  )
}

fmt_est_se_star <- function(est, se, p, digits = 3) {
  paste0(
    formatC(est, format = "f", digits = digits),
    " (", formatC(se, format = "f", digits = digits), ")",
    sig_stars(p)
  )
}

# ---------- model fitting ----------
fit_one_lm <- function(yield_coefs, model_ids,
                       response_trait = "Protein.yield") {
  
  dt_wide <- dcast.data.table(
    subset(yield_coefs, Model %in% model_ids),
    genotype.id + variable_measured + value ~ variable_fitted,
    value.var = "estimate"
  )
  
  dt_sub <- subset(dt_wide, variable_measured == response_trait)
  dt_sub <- na.omit(dt_sub)
  
  var_names <- names(dt_sub)[4:ncol(dt_sub)]
  if (length(var_names) == 0L)
    stop(paste("No predictors for", paste(model_ids, collapse = "+")))
  
  fml <- as.formula(paste("value ~", paste(var_names, collapse = " + ")))
  lm(fml, data = dt_sub)
}

# ---------- tidy output ----------
tidy_lm <- function(fit, model_name, digits_se = 3) {
  s <- summary(fit)
  
  cf <- as.data.table(s$coefficients, keep.rownames = "Predictor")
  setnames(cf, c("Predictor", "Estimate", "SE", "t", "p"))
  
  cf[, Model := model_name]
  
  # estimate: signif(4), SE: fixed decimals, stars unchanged
  cf[, Estimate_SE :=paste0(
    format(signif(Estimate, 4), scientific = FALSE),
    " (", format(signif(SE, digits_se), scientific = FALSE), ")",
    sig_stars(p)
  )]
  
  cf[, `:=`(
    R2 = round(unname(s$r.squared), 2),
    Adj_R2 = round(unname(s$adj.r.squared), 2),
    n = length(fit$fitted.values)
  )]
  
  cf[]
}


# ---------- specify which models to run ----------
main_models   <- paste0("Model", c(0, 10))   # Model0 & Model10
growth_models <- paste0("Model", 1:5)        # Model1 ... Model5

model_sets <- list()

# single models
for (m in c(main_models, growth_models)) {
  model_sets[[m]] <- c(m)
}

# combinations with Model10
for (m in c("Model0", growth_models)) {
  model_sets[[paste0(m, "+Model10")]] <- c(m, "Model10")
}

# ---------- fit all models ----------
fits <- lapply(model_sets, function(ids)
  fit_one_lm(yield_coefs, ids, response_trait = "Protein.yield")
)

# ---------- collect results ----------
coef_table <- rbindlist(
  lapply(names(fits), function(nm)
    tidy_lm(fits[[nm]], model_name = nm)),
  use.names = TRUE,
  fill = TRUE
)

library(data.table)

# coef_table must contain at least: Model, Predictor, Estimate_SE, R2, Adj_R2, n

# ----------------------------
# 1) Clean Predictor names (your mapping stays)
# ----------------------------
coef_table[, Predictor := gsub("^Asym\\.Growth$", "Asym.Gen (Growth)", Predictor)]
coef_table[, Predictor := gsub("^Asym\\.Senescence$", "Asym.Gen (Senescence)", Predictor)]
coef_table[, Predictor := gsub("^InteractionPhot\\.Growth$", "Scal.Gen:Phot (Growth)", Predictor)]
coef_table[, Predictor := gsub("^InteractionPhot\\.Senescence$", "Scal.Gen:Phot (Senescence)", Predictor)]
coef_table[, Predictor := gsub("^InteractionTemp\\.Growth$", "Scal.Gen:Temp (Growth)", Predictor)]
coef_table[, Predictor := gsub("^InteractionPrec\\.Growth$", "Scal.Gen:Prec (Growth)", Predictor)]
coef_table[, Predictor := gsub("^InteractionRad\\.Growth$", "Scal.Gen:Rad (Growth)", Predictor)]
coef_table[, Predictor := gsub("^scal\\.Growth$", "Scal.Gen (Growth)", Predictor)]
library(data.table)

# ----------------------------
# 0) Start from coef_table (Model must still be Model0, Model2+Model10, Model10, ...)
# ----------------------------
coef_table_final <- coef_table[, .(Model, Predictor, Estimate_SE, R2, Adj_R2, n)]

# ----------------------------
# 1) Clean Predictor suffixes (remove "(Growth)" / "(Senescence)" if present)
# ----------------------------
coef_table_final[, Predictor := gsub("\\s*\\((Growth|Senescence)\\)", "", Predictor)]

# ----------------------------
# 2) Parse model structure + define Period
# ----------------------------
coef_table_final[, Model_id := Model]                                # keep original
coef_table_final[, BaseModel := sub("\\+Model10$", "", Model_id)]    # remove +Model10 if present

coef_table_final[, Period := fifelse(
  Model_id == "Model10", "Sen",
  fifelse(grepl("\\+Model10$", Model_id), "Growth+Sen", "Growth")
)]

# ----------------------------
# 3) Map to your underscore model names
# ----------------------------
model_map <- c(
  "Model0"  = "Growth_E.GxPTxP",
  "Model1"  = "Growth1_G",
  "Model2"  = "Growth2_E.GxT",
  "Model3"  = "Growth3_E.GxPT",
  "Model4"  = "Growth4_E.GxR",
  "Model5"  = "Growth5_E.GxP",
  "Model6"  = "Growth6_E.GxPT",
  "Model7"  = "Growth7_E.GxRxP",
  "Model10" = "Sen_E.GxPT"
)

# Base label
coef_table_final[, Model_label := model_map[BaseModel]]

# Append Senescence part for combos with Model10
coef_table_final[grepl("\\+Model10$", Model_id),
                 Model_label := paste0(Model_label, "+", model_map["Model10"])
]

# Standalone Model10 label
coef_table_final[Model_id == "Model10", Model_label := model_map["Model10"]]

stopifnot(!any(is.na(coef_table_final$Model_label)))

# ----------------------------
# 4) Remove intercept terms (robust)
# ----------------------------
coef_table_final <- coef_table_final[!grepl("^\\(Intercept\\)$", Predictor)]

# ----------------------------
# 5) Clean Estimate_SE formatting (remove .0000 etc.)
# ----------------------------
coef_table_final[, Estimate_SE := gsub("(?<=\\d)\\.0+(?=\\D|$)", "", Estimate_SE, perl = TRUE)]
coef_table_final[, Estimate_SE := gsub("\\(\\s+", "(", Estimate_SE)]
coef_table_final[, Estimate_SE := gsub("\\s+\\)", ")", Estimate_SE)]
coef_table_final[, Estimate_SE := gsub("\\s{2,}", " ", Estimate_SE)]

# ----------------------------
# 6) Create combined Model–Period cell for pretty tables (optional)
# ----------------------------
setorder(coef_table_final, Model_label, Period, Predictor)

coef_table_final[, ModelPeriod := Model_label]#paste0(Model_label, "–", Period)]
coef_table_final[, block_id := paste(Model_label, Period)]

coef_table_final[, Model_print := ifelse(!duplicated(block_id), ModelPeriod, "")]
coef_table_final[, R2_print    := ifelse(!duplicated(block_id), sprintf("%.2f", R2), "")]
coef_table_final[, AdjR2_print := ifelse(!duplicated(block_id), sprintf("%.2f", Adj_R2), "")]
coef_table_final[, n_print     := ifelse(!duplicated(block_id), as.character(n), "")]

# ---------------------------------------
# Final table used for LaTeX
# ---------------------------------------
tab <- coef_table_final[, .(
  Model = Model_print,
  Predictor,
  Estimate = Estimate_SE,
  `R^2` = R2_print,
  `Adj. R^2` = AdjR2_print,
  n = n_print
)]

# ---------------------------------------
# Escape LaTeX characters
# ---------------------------------------
latex_escape <- function(x) {
  x <- gsub("([&_#%$])", "\\\\\\1", x, perl = TRUE)
  x
}

tab[, Predictor := latex_escape(Predictor)]
tab[, Estimate  := latex_escape(Estimate)]
tab[, Model     := latex_escape(Model)]

# ---------------------------------------
# Generate LaTeX (booktabs)
# ---------------------------------------
latex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Linear models explaining genotypic variability in protein yield using coefficients from non-linear green canopy cover (CC) growth and senescence models as predictors. Estimates are shown with standard errors in parentheses, $R^2$, and the number of genotypes (\textit{n}).}",
  "\\label{tab:protein_yield_models}",
  "\\begin{tabular}{llcrcr}",
  "\\toprule",
  "\\textbf{Model} & \\textbf{Predictor} & \\textbf{Estimate (SE)} & \\textbf{$R^2$} & \\textbf{Adj. $R^2$} & \\textbf{$n$} \\\\",
  "\\midrule"
)

latex_body <- tab[, sprintf(
  "%s & %s & %s & %s & %s & %s \\\\",
  Model, Predictor, Estimate, `R^2`, `Adj. R^2`, n
)]

latex_lines <- c(
  latex_lines,
  latex_body,
  "\\bottomrule",
  "\\end{tabular}",
  "\\\\[1mm]",
  "\\footnotesize{Significance: $^{*}p<0.05$, $^{**}p<0.01$, $^{***}p<0.001$.}",
  "\\end{table}"
)

cat(paste(latex_lines, collapse = "\n"))

###############

yield_coefs_wide <- dcast.data.table(subset(yield_coefs, Model%in%c("Model10","Model0")), genotype.id+variable_measured+value~variable_fitted, value.var = "estimate")
yield_coefs_wide_sub <- subset(yield_coefs_wide, variable_measured=="End.of.maturity")
# plot(yield_coefs_wide_sub$scal.Growth, yield_coefs_wide_sub$InteractionPhot.Growth)
var_names <- names(yield_coefs_wide_sub)[4:ncol(yield_coefs_wide_sub)]
# var_names <- var_names[grepl("Senescence",var_names)]
yield_coefs_wide_sub <- na.omit(yield_coefs_wide_sub)
formula_string <- paste(var_names, collapse = "+")
formula_object01 <- as.formula(paste("value ~", formula_string))
lm01 <- lm(formula_object01,data=yield_coefs_wide_sub)
summary(lm01)
drop1(lm01,test = "F")
require(plgraphics)
plregr(lm01)
require(car)
vif(lm01)
##########
library(scales)

p <- subset(yield_coefs)

p$dataset <- "Reduced"
p$dataset[p$Model%in%c("Model0","Model10")] <- "Main"
p$dataset <- as.factor(p$dataset)

p <- subset(p, dataset=="Main")
# p <- rbind(subset(p, dataset=="Reduced"& genotype.id%in%p_Sen$genotype.id),
#       subset(p, dataset=="Main"))


p <- p[!is.na(value),]
p <- p[!is.na(estimate),]
r2 <- subset(p)
r2 <- setDT(r2)[, list(r=cor(value, estimate),p_cor= cor.test(value, estimate)$p.value, N=nrow(.SD), xx=min((value),yy=min(estimate),na.rm = T), yy=max(estimate,na.rm = T)), by=.(variable.y,variable.x,dataset,Model)]
r2$Significance <- ""
r2$Significance[r2$p_cor<0.1] <- "~'.'"
r2$Significance[r2$p_cor<0.05] <- "~'*'"
r2$Significance[r2$p_cor<0.01] <- "~'**'"
r2$Significance[r2$p_cor<0.001] <- "~'***'"

r2$r <- paste("r=",round(r2$r,digits=2),r2$Significance )
r2 <- setDT(r2)[, xx:=min((xx),na.rm = T), by=.(variable.y,variable.x,dataset,Model)]
r2 <- setDT(r2)[, yy:=min((yy),na.rm = T), by=.(variable.y,variable.x,dataset,Model)]

p <- merge(p,r2, by=c("variable.x","variable.y","dataset","Model"))
p <- p[, xx:=min((value),na.rm = T), by=.(variable.x,dataset,Model)]

p <- subset(p, Model%in%paste0("Model",c(0,10,3,5))&variable_measured%in%c("Protein.yield","End.of.maturity"))
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


p <- p[, xx:=min((value-standard.errors),na.rm = T), by=.(variable.y,variable.x,dataset,Model)]


unique(p$variable_fitted)
p <- rbind(subset(p, variable_fitted%in%c("Scal.Gen:Phot","Scal.Gen:Prec")&Period=="Growth"), subset(p,variable_fitted%in%c("Asym")))


library(data.table)
setDT(p)

# one r per facet x Model (based on your existing r column after merge)
lab <- unique(p[, .(r, xx), by = .(Period, variable_fitted, variable_measured, Model, dataset)])
lab$label <- gsub("r=", "", lab$r)                 # "0.81***"
lab$label <- paste0("italic(r)~'='~", lab$label)
lab$label <- gsub(" ", "", lab$label)
lab$label
# lab$label[lab$dataset=="Main"] <- paste( "\n", lab$label)[lab$dataset=="Main"]
lab$symbol <- ifelse(
  lab$dataset == levels(lab$dataset)[1],
  "'\u25B3'",  # △ empty triangle
  "'\u25CF'"   # ● filled circle
)
# 
# lab$label <- paste0(
#   lab$label,
#   "~'('~", lab$symbol, "~')'"
# )


ggBeanCoef_sub <- ggplot(
  data = p,
  aes(x = value, y = estimate, group=dataset)) +
  xlab(expression("Reference trait")) +
  ylab("Model coefficient") +
  
  theme_bw() +
  theme(
    panel.spacing = unit(-0.2, "lines"),
    plot.title = element_text(hjust = -0.2),
    strip.placement = "outside",
    strip.background = element_blank(),
    legend.key.size = unit(0.6, "lines"),
    # legend.title = element_blank(),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    text = element_text(size = 8),
    legend.margin = margin(t = 0, b = -5, l = 0, r = 0)
  ) +
  
  ## vertical error bars (estimate ± StdError)
  geom_errorbar(
    aes(ymin = estimate - StdError, ymax = estimate + StdError),
    color = "grey90",
    width = 0.000001
  ) +
  
  ## horizontal error bars (value ± standard.errors)
  geom_errorbarh(
    aes(xmin = value - standard.errors, xmax = value + standard.errors),
    color = "grey90",
    height = 0.000001
  ) +
  
  geom_point(
    aes(shape=dataset, fill=dataset),
    size = 0.5,
    alpha = 0.9
  ) +
  
  scale_fill_manual(name="Model",values = c("grey30", "black")) +
  scale_shape_manual(name="Model",values = c(2,19))+
  scale_color_manual(name="Model",values = c("#661100", "#CC6677")) +
  scale_y_continuous(breaks = pretty_breaks(n = 3)) +
  
  guides(
    shape = guide_legend(override.aes = list(size = 1.5)),
    # color = guide_legend(override.aes = list(size = 2))
  ) +
  
  geom_smooth(aes(color = dataset),
              method = "lm",
              formula = y ~ x,
              alpha = 0.5, size=0.5
              # show.legend = FALSE
  ) +


  facet_grid(
    Period + variable_fitted ~ variable_measured,
    scales = "free",
    switch = "both"
  ) +
  
  geom_text(
    data = subset(lab, dataset == levels(lab$dataset)[1]),
    aes(x = xx, y = Inf, label = label),
    inherit.aes = FALSE,
    color = "#661100",
    size = 6 / (14 / 5),
    vjust = 1.5,
    hjust = 0,
    parse = TRUE,
    show.legend = FALSE
  ) +
  geom_text(
    data = subset(lab, dataset == levels(lab$dataset)[2]),
    aes(x = xx, y = Inf, label = label),
    inherit.aes = FALSE,
    color = "#CC6677",
    size = 6 / (14 / 5),
    vjust = 2.9,   # slightly lower so they stack nicely
    hjust = 0,
    parse = TRUE,
    show.legend = FALSE
  )


ggBeanCoef_sub
######
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
p <- p[, xx:=min((value),na.rm = T), by=.(variable.x,dataset,Model)]

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


p <- subset(p, Model%in%c("Model0","Model1","Model2","Model3","Model4","Model5","Model10")&variable_measured%in%c("Protein.yield","End.of.maturity","Yield"))
p$variable_measured <- gsub("Protein.yield","Protein yield (t/ha)",p$variable_measured)
p$variable_measured <- gsub("End.of.maturity","Maturity (d)",p$variable_measured)
p$variable_measured <- gsub("Yield","Yield (t/ha)",p$variable_measured)

# p$Model <- gsub("Model13","Model3",p$Model)

# p$Model <- as.factor(p$Model)
# p$Model <- factor(p$Model, levels = levels(p$Model)[2:1])
p$variable_fitted <- gsub("Interaction","Scal.Gen:",p$variable_fitted )
p$variable_fitted <- gsub(".Growth","",p$variable_fitted )
p$variable_fitted <- gsub(".Senescence","",p$variable_fitted )
p$variable_fitted <- gsub("scal","Scal",p$variable_fitted )

p$variable_fitted <- as.factor(p$variable_fitted)
# p$variable_fitted <- factor(p$variable_fitted, levels = levels(p$variable_fitted)[c(1,3,5,2,4)])

library(scales)

# ggBeanCoef <- ggplot(data=p, aes(x=estimate,y=value)) + ylab(expression("Reference trait"))+xlab("Model coefficient")+
#   theme_bw()+theme(panel.spacing.x = unit(-0.2, "lines"),plot.title=element_text(hjust=-0.2), strip.placement = "outside",strip.background = element_blank(),
#                    legend.key.size = unit(0.6, "lines"),legend.title=element_blank(), legend.position="none",
#                    panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
#                    axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=8))+
#   geom_errorbar(aes(ymin=value-standard.errors, ymax=value+standard.errors),color="grey90",width=0.000001)  +
#   geom_errorbarh(aes(xmin=estimate-StdError, xmax=estimate+StdError),height=0.000001,color="grey90")+
#   geom_point(aes(x=estimate,y=value,fill=estimate),size=1, alpha=0.75)+
#   scale_color_gradientn(colours = c("yellow3","darkblue") )+
#   scale_x_continuous(breaks = pretty_breaks(n = 3))+
# # scale_x_continuous(labels = function(x) format(x, scientific = TRUE))+
#   # scale_shape_manual(values = c(19,2,4,15,12,6))+
#   guides(shape = guide_legend(override.aes = list(size=2)),color = guide_legend(override.aes = list(size=2)))+
#   geom_smooth(method="lm",formula = y ~ x, aes(group=1),   alpha=0.5, show.legend = F, color="#661100")+
#   facet_grid(variable_measured~Period+variable_fitted, scales = "free", switch = "both")+
#   # geom_vline(aes(xintercept = Quantile),linetype="dashed")+  # geom_boxplot(outlier.colour = "grey")+#stat_boxplot(geom = "errorbar", width = 0.2)+
#   # geom_text_repel(aes(label = Label), color='grey3',  size=2.5, box.padding = 1 )+
#   geom_text(size=6/ (14/5), color="black", show.legend = F, aes(x=xx, y=Inf,label=r ,vjust=1.5, hjust=0), check_overlap = T)

p <- p[, xx:=min((value-standard.errors),na.rm = T), by=.(variable.y,variable.x,dataset,Model)]
# p$Model <- gsub("Model0", "Main Model",p$Model)
# p$Period <- paste0(p$Period, ", " ,p$Model)
p[, Model_new := model_map[Model]]
p$Model_new <- gsub("Growth", "Gro",p$Model_new)

ggBeanCoef <-ggplot(
  data = p,
  aes(x = value, y = estimate)) +
  xlab(expression("Reference trait")) +
  ylab("Model coefficient") +
  
  theme_bw() +
  theme(
    panel.spacing = unit(-0.2, "lines"),
    plot.title = element_text(hjust = -0.2),
    strip.placement = "outside",
    strip.background = element_blank(),
    legend.key.size = unit(0.6, "lines"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    text = element_text(size = 8)
  ) +
  
  ## vertical error bars (estimate ± StdError)
  geom_errorbar(
    aes(ymin = estimate - StdError, ymax = estimate + StdError),
    color = "grey90",
    width = 0.000001
  ) +
  
  ## horizontal error bars (value ± standard.errors)
  geom_errorbarh(
    aes(xmin = value - standard.errors, xmax = value + standard.errors),
    color = "grey90",
    height = 0.000001
  ) +
  
  geom_point(
    aes(fill = estimate),
    size = 1,
    alpha = 0.75
  ) +
  
  scale_color_gradientn(colours = c("yellow3", "darkblue")) +
  scale_y_continuous(breaks = pretty_breaks(n = 3)) +
  
  guides(
    shape = guide_legend(override.aes = list(size = 2)),
    color = guide_legend(override.aes = list(size = 2))
  ) +
  
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    aes(group = 1),
    alpha = 0.5,
    show.legend = FALSE,
    color = "#661100"
  ) +
  
  facet_grid(
    Model_new + variable_fitted ~ variable_measured,
    scales = "free",
    switch = "both"
  ) +
  
  geom_text(
    aes(x = xx, y = Inf, label = r),
    size = 6 / (14 / 5),
    color = "black",
    vjust = 1.5,
    hjust = 0,
    show.legend = FALSE,
    check_overlap = TRUE
  )
ggBeanCoef

# ggsave("Coef_RefTraits.pdf", width = 170, height = 250, units = "mm", dpi = 300, ggBeanCoef)


#############

unique(p$variable_fitted)
p <- subset(p, variable_fitted%in%c("Asym","Scal.Gen:Phot")&!(variable_fitted%in%c("Scal.Gen:Phot")&Period=="Senescence"))
p <- subset(p, !variable_measured%in%c("Yield (t/ha)")&Model%in%c("Model0","Model10"))

ggplot(
  data = p,
  aes(x = value, y = estimate)) +
  xlab(expression("Reference trait")) +
  ylab("Model coefficient") +
  
  theme_bw() +
  theme(
    panel.spacing.x = unit(-0.2, "lines"),
    plot.title = element_text(hjust = -0.2),
    strip.placement = "outside",
    strip.background = element_blank(),
    legend.key.size = unit(0.6, "lines"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    text = element_text(size = 8)
  ) +
  
  ## vertical error bars (estimate ± StdError)
  geom_errorbar(
    aes(ymin = estimate - StdError, ymax = estimate + StdError),
    color = "grey90",
    width = 0.000001
  ) +
  
  ## horizontal error bars (value ± standard.errors)
  geom_errorbarh(
    aes(xmin = value - standard.errors, xmax = value + standard.errors),
    color = "grey90",
    height = 0.000001
  ) +
  
  geom_point(
    aes(fill = estimate),
    size = 1,
    alpha = 0.75
  ) +
  
  scale_color_gradientn(colours = c("yellow3", "darkblue")) +
  scale_y_continuous(breaks = pretty_breaks(n = 3)) +
  
  guides(
    shape = guide_legend(override.aes = list(size = 2)),
    color = guide_legend(override.aes = list(size = 2))
  ) +
  
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    aes(group = 1),
    alpha = 0.5,
    show.legend = FALSE,
    color = "#661100"
  ) +
  
  facet_grid(
    Period + variable_fitted ~ variable_measured,
    scales = "free",
    switch = "both"
  ) +
  
  geom_text(
    aes(x = xx, y = Inf, label = r),
    size = 6 / (14 / 5),
    color = "black",
    vjust = 1.5,
    hjust = 0,
    show.legend = FALSE,
    check_overlap = TRUE
  )



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




#########
Weather_data_Melt <- data.table::fread( "data/weather_data_for_modelling.csv")
names(Weather_data_Melt)
unique(Weather_data_Melt$WeatherVariable)
WeatherVariableSel <- c("Temperature","Radiation","Precipitation","VPD") #,"Windspeed"


Weather_data_Melt[, WeatherVariable2 := fcase(
  WeatherVariable == "Humidity",        "Humidity~(plain('%'))",
  WeatherVariable == "Precipitation",   "Precipitation~(mm)",
  WeatherVariable == "PPFR",            "PPFR~(mu*mol~m^{-2}*s^{-1})",
  WeatherVariable == "Radiation",       "Radiation~(W~m^{-2})",
  WeatherVariable == "Temperature",     "Temperature~('\u00B0'*C)",
  WeatherVariable == "VPD",             "VPD~(kPa)",
  default = WeatherVariable
)]

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
  facet_grid(WeatherVariable2~Location, scales = "free", space="free_x", switch="y", labeller = label_parsed)+
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

library(ggh4x)
ggDensity <- ggplot(p,aes(x=dailymean, fill=Year, color=Year))+ ylab("Value")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
  geom_density(alpha=0.5)+
  scale_color_manual(values = tol8qualitative)+
  scale_fill_manual(values = tol8qualitative)+
  facet_grid2(
    WeatherVariable2 ~ Location,
    scales = "free",
    independent = "x",
    switch = "y",
    labeller = label_parsed) 
ggDensity

p$month <-  as.factor(format(p$Date,"%m"))

p[WeatherVariable=="Precipitation",list(sum(dailymean,na.rm=T)),by=.(Year,Location,month)]
p[WeatherVariable=="Radiation",list(sum(dailymean,na.rm=T)),by=.(Year,Location,month)]

library(cowplot)
first_row <- plot_grid(ggFluc, ggDensity,  rel_widths =   c(1,0.5), ncol = 2, labels = c("AUTO"))  #,vjust=0.5+

o1 <- plot_grid(legendGeno, first_row,  rel_heights  =   c(0.1,2), ncol = 1, labels = c(""))  #,vjust=0.5+
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
  facet_wrap(paste(WeatherVariable)~., scales = "free", ncol=1, strip.position = "bottom")
ggDensityModel


require(cowplot)
second_row <- plot_grid(ggDensityModel, ggBeanCoef, ncol = 2, rel_widths = c(0.45,1), labels = c("B","C"))  #,vjust=0.5+
first_row <- plot_grid(ggGrowthCurves, second_row,  ncol = 1, rel_heights = c(1,0.45), labels = c("A",""))  #,vjust=0.5+


# ggsave("GrowthFitCoef_Soybean.png", width = 180, height = 210, units = "mm", dpi = 300, first_row)
# ggsave("GrowthFitCoef_Soybean.pdf", width = 180, height = 210, units = "mm", dpi = 100, first_row)

second_row <- plot_grid(ggDensityModel, ggBeanCoef, ncol = 2, rel_widths = c(0.45,1), labels = c("B","C"))  #,vjust=0.5+

########



##########

