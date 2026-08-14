# setwd("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/")
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

load("data/Growth_data.RData") 

# Packages
library(nlme)
library(ggplot2)
library(data.table)

df$platform <- as.factor(df$platform)
unique(df$year_site.UID)[order(unique(df$year_site.UID))]
uniqueN(df$year_site.UID)
uniqueN(df$genotype.id)

df$year_loc <- paste(df$year, df$Location, sep="_")

## ----------------------------
## setup
## ----------------------------
envs <- unique(df$year_loc)
df$row_id <- seq_len(nrow(df))
df_filtered <- as.data.frame(df)

## ----------------------------
## CV function
## ----------------------------
cv_results <- lapply(envs, function(env_train){
  
  cat("Processing:", env_train, "\n")
  
  train_df <- df_filtered[df_filtered$year_loc != env_train, ]
  test_df  <- df_filtered[df_filtered$year_loc == env_train, ]
  
  keep_geno <- unique(train_df$genotype.id)
  test_df   <- test_df[test_df$genotype.id %in% keep_geno, ]
  train_df  <- train_df[train_df$genotype.id %in% test_df$genotype.id, ]
  
  if(nrow(test_df) == 0){
    return(data.frame(train_env = env_train,
                      error = "empty test set after filtering"))
  }
  
  ## Align factor levels
  train_df$genotype.id <- droplevels(as.factor(train_df$genotype.id))
  test_df$genotype.id  <- factor(test_df$genotype.id, levels = levels(train_df$genotype.id))
  train_df$platform    <- droplevels(as.factor(train_df$platform))
  test_df$platform     <- factor(test_df$platform, levels = levels(train_df$platform))
  
  ## grouped data
  train_g <- groupedData(value ~ time_since_sowing | plot_grouped_global,
                         data = train_df)
  
  ## nlsList
  fm_train <- tryCatch(
    nlsList(
      value ~ SSlogis(time_since_sowing, Asym, xmid, scal),
      data = train_g,
      control = list(maxIter = 1500)
    ),
    error = function(e) e
  )
  if(inherits(fm_train, "error")){
    return(data.frame(train_env = env_train,
                      error = paste("nlsList failed:", fm_train$message)))
  }
  
  ## base model
  base_model <- tryCatch(
    nlme(fm_train,
         random  = Asym + xmid ~ 1,
         weights = varPower(),
         control = nlmeControl(maxIter = 200, msMaxIter = 200,
                               pnlsMaxIter = 20, tolerance = 1e-5,
                               pnlsTol = 1e-2, opt = "nlminb")),
    error = function(e) e
  )
  if(inherits(base_model, "error")){
    return(data.frame(train_env = env_train,
                      error = paste("nlme failed:", base_model$message)))
  }
  
  ## start values
  soyFix  <- fixef(base_model)
  n_geno  <- length(levels(train_df$genotype.id))
  
  start_vec <- c(
    soyFix[1], rep(0, n_geno),
    soyFix[2], rep(0, 3),
    soyFix[3], rep(0, 2 * n_geno + 1)
  )
  
  ## full model
  full_model <- tryCatch(
    update(base_model,
           data = train_df,
           fixed = list(
             Asym ~ genotype.id + platform,
             xmid ~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,
             scal ~ genotype.id:(avg_photothermal_14 + avg_precipitation_14) + platform
           ),
           start = start_vec,
           control = list(maxIter = 500, msMaxIter = 500)
    ),
    error = function(e) e
  )
  if(inherits(full_model, "error")){
    return(data.frame(train_env = env_train,
                      error = paste("update failed:", full_model$message)))
  }
  
  ## --- Predictions ---
  
  ## Base: population mean (level = 0 is correct)
  pred_base <- tryCatch(
    predict(base_model, newdata = test_df, level = 0),
    error = function(e) e
  )
  
  ## Full: genotype-specific fixed-effect predictions via manual model.matrix
  pred_full <- tryCatch({
    
    beta <- fixef(full_model)
    
    ## Build design matrices matching the fixed effects formulas
    X_Asym <- model.matrix(~ genotype.id + platform,                                              data = test_df)
    X_xmid <- model.matrix(~ avg_temperature_14 + avg_precipitation_14 + avg_radiation_14,        data = test_df)
    X_scal <- model.matrix(~ genotype.id:(avg_photothermal_14 + avg_precipitation_14) + platform,  data = test_df)
    
    ## Extract coefficient blocks (nlme stores them in parameter order: Asym, xmid, scal)
    nA <- ncol(X_Asym)
    nX <- ncol(X_xmid)
    nS <- ncol(X_scal)
    
    Asym_pred <- as.numeric(X_Asym %*% beta[1:nA])
    xmid_pred <- as.numeric(X_xmid %*% beta[(nA + 1):(nA + nX)])
    scal_pred <- as.numeric(X_scal %*% beta[(nA + nX + 1):(nA + nX + nS)])
    
    ## SSlogis: Asym / (1 + exp((xmid - x) / scal))
    Asym_pred / (1 + exp((xmid_pred - test_df$time_since_sowing) / scal_pred))
    
  }, error = function(e) e)
  
  if(inherits(pred_full, "error") || inherits(pred_base, "error")){
    return(data.frame(train_env = env_train,
                      error = "prediction failed"))
  }
  
  if(length(pred_full) != nrow(test_df) ||
     length(pred_base) != nrow(test_df)){
    return(data.frame(train_env = env_train,
                      error = "prediction length mismatch"))
  }
  
  ## return results
  data.frame(
    row_id      = test_df$row_id,
    train_env   = env_train,
    test_env    = test_df$year_site.UID,
    genotype.id = test_df$genotype.id,
    obs         = test_df$value,
    pred_full   = pred_full,
    pred_base   = pred_base,
    error       = NA
  )
})


## ----------------------------
## combine results
## ----------------------------
cv_results_df <- rbindlist(cv_results, fill = TRUE)
# cv_results_df_year <- cv_results_df

## check errors
table(cv_results_df$error, useNA = "ifany")

unique(cv_results_df$train_env)
unique(cv_results_df$test_env)

cv_results_df[, N := uniqueN(genotype.id), by = .(test_env, train_env)]
hist(cv_results_df$N)
cv_results_sub <- subset(cv_results_df)


## ----------------------------
## plot
## ----------------------------
d1 <- merge(cv_results_sub[is.na(error)], df_filtered[, c("row_id", "time_since_sowing", "year_site.UID", "platform","Location","year")],
            by = "row_id", all.x = TRUE)
d1$year_loc <- paste(d1$Location, d1$year, sep=", ")

## long format
plot_df <- melt.data.table(d1, measure.vars = c("pred_full", "pred_base"), variable.name = "model")
plot_df$value <- sin(plot_df$value)^2
plot_df$obs   <- sin(plot_df$obs)^2

plot_df$model <- gsub("pred_full", "Main model",plot_df$model)
plot_df$model <- gsub("pred_base", "Base model",plot_df$model)

## stats per facet: add year_loc
stats <- setDT(plot_df)[, .(r     = cor(obs, value, use = "complete.obs"),
                            r2    = 1 - sum((obs - value)^2,                na.rm = TRUE) /
                              sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE),
                            rmse  = sqrt(mean((obs - value)^2, na.rm = TRUE)),
                            rrmse = sqrt(mean((obs - value)^2, na.rm = TRUE)) / mean(obs, na.rm = TRUE),
                            mae   = mean(abs(obs - value),     na.rm = TRUE),
                            N_geno = uniqueN(genotype.id),
                            N = .N),
                        by = .(model, train_env,  year_loc)]


stats_melt <- melt.data.table(stats, measure.vars = c("r","r2","rmse","rrmse","mae"))
stats_melt[, list(mean= mean(value),SD=sd(value)), by = .(model,variable)]


## mean(r) and sd(r) per model x train_env x year_loc (across test environments)
stats_label <- stats[, .(mean_r = mean(r, na.rm = TRUE),
                         sd_r   = sd(r,   na.rm = TRUE)),
                     by = .(model, train_env, year_loc)]

# stats_label[, label := paste0("r = ", round(mean_r, 3),
#                               " (", round(sd_r, 3), ")")]
stats[, label := paste0(" r = ", round(r, 3), "\n rmse = ",round(rmse,3))]

stats_mean <- stats[, list(r_mean=mean(r)), by = .(model,year_loc)]
stats_mean
stats_mean[, mean(r_mean), by = model]

plot_df$train_env <- as.factor(plot_df$train_env)

labeller_fun <- function(x) {
  gsub("\\,", ",\n", x)
}



## plot
ggPred <- ggplot(plot_df, aes(x = obs, y = value,color = year_loc)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_grid(year_loc ~ model, switch = "y",
             labeller = labeller(year_loc = labeller_fun))+
geom_text(data = stats,
            aes(x = -Inf, y = Inf,
                label = label),
            inherit.aes = FALSE,
            hjust = -0.05, vjust = 1.1,
            size = 2) +
  ylab(expression("Canopy cover"[predicted]*" (%)")) +
  xlab(expression("Canopy cover"[observed]*" (%)")) +
  scale_color_manual(values=tol10qualitative)+
  theme_bw() +
  theme(
    panel.spacing.y  = unit(-0.2, "lines"),
    legend.key.height = unit(0.5, "lines"),
    legend.spacing.y   = unit(0.1, "lines"),
    strip.placement  = "outside",
    legend.position  = "top",
    legend.key.size  = unit(0.9, "lines"),
    strip.background = element_blank(),
    legend.title     = element_blank(),
    panel.grid.minor = element_blank(),
    text             = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0)
    ) +
  guides(color = guide_legend(nrow = 2))

ggPred

## ----------------------------
## plot: time series obs vs pred
## ----------------------------
ts_df <- melt.data.table(
  d1,
  measure.vars  = c("obs", "pred_full", "pred_base"),
  variable.name = "type",
  value.name    = "value"
)
ts_df$value <- sin(ts_df$value)^2

## relabel type levels
ts_df$type <- factor(ts_df$type,
                     levels = c("obs", "pred_full", "pred_base"),
                     labels = c("Observed", "Main model", "Base model"))

stats_r2 <- subset(stats, model=="Main model")
stats_r2[, label := paste0(" R² = ", round(r2, 2))]


ggFit <- ggplot(ts_df, aes(x = time_since_sowing, y = value,
                  color = type, shape = type, linetype = type,
                  group = paste(genotype.id, type))) +
  geom_point(size = 0.5, alpha = 0.6) +
  geom_line(data = subset(ts_df, type != "Observed"),
            size = 0.4, alpha = 0.8) +
  facet_wrap(. ~ year_loc, strip.position = "top", ncol = 5) +
  scale_color_manual(values = tol3qualitative) +
  scale_shape_manual(   values = c("Observed" = 1, "Main model" = 16, "Base model" = 17)) +
  scale_linetype_manual(values = c("Observed" = 0, "Main model" = 1,  "Base model" = 3)) +
  ylab(expression("Canopy cover"*" (%)")) +
  xlab("Days after sowing (d)") +
  theme_bw() +
  theme(
    panel.spacing.x  = unit(-0.2, "lines"),
    legend.key.height = unit(0.5, "lines"),
    strip.placement  = "outside",
    strip.background = element_blank(),
    legend.key.size  = unit(0.9, "lines"),
    legend.position  = "top",
    panel.border     = element_rect(colour = "black", fill = NA, size = 1),
    legend.title     = element_blank(),
    axis.text.x      = element_text(angle = 0, hjust = 0.5),
    text             = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0)
  ) +
  geom_text(data = stats_r2,
            aes(x = -Inf, y = Inf,
                label = label),
            inherit.aes = FALSE,
            hjust = -0.05, vjust = 1.5,
            size = 2.5) +
  guides(color = guide_legend(nrow = 1, override.aes = list(size = 1.5)))
ggFit

library(cowplot)
first_row <- plot_grid(ggFit, ggPred, rel_heights = c(1,1.33),  nrow = 2, labels = c("AUTO"))  #,vjust=0.5+

# ggsave("cross-validation.png",  width = 170, height = 260, units = "mm", first_row, bg="white")
# ggsave("cross-validation.pdf",  width = 170, height = 260, units = "mm", first_row, bg="white")
