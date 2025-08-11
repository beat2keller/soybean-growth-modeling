require(cowplot)
require(data.table)
require(ggplot2)
require(imager)
# require(magick)

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

NasaMonoRed <- c("#99231b", "#c62d1f", "#e59892", "#f9e0de")
NasaGold <- c("#ff9d1e","#f9aa43", "#ffc375", "#ffebd1")
NasaGreen <- c("#2e8540","#4aa564","#94bfa2","#e7f4e4")
NasaBlue <- c("#205493","#4773aa","#8ba6ca","#dce4ef")


remove_outliers <- function(x,IQR_times, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25,0.5, .75), na.rm = na.rm, ...)
  H <- IQR_times #* IQR(x, na.rm = na.rm)
  y <- x
  NAbefore <- length(y[is.na(y)])
  y[x < (qnt[2] - H* (qnt[3]-qnt[1]) )] <- NA
  y[x > (qnt[2] +  H* (qnt[3]-qnt[1]) )] <- NA
  y <- as.numeric(y)
  NAafter <- length(y[is.na(y)])
  # print(paste("removed",NAafter-NAbefore))
  
  return(y)
}

estimate_row_distance_pixel <-  function(working_dir,image_number) {
  
  # working_dir <- "./SB007/RGB1/Segmentation/2017_06_13_17_11_segmented"
  image_files <- list.files(path = working_dir,pattern="_segmentation.png",recursive=F,full.names = T)
  # load the necessary library
  # read the images
  images <- lapply(image_files[image_number], function(file) {
    load.image(file)
  })
  
  # set up a 2x2 grid for plotting
  # par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)
  
  # plot each image
  for (i in 1:length(images)) {
    # plot the image
    plot(images[[i]])
  }
  
}

########

get_middle_rows_pixel <-  function(folder_ii, makeTrainingset, cut=0, Row_distance_min,LeaveSpecies, max_number_of_rows="Default",min_cluster=1,cut_below=0,cut_above=0) {
  
require(cowplot)
require(data.table)
require(ggplot2)
require(imager)
require(grid)
  
  # for (folder_ii in 1:length(folders)) {
print(paste("I'll see what I can find.."))  
  # working_dir <- "./SB016/RGB1/Segmentation/2022_06_07_08_51_Lot3_segmented"
  
working_dir <-   folder_ii#folders_train[folder_ii]
print(paste("Working on", working_dir))  
# fnames <- fnamesAll[grepl(gsub("\\./","",working_dir),fnamesAll)]
fnames<- list.files(path = working_dir,pattern="_mask.png",recursive=F,full.names = T)
fnames <- fnames[!grepl("_q90_mask.png",fnames)]
mask <- T
if (length(fnames)==0) {
fnames<- list.files(path = working_dir,pattern="_segmentation.png",recursive=F,full.names = T)
mask <- F
}
if (makeTrainingset==T) {
fnames<- list.files(path = working_dir,pattern="_segmentation.png",recursive=F,full.names = T)
mask <- F
}

if (length(fnames)==0) {
  print(paste("Found no pictures.. go to next folder"))
}else{

print(paste("Found", length(fnames), "pictures"))  
print(paste("Starting processing.. please wait"))  

# fnames<- fnames[1:10]
# fnames


# ii <-grepl("FPSB0140051",fnames) #76
data_rows_middle_all <- NULL

for (ii in 1:length(fnames)) {
  gc()
  set.seed(123)
  
  file <- fnames[ii]
  filename <- gsub("_segmentation","",file)
  filename <- gsub("_q90_mask","",filename)
  filename <- gsub("_mask","",filename)
  filename_only <- strsplit(filename, "segmented\\/")[[1]][length(strsplit(filename, "segmented\\/")[[1]])]
  
  filename_only
  
  if(is.character(LeaveSpecies)==F){
    LeaveSpecies2 <- LeaveSpecies$Crop[gsub("_q90","",LeaveSpecies$Filename_png)==filename_only][1]
  # LeaveSpecies
  if (is.na(LeaveSpecies2)==T) {next} # find file, i.e., plot, with specified leave species
  LeaveSpecies <- LeaveSpecies2
  # print(LeaveSpecies)
  }
  if (makeTrainingset==T){ print(paste("Your middle row will be labelled with",LeaveSpecies))}
 
  if (cut !=0){
    cut_above <- cut
    cut_below <- cut
  }
  load_cut_image <- function(file){
    
  require(imager)
  image <- load.image(file)
  df_image_res90 <- as.data.frame(image)
  if(max(df_image_res90$y)<2*cut){paste("Adjust cut value")}
  

  
  if (is.null(df_image_res90$cc)) {df_image_res90$cc <- 4   }

  if(max(df_image_res90$cc)==3){
    df_image_res90 <- subset(df_image_res90, cc==2&y<max(df_image_res90$y)-cut_above&y>cut_below)}else{
    df_image_res90 <- subset(df_image_res90, cc==max(df_image_res90$cc)&y<max(df_image_res90$y)-cut&y>cut)
    }
  return(df_image_res90)
  }
  
 
  
  df_image_res90 <- load_cut_image(file)
  if(mask == T){
  df_image_Mask <-   df_image_res90
  df_image_res90 <- load_cut_image(gsub("_q90_mask.png","_segmentation.png",file))
  df_image_res90$value[round(df_image_Mask$value,digits=2)==0.78] <- 0 ## weed;  overwrite segmented pixel values where mask predicted weed
  df_image_Mask <- NULL
  }
  
  df_image_res90$value[df_image_res90$value==1] <- 0.5555
  df_image_res90$value[round(df_image_res90$value,digits=2)==0.39] <- 0.5555 ## soybean
  
  middle_y <- ceiling(0.5*max(df_image_res90$y))
  middle_x <- ceiling(0.5*max(df_image_res90$x))
  
  # require(imager)
  # image_pixel <- as.cimg(df_image_res90[,c("x","y","value")])  
  # plot(image_pixel)

  df_image_res90$Filename <- file

  df_image <- setDT(df_image_res90)
  

  df_image[,value_rollmean_x := frollmean(value,n=10), by=x]
  df_image <- df_image[order(df_image$Filename, df_image$x),]
  df_image[,value_rollmean_y := frollmean(value,n=10), by=y]
  df_image$value[df_image$value_rollmean_y<0.2&df_image$value_rollmean_x<0.2] <- NA
  df_image$value_rollmean_x <- NULL
 
  med <- median(df_image$value_rollmean_y[df_image$value_rollmean_y!=0],na.rm = T)
  lower_quant <- quantile(df_image$value_rollmean_y[df_image$value_rollmean_y!=0],na.rm = T,0.25)
  lower_IQR <- med-1.5*(med-as.numeric(lower_quant))
  
  df_image$value[df_image$value_rollmean_y<lower_IQR] <- 0
  df_image$value_rollmean_y <- NULL
  df_image$value[df_image$value==0] <- NA
  

  df_image_denoised <- na.omit(df_image)
  
  # ggplot(df_image_denoised[sample(1:nrow(df_image_denoised),10000),],aes(y=y,x=x,color=as.factor(value)))+
  #   theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.text=element_blank(),strip.background = element_blank(),legend.key.size = unit(0.6, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=11),axis.title = element_text(size = 11))+
  #   geom_point(size=0.2)+
  #   # geom_line()+
  #   scale_color_manual(values = rev(tol11qualitative))+
  #   facet_grid(.~Filename)


  
  correct_get_slope <- function(df_image_denoised, retries = 4, delay = 1) {
    for(i in 1:retries) {
      tryCatch({
  # df_image_denoised <- subset(data_rows_all, Row_cluster_corr=="Row 1")
  df_image_denoised$y_r <-  round(df_image_denoised$y/Row_distance_min, digits = 0 )*Row_distance_min*0.5
  df_image_denoised$x_r <-  round(df_image_denoised$x/Row_distance_min, digits = 0 )*Row_distance_min*0.5
  df_image_denoised[,N_perGroup:=nrow(.SD),by=.(x_r,y_r)]
  # length(unique(df_image_denoised$N_perGroup))
  p_stratified <- subset(df_image_denoised, N_perGroup>50)
  p_stratified <- p_stratified[,.SD[sample(1:nrow(.SD),50)],by=.(x_r,y_r)]
  if(nrow(p_stratified)<1000){  p_stratified <- df_image_denoised}
  if(nrow(p_stratified)>10000){  p_stratified <- df_image_denoised[sample(1:nrow(df_image_denoised),2500)] }
  
  # ggplot(p_stratified,aes(y=y,x=x,color=as.factor(value)))+
  #   theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.text=element_blank(),strip.background = element_blank(),legend.key.size = unit(0.6, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=11),axis.title = element_text(size = 11))+
  #   geom_point()+
  #   # geom_line()+
  #   scale_color_manual(values = rev(tol11qualitative))


  library(dbscan)
  
  if(max_number_of_rows=="Default"){
    max_number_of_rows <- (dim(image)[1]-(cut_below+cut_above))/Row_distance_min
    max_number_of_rows <- round(max_number_of_rows, digits = 0)}
  if(max_number_of_rows>6){max_number_of_rows <- 6}
  
  
  
  p <- p_stratified[,c("y","x")]
  p$x <- p$x/100
  cl <- hdbscan(p, minPts =200)
  
 
  if(length(unique(cl$cluster))>max_number_of_rows){
    cl <- hdbscan(p, minPts =ceiling(nrow(p)/(max_number_of_rows+2)))
    }
  if(length(unique(cl$cluster))<min_cluster){
    cl <- hdbscan(p, minPts =1000)
  }
  
  p_stratified$cluster <- cl$cluster
  # p_stratified <- subset(p_stratified, cluster!=0)
  # print(ggplot(p_stratified, aes(x=x, y=y,color=as.factor(cluster)))+geom_point())
  p_stratified[,N_group:=nrow(.SD),by=.(cluster)]
  p_stratified[,row_dist:=max(y)-min(y),by=cluster]
  p_stratified[,row_length:=max(x)-min(x),by=cluster]
  p_stratified <- subset(p_stratified, row_length>2*Row_distance_min)  
  
  if(nrow(subset(p_stratified, row_dist<Row_distance_min*3))!=0){
    p_stratified <- subset(p_stratified, row_dist<Row_distance_min*3)  
  }
  if(nrow(subset(p_stratified, row_dist<Row_distance_min*2))!=0){
    p_stratified <- subset(p_stratified, row_dist<Row_distance_min*2)}
  
  if(nrow(subset(p_stratified, row_dist<Row_distance_min*1.5))==0){
    p <- p_stratified[,c("y","x")]
    p$x <- p$x/100
    cl <- hdbscan(p, minPts =100)
    p_stratified$cluster <- cl$cluster}
  # p_stratified[,N_group:=nrow(.SD),by=.(cluster)]
  p_stratified[,row_dist:=max(y)-min(y),by=cluster]
  
  if(nrow(subset(p_stratified, row_dist<Row_distance_min*2))!=0){
    p_stratified <- subset(p_stratified, row_dist<Row_distance_min*2)}
  
  p_stratified[,N_group:=nrow(.SD),by=.(cluster)]
  p_stratified <- subset(p_stratified, N_group>50)
  
  require(MASS)
  
  p_lm1 <- p_stratified[,rlm(y ~ 1+x, data = .SD)$coef[2],by=cluster]
  # plot(lm1)
  
  slope1<- mean(p_lm1$V1)
  slope1 

  
  return(slope1)
      }, error = function(e) {
        message(paste("Attempt", i, "failed due to error:", e$message))
        if(i < retries) {
          Sys.sleep(delay) # Wait for a bit before retrying
        } else {
          stop(paste("All attempts failed after", retries, "tries"))
        }
      })
    }
  }
  
  slope1 <- correct_get_slope(df_image_denoised)
  slope1

  df_image_denoised_linearCorr <- df_image_denoised
  df_image_denoised_linearCorr$y_corr <-  df_image_denoised_linearCorr$y-(df_image_denoised_linearCorr$x-median(df_image_denoised_linearCorr$x))*slope1



  # ggLinearCorr <- ggplot(df_image_denoised_linearCorr[sample(1:nrow(df_image_denoised_linearCorr),10000),],aes(y=y_corr,x=x,color=as.factor(value)))+
  #   theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.text=element_blank(),strip.background = element_blank(),legend.key.size = unit(0.6, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor.x = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=11),axis.title = element_text(size = 11))+
  #   geom_point(data=df_image_denoised[sample(1:nrow(df_image_denoised),10000),], aes(y=y,x=x), color="grey")+
  #   geom_point()+
  #   # geom_line()+
  #   scale_color_manual(values = rev(tol11qualitative))
  # # facet_grid(.~Filename)
  # ggLinearCorr
  
  
  y_data_rows_sub <-  df_image_denoised_linearCorr#[sample(1:nrow(df_image_denoised_linearCorr),50000),]
  y_data_rows_sub$y_new <-  round(y_data_rows_sub$y_corr/5, digits = 0 )*5
  y_data_rows_sub[, N:=nrow(.SD),by=.(y_new,Filename)]
  
  # if (Row_distance_min<=400) {
  #   # # remove_border <- quantile(y_data_rows_sub$N,0.01)
  #   # y_data_rows_sub <- y_data_rows_sub[y_data_rows_sub$N>remove_border,]
  #   }
  #   
  if (Row_distance_min>400) {
    remove_border <- quantile(y_data_rows_sub$N,0.1)
    y_data_rows_sub <- y_data_rows_sub[y_data_rows_sub$N>remove_border,] }
  
  # if(Row_distance_min>1000){  remove_border <- quantile(y_data_rows_sub$N,0.1)
  # y_data_rows_sub <- y_data_rows_sub[y_data_rows_sub$N>remove_border,]} 
  # ggplot(y_data_rows_sub,aes(x, y))+geom_point()
  
  
  y_data_rows_sub$y_new <-  round(y_data_rows_sub$y_corr/10, digits = 0 )*10
  y_data_rows_sub$x_new <-  round(y_data_rows_sub$x/1000, digits = 0 )*1000
  
  # y_data_rows_N <- setDT(y_data_rows_sub)[, list(N=nrow(.SD)),by=.(y_new,Filename)]
  y_data_rows_sub[, N:=nrow(.SD),by=.(y_new,x_new, Filename)]
  if(Row_distance_min<=400){
  y_data_rows_N <-y_data_rows_sub
  }else{
  y_data_rows_N <- subset(y_data_rows_sub, N>1000)}
  # ggplot(y_data_rows_N,aes(x, y))+geom_point()
  
  if(nrow(y_data_rows_N)==0){y_data_rows_N <- y_data_rows_sub}
  y_data_rows_N[,ID_group:=1:nrow(.SD),by=.(y_new,x_new,Filename)]
  # ggplot(y_data_rows_N,aes(x, y))+geom_point()
  if(nrow(y_data_rows_N)<50000){
  y_data_rows_N <- y_data_rows_N[,.SD[sample(1:nrow(.SD),ceiling(nrow(.SD)/2))],by=.(y_new,x_new,Filename)]
  }else{
    if(nrow(y_data_rows_N)<250000){
    y_data_rows_N <- y_data_rows_N[,.SD[sample(1:nrow(.SD),ceiling(nrow(.SD)/10))],by=.(y_new,x_new,Filename)]
    }else{
    y_data_rows_N <- subset(y_data_rows_N, ID_group<50000)
    y_data_rows_N <- y_data_rows_N[,.SD[sample(1:nrow(.SD),ceiling(nrow(.SD)/500))],by=.(y_new,x_new,Filename)]
    }
  }
  y_data_rows_N$x_dummy <- sample(1:1000, nrow(y_data_rows_N),replace = T)/10000
    

  library(dbscan)
 
   p_cluster <- y_data_rows_N[,c("y")]
  # ggplot(p_cluster,aes(x_dummy, y))+geom_point()
  
  #
  
  # Initialize variables
  No_clusters <- min_cluster - 1
  No_clusters_increase <- 1
  
  min_points_old <- 0
  # p$x <- p$x/1000
  while ((No_clusters < min_cluster) ) {    # Clustering with adjusted minPts
    min_points <- ceiling(nrow(p_cluster)*0.5 / (max_number_of_rows + No_clusters_increase))
    if(   min_points_old == min_points){min_points <- min_points-1}
    p_cluster <- y_data_rows_N[,c("x_dummy","y")]
    if(min_points<=2){ 
    cl <- hdbscan(p_cluster, minPts =2 )
    No_clusters <-  min_cluster
    print("Not enough data points")
    }else{
    cl <- hdbscan(p_cluster, minPts =min_points )
    
    # Calculate row distances within each cluster
    p_cluster$Row_cluster <- paste("Row",cl$cluster)
    p_cluster[,row_dist:=max(y)-min(y),by=Row_cluster]
    # Filter rows based on distance criteria
    p_cluster_sub <- subset(p_cluster, row_dist < Row_distance_min * 2&Row_cluster!=0)
     
    
    # Count unique clusters after filtering
    No_clusters <- length(unique(p_cluster_sub$Row_cluster))
    # Update No_clusters_increase
    No_clusters_increase <- No_clusters_increase + 2
    
    # print(No_clusters)
    if(nrow(p_cluster)<10){
    p_cluster <- y_data_rows_N[,c("y")]
    cl <- hdbscan(p_cluster, minPts =2 )
    print("Not enough data points")
    No_clusters <-  min_cluster}
    }
    
    min_points_old <- min_points
    if(No_clusters_increase>20){No_clusters <-  min_cluster
    # p_cluster <- y_data_rows_N[,c("x_dummy","y")]
    # cl <- hdbscan(p_cluster, minPts =20 )
    print("Not enough data points")
    }
    
  }
  
  No_clusters <- length(unique(cl$cluster))
  No_clusters_decrease <- 1
  
  # min_points_old <- 0
  # p$x <- p$x/1000
  while ((No_clusters > max_number_of_rows) ) {    # Clustering with adjusted minPts
    min_points <- ceiling(min_points_old*1.1)
    # if(   min_points_old == min_points){min_points <- min_points+1}
    if(   is.infinite(min_points) == T){min_points <- min_points_old+10}
    p_cluster <- y_data_rows_N[,c("y")]
    cl <- hdbscan(p_cluster, minPts =min_points )
    No_clusters <- length(unique(cl$cluster))
    # print(No_clusters)
    No_clusters_decrease <- No_clusters_decrease + 2
    min_points_old <- min_points
    if(No_clusters_increase>20){No_clusters <-  max_number_of_rows
    # p_cluster <- y_data_rows_N[,c("x_dummy","y")]
    # cl <- hdbscan(p_cluster, minPts =20 )
    print("Too many data points")
    }
  }
  
  
  print(paste("Final number of clusters:",   length(unique(cl$cluster))))
  
  gc()
  Cluster_row_N <- y_data_rows_N
  Cluster_row_N$Row_cluster <- paste("Row",cl$cluster)
  # Cluster_row_N <- subset(Cluster_row_N, Row_cluster!="Row 0" )
  # Cluster_row_N[,row_dist:=max(y)-min(y),by=Row_cluster]
  if(nrow(subset(Cluster_row_N, Row_cluster!="Row 0"))!=0)
  {Cluster_row_N <- subset(Cluster_row_N, Row_cluster!="Row 0")}
  # 
    
  y_data_rowmiddle <- Cluster_row_N[,list(Row_middle=median(y)),by=.(Filename,Row_cluster)]

  # ggRow_cluster <- ggplot(Cluster_row_N,aes(y=y,x=x,color=Row_cluster))+xlab("Plot width [Pixels]")+
  #   theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.text=element_blank(),strip.background = element_blank(),legend.key.size = unit(0.6, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=11),axis.title = element_text(size = 11))+
  #   geom_point()+
  #   # geom_line()+
  #   scale_color_manual(values = rev(tol7qualitative))+
  #   geom_hline(data=y_data_rowmiddle, aes(yintercept=y_data_rowmiddle$Row_middle), linetype=2)+
  #   facet_grid(.~Filename)
  # ggRow_cluster
  
  y_data_rowmiddle <- y_data_rowmiddle[order(y_data_rowmiddle$Row_middle),]
  y_data_rowmiddle$Row_distance <- abs(y_data_rowmiddle$Row_middle-shift(y_data_rowmiddle$Row_middle))
  y_data_rowmiddle$Row_distance_lead <- abs(y_data_rowmiddle$Row_middle-shift(y_data_rowmiddle$Row_middle,type="lead"))
  y_data_rowmiddle$Row_cluster_lead <- shift(y_data_rowmiddle$Row_cluster,type="lead")
  
  double_row <- subset(y_data_rowmiddle, !(Row_distance>Row_distance_min&Row_distance_lead>Row_distance_min))
  double_row <- double_row[order(double_row$Row_distance),]
  double_row$Row_cluster_corr <- double_row$Row_cluster
  
  Cluster_row_N$Row_cluster_corr <- Cluster_row_N$Row_cluster
 
  while(nrow(double_row)>2){
    double_row <- double_row[order(double_row$Row_distance_lead),]
    Cluster_row_N$Row_cluster_corr <- gsub(double_row$Row_cluster[2], double_row$Row_cluster[1], Cluster_row_N$Row_cluster_corr)
    
    y_data_rowmiddle <- Cluster_row_N[,list(Row_middle=median(y_corr)),by=.(Filename,Row_cluster_corr)]
    y_data_rowmiddle <- y_data_rowmiddle[order(y_data_rowmiddle$Row_middle),]
    y_data_rowmiddle$Row_distance <- abs(y_data_rowmiddle$Row_middle-shift(y_data_rowmiddle$Row_middle))
    y_data_rowmiddle$Row_distance_lead <- abs(y_data_rowmiddle$Row_middle-shift(y_data_rowmiddle$Row_middle,type="lead"))
    
    double_row <- subset(y_data_rowmiddle, !(Row_distance>Row_distance_min&Row_distance_lead>Row_distance_min))
    double_row <- double_row[order(double_row$Row_distance_lead),]
    
  }
  if(nrow(double_row)==2){
    Cluster_row_N$Row_cluster_corr <- gsub(double_row$Row_cluster_corr[2], double_row$Row_cluster_corr[1], Cluster_row_N$Row_cluster_corr)
  }

  
## define row space
  y_data_rows_space <- (Cluster_row_N)
  # y_data_rows_space[,upper_quantile:=quantile(y_corr,0.75),by=.(Filename,Row_cluster_corr)]
  # y_data_rows_space[,lower_quantile:=quantile(y_corr,0.25),by=.(Filename,Row_cluster_corr)]
  y_data_rows_space[,median:=median(y_corr),by=.(Filename,Row_cluster_corr)]
  y_data_rows_space <- y_data_rows_space[order(y_data_rows_space$median),]
  row_distances <- unique(y_data_rows_space$median)-shift(unique(y_data_rows_space$median),1)
  # y_data_rows_space[,IQR:=upper_quantile-lower_quantile,by=.(Filename,Row_cluster_corr)]
  # y_data_rows_space$IQR[y_data_rows_space$IQR>Row_distance_min*0.5] <- Row_distance_min*0.5
  # 
  # if(length(fnames[grepl("_mask",fnames)])!=0){
  # y_data_rows_space[,upper_row_border:=median+IQR*5]
  # y_data_rows_space[,lower_row_border:=median-IQR*5]
  # # }else if(length(fnames[grepl("_mask",fnames)])==0&makeTrainingset==F){
  # #   y_data_rows_space[,upper_row_border:=upper_quantile+(upper_quantile-median)*3]
  # #   y_data_rows_space[,lower_row_border:=lower_quantile-(median-lower_quantile)*3]
  # }else{
  #   y_data_rows_space[,upper_row_border:=median+IQR*1.5]
  #   y_data_rows_space[,lower_row_border:=median-IQR*1.5]
  # }
  max_row_dist <- median(row_distances,na.rm = T)
  if(is.na(max_row_dist)==T){max_row_dist <- 2*middle_y}#fix me
  

    if(0.75*Row_distance_min>max_row_dist*0.5){
    y_data_rows_space[,upper_row_border:=median+max_row_dist*0.5]
    y_data_rows_space[,lower_row_border:=median-max_row_dist*0.5]
    }else{
      if (min_cluster==1) {
        y_data_rows_space[,upper_row_border:=median+1*Row_distance_min]
        y_data_rows_space[,lower_row_border:=median-1*Row_distance_min]
      }else{
      y_data_rows_space[,upper_row_border:=median+0.75*Row_distance_min]
      y_data_rows_space[,lower_row_border:=median-0.75*Row_distance_min]
      }
  }
  y_data_rows_space <- y_data_rows_space[y_corr<upper_row_border]
  y_data_rows_space <- y_data_rows_space[y_corr>lower_row_border]
 
  
  y_data_rows_N_cleaned <- y_data_rows_space#setDT(y_data_rows_space)[, list(N=nrow(.SD)),by=.(y_corr,Filename,Row_cluster)]
  y_data_rowmiddle_cleaned <- y_data_rows_space[,list(Row_middle=median(y_corr), Lower_row_border=lower_row_border[1], Upper_row_border=upper_row_border[1]),by=.(Filename,Row_cluster_corr)]
  
  # ggRow_cluster <- ggplot(y_data_rows_N_cleaned,aes(x=N,y=y_corr,color=Row_cluster_corr))+ylab("Plot width (Pixels)")+xlab("Number of green pixels")+
  #   theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.text=element_blank(),strip.background = element_blank(),legend.key.size = unit(0.6, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=11),axis.title = element_text(size = 11))+
  #   geom_point(data=y_data_rows_N, color="grey")+
  #   geom_point()+
  #   scale_color_manual(values = rev(tol6qualitative)[c(2,1,3,4:6)])+
  #   geom_hline(data=y_data_rowmiddle_cleaned, aes(yintercept=y_data_rowmiddle_cleaned$Row_middle), linetype=2)+
  #   scale_y_reverse ()+
  #   ggtitle(" \n")+
  #   facet_grid(.~Filename)
  # ggRow_cluster
  
  # ggsave("plot_row_clusters.png",  width = 60, height = 140, units = "mm", dpi = 100, bg="white",ggRow_cluster)
  
  get_row <- function(xyData,upper,lower){
    xyDataSub <- xyData[y_corr>lower,]
    xyDataSub <- xyDataSub[y_corr<upper,]
    xyDataSub$Filename <- NULL
    
    return(xyDataSub)
    }
  
  df_image_corr <- subset(df_image_res90,value!=0) # denoised is not good enough
  df_image_corr$y_corr <-  df_image_corr$y-(df_image_corr$x-middle_x)*slope1
  df_image_corr <- setDT(df_image_corr)
  
  data_rows_all <- y_data_rowmiddle_cleaned[,get_row(df_image_corr, upper=Upper_row_border, lower= Lower_row_border),by=.(Filename,Row_cluster_corr)]
  
  # p <-  data_rows_all[sample(1:nrow(data_rows_all),25000),]
  # ggplot(  p,aes(y=y_corr,x=x,color=as.factor(Row_cluster_corr)))+
  #   theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.text=element_blank(),strip.background = element_blank(),legend.key.size = unit(0.6, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor.x = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=11),axis.title = element_text(size = 11))+
  #   # geom_point(data=df_image_denoised, aes(y=y,x=x), color="grey")+
  #   geom_point()+
  #   # geom_line()+
  #   scale_color_manual(values = rev(tol5qualitative))

  data_rows_all$Row_section <- "First_section"
  # data_rows_all$Row_section[data_rows_all$x >quantile(data_rows_all$x,0.5 )] <- "Second_section"
  # data_rows_all$Row_section[data_rows_all$x >quantile(data_rows_all$x,0.66 )] <- "Third_section"
  # 

  
  correct_row_slope <- function(df_image_denoised){
    # df_image_denoised <- subset(data_rows_all, Row_cluster_corr=="Row 1")
    df_image_denoised$y_adj <- df_image_denoised$y-mean(df_image_denoised$y)
    # 

    require(MASS)
    
    lm2 <- rlm(y ~ 1+x, data = df_image_denoised)

    # Create a scatterplot
    # plot(df_image_denoised$x, df_image_denoised$y, main="Scatterplot with Linear Regression Line", 
         # xlab="X", ylab="Y")
    
    # Add the regression line
    # abline(lm2, col = "red")
    
    slope2<- coef(lm2)[2]
    slope2 
    
    
    return(slope2)
  }
  
  
  
  
  
  row_slope2 <-data_rows_all[,list(Slope_row=correct_row_slope(.SD)), by=.(Filename,Row_cluster_corr,Row_section)]
  row_slope2

  
  data_rows <- merge(data_rows_all, row_slope2, by=c("Filename","Row_cluster_corr","Row_section"))

  data_rows$y_corr2 <-  data_rows$y-(data_rows$x-middle_x)*data_rows$Slope_row
  # 
  # 
  # p <-  data_rows[sample(1:nrow(data_rows),100000),]
  data_rows_space <- (data_rows)
  
  ## needs zeros in xy data
  # data_rows_space$y_corr2_rollmean <- frollmean(data_rows_space$y_corr2,n=25)
  # hist(  data_rows_space$y_corr2_rollmean)
  # data_rows_space$y_corr2[data_rows_space$y_corr2_rollmean<0.4] <- NA
  # data_rows_space <- data_rows_space[order(data_rows_space$Filename, data_rows_space$x),]
  # data_rows_space$y_corr2_rollmean <- frollmean(data_rows_space$y_corr2,n=50)
  # data_rows_space$y_corr2[data_rows_space$y_corr2_rollmean<0.4] <- NA
  # 
  # data_rows_space <- na.omit(data_rows_space)
  # 
  data_rows_space[,upper_quantile:=quantile(y_corr2,0.75),by=.(Filename,Row_cluster_corr)]
  data_rows_space[,lower_quantile:=quantile(y_corr2,0.25),by=.(Filename,Row_cluster_corr)]
  data_rows_space[,median:=median(y_corr2),by=.(Filename,Row_cluster_corr)]
  
  # ggplot(data_rows_space, aes(x=Row_cluster_corr,y=y_corr2))+
  #   geom_boxplot()
  data_rows_space[,IQR:=upper_quantile-lower_quantile,by=.(Filename,Row_cluster_corr)]
  unique(data_rows_space$IQR)
  data_rows_space$IQR[data_rows_space$IQR>Row_distance_min*0.5] <- Row_distance_min*0.5
  
  data_rows_space[,upper_row_border:=median+IQR*2.5]
  data_rows_space[,lower_row_border:=median-IQR*2.5]

  # ggplot(between_rows, aes(x=x,y=y_corr2))+
    # geom_point()

  
  data_rows_space_cleaned <- data_rows_space[y_corr2<upper_row_border]
  data_rows_space_cleaned <- data_rows_space_cleaned[y_corr2>lower_row_border]
  
  
  
  y_data_rowmiddle_cleaned2 <- data_rows_space_cleaned[,list(Row_middle=median(y_corr2), Plot_begin=quantile(y_corr2,0.025),Plot_end=quantile(y_corr2,0.975)),by=.(Filename,Slope_row,Row_cluster_corr)]
  y_data_rowmiddle_cleaned2$Middle_y <- middle_y
  y_data_rowmiddle_cleaned2$Distance_from_Middle <-   abs(y_data_rowmiddle_cleaned2$Middle_y-  y_data_rowmiddle_cleaned2$Row_middle)
  middle_row <- y_data_rowmiddle_cleaned2$Row_cluster_corr[y_data_rowmiddle_cleaned2$Distance_from_Middle==min(y_data_rowmiddle_cleaned2$Distance_from_Middle)]
  y_data_rowmiddle_cleaned2 <- y_data_rowmiddle_cleaned2[order(y_data_rowmiddle_cleaned2$Distance_from_Middle),]
  if (nrow(y_data_rowmiddle_cleaned2)>min_cluster) {
    
    print("Further distant rows will be dropped because more rows were identified than specified in min_cluster")
  y_data_rowmiddle_cleaned2 <- y_data_rowmiddle_cleaned2[1:min_cluster,]
  }
  
  
  data_rows_space_cleaned <- subset(data_rows_space_cleaned, Row_cluster_corr%in%y_data_rowmiddle_cleaned2$Row_cluster_corr)
  data_rows_space_cleaned$Row <- data_rows_space_cleaned$Row_cluster_corr
  data_rows_space_cleaned$Row[data_rows_space_cleaned$Row_cluster_corr==middle_row] <- "Middle"
  

  if (nrow(data_rows_space_cleaned)>50000) {
    
 
  ggRows_Middle <- ggplot(data_rows_space_cleaned[sample(1:nrow(data_rows_space_cleaned),50000),],aes(y=y_corr2,x=x,color=Row))+ylab("Width (Pixels)")+xlab("Length (Pixels)")+
    theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.text=element_blank(),strip.background = element_blank(),legend.box.spacing = unit(0, "pt"),legend.key.size = unit(0.6, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor.x = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=11),axis.title = element_text(size = 11))+
    geom_point(data=df_image_corr[sample(1:nrow(df_image_corr),50000),], aes(y=y,x=x), color="grey",size=0.00000000001)+
    geom_point(size=0.0000000001)+
    # geom_line()+
    scale_color_manual(values = rev(tol6qualitative))+
    guides(color = guide_legend(override.aes = list(size=2))) + 
    scale_y_reverse ()
  # facet_grid(.~Filename)
  }else{
    ggRows_Middle <- ggplot(data_rows_space_cleaned,aes(y=y_corr2,x=x,color=Row))+ylab("Width (Pixels)")+xlab("Length (Pixels)")+
      theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.text=element_blank(),strip.background = element_blank(),legend.box.spacing = unit(0, "pt"),legend.key.size = unit(0.6, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor.x = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=11),axis.title = element_text(size = 11))+
      geom_point(data=df_image_corr, aes(y=y,x=x), color="grey",size=0.00000000001)+
      geom_point(size=0.0000000001)+
      # geom_line()+
      scale_color_manual(values = rev(tol6qualitative))+
      guides(color = guide_legend(override.aes = list(size=2))) + 
      scale_y_reverse ()
    # facet_grid(.~Filename)
  }
  
   # combined_plot <-plot_grid(ggRows_Middle,  ggRow_cluster,  ncol =  2, rel_widths = c(1,0.35),  labels = c("AUTO"))
   # ggsave("plot_row_clusters.png",  width = 280, height = 120, units = "mm", dpi = 100, bg="white",combined_plot)
   
###########
  
  data_rows_space_cleaned_middle <- subset(data_rows_space_cleaned, Row=="Middle")

  p_Pixel1 <- data_rows_space_cleaned_middle[,list(SumPixel=nrow(.SD)),by=.(x)]
  p_Pixel1 <- p_Pixel1[,list(Sum_Pixel_row=sum(SumPixel), SD_Pixel_row=sd(SumPixel))]

  # 
  fractional_part <- data_rows_space_cleaned_middle$x / 1000 ## divide plots: get 1000 pixel width parts of the plot
  highest_fractional_part <- max(floor(fractional_part))
  cut_x <- max(data_rows_space_cleaned_middle$x)-highest_fractional_part*1000
  cut_x <- round(cut_x/2) #from both sides
  
  data_rows_space_cleaned_middle$x_r <-  round((data_rows_space_cleaned_middle$x-cut_x)/1000, digits = 0 )*1000
  
  p <- subset(data_rows_space_cleaned_middle, x_r==max(data_rows_space_cleaned_middle$x_r ))
  if(nrow(p)!=1000){
    data_rows_space_cleaned_middle <- subset(data_rows_space_cleaned_middle, x_r!=max(data_rows_space_cleaned_middle$x_r ))}
  
  data_rows_space_cleaned_middle$x_r <-  paste0("Sum_pixel_",data_rows_space_cleaned_middle$x_r)
  if (nrow(data_rows_space_cleaned_middle)<50) {print("No middle row identified. Proceed with next plot")
    next
  }
  data_rows_middle <- data_rows_space_cleaned_middle[,list(SumPixel=nrow(.SD)),by=.(Filename, Row_cluster_corr, x_r,upper_row_border,lower_row_border, median)]
  data_rows_middle <- dcast.data.table(data_rows_middle, ...~x_r, value.var = "SumPixel")

  data_rows_middle <- cbind(data_rows_middle,p_Pixel1)
   
  row_middle_coord <- median(data_rows_space_cleaned$y_corr[data_rows_space_cleaned$Row_cluster_corr==middle_row])
  
  p <- y_data_rowmiddle_cleaned2
  p$Row_middle_distance2middle <-   abs(p$Row_middle-row_middle_coord)
  p <- p[order(  p$Row_middle_distance2middle , decreasing = F),]
  
  # p <- p[1:3,] ## three rows; fix me
  # if (nrow(na.omit(p))!=3) {next}

  p$Row_cluster_corr <- gsub("Row ","Row_middle_",p$Row_cluster_corr)
  upperst_row_coordinates <- max(p$Row_middle)#(row_coordinates)[,..p]
  lowest_row_coordinates <- min(p$Row_middle)
  upperst_slope <- p$Slope_row[p$Row_middle==max(p$Row_middle)]
  lowest_slope <- p$Slope_row[p$Row_middle==min(p$Row_middle)]
  Plot_slope <- mean(p$Slope_row, na.rm=T)
    
  row_coordinates <- dcast.data.table(p, Filename~Row_cluster_corr, value.var = c("Row_middle"))
  
  # if (Row_distance_min>1000) {
  if (nrow(p)<3) {
  p_plot <-  subset(p,Row_cluster_corr==gsub("Row ","Row_middle_",middle_row)) 
  lowest_slope <- p_plot$Slope_row[1]
  upperst_slope <- p_plot$Slope_row[1]
  row_coordinates <- dcast.data.table(p_plot, Filename~Row_cluster_corr, value.var = c("Plot_begin","Plot_end"))
  upperst_row_coordinates <- p_plot$Plot_end
  lowest_row_coordinates <- p_plot$Plot_begin
  Plot_slope <- p_plot$Slope_row[1]
  }
  
  p_Pixel2 <- data.frame(min_y_at_x0=lowest_row_coordinates-(middle_x*lowest_slope), max_y_at_x0=upperst_row_coordinates-(middle_x*upperst_slope), 
                         min_y_at_xmax=lowest_row_coordinates+(middle_x*lowest_slope), max_y_at_xmax=upperst_row_coordinates+(middle_x*upperst_slope),
                         Plot_slope=Plot_slope)
  
  data_rows_middle <- cbind(data_rows_middle,p_Pixel2)
  
  # p <- ncol(row_coordinates)

  data_plot <- data_rows_space_cleaned[y_corr2>as.numeric(lowest_row_coordinates)&y_corr2<as.numeric(upperst_row_coordinates)]
  # data_plot <- df_image_denoised_linearCorr[y_corr>lowest_row_coordinates&y_corr<upperst_row_coordinates]

  data_rows_middle <- cbind(data_rows_middle, row_coordinates[,2:ncol(row_coordinates)])
  
  p <- ncol(row_coordinates)
  if (nrow(data_plot)>20000) {
    
  yintercept <-c(as.numeric(row_coordinates[,2:ncol(row_coordinates)]), rep(NA,times=20001-p))
    
  ggPlot_Pixels <- ggplot(data_plot[sample(1:nrow(data_plot),20000),],aes(y=y_corr2,x=x))+ylab("Plot width (Pixels)  ")+xlab("Length (Pixels)")+
    theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.text=element_blank(),strip.background = element_blank(),legend.key.size = unit(0.6, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor.x = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=11),axis.title = element_text(size = 11))+
    # geom_point(data=df_image_corr[sample(1:nrow(df_image_corr),50000),], aes(y=y,x=x), color="grey",size=0.00000000001)+
    geom_point(size=0.0000000001)+
    # geom_line()+
    geom_hline(yintercept = yintercept,color="darkred",linetype="dashed")+
    scale_color_manual(values = rev(tol4qualitative))+
    guides(color = guide_legend(override.aes = list(size=2))) +
    scale_y_reverse ()
  }else{
    yintercept <-c(as.numeric(row_coordinates[,2:ncol(row_coordinates)]), rep(NA,times=nrow(data_plot)+1-p))
    
    ggPlot_Pixels <- ggplot(data_plot,aes(y=y_corr2,x=x))+ylab("Plot width (Pixels)  ")+xlab("Length (Pixels)")+
      theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.text=element_blank(),strip.background = element_blank(),legend.key.size = unit(0.6, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor.x = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=11),axis.title = element_text(size = 11))+
      # geom_point(data=df_image_corr[sample(1:nrow(df_image_corr),50000),], aes(y=y,x=x), color="grey",size=0.00000000001)+
      geom_point(size=0.0000000001)+
      # geom_line()+
      geom_hline(yintercept = yintercept,color="darkred",linetype="dashed")+
      scale_color_manual(values = rev(tol4qualitative))+
      guides(color = guide_legend(override.aes = list(size=2))) +
      scale_y_reverse ()
    
  }
  
  Sum_Pixel_plot <- nrow(data_plot)
  data_rows_middle <- cbind(data_rows_middle, Sum_Pixel_plot)
  data_rows_middle
  
get_pixel_and_save <- function(data, df_image_res90, image_q90_df,folderTrain){
    # data <- subset(data_inbetween_row_123, x_50==900&y_50==1500)
    # data <- subset(data_middle_row_sub, x_50==2250&y_50==1950)
    # print(paste("x_50",min(data$x,na.rm = T),", y_50",min(data$y,na.rm = T)))
    
    #     
    imageSeg_pixel_df <- subset(df_image_res90, y%in%data$y) 
    imageSeg_pixel_df <- subset(imageSeg_pixel_df, paste(x,y)%in%c(paste(data$x,data$y))) ## for y_corr
    if(nrow(imageSeg_pixel_df)>10000) {
      imageSeg_pixel_df$value[imageSeg_pixel_df$value!=0] <-1
      
      #print(paste("mean pixel value =",mean(imageSeg_pixel_df$value))) ## select only when having some green pixels
      
      min_canopy_cover <- 0.25
      if (folderTrain=="Weed") {min_canopy_cover <- 0.1}
      
      if(mean(imageSeg_pixel_df$value)>min_canopy_cover){
        if(mean(imageSeg_pixel_df$value)<0.85){
          
          
          ### clear small leaves (weeds) ##
          # if (folderTrain!="Weed") {
          # spannvalue <- 10
          # imageSeg_pixel_df <- imageSeg_pixel_df[order(imageSeg_pixel_df$y),]
          # imageSeg_pixel_df[,value_rollmean_x := frollmean(value,n=spannvalue, fill=NA), by=y]
          # imageSeg_pixel_df <- imageSeg_pixel_df[order(imageSeg_pixel_df$x),]
          # imageSeg_pixel_df[,value_rollmean_y := frollmean(value,n=spannvalue, fill=NA), by=x]
          imageSeg_pixel_df <- na.omit(imageSeg_pixel_df)
          # imageSeg_pixel_df$value[imageSeg_pixel_df$value_rollmean_y<0.5&imageSeg_pixel_df$value_rollmean_x<0.5] <- 0
          # imageSeg_pixel_df$value[imageSeg_pixel_df$value_rollmean_y>0.75&imageSeg_pixel_df$value_rollmean_x>0.75] <- 1
          # 
          # }
          ###
          if (folderTrain=="Weed") {imageSeg_pixel_df$value[imageSeg_pixel_df$value==1] <-0.5}
          
          if (identical(folderTrain,LeaveSpecies)==F) {
          folderTrain <- paste0(LeaveSpecies,"as",folderTrain)
          }
          
          imageSeg_pixel_df$x <- imageSeg_pixel_df$x-min(imageSeg_pixel_df$x)+1
          imageSeg_pixel_df$y <- imageSeg_pixel_df$y-min(imageSeg_pixel_df$y)+1
          imageSeg_pixel_df$value[is.na(imageSeg_pixel_df$value)] <-0
          require(imager)
          image_pixel <- as.cimg(imageSeg_pixel_df[,c("x","y","value")])  
          
          # plot(image_pixel)
          p <- gsub("segmented",paste0("segmented/Mask"),gsub(".png","",filename))
          dir.create(dirname(p), showWarnings = FALSE)
          save.image(image_pixel,  paste0(p,"_",folderTrain,"_x_",data$x[1],"_y_",data$y[1],".png"))
          ##############
          ##############
          image_pixel_df <- subset(image_q90_df, y%in%data$y) 
          image_pixel_df <- subset(image_pixel_df, paste(x,y)%in%c(paste(data$x,data$y))) ## for corr
          
          ## to adjust size as in mask ## fix me
          # image_pixel_df[,value_rollmean_x := frollmean(value,n=spannvalue), by=.(y,cc)]
          # image_pixel_df <- image_pixel_df[order(image_pixel_df$x),]
          # image_pixel_df[,value_rollmean_y := frollmean(value,n=spannvalue), by=.(x,cc)]
          image_pixel_df <- na.omit(image_pixel_df)
          #
          # 
          
          image_pixel_df$x <- image_pixel_df$x-min(image_pixel_df$x)+1
          image_pixel_df$y <- image_pixel_df$y-min(image_pixel_df$y)+1
          image_pixel_df$value[is.na(image_pixel_df$value)] <-0
          
          require(imager)
          image_pixel <- as.cimg(image_pixel_df[,c("x","y","cc","value")])  
          
          # plot(image_pixel)
          p <- gsub("segmented",paste0("segmented/RGB"),gsub(".png","",filename))
          dir.create(dirname(p), showWarnings = FALSE)
          save.image(image_pixel, paste0(p,"_",folderTrain,"_x_",data$x[1],"_y_",data$y[1],".png"))
          # gc()
        } } }
    
  }
  
  
  
  
  data_rows_space_cleaned_corrected <- data_rows_space_cleaned[!duplicated(paste(Filename,Row_cluster_corr))]
  data_rows_space_cleaned_corrected <- data_rows_space_cleaned_corrected[order(data_rows_space_cleaned_corrected$lower_row_border),]
  # data_rows_space_cleaned_corrected
  
  filename_q90 <- gsub("_segmentation.png","_q90.jpg",file)
  filename_q90 <- gsub("_q90_mask.png","_q90.jpg",filename_q90)
  filename_q90 <- gsub("_mask.png","_q90.jpg",filename_q90)
  
  require(imager)
  image_q90 <- load.image(filename_q90)
  # plot(image)
  image_q90_df <- as.data.frame(image_q90)
  max(image_q90_df$y)
  image_q90_df <- setDT(image_q90_df)
  image_q90_df <- subset(image_q90_df, y<max(image_q90_df$y)-cut_above&y>cut_below)
  image_q90_df$y_corr <- image_q90_df$y-(image_q90_df$x-middle_x)*slope1
  image_q90_df$x_50 <-  round(image_q90_df$x/128, digits = 0 )*128 ## minus 0xspannwith = 128
  # image_q90_df$y_50 <-  round(image_q90_df$y_corr/128, digits = 0 )*128 ## minus 0xspannwith = 128
  
  
  if (makeTrainingset==T) {
    
  data_inbetween_rows <- subset(image_q90_df)#, !paste(x,y)%in%c(paste(data_rows_space_cleaned$x,data_rows_space_cleaned$y))) ## add filename?
  data_inbetween_rows1 <- data_inbetween_rows[data_inbetween_rows$y_corr>data_rows_space_cleaned_corrected$upper_row_border[1]&data_inbetween_rows$y_corr<data_rows_space_cleaned_corrected$lower_row_border[2]]
  data_inbetween_rows1$Row_cluster_corr <- "Between_row_1_2"
  data_inbetween_rows2 <- NULL
  if (nrow(data_rows_space_cleaned_corrected)>2) {
    data_inbetween_rows2 <- data_inbetween_rows[data_inbetween_rows$y_corr>data_rows_space_cleaned_corrected$upper_row_border[2]&data_inbetween_rows$y_corr<data_rows_space_cleaned_corrected$lower_row_border[3]]
    data_inbetween_rows2$Row_cluster_corr <- "Between_row_2_3"
  }
  data_inbetween_row_123 <- rbind(data_inbetween_rows1,data_inbetween_rows2)
  
  data_inbetween_row_123[,y_50:=round((y_corr-min(y_corr)+56)/128, digits = 0 )*128 , by=.(Row_cluster_corr)]# start at pix 56 and discard until 64 (from 1 to 64) and keep 65 to 128+64
  data_inbetween_row_123[,y_50_top:=round((max(y_corr)-y_corr+56)/128, digits = 0 )*128 , by=.(Row_cluster_corr)]
  data_inbetween_row_123[,length(y_corr),by=y_50]
  
  # between_rows <- data_inbetween_row_123[,list(min_y=min(y_50), max_y=max(y_50)), by=.(Row_cluster_corr)]
  
  # between_rows$Distance <- between_rows$max_y -between_rows$min_y
  # between_rows
  # hist(data_inbetween_row_123$y_50)
  # data_inbetween_row_123_sub <- subset(data_inbetween_row_123, !(y_50%in%c(between_rows$min_y,between_rows$max_y)))
  data_inbetween_row_123_sub <- subset(data_inbetween_row_123, y_50!=0&y_50_top!=0)
  data_inbetween_row_123_sub[Row_cluster_corr=="Between_row_2_3",y_50:=y_50+1000, by=.(Row_cluster_corr)]

  data_inbetween_row_123_sub[,x1000:=round(x/1000,digits = 0)*1000]
  if(nrow(data_inbetween_row_123_sub)==0){print("Not enough space between the rows. Will skip imaging")}else{
  
  ### on the filtered image
  data_inbetween_rows <- subset(df_image)
  data_inbetween_rows$value[is.na(data_inbetween_rows$value)] <- 0
  data_inbetween_rows$y_corr <- data_inbetween_rows$y-data_inbetween_rows$x*slope1

  data_inbetween_rows1 <- data_inbetween_rows[data_inbetween_rows$y_corr>data_rows_space_cleaned_corrected$upper_row_border[1]&data_inbetween_rows$y_corr<data_rows_space_cleaned_corrected$lower_row_border[2]]
  data_inbetween_rows1$Row_cluster_corr <- "Between_row_1_2"
  data_inbetween_rows2 <- NULL
  if (nrow(data_rows_space_cleaned_corrected)>2) {
    data_inbetween_rows2 <- data_inbetween_rows[data_inbetween_rows$y_corr>data_rows_space_cleaned_corrected$upper_row_border[2]&data_inbetween_rows$y_corr<data_rows_space_cleaned_corrected$lower_row_border[3]]
    data_inbetween_rows2$Row_cluster_corr <- "Between_row_2_3"
  }
  weeds_inbetween_row <- rbind(data_inbetween_rows1,data_inbetween_rows2)
  weeds_inbetween_row[,x1000:=round(x/1000,digits = 0)*1000]
  weeds_inbetween_row$value[weeds_inbetween_row$value!=0] <-1
  mean_value_per1000 <- weeds_inbetween_row[, list(mean_value=mean(value)), by=.(x1000,Row_cluster_corr)] #, min_y=min(y), max_y=max(y)
  p <- subset(mean_value_per1000, mean_value>0.0025)
  if(nrow(data_inbetween_row_123_sub)>0){print("Quite some space between the rows")}
    
  data_inbetween_row_123_subRow <- subset(data_inbetween_row_123_sub, paste(Row_cluster_corr, x1000)%in%paste(p$Row_cluster_corr, p$x1000))

  if(nrow(data_inbetween_row_123_subRow)>0){print("Enough green.. Looking for weeds")
  gc()
  data_inbetween_row_123_subRow[,get_pixel_and_save(.SD, df_image_res90, image_q90_df,  folderTrain="Weed"),by=.(x_50,y_50)]
  }
  # p <- na.omit(data_inbetween_row_123_sub)
  # image_pixel_df <- subset(image_q90_df, paste(x,y)%in%c(paste(p$x,p$y)))
  # p <- as.cimg(image_pixel_df[,c("x","y","cc","value")])
  # ggdraw()+draw_image(p)
  # p <- subset(mean_value_per1000, mean_value>0.005)
  data_inbetween_row_123_sub <- subset(data_inbetween_row_123_sub,  x1000%in% p$x1000)
  
  
  # ggplot(data_inbetween_row_123, aes(x=x, y=y_corr, color=value ))+
  # geom_point()
  data_middle_row <- subset(image_q90_df)
  data_middle_row <- data_middle_row[data_middle_row$y_corr<data_rows_space_cleaned_corrected$upper_row_border[data_rows_space_cleaned_corrected$Row=="Middle"]&data_middle_row$y_corr>data_rows_space_cleaned_corrected$lower_row_border[data_rows_space_cleaned_corrected$Row=="Middle"]]
  data_middle_row <- subset(data_middle_row, !x%in% data_inbetween_row_123_sub$x)
  if (nrow(data_middle_row)==0) {
  paste("Too many weeds, will skip looking for soybean")
  }else{
  data_middle_row$Row_cluster_corr <- paste("Middle", LeaveSpecies, sep="_")
  data_middle_row <- setDT(data_middle_row)
  data_middle_row[,y_50:=round((y_corr-min(y_corr)+16)/128, digits = 0 )*128 , by=.(Row_cluster_corr)]# start at pix 16 and discard until 64 (from 1 to 64) and keep 65 to 128+64
  data_middle_row[,y_50_top:=round((max(y_corr)-y_corr+16)/128, digits = 0 )*128 , by=.(Row_cluster_corr)]
  data_middle_row[,length(y_corr),by=y_50]
  
 
  data_middle_row_sub <- subset(data_middle_row, y_50!=0&y_50_top!=0)

  
  data_middle_row_sub <- data_middle_row_sub[order(data_middle_row_sub$Row_cluster_corr,data_middle_row_sub$cc,data_middle_row_sub$x,data_middle_row_sub$y),]
  
  ## clean row for heterogenity ##
  data_middle_row_Seg <- subset(df_image_res90)#, !paste(x,y)%in%c(paste(data_rows_space_cleaned$x,data_rows_space_cleaned$y))) ## add filename?
  data_middle_row_Seg <- data_middle_row_Seg[data_middle_row_Seg$y<data_rows_space_cleaned_corrected$upper_row_border[data_rows_space_cleaned_corrected$Row=="Middle"]&data_middle_row_Seg$y>data_rows_space_cleaned_corrected$lower_row_border[data_rows_space_cleaned_corrected$Row=="Middle"]]
  
  mean_x <- data_middle_row_Seg[,list(mean_x=mean(value,na.rm=T)),by=.(x)]
  # mean_x$x_round <- round(mean_x$x/5,digits = 0)*5
  mean_x$rollmean_x <- frollmean(  mean_x$mean_x,na.rm=T, n=100)
  # hist(mean_x$rollmean_x)
  # mean_x$rollmean_x <- remove_outliers(mean_x$rollmean_x,2)
  mean_x$rollmean_x[mean_x$rollmean_x<median(mean_x$rollmean_x, na.rm=T)- 1*(quantile(mean_x$rollmean_x,0.5, na.rm=T)-quantile(mean_x$rollmean_x,0.25,na.rm=T))] <- NA
  nrow(data_middle_row_sub)
  length(  mean_x$rollmean_x[is.na(  mean_x$rollmean_x)])
  lengths <- rle( mean_x$rollmean_x)$lengths
  lengths <- as.data.table(lengths)
  lengths$id <- 1:nrow(lengths)
  mean_x$lengths <- lengths[, rep(lengths, each=lengths[1]), by=id]$V1
  mean_x <- subset(mean_x, lengths<20)
  
  data_middle_row_sub$value[!data_middle_row_sub$x%in%mean_x$x] <- NA
  data_middle_row_sub <- na.omit(data_middle_row_sub)
  nrow(data_middle_row_sub)
    # hist(data_middle_row_sub$value)
  
  # p <- na.omit(data_middle_row_sub)
  # image_pixel_df <- subset(image_q90_df, paste(x,y)%in%c(paste(p$x,p$y)))
  # p <- as.cimg(image_pixel_df[,c("x","y","cc","value")])
  # ggdraw()+draw_image(p)
  
  
  ###
  data_middle_row_sub[,get_pixel_and_save(.SD, df_image_res90, image_q90_df,  folderTrain=LeaveSpecies),by=.(x_50,y_50)]
  }
  }
  
  }## from skip imaging




  
  # image_q90_between_rows <- subset(image_q90_df, paste(x,y)%in%c(paste(data_inbetween_row_123_sub$x,data_inbetween_row_123_sub$y))) ## add filename?
  # hist(image_q90_between_rows$y)
  # # image_q90_between_rows <- subset(image_q90_df, !paste(x,y)%in%c(paste(data_rows_space_cleaned$x,data_rows_space_cleaned$y))) ## add filename?
  # image_q90_between_rows$y <- image_q90_between_rows$y-min(image_q90_between_rows$y)+1
  # 
  # p <- as.cimg(image_q90_between_rows)
  # 
  # ggimage_inbetween <- ggdraw()+draw_image(p)
  # ggimage_inbetween
  
  
   
  require(png)
  imagePNG <- readPNG(file)
  plot_blank <- ggplot() + theme_void()
  # Add the image to the plot using draw_image
  # ggimage_res <- draw_image(imagePNG)
  
  # ggimage_res <- ggdraw(plot_blank)+draw_image(imagePNG) # not working for mask
  require(grid)
  ggimage_res <-plot_blank +
    annotation_custom(rasterGrob(imagePNG, interpolate = TRUE), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  
  
  require(cowplot)
  first_row <- plot_grid(ggimage_res, ggRows_Middle, ggPlot_Pixels, rel_heights = c(1.23,1,1), ncol = 1, labels = c("AUTO"))  #,vjust=0.5+
  # first_row
  if(mask==T){
  ggsave(gsub(".png","_MaskRows.png",filename),  width = 80, height = 160, units = "mm", dpi = 100, bg="white",first_row)
  }else{  ggsave(gsub(".png","_rows.png",filename),  width = 80, height = 160, units = "mm", dpi = 100, bg="white",first_row)}
  # data_rows_collectAll <- rbind(data_rows_collectAll, data_rows_space_cleaned)
  # write.csv(data_rows_space_cleaned, gzfile(paste0(gsub(".png","",filename),"_data_rows.csv.gz")))
  
  if (makeTrainingset!=T) {
    p <- data_plot
    
    # Assuming 'p' is your original data frame
    # Find the maximum value of 'x' and 'y'
    max_value <- max(image_q90_df$x,na.rm = TRUE)
    
    # Create a complete sequence of 'x' and 'y' values from 1 to max_value
    complete_sequence <- expand.grid(x = 1:max_value)
    
    # Merge with the original data frame using a left join
    # This will fill in the missing 'x' and 'y' pairs with NA values
    p_ordered <- merge(complete_sequence, p, by = c("x"), all.x = TRUE)
    
    # Replace any missing values in 'x' and 'y' with NA
    p_ordered[is.na(p_ordered)] <- NA
    
    
    
    p$y_new <- round(p$y_corr2)
    p <- p[,list(value=mean(value)),by=.(x,y_new)]
    p <- dcast.data.table(p,y_new~x, value.var = "value")
    # p[1:10,1:10]
    # max_y <- max(image_q90_df$y)-2*cut
    p <- melt.data.table(p, id.vars = "y_new", variable.name = "x")
    p$x <- as.numeric(as.character(p$x))
    p$value[is.na(p$value)] <- 0
    p$y <- p$y_new-min(p$y_new)+1
    p <- p[order(p$x,p$y),]
    p <- as.data.table(p[,c("x","y","value")])
    
    image <- as.cimg(p)
    #####
    
    require(jpeg)
    imagePNG <- readJPEG(filename_q90)
    
    maxY <- dim(imagePNG)[1]
    maxX <- dim(imagePNG)[2]
    
    g <- rasterGrob(imagePNG, interpolate=F)
    # Prepare the data for the points
    points_data <- data.frame(
      x = c(0, 0, max(df_image_res90$x), max(df_image_res90$x)),
      y = c(maxY-p_Pixel2$min_y_at_x0[1], maxY-p_Pixel2$max_y_at_x0[1], maxY-p_Pixel2$min_y_at_xmax[1], maxY-p_Pixel2$max_y_at_xmax[1])
    )
    
    ggimage_plot <- ggplot(points_data, aes(x,y),geom="blank") +
      scale_y_continuous(limits=c(0,maxY)) +
      scale_x_continuous(limits=c(0,max(df_image_res90$x))) +
      theme(legend.position="none",axis.text = element_blank(), axis.ticks = element_blank(), axis.title= element_blank(),
            plot.margin=unit(c(0,0,0,0),"line")) +
      annotation_custom(g, xmin=0, xmax=maxX, ymin=0, ymax=maxY) +
      # geom_image(image=file)+
      geom_point(aes(x,y), color="darkred", size=3,shape=13)+
      coord_fixed()
    
    if(mask==T){
    ggsave(gsub("_q90.jpg","_MaskModified.png",filename_q90),  width = floor(maxX/20), height = floor(maxY/20), units = "px", dpi = 100, bg="white",ggimage_plot)
    ggsave(gsub(".png","_MaskRowsMiddle.png",filename),  width = 80, height = 60, units = "mm", dpi = 100, bg="white",ggRows_Middle)
    save.image(image,gsub(".png","_MaskPixels.png",filename))
    }else{
      ggsave(gsub("_q90.jpg","_modified.png",filename_q90),  width = floor(maxX/20), height = floor(maxY/20), units = "px", dpi = 100, bg="white",ggimage_plot)
      ggsave(gsub(".png","_RowsMiddle.png",filename),  width = 80, height = 60, units = "mm", dpi = 100, bg="white",ggRows_Middle)
      save.image(image,gsub(".png","_pixels.png",filename))    
      }
  }
  
  
  print(paste("Image",ii, "processed without error"))
  
  
  
  data_rows_middle_all <- rbind(data_rows_middle_all, data_rows_middle,fill=T)
  # gc()
  }
if(mask==T){
write.csv(data_rows_middle_all, paste0(working_dir,"_data_MaskRows.csv"), quote=F, row.names = F)
  }else{
    write.csv(data_rows_middle_all, paste0(working_dir,"_data_rows.csv"), quote=F, row.names = F)
  }

return(data_rows_middle_all)
}# if else from fnames==0
}

###############

# get_pixels <- lapply(folders, function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 1000, Row_distance_min=400,LeaveSpecies="Soybean" ))
# write.csv(get_pixels, "data_rows_All_2022.csv")


################


# folders_train <- c(folders[grepl("2022_06_",folders)],folders[grepl("2022_05_",folders)])
# folders_train <- folders_train[order(folders_train)]
# folders_train <- folders_train[7:9]
# folders_train
# # get_pixels <- lapply(folders_train, function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=T, cut = 1000, Row_distance_min=400 ))
# folders_train <- folders[grepl("2021_06_",folders)]
# folders_train <- folders_train[order(folders_train)]
# folders_train <- folders_train[2:3]
# folders_train
# get_pixels <- lapply(folders_train, function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=T, cut = 1000, Row_distance_min=400 ,LeaveSpecies="Soybean" ))
# write.csv(get_pixels, "data_rows_2021_06_23.csv")




# folder_i <- 1

get_training_pictures <- function (folders, augment_weed_dir,filename_to_exclude=NA) {
  # folder_ii in 1:length(folders_train)
fnames_Leaves_df_all <- NULL
fnames_Weed_df_all <-  NULL

for (folder in folders) {
  fnames_Leaves <- NULL
  fnames_Weed<- NULL
working_dir <-   folder
print(paste("Working on", working_dir))  
fnames_Leaves <- list.files(path=paste0(working_dir,"/RGB/"),recursive=F)
species_pattern <- data.frame(do.call('rbind', strsplit(fnames_Leaves,'_',fixed=TRUE)))
species_pattern <- species_pattern[,(length(species_pattern)-4)]
species_pattern <- species_pattern[!grepl("Weed", species_pattern)]
species_pattern <- species_pattern[1]
print(paste("Looking for",species_pattern, "leaves"))

fnames_Leaves <- list.files(path=paste0(working_dir,"/RGB"),pattern=paste0("_",species_pattern),recursive=F)
fnames_Leaves <- fnames_Leaves[!grepl("Weed", fnames_Leaves)]
fnames_Leaves <- fnames_Leaves[!grepl(paste0("as", species_pattern), fnames_Leaves)]

fnames_Leaves
soyfile2exclude <- filename_to_exclude[!grepl("Weed", filename_to_exclude)]
# soyfile2exclude <- gsub("\\./","",soyfile2exclude)
print(paste("Exclude", length(fnames_Leaves)-length(fnames_Leaves[!paste0(working_dir,"/RGB/",fnames_Leaves)%in%soyfile2exclude]), species_pattern, "pictures"))
fnames_Leaves <- fnames_Leaves[!paste0(working_dir,"/RGB/",fnames_Leaves)%in%soyfile2exclude]


working_dir_augment <- ""
weed_pattern <- ""
# if (is.character(augment_weed_dir)==T) {
#   
#   foldersAll2 <- list.dirs(full.names = F, path =paste0(augment_weed_dir))
#   folders_augment <- foldersAll2[grepl("_segmented/RGB",foldersAll2)]
#   if (folder_i>length(folders_augment)){ paste("No more augmented folders")}
#   working_dir_augment <- folders_augment[folder_i]
#   
#   fnames_Weed <- list.files(path=paste0(augment_weed_dir,working_dir_augment),recursive=F)
#   weed_pattern <- data.frame(do.call('rbind', strsplit(fnames_Weed,'_',fixed=TRUE)))
#   weed_pattern <- weed_pattern[,(length(weed_pattern)-4)]
#   weed_pattern <- weed_pattern[!grepl(species_pattern, weed_pattern)]
#   weed_pattern <- weed_pattern[!grepl("Weed", weed_pattern)]
#   weed_pattern <- weed_pattern[1]
#   fnames_Weed <- list.files(path=paste0(augment_weed_dir,working_dir_augment),pattern=weed_pattern,recursive=F,full.names = T)
#   fnames_Weed 
# }else{
augment_weed_dir <- ""
fnames_Weed <- list.files(path=paste0(working_dir,"/RGB"),pattern="Weed",recursive=F,full.names = T)
fnames_Weed

print(paste("Exclude", length(fnames_Weed)-length(fnames_Weed[!fnames_Weed%in%filename_to_exclude[grepl("Weed", filename_to_exclude)]]), "Weed pictures"))
fnames_Weed <- fnames_Weed[!fnames_Weed%in%filename_to_exclude[grepl("Weed", filename_to_exclude)]]

### make filename data frames

fnames_Leaves_df <- data.frame(fnames_Leaves=fnames_Leaves, Number=1:length(fnames_Leaves),working_dir=working_dir)


fnames_Weed_df <- data.frame(fnames_Weed=fnames_Weed, Number=1:length(fnames_Weed),working_dir=working_dir)


fnames_Leaves_df_all <- rbind(fnames_Leaves_df,fnames_Leaves_df_all)
fnames_Weed_df_all <- rbind(fnames_Weed_df,fnames_Weed_df_all)

}

fnames_Leaves_df <- fnames_Leaves_df_all
fnames_Leaves_df$Number_r <- rep(1: nrow(fnames_Leaves_df), each=3)[1:nrow(fnames_Leaves_df)]
fnames_Leaves_df <- fnames_Leaves_df[1:(floor(nrow(fnames_Leaves_df)/3)*3),]


fnames_Weed_df <- fnames_Weed_df_all
fnames_Weed_df$Number_r <- rep(1: nrow(fnames_Weed_df), each=1)[1:nrow(fnames_Weed_df)]
fnames_Weed_df <- fnames_Weed_df[1:(floor(nrow(fnames_Weed_df)/1)*1),]

fnames_df <- merge(fnames_Leaves_df,fnames_Weed_df,by=c("Number_r","working_dir"))
nrow(fnames_df)
fnames_df$working_dir_add <- fnames_df$working_dir
## iterate without working dir to match images from different folders
fnames_Leaves_df <- subset(fnames_Leaves_df_all, !fnames_Leaves%in%fnames_df$fnames_Leaves)
fnames_Leaves_df$Number_r <- rep(1: nrow(fnames_Leaves_df), each=3)[1:nrow(fnames_Leaves_df)]
fnames_Leaves_df <- fnames_Leaves_df[1:(floor(nrow(fnames_Leaves_df)/3)*3),]

fnames_Weed_df <- subset(fnames_Weed_df_all, !fnames_Weed_df%in%fnames_df$fnames_Weed_df)
fnames_Weed_df$Number_r <- rep(1: nrow(fnames_Weed_df), each=1)[1:nrow(fnames_Weed_df)]
fnames_Weed_df <- fnames_Weed_df[1:(floor(nrow(fnames_Weed_df)/1)*1),]
fnames_Weed_df$working_dir_add <- fnames_Weed_df$working_dir
fnames_Weed_df$working_dir <- NULL

fnames_df_add <- merge(fnames_Leaves_df,fnames_Weed_df,by=c("Number_r"))
nrow(fnames_df_add)
fnames_df_add$Number_r <- fnames_df_add$Number_r+max(fnames_df$Number_r)
## 

fnames_df <- rbind(fnames_df, fnames_df_add)
fnames_df$Number_r2 <-fnames_df$Number_r

setDT(fnames_df)[,N:=length(fnames_Leaves),by=Number_r]
fnames_df <- subset(fnames_df, N==3)
fnames_df <- unique(fnames_df)
# o <- subset(fnames_df_Mask, Number_r2==1848)
print(paste("Will produce 2 x", length(unique(fnames_df$fnames_Weed)),"mosaics"))


fnames_df_RGB <- fnames_df
# fnames_df_RGB$fnames_Weed
fnames_df_RGB$fnames_Leaves <-  paste0(fnames_df_RGB$working_dir,"/RGB/",fnames_df_RGB$fnames_Leaves)
fnames_df_RGB$Folder <- "./Collage/RGB/"

fnames_df_Mask <- fnames_df
fnames_df_Mask$fnames_Weed <-  gsub("/RGB/","/Mask/",fnames_df_Mask$fnames_Weed)
fnames_df_Mask$fnames_Leaves <-  paste0(fnames_df_Mask$working_dir,"/Mask/",fnames_df_Mask$fnames_Leaves)
fnames_df_Mask$Folder <- "./Collage/Mask/"

library(png)
library(grid)
library(gridExtra)

arrange_images <- function(fnames_df,RandomInsertion=F){
  
  # fnames_df <- subset(fnames_df_RGB, Number_r==20)
  # fnames_df <- subset(fnames_df_Mask, Number_r==1)
  # fnames_df <- subset(fnames_df_insertion, Number_r==4)

  
  
  fnames_Leaves_sub <-  fnames_df$fnames_Leaves[!duplicated(fnames_df$fnames_Leaves)]
  fnames_Weed_sub <-  fnames_df$fnames_Weed[!duplicated(fnames_df$fnames_Weed)]
  # print(fnames_df$Number_r2)
  
 
if(RandomInsertion==T){
  if (nrow(fnames_df)<6) {print("No more pictures")
    next
  }

  resize_image <- function(weed_only_mask) {
    # weed_only_mask <-png::readPNG(gsub("/RGB/","/Mask/",fnames_Leaves_sub[4]))

  newImage <- matrix(0, nrow = 126, ncol = 126)
  dim_pic <- dim(weed_only_mask)[3]
  dim_pic[is.na(dim_pic)] <- 1
  # print(dim_pic)
  
  dim1 <- dim(weed_only_mask)[1]
  if(dim1>126){dim1 <- 126}
  dim2 <- dim(weed_only_mask)[2]
  if(dim2>126){dim2 <- 126}
  
  if (dim_pic>1) {
    
  newImage <-  array(0, dim = c(126, 126, dim_pic))
    
  for (dimension in 1:dim_pic) {
    
  newImage[1:dim1,1:dim2,dimension] <- weed_only_mask[1:dim1,1:dim2,dimension]
  }
    }else{
      
    newImage[1:dim1,1:dim2] <- weed_only_mask[1:dim1,1:dim2]
  }
  # weed_only_mask <- newImage
  return(newImage)
  
  }
  
  image_weed <- png::readPNG(fnames_Weed_sub[1])
  image_weed <- resize_image(image_weed)

  dim(image_weed)
  second_image_weed <- png::readPNG(fnames_Weed_sub[2])
  second_image_weed <- resize_image( second_image_weed)
  
  weed_mask <-png::readPNG(gsub("/RGB/","/Mask/",fnames_Weed_sub[1]))
  weed_mask <- resize_image(weed_mask)
  weed_mask[weed_mask>0] <-  2/255
 
  second_weed_mask <-png::readPNG(gsub("/RGB/","/Mask/",fnames_Weed_sub[2]))
  second_weed_mask <- resize_image(second_weed_mask)
  second_weed_mask[second_weed_mask>0] <-  2/255

  
  # Read the soybean images
  images_mask <- lapply(gsub("/RGB/","/Mask/",fnames_Leaves_sub), readPNG)
  images_mask <- lapply(images_mask, resize_image)
  images_mask <- lapply(images_mask, function(x) {
    # print(mean(x[x > 0]))
    x[x > 0] <- 1/255
    return(x)
  })

  
  
  # Calculate the mean of each image and store it along with the image
  means <- lapply(images_mask, function(img) {
    # Convert to grayscale if necessary
    # gray_img <- grayscale(img)
    # Calculate the mean
    mean_val <- mean(img)
    return(mean_val)
  })
  
  images <- lapply(fnames_Leaves_sub, readPNG)
  images <- lapply(images, resize_image)
  
      # Find the index of the image with the lowest mean
  lowest_mean_index <- which.min(unlist(means))
  second_lowest_mean_index <- which.min(unlist(means)[-lowest_mean_index])
    # Select the image with the lowest mean
  image_soybean <- images[[lowest_mean_index]]
  second_image_soybean <- images[[second_lowest_mean_index]]
  
  
  soybean_mask <-images_mask[[lowest_mean_index]]
  soybean_mask <- resize_image(soybean_mask)
  
  second_soybean_mask <-images_mask[[second_lowest_mean_index]]
  second_soybean_mask <- resize_image(second_soybean_mask)
  
  
  soybean_only <- image_soybean
  soybean_only[soybean_mask==0] <- 0
  
  second_soybean_only <- second_image_soybean
  second_soybean_only[second_soybean_mask==0] <- 0
  
  
  mask_fused <- soybean_mask
  mask_fused[weed_mask!=0] <-   weed_mask[weed_mask!=0] 
  mask_fused[soybean_mask!=0] <-   soybean_mask[soybean_mask!=0] 
  
  rgb_fused <- soybean_only
  rgb_fused[soybean_only==0] <-   image_weed[soybean_only==0] 

  
  second_mask_fused <- second_soybean_mask
  second_mask_fused[second_weed_mask!=0] <-   second_weed_mask[second_weed_mask!=0] 
  second_mask_fused[second_soybean_mask!=0] <-   second_soybean_mask[second_soybean_mask!=0] 
  
  second_rgb_fused <- second_soybean_only
  second_rgb_fused[second_soybean_only==0] <-   second_image_weed[second_soybean_only==0] 
  
  working_folder <- strsplit(working_dir, "\\/")[[1]][length(strsplit(working_dir, "\\/")[[1]])]
  
  list_rgb <- list(second_rgb_fused,second_image_weed,rgb_fused,image_soybean)
  order <- sample(length(list_rgb))
  
  collage_name <- paste0(fnames_df$Folder[1],'collage3_',species_pattern,"_",working_folder,"_",weed_pattern,"_",gsub("/RGB","",working_dir_augment), "_",fnames_df$Number_r2[1],".png")
  png(collage_name, width=256,height=256, units = "px")
  
  list_rgb <- list_rgb[order]
  

  plot.new()
  par(mfrow = c(2, 2),mar=c(0,0,0,0),  mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
  usr<-par("usr")
  plot.new()
  plot.window(0:1, 0:1)
  rasterImage(list_rgb[[1]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
  plot.new()
  plot.window(0:1, 0:1)
  rasterImage(list_rgb[[2]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
  plot.new()
  plot.window(0:1, 0:1)
  rasterImage(list_rgb[[3]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
  plot.new()
  plot.window(0:1, 0:1)
  rasterImage(list_rgb[[4]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
  
  dev.off()
  
  collage_name_mask <- paste0(gsub("/RGB/","/Mask/",fnames_df$Folder[1]),'collage3_',species_pattern,"_",working_folder,"_",weed_pattern,"_",gsub("/RGB","",working_dir_augment), "_",fnames_df$Number_r2[1],".png")
  png(collage_name_mask, width=256,height=256, units = "px")
  
  list_mask <- list(second_mask_fused,second_weed_mask,mask_fused,soybean_mask)
  list_mask <- list_mask[order]
  
  plot.new()
  par(mfrow = c(2, 2),mar=c(0,0,0,0),  mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
  usr<-par("usr")
  plot.new()
  plot.window(0:1, 0:1)
  rasterImage(list_mask[[1]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
  plot.new()
  plot.window(0:1, 0:1)
  rasterImage(list_mask[[2]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
  plot.new()
  plot.window(0:1, 0:1)
  rasterImage(list_mask[[3]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
  plot.new()
  plot.window(0:1, 0:1)
  rasterImage(list_mask[[4]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
  
  dev.off()
  
  

  
  }else{
  
  # png::readPNG(fnames_Weed_sub[1])
  foo<-list()
  length_fnames <- length(fnames_Leaves_sub)
  for(j in 1:length_fnames){
    foo[[j]]<-png::readPNG(fnames_Leaves_sub[j])
    if(length(fnames_Leaves_sub[grepl("Mask", fnames_Leaves_sub)])>0) foo[[j]][foo[[j]]!=0] <- 1/255
  }
  
  foo[[length_fnames+1]]<-png::readPNG(fnames_Weed_sub[1])

  
  if(length(fnames_Leaves_sub[grepl("Mask", fnames_Leaves_sub)])>0){
    foo[[length_fnames+1]][foo[[length_fnames+1]]!=0] <- 2/255

  }
  
  numberOfphotos<- length(foo)
  plist1 <- sample(foo,numberOfphotos)

  working_folder <- strsplit(working_dir, "\\/")[[1]][length(strsplit(working_dir, "\\/")[[1]])]
  working_folder <- gsub("_segmented","",working_folder)
  collage_name <- paste0(fnames_df$Folder[1],'collage_',species_pattern,"_",working_folder,"_",weed_pattern,"_",gsub("/RGB","",working_dir_augment), "_",fnames_df$Number_r2[1],".png")
  dir.create(dirname(collage_name), showWarnings = FALSE)
  # ggsave(filename=collage_name, plot=tmp, width=256,height=256, units = "px", bg="black") #, bg="black"
  
  png(collage_name, width=256,height=256, units = "px")
  # par(mfrow = c(2, 2),mar=c(0,0,0,0),  mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)

    # usr<-par("usr")
    plot.new()
    plot.window(0:1, 0:1)
    par(mfrow = c(2, 2),mar=c(0,0,0,0),  mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
    
    usr<-par("usr")
    plot.new()
    plot.window(0:1, 0:1)
    rasterImage(plist1[[1]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
    plot.new()
    plot.window(0:1, 0:1)
    rasterImage(plist1[[2]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
    plot.new()
    plot.window(0:1, 0:1)
    rasterImage(plist1[[3]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
    plot.new()
    plot.window(0:1, 0:1)
    rasterImage(plist1[[4]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
  
    # print(grid.arrange(arrangeGrob(grobs = plist, ncol = 2)))
  dev.off()
  
  # 
  plist2 <- sample(foo,numberOfphotos)
  if (identical(plist1,plist2)==T) { plist2 <- sample(foo,numberOfphotos) }
  if (identical(plist1,plist2)==T) { plist2 <- sample(foo,numberOfphotos) }
  if (identical(plist1,plist2)==T) { plist2 <- sample(foo,numberOfphotos) }

  # plist <- lapply(plist2, rasterGrob)
  # tmp <- arrangeGrob(grobs = plist, ncol = 2)
  # tmp <- grid.arrange(arrangeGrob(grobs = plist, ncol = 2))
  collage_name <- paste0(fnames_df$Folder[1],'collage2_',species_pattern,"_",working_folder,"_",weed_pattern,"_",gsub("/RGB","",working_dir_augment), "_",fnames_df$Number_r2[1],".png")

  # ggsave(collage_name, tmp, width=256,height=256, units = "px", bg="black") #, bg="black"
  png(collage_name, width=256,height=256, units = "px")
  # print(grid.arrange(arrangeGrob(grobs = plist, ncol = 2)))
  plot.new()
  plot.window(0:1, 0:1)
  par(mfrow = c(2, 2),mar=c(0,0,0,0),  mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)

  par(mfrow = c(2, 2),mar=c(0,0,0,0),  mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)

  usr<-par("usr")
  plot.new()
  plot.window(0:1, 0:1)
  rasterImage(plist2[[1]], xleft=1.04, ybottom=1.04, xright=2.12, ytop=2.12, interpolate = FALSE,angle=180)#
  plot.new()
  plot.window(0:1, 0:1)
  rasterImage(plist2[[2]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
  plot.new()
  plot.window(0:1, 0:1)
  rasterImage(plist2[[3]], xleft=1.04, ybottom=1.04, xright=2.12, ytop=2.12, interpolate = FALSE,angle=180)#
  plot.new()
  plot.window(0:1, 0:1)
  rasterImage(plist2[[4]], usr[1], usr[3], usr[2], usr[4], interpolate = FALSE)
  
  dev.off()
  
  }
}

# layout(matrix(1:12,nr=4,byr=T))
# for (j in 1:12) plot(foo[[j]])
set.seed(321)
setDT(fnames_df_Mask)[,arrange_images(.SD),by=Number_r]
set.seed(321)
setDT(fnames_df_RGB)[,arrange_images(.SD),by=Number_r]
set.seed(432)
fnames_df_insertion <- fnames_df_RGB
fnames_df_insertion$Number_r <- as.numeric(as.factor(fnames_df_insertion$Number_r))
fnames_df_insertion$Number_r[fnames_df_insertion$Number_r %% 2 == 1] <- fnames_df_insertion$Number_r[fnames_df_insertion$Number_r %% 2 == 1]+1
fnames_df_insertion$Number_r2 <- fnames_df_insertion$Number_r

# print(paste("Will produce additonally 1 x", length(unique(fnames_df_insertion$Number_r)),"mosaics with insertion"))


# setDT(fnames_df_insertion)[,arrange_images(.SD, RandomInsertion = T),by=Number_r]


print(paste("Collage", folders, "processed"))
# grid.arrange(rasterGrob(plot1),rasterGrob(plot2),ncol=1)
return(as.list(data.frame(fnames_df_RGB),data.frame(fnames_df_Mask)))

dev.off()

dev.off()

}


# numbersToDelete <- c(642)
DeletePlantRGB <- function(fnames_df_RGB, numbersToDelete){
toDelete <- subset(fnames_df_RGB, Number.y%in%numbersToDelete)
file.remove(toDelete$fnames_Leaves)
}

DeleteWeedRGB <- function(fnames_df_RGB, numbersToDelete){
  toDelete <- subset(fnames_df_RGB, Number.y%in%numbersToDelete)
  file.remove(toDelete$fnames_Weed)
}
# lapply(folders_train, function(x) get_training_pictures(x, "/home/kellebea/public/Evaluation/Projects/KP0023_legumes/Pea/2022/PROMISE/Literal/Segmentation/"))
# 
# 
# 
# #################### correct collages
# 
correct_collage <- function(collage_dir){
fnames_collage <- list.files( full.names = T, path=paste0(collage_dir,"/Collage/Mask"),pattern="collage",recursive=F)
# iii <- 1876
all_dim <- NULL
for(iii in 1:length(fnames_collage) ){
file_collage <- fnames_collage[iii]
mask_collage <-png::readPNG(file_collage)

h<-dim(mask_collage)[1]
w<-dim(mask_collage)[2]
dims <- data.frame(h=h,w=w,ID=iii, values=unique(mask_collage[mask_collage!=0]))
all_dim <- rbind(all_dim,dims)
# 
if(max(dims$values)==1){
  print("Will correct one mosaic mask")
mask_collage[mask_collage==1] <- 0
# mask_collage <- round(mask_collage, digits = 2)
# levels(as.factor(mask_collage))
# mask_collage[mask_collage!=0.4&mask_collage!=0.6] <- 0
### check
# mask_collage[mask_collage==0.012] <- 0.01
# mask_collage[mask_collage==0.4] <- 0.01
# mask_collage[mask_collage==0.6] <- 0.02

# hist( mask_collage)
#open new file for output
png(file_collage, width=256, height=256)
par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
plot.new()
plot.window(0:1, 0:1)

#fill plot with image
usr<-par("usr")
rasterImage(mask_collage, usr[1], usr[3], usr[2], usr[4], interpolate = F)
dev.off()
}
}
return(all_dim)

oo <-png::readPNG(file_collage)
hist(oo)
oo[oo>0.005] <- 1
par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
plot.new()
plot.window(0:1, 0:1)
usr<-par("usr")
rasterImage(oo, usr[1], usr[3], usr[2], usr[4])
file_collage
# dev.off()
}



check_RGB_collage <- function(collage_dir){
  fnames_collage <- list.files( full.names = T, path=paste0(collage_dir,"/Collage/RGB"),pattern="collage",recursive=F)
  # iii <- 500
  print(paste(length(fnames_collage[duplicated(fnames_collage)]),"duplicated file names"))
  all_dim <- NULL
  for(iii in 1:length(fnames_collage) ){
    file_collage <- fnames_collage[iii]
    mask_collage <-png::readPNG(file_collage)
    # hist(mask_collage)
    h<-dim(mask_collage)[1]
    w<-dim(mask_collage)[2]
    dims <- data.frame(h=h,w=w,ID=iii)
    all_dim <- rbind(all_dim,dims)
    # nrow(all_dim)
    # subset(all_dim,h!=256)
    # subset(all_dim,w!=256)
    
  }
  return(all_dim)
  
}


make_plots <- function(png_files_exclude){
  # png_files <- png_files[1:100]
  library(png)
  library(grid)
  library(cowplot)
  
  plots <- lapply(png_files_exclude$File, function(file) {
    img <- readPNG(file)
    rasterGrob(img, interpolate = TRUE)
  })
  
  gg <- plot_grid(plotlist = plots, ncol = 10, labels = png_files_exclude$TitleNr)
  print(gg)
  # return(gg)
}

# make_plots(png_files)

# make_plots <- function(png_files) {
#   # Select the first 100 PNG files
#   png_files <- png_files[1:100]
#   
#   plots <- lapply(1:length(png_files), function(i) {
#     img <- readPNG(png_files[i])
#     plot <- rasterGrob(img, interpolate = TRUE)
#     # Add title
#     title <- paste("Img", i)
#     plot_with_title <- gTree(children = gList(plot, grid.text(label = title, x = 0.5, y = 1, hjust = 0.5, vjust = -0.5)))
#     return(plot_with_title)
#   })
#   
#   print(grid.arrange(grobs = plots,nrow=10, ncol = 10))
# }
# does not work





make_100er_plots <- function(png_files, save, path="TS",Title=1:length(png_files),Color="black",ncol = 10) {
  require(grid)
  png_files <- data.frame(png_files,Title,Color)
  png_files_sub <- png_files
  i <- 1
  while((i*100+1-100)<nrow(png_files)){
    print(paste("Print plot into",getwd()))  
    
    png_files_sub_100 <- png_files_sub[1:100,]
    png_files_sub_100 <- png_files_sub_100[!is.na(png_files_sub_100$png_files),]
    
    if(all.equal(Title, 1:nrow(png_files))==T) {
      plots <- lapply(png_files_sub_100$png_files, function(file) {
        img <- png::readPNG(file)
        rasterGrob(img, interpolate = F)
      })
      
      gg <- plot_grid(plotlist = plots, ncol = 10, label_size=7, label_colour = "#88CCEE", labels = paste0("", seq_along(plots)+(i*100-100)))
      gc()
      if(save==F) {print(gg)
        Sys.sleep(0)}
      if(save==T){ggsave(paste(path,i,".png" ))}
      
      
    }else{
    
      
    plots <- lapply(c(1:nrow(png_files_sub_100)), function(ii) {
      file <- png_files_sub_100$png_files[ii]
      img <- png::readPNG(file)
      # rasterGrob(img, interpolate = F)
      Title <- png_files_sub_100$Title[ii]
      Color <- png_files_sub_100$Color[ii]
      grid_obj <- grid.arrange(
        rasterGrob(img, interpolate = TRUE),
        top = textGrob(Title, gp=gpar(col=Color, fontsize=14), just=c("center", "center")),
        ncol = 1
      )
    })
    
    # gg <- plot_grid(plotlist = plots, ncol = ncol,  label_colour = "#88CCEE", labels = paste0("", seq_along(plots)+(i*100-100)))
    gc()
    if(save==F) {print(gg)
      Sys.sleep(0)}
    if(save==T){
      
      png(file = paste0(path,i,".png" ), width = ncol*126, height = nrow(png_files_sub_100)/ncol*126, unit="px")
      do.call(grid.arrange, c(plots, ncol = ncol))
      dev.off()
    }
    
   
  }# if else
  png_files_sub <- png_files[(i*100+1):nrow(png_files),]
  png_files_sub <- png_files_sub[!is.na(png_files_sub),]
  i <- i+1
  }
  return(data.frame(TitleNr=Title, File=png_files$png_files))
}

# make_100er_plots(png_files, save=T)



get_green_canopy_plot_cover <-  function(folder_ii, Plot_borders_mean) {
  
  require(cowplot)
  require(data.table)
  require(ggplot2)
  require(imager)
  require(grid)
  
  # for (folder_ii in 1:length(folders)) {
  print(paste("I'll see what I can find.."))  
  # working_dir <- "./SB007/RGB1/Segmentation/2017_06_02_15_54_segmented"
  
  working_dir <-   folder_ii#folders_train[folder_ii]
  print(paste("Working on", working_dir))  
  # fnames <- fnamesAll[grepl(gsub("\\./","",working_dir),fnamesAll)]
  fnames<- list.files(path = working_dir,pattern="q90_mask.png",recursive=F,full.names = T)
  if (length(fnames)==0) {
    fnames<- list.files(path = working_dir,pattern="_segmentation.png",recursive=F,full.names = T)
  }

  
  if (length(fnames)==0) {
    print(paste("Found no pictures.. go to next folder"))
  }else{
    
    print(paste("Found", length(fnames), "pictures"))  
    print(paste("Starting processing.. please wait"))  
    
    # fnames<- fnames[1:10]
    # fnames
    
    
    # ii <-1
    data_rows_middle_all <- NULL
    
    for (ii in 1:length(fnames)) {
      gc()
      set.seed(123)
      
      file <- fnames[ii]
      filename <- gsub("_segmentation","",file)
      # filename <- gsub("_NoMask","",filename)
      filename_only <- strsplit(filename, "segmented\\/")[[1]][length(strsplit(filename, "segmented\\/")[[1]])]
      UID_filename <- strsplit(filename_only, "_")[[1]][1]

      
      
      require(imager)
      image <- load.image(file)
      # image <- imrotate(image,angle) 
      require(cowplot)
      # ggimage_res <- ggdraw()+draw_image(image)
      # Create a blank plot
      require(png)
      imagePNG <- readPNG(file)
      plot_blank <- ggplot() + theme_void()
      # Add the image to the plot using draw_image
      # ggimage_res <- draw_image(imagePNG)
      
      # ggimage_res <- ggdraw(plot_blank)+draw_image(imagePNG) # not working for mask
      require(grid)
      ggimage_res <-plot_blank +
        annotation_custom(rasterGrob(imagePNG, interpolate = TRUE), xmin = 0, xmax = 1, ymin = 0, ymax = 1)
      
      Plot_borders <- Plot_borders_mean[grep(UID_filename, Plot_borders_mean$Filename_only),]
      if(nrow(Plot_borders)==0){
        cut_lower <- median(Plot_borders_mean$lower_plot_border,na.rm = T)
        cut_upper <- median(Plot_borders_mean$upper_plot_border, na.rm = T)
        plot_slope <- 0
        
        paste("Plot size was approximated")
      }else{
      # cut_lower <- round(Plot_borders$lower_plot_border+0.5*IQR(Plot_borders$lower_plot_border),digits = 0)
      # cut_upper <- round(Plot_borders$upper_plot_border-0.5*IQR(Plot_borders$lower_plot_border),digits = 0)}
      # 
      # if(cut_lower>cut_upper){
      cut_lower <- round(median(Plot_borders$lower_plot_border),digits = 0)
      cut_upper <- round(median(Plot_borders$upper_plot_border),digits = 0)
      plot_slope <- median(Plot_borders_mean$Plot_slope, na.rm = T)}
      
      
      # Plot_borders_mean$delta_y_below <-   Plot_borders_mean$min_y_at_x0-  Plot_borders_mean$middle_x
      # Plot_borders_mean$delta_y_up <-   Plot_borders_mean$max_y_at_x0-  Plot_borders_mean$max_y_at_xmax
      # Plot_borders_mean$delta_y <-   (Plot_borders_mean$delta_y_below+Plot_borders_mean$delta_y_up)*0.5
      # 
      df_image_res90 <- as.data.frame(image)
      
      
      # delta_x <- max(df_image_res90$x)-min(df_image_res90$x)
      # plot_slope <- mean(Plot_borders_mean$delta_y) /delta_x
      # adjust y for slope
      df_image_res90$y <-  df_image_res90$y-(df_image_res90$x-median(df_image_res90$x))*plot_slope
      df_image_res90$y <- round(df_image_res90$y, digits=0)
      
      
      if (is.null(df_image_res90$cc)) {df_image_res90$cc <- 4   }
      
      if(max(df_image_res90$cc)==3){
        df_image_res90 <- subset(df_image_res90, cc==2&y<max(df_image_res90$y)-cut_lower&y>cut_lower)}else{
          df_image_res90 <- subset(df_image_res90, cc==max(df_image_res90$cc)&y<cut_upper&y>cut_lower)
        }
      
      y_height <- cut_upper-cut_lower
      x_width <- max(df_image_res90$x)
      
      df_image_res90$value[df_image_res90$value==1] <- 0.5555
      # levels(as.factor(  df_image_res90$value))
      df_image_res90$value[round(df_image_res90$value,digits=2)==0.78] <- 0
      df_image_res90$value[round(df_image_res90$value,digits=2)==0.39] <- 0.5555
      

      df_image_res90$Filename <- file
      # hist(df_image_res90$value)

      df_image <- setDT(df_image_res90)
      df_image <- subset(df_image, value!=0)
      
      # nrow(df_image)/nrow(df_image_res90)
      
      p <- df_image[,list(SumPixel=nrow(.SD)),by=.(x)]
      p <- p[,list(Sum_Pixel_row=sum(SumPixel), SD_Pixel_row=sd(SumPixel))]
      p$Canopy_cover <- p$Sum_Pixel_row/nrow(df_image_res90)
      p$Plot_width <- y_height
      
      df_image$x_r <-  round((df_image$x-500)/1000, digits = 0 )*1000
      df_image <- subset(df_image, x_r!=max(df_image$x_r ))
      df_image$x_r <-  paste0("Sum_pixel_",df_image$x_r)
      
      
      
      data_rows_middle <- df_image[,list(SumPixel=nrow(.SD)),by=.(Filename, x_r)]
      data_rows_middle <- dcast.data.table(data_rows_middle, ...~x_r, value.var = "SumPixel")
   
      p_Pixel1 <- cbind(data_rows_middle,p)
      
      # x_cord <- unique(df_image$x)
      # x_cord <- x_cord[order(x_cord)]
      # xmins <- x_cord[1:100]
      # xmaxs <- x_cord[(length(x_cord)-100):length(x_cord)]
      
      min_y_at_x0 <-  cut_lower-(0-median(df_image_res90$x))*plot_slope
      min_y_at_xmax <-  cut_lower-(max(df_image_res90$x)-median(df_image_res90$x))*plot_slope
      max_y_at_x0 <-  cut_upper-(0-median(df_image_res90$x))*plot_slope
      max_y_at_xmax <-  cut_upper-(max(df_image_res90$x)-median(df_image_res90$x))*plot_slope
      
      # p_Pixel2 <- df_image[,list(min_y_at_x0=min(y[x==min(x)]), max_y_at_x0=max(y[x==min(x)]),min_y_at_xmax=min(y[x==max(x)]), max_y_at_xmax=max(y[x==max(x)])  )]
      p_Pixel2 <- data.frame(min_y_at_x0, max_y_at_x0,min_y_at_xmax,max_y_at_xmax)
      data_rows_middle <- cbind(p_Pixel1,p_Pixel2)
      
      p <- df_image
      
      ggPlot_Pixels <- ggplot(p[sample(1:nrow(p),40000, replace = T),],aes(y=y,x=x))+
        labs(title=paste("Green canopy cover fraction:", round(data_rows_middle$Canopy_cover,digits = 3 )))+
        theme_bw()+theme(strip.text=element_blank(),strip.background = element_blank(),legend.key.size = unit(0.6, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor.x = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=9),axis.title = element_text(size = 9))+
        # geom_point(data=df_image_corr[sample(1:nrow(df_image_corr),50000),], aes(y=y,x=x), color="grey",size=0.00000000001)+
        geom_point(size=0.0000000001)+
        # geom_line()+
        # geom_hline(yintercept = yintercept,color="darkred",linetype="dashed")+
        scale_color_manual(values = rev(tol4qualitative))+
        guides(color = guide_legend(override.aes = list(size=2))) +
        scale_y_reverse ()
      
      require(cowplot)
      first_row <- plot_grid(ggimage_res, ggPlot_Pixels, rel_heights = c(1.5,1), ncol = 1, labels = c("AUTO"))  #,vjust=0.5+
      # first_row
      
      ggsave(gsub(".png","_canopy_plot.png",filename),  width = 80, height = 120, units = "mm", dpi = 100, bg="white",first_row)
      
      
      ######
      
      p <- df_image
      
      # Assuming 'p' is your original data frame
      # Find the maximum value of 'x' and 'y'
      max_value <- max(df_image_res90$x,na.rm = TRUE)
      
      # Create a complete sequence of 'x' and 'y' values from 1 to max_value
      complete_sequence <- expand.grid(x = 1:max_value)
      
      # Merge with the original data frame using a left join
      # This will fill in the missing 'x' and 'y' pairs with NA values
      p_ordered <- merge(complete_sequence, p, by = c("x"), all.x = TRUE)
      
      # Replace any missing values in 'x' and 'y' with NA
      p_ordered[is.na(p_ordered)] <- NA
      
      
      
      p$y_new <- round(p$y)
      p <- p[,list(value=mean(value)),by=.(x,y_new)]
      p <- dcast.data.table(p,y_new~x, value.var = "value")
      # p[1:10,1:10]
      # max_y <- max(image_q90_df$y)-2*cut
      p <- melt.data.table(p, id.vars = "y_new", variable.name = "x")
      p$x <- as.numeric(as.character(p$x))
      p$value[is.na(p$value)] <- 0
      p$y <- p$y_new-min(p$y_new)+1
      p <- p[order(p$x,p$y),]
      p <- as.data.table(p[,c("x","y","value")])
      
      image <- as.cimg(p)
      save.image(image,gsub(".png","_plot-pixels.png",filename))    
      #####
      
      filename_q90 <- gsub("_segmentation.png","_q90.jpg",file)
      
      require(jpeg)
      imagePNG <- readJPEG(filename_q90)
      
      maxY <- dim(imagePNG)[1]
      maxX <- dim(imagePNG)[2]
      
      g <- rasterGrob(imagePNG, interpolate=F)
      # Prepare the data for the points
      points_data <- data.frame(
        x = c(0, 0, max(df_image_res90$x), max(df_image_res90$x)),
        y = c(maxY-p_Pixel2$min_y_at_x0[1], maxY-p_Pixel2$max_y_at_x0[1], maxY-p_Pixel2$min_y_at_xmax[1], maxY-p_Pixel2$max_y_at_xmax[1])
      )
      
      ggimage_plot <- ggplot(points_data, aes(x,y),geom="blank") +
        scale_y_continuous(limits=c(0,maxY)) +
        scale_x_continuous(limits=c(0,max(df_image_res90$x))) +
        theme(legend.position="none",axis.text = element_blank(), axis.ticks = element_blank(), axis.title= element_blank(),
              plot.margin=unit(c(0,0,0,0),"line")) +
        annotation_custom(g, xmin=0, xmax=maxX, ymin=0, ymax=maxY) +
        # geom_image(image=file)+
        geom_point(aes(x,y), color="darkred", size=3,shape=13)+
        coord_fixed()
      ggsave(gsub("_q90.jpg","_plot-modified.png",filename_q90),  width = floor(maxX/20), height = floor(maxY/20), units = "px", dpi = 100, bg="white",ggimage_plot)
      
      
      # data_rows_collectAll <- rbind(data_rows_collectAll, data_rows_space_cleaned)
      # write.csv(data_rows_space_cleaned, gzfile(paste0(gsub(".png","",filename),"_data_rows.csv.gz")))
      print(paste("Image",ii, "processed without error"))
      
      
      
      data_rows_middle_all <- rbind(data_rows_middle_all, data_rows_middle,fill=T)
      gc()
    }
    write.csv(data_rows_middle_all, paste0(working_dir,"canopy_data_plot.csv"), quote=F, row.names = F)
    
    return(data_rows_middle_all)
  }# if else from fnames==0
}



get_plot_borders_mean <- function(pattern="_data_rows.csv"){
  fnames_all = list.files(path= paste0(getwd()) ,pattern=pattern,recursive=T, full.names = T)
  fnames_all<-fnames_all[ grepl("_segmented_data_",fnames_all) ]
  # fnames_df <- data.frame(files=fnames)
  # fnames_df$mask <- "NoMask"
  # fnames_df$mask[grepl("data_MaskRows",fnames_df$files)] <- "Mask"
  # 
  fnames <- gsub("data_MaskRows.csv","data_rows.csv",fnames_all)
  dup_fnames <- fnames[duplicated(fnames)]
  dup_fnames_mask <- gsub("data_rows.csv","data_MaskRows.csv",dup_fnames) ## if masked rows exists prefer it
  NoDup_fnames <-   fnames[!fnames%in%dup_fnames]
  fnames <- c(NoDup_fnames,dup_fnames_mask)
  fnames <- fnames[!duplicated(fnames)]
  print(fnames)
  
  require(plyr)
  Data_rows = ldply(fnames, function(filename) {
    dum = fread(filename,header=T, sep="," , na.strings=c("NA","","x"))
    #If you want to add the filename as well on the column
    return(dum) })
  detach("package:plyr", unload=TRUE)
  
  Data_rows$Filename_org <- Data_rows$Filename
  Data_rows$Filename <- gsub("\\./","", Data_rows$Filename)
  Data_rows$Filename <- gsub("\\.png","", Data_rows$Filename)
  Data_rows$Filename <- gsub("Plot(\\d+)_(\\d+)", "Plot\\1-\\2", Data_rows$Filename)  # change naming PlotRowNr_RangeNr

  
  Data_rows <- setDT(Data_rows)[, c("Exp","Cam","Seg","Date","Filename") := tstrsplit(Filename, "/", fixed=TRUE)]

  if(  length(tstrsplit(Data_rows$Filename, "_", fixed=TRUE))==4){print("Literal nameing style")
  Data_rows <- setDT(Data_rows)[, c("plot_UID","Cam","Date","Segmentation") := tstrsplit(Filename, "_", fixed=TRUE)]
  Data_rows$Time <- NA
  Data_rows$Mask <- NA
    }
  
  rgb1 <- length(Data_rows$Filename[grepl("_RGB1_",  Data_rows$Filename)])

  if ( rgb1<1){print("Year 2015 nameing style")
  Data_rows <- setDT(Data_rows)[, c("Year","Month","Day","plot_UID","Segmentation") := tstrsplit(Filename, "_", fixed=TRUE)]
  Data_rows$Date <- paste0(Data_rows$Year, Data_rows$Month,Data_rows$Day)
  Data_rows$Month <- NULL
  Data_rows$Day <- NULL
  Data_rows$Time <- NA
  Data_rows$Mask <- NA
  # Data_rows[, c("plot_UID","Cam","Date","Time","Segmentation","Mask")]

   }else{
      
  Data_rows$Filename <- gsub("_segmentation","_segmentation_NoMask",Data_rows$Filename )
  Data_rows <- setDT(Data_rows)[, c("plot_UID","Cam","Date","Time","Segmentation","Mask") := tstrsplit(Filename, "_", fixed=TRUE)]

     }
  Data_rows$Date <- gsub("-","",Data_rows$Date)
  Data_rows$Date <- as.Date(Data_rows$Date, format = '%Y%m%d')
  Data_rows$Year <- strftime(Data_rows$Date , format = "%Y")
  Data_rows$Week <- strftime(Data_rows$Date , format = "%V")
  
  Data_rows[duplicated(paste(Date, plot_UID)),]
  Data_rows[,plot_UID[!duplicated(plot_UID)]]
  Data_rows
  
  Data_rows2 <- Data_rows[is.na(Data_rows$Plot_begin_Row_middle_1),]
  
  if (!"Plot_begin_Row_middle_1" %in% colnames(Data_rows)) {
    Data_rows$Plot_begin_Row_middle_1 <- NA
  }
  
  isNAlength1 <- length(Data_rows$Plot_begin_Row_middle_1[is.na(Data_rows$Plot_begin_Row_middle_1)])
  isNAlength <- isNAlength1+length(Data_rows$Plot_begin_Row_middle_2[is.na(Data_rows$Plot_begin_Row_middle_2)])
  isNAlength <- isNAlength/2
  Data_rows[,which(unlist(lapply(Data_rows, function(x)!all(is.na(x))))),with=F]
  
  if(  isNAlength/nrow(Data_rows)<0.55){
      
  print("Looking at whole plot")
  meas_var <-  names(Data_rows)[names(Data_rows)%in%c( paste0("Plot_begin_Row_middle_",1:3), paste0("Plot_end_Row_middle_",1:3))]
  Data_rows_melt <- melt.data.table(Data_rows, id.vars=c("Filename","plot_UID","Date","Week"), measure.vars = meas_var)
  }else{
  Data_rows_melt <- melt.data.table(Data_rows, id.vars=c("Filename","plot_UID","Date","Week"), measure.vars = c(paste0("Row_middle_",1:3)))
  }
  if (nrow(Data_rows2)!=0&length(names(Data_rows2)[names(Data_rows2)=="Row_middle_1"])==1) {
  Data_rows_melt2 <- melt.data.table(Data_rows2, id.vars=c("Filename","plot_UID","Date","Week"), measure.vars = c(paste0("Row_middle_",1:3)))
  Data_rows_melt <- rbind(Data_rows_melt, Data_rows_melt2)
  print("Adding row information")
  }
  # p <- subset(Data_rows_melt, plot_UID=="FPSB0160042")
  # p <- p[!is.na(p$value),]
  # p$plot_UID[!duplicated(p$plot_UID)]
  
  Data_rows_melt <- Data_rows_melt[!is.na(Data_rows_melt$value),]
  Data_rows_melt$plot_UID[!duplicated(Data_rows_melt$plot_UID)]
  
  
  Plot_borders <- Data_rows_melt[,list(lower_plot_border=min(value), upper_plot_border=max(value)),by=.(Filename,plot_UID,Date,Week)]
  
  Plot_borders <- Plot_borders[,lower_plot_border_cleaned:=remove_outliers(lower_plot_border,1.5),by=.(plot_UID)]
  Plot_borders <- Plot_borders[,upper_plot_border_cleaned:=remove_outliers(upper_plot_border,1.5),by=.(plot_UID)]

  
  p <- melt.data.table(Plot_borders, measure.vars = c("lower_plot_border_cleaned","upper_plot_border_cleaned"))
  gg1 <- ggplot(p)+
    geom_point(aes(x=Week,y=value,color=plot_UID, shape=variable),size=3,show.legend = F)+
    ggtitle("Border values of the plots (colored) over time")
  # facet_grid(.~variable)
  print(gg1)
  
  Plot_borders_mean <- Plot_borders[,list(lower_plot_border=mean(lower_plot_border,na.rm=T), upper_plot_border=mean(upper_plot_border,na.rm=T)),by=.(plot_UID)]
  Plot_borders_mean
  
  p <- unique(Data_rows[,c("Filename","plot_UID","Plot_slope","min_y_at_x0","max_y_at_x0","min_y_at_xmax","max_y_at_xmax")])
  
  Plot_borders_mean <- merge(Plot_borders_mean, p, by="plot_UID")
  Plot_borders_mean$Filename_only <- gsub("_segmentation_NoMask",".png",Plot_borders_mean$Filename)
  Plot_borders_mean$Filename_only <- gsub("_segmentation",".png",Plot_borders_mean$Filename_only)
  Plot_borders_mean$Filename_only <- gsub("_q90_mask",".png",Plot_borders_mean$Filename_only)
  
  Plot_borders_mean[,min_y_at_x0:=remove_outliers(min_y_at_x0,1.5),by=.(plot_UID)]
  Plot_borders_mean[,max_y_at_x0:=remove_outliers(max_y_at_x0,1.5),by=.(plot_UID)]
  Plot_borders_mean[,min_y_at_xmax:=remove_outliers(min_y_at_xmax,1.5),by=.(plot_UID)]
  Plot_borders_mean[,max_y_at_xmax:=remove_outliers(max_y_at_xmax,1.5),by=.(plot_UID)]
  
  Plot_borders_mean[,min_y_at_x0:=mean(min_y_at_x0,na.rm=T),by=.(plot_UID)]
  Plot_borders_mean[,max_y_at_x0:=mean(max_y_at_x0,na.rm=T),by=.(plot_UID)]
  Plot_borders_mean[,min_y_at_xmax:=mean(min_y_at_xmax,na.rm=T),by=.(plot_UID)]
  Plot_borders_mean[,max_y_at_xmax:=mean(max_y_at_xmax,na.rm=T),by=.(plot_UID)]
  
  Plot_borders_mean[,Plot_slope:=mean(Plot_slope,na.rm=T),by=.(plot_UID)]
  
  
  return(Plot_borders_mean)
}


draw_and_plot <- function(df,legend) {
  # Load the first image
  im <- load.image(df$FilenameQ90[1])
  
  # Plot the image
  png(paste0(df$FilenameQ90[1],"_WeedLocation.png"), width =  560*2, height =  400*2)
  
  plot(im)
  
  # Iterate over all rows in the dataframe
  for (i in  1:nrow(df)) {
    rect(xleft = df$x_coord[i], ybottom = df$y_coord[i],
         xright = df$x_coord[i] +  126, ytop = df$y_coord[i] +  126,
         col = NA, border = df$color[i], lwd = 3)
    text(x = df$x_coord[i] +  60, y = df$y_coord[i] +160, labels = df$TitelNr[i], col = df$color[i], cex =  1, lwd = 2)
    
  }
  
  title( xlab = "X Coordinate (Pixel)", ylab = "Y Coordinate (Pixel)")
  
  if (legend==T) {
    
    
    # Add a legend for the rectangle color
    legend("topright",  # Position of the legend
           legend = c("Selected","Discarded","Soybean"),  # Labels for the legend
           border = c("blue3","darkred","#DDCC77"), 
           fill="white",
           #title = "Rectangle Color",  # Title of the legend box
           box.lty =  0,  # No border for the legend box
           box.lwd =  1,  # Border width for the legend box
           bg = "white",  # Background color of the legend box
           cex =  1.6,  # Text size
           horiz = FALSE)  # Orientation of the legend
  }
  # Save the image
  dev.off()
}



draw_and_plot_middle <- function(df, row_info, legend = TRUE) {
  # Load the first image
  row_info <- subset(row_info, Filename==gsub("_q90.jpg","_segmentation.png",df$FilenameQ90[1]))
  im <- load.image(df$FilenameQ90[1])
  
  # Plot the image
  png(paste0(df$FilenameQ90[1], "_WeedLocation.png"), width = 560 * 2, height = 400 * 2)
  
  plot(im)
  
  # Plot middle lines based on row_info
  for (i in 1:nrow(row_info)) {
    slope <- row_info$Plot_slope[i]
    
    for (j in 1:3) {  # Plot lines for Row_middle_1, Row_middle_2, Row_middle_3
      intercept <- row_info[[paste0("Row_middle_", j)]][i]
      
      # Draw the line: y = slope * x + intercept
      abline(a = intercept, b = slope, col = "black", lwd = 5, lty=4)
    }
  }
  
  # Iterate over all rows in the dataframe
  for (i in 1:nrow(df)) {
    rect(xleft = df$x_coord[i], ybottom = df$y_coord[i],
         xright = df$x_coord[i] + 126, ytop = df$y_coord[i] + 126,
         col = NA, border = df$color[i], lwd = 3)
    text(x = df$x_coord[i] + 60, y = df$y_coord[i] + 160, 
         labels = df$TitelNr[i], col = df$color[i], cex = 1, lwd = 2)
  }
  
  title(xlab = "X Coordinate (Pixel)", ylab = "Y Coordinate (Pixel)")
  
  if (legend) {
    # Add a legend for the rectangle color
    legend("topright",  
           legend = c("Selected", "Discarded", "Soybean"),  
           border = c("blue3", "darkred", "#DDCC77"), 
           fill = "white",
           box.lty = 0,  
           box.lwd = 1,  
           bg = "white",  
           cex = 1.6,  
           horiz = FALSE)
  }
  
  # Save the image
  dev.off()
}

############


library(png)
library(grid)
library(gridExtra)

make_plots_ncol <- function(png_files_exclude, ncol=2, save=F, Specie=NA, Title=c(1:nrow(png_files_exclude))){
    png_files_exclude <- subset(png_files_exclude, Species==Specie)
   
    plots <- lapply(seq_along(png_files_exclude$File), function(i) {
    file <- png_files_exclude$File[i]
    img <- readPNG(file)
    Title <- Title[i]
    
    # Create a grid object with the image and the text annotation
    grid_obj <- grid.arrange(
      rasterGrob(img, interpolate = TRUE),
      top = textGrob(Title, gp=gpar(col=png_files_exclude$color[i], fontsize=14), just=c("center", "center")),
      ncol = 1
    )
    
    return(grid_obj)
  })
  
  if(save==F) {
    do.call(grid.arrange, c(plots, ncol = ncol))
    Sys.sleep(0)
  }
  if(save==T){
    png(file = paste0(gsub("\\.jpg","",png_files_exclude$FilenameQ90[1]),"_",Specie,".png" ), width = ncol*126, height = nrow(png_files_exclude)/ncol*126, unit="px")
    do.call(grid.arrange, c(plots, ncol = ncol))
    dev.off()
  }
  return(plots) # Return the list of grid objects for further use
}
############

make_loop_plots  <-  function(year3,investigate,RGBorMask= "_segmentation.png",n_row=1){
  # investigate needs UID, date and year
  year2 <- as.character(year3)
  print(year2)
  files <- list.files(path=paste0("~/public/Evaluation/Projects/KP0023_legumes/Soybean/",year2),pattern=RGBorMask,recursive = T,full.names = T)
  files <- files[grepl("/RGB1/",files)]
  toplot_sub <- subset(investigate, year==year2)
  toplot_sub$UID2 <- toplot_sub$UID
  toplot_sub <- toplot_sub[order(toplot_sub$date),]
  toplot_sub$date2 <- as.character(toplot_sub$date)
  toplot_sub <- na.omit(toplot_sub)
  # toplot_sub <- subset(toplot_sub, UID=="FPSB0050019"&date=="2016-06-27")
  # files[grepl(toplot_sub$UID2,files)&grepl(gsub("-","_",toplot_sub$date2), files)]
  setDT(toplot_sub)[,Filename_row:=files[grepl(.SD$UID2,files)&grepl(gsub("-","_",.SD$date2), files)][1],by=.(UID,date,year_site.UID)]
  
  make_plots <- function(png_files_exclude,n_row=n_row){
    library(magick)
    require(grid)
    plots <- lapply(png_files_exclude$File, function(file) {
      print(paste("Plot",file))
      img <- image_read(file)
      img <- image_scale(img, "25%")
      # Get the dimensions of the image
      img_height <- image_info(img)$height
      img_width <- image_info(img)$width
      
      # Calculate the number of pixels to crop from the top and bottom
      crop_pixels_top <- floor((10 / 100) * img_height)
      crop_pixels_bottom <- ceiling((10 / 100) * img_height)
      
      # Crop the image from top and bottom
      img <- image_crop(img, geometry = paste(img_width, img_height - crop_pixels_top - crop_pixels_bottom, "+0+", crop_pixels_top, sep = "x"))
      
      rasterGrob(img, interpolate = TRUE)
    })
    require(cowplot)
    gg <- plot_grid(plotlist = plots, nrow = n_row, labels = png_files_exclude$TitleNr,  label_size  = 5)
    title <- ggdraw() + draw_label(paste("Breeding line:",png_files_exclude$genotype.id[1],",","Plot:", png_files_exclude$UID[1]), fontface='bold',size = 7)
    gg <- plot_grid(title, gg, ncol=1, rel_heights=c(0.1, 1)) 
    # ggsave(paste0("plot_", png_files_exclude$genotype.id, ".png"), plot = final_plot, dpi = 300, units = "in")
    print(gg)
    return(gg)
  }
  
  
  ToPlot <- data.table(File=toplot_sub$Filename_row,TitleNr=toplot_sub$date, UID=toplot_sub$UID,genotype.id=toplot_sub$genotype.id)
  ToPlot <- na.omit(ToPlot)
  if(nrow(ToPlot)==0){print(paste("Skip",year2)) }else{
    # gg_list <- ToPlot[,list(make_plots(.SD,ncol=nrow(.SD))),by=UID]
    gg_list <-  lapply(unique(ToPlot$UID), function(xx) make_plots(subset(ToPlot,UID==xx),n_row=n_row))
    
    library(gridExtra)
    final_plot <- arrangeGrob(grobs =gg_list, ncol=1)
    ggsave(paste0(year2,RGBorMask,"_row",n_row,"_genid",ToPlot$genotype.id[1],"_plot_extremes.png"), plot = final_plot, dpi = 300, units = "mm", height = n_row*15*length(unique(ToPlot$UID)), width=20*length(unique(ToPlot$TitleNr))/n_row)
  }
}



# get_training_pictures_single <- function (folder_i, folders, augment_weed_dir,filename_to_exclude=NA) {
#   # folder_ii in 1:length(folders_train)
#   working_dir <-   folders[folder_i]
#   
#   fnames_Leaves <- list.files(path=paste0(working_dir,"/RGB/"),recursive=F)
#   fnames_filename <- list.files(path=paste0(working_dir,"/RGB/"),full.names = F)
#   
#   soyfile2exclude <- filename_to_exclude[!grepl("Weed", filename_to_exclude)]
#   # soyfile2exclude <- gsub("\\./","",soyfile2exclude)
#   fnames_Leaves <- fnames_Leaves[!paste0(working_dir,"/RGB/",fnames_Leaves)%in%soyfile2exclude]
#   
#   fnames_Leaves_df <- data.frame(fnames_Leaves=fnames_Leaves, Number=1:length(fnames_Leaves))
#   
#   
#   fnames_df_RGB <- fnames_Leaves_df
#   # fnames_df_RGB$fnames_Weed
#   fnames_df_RGB$fnames_Leaves <-  paste0(working_dir,"/RGB/",fnames_Leaves_df$fnames_Leaves)
#   
#   
#   library(png)
#   library(grid)
#   library(gridExtra)
#   
#   rgb_images <- function(filename){
#     # filename <- fnames_df_RGB$fnames_Leaves[1]
#     image<-png::readPNG(filename)
#     
#     p <- length(strsplit(filename, "\\/")[[1]])
#     filename_only <- strsplit(filename, "\\/")[[1]][p]
#     working_folder <- gsub("_segmented","",working_folder)
#     single_name <- paste0(getwd(),"/Single/RGB/",filename_only,".png")
#     dir.create(dirname(single_name), showWarnings = FALSE)
#     # ggsave(filename=single_name, plot=tmp, width=256,height=256, units = "px", bg="black") #, bg="black"
#     # Save the modified image
#     writePNG(image, target = single_name)
#     
#     
#     
#     filename_mask <- gsub("/RGB/","/Mask/",filename)
#     
#     image<-png::readPNG(filename_mask)
#     
#     # if(length(grep("Weed", filename_mask))==0){image[image!=0] <- 1/255}else
#     if(length(grep("Weed", filename_mask))==1){image[image!=0] <- 2/255}else{
#       image[image!=0] <- 1/255
#     }
#     
#     p <- length(strsplit(filename_mask, "\\/")[[1]])
#     filename_only <- strsplit(filename_mask, "\\/")[[1]][p]
#     working_folder <- gsub("_segmented","",working_folder)
#     single_name <- paste0(getwd(),"/Single/Mask/",filename_only,".png")
#     dir.create(dirname(single_name), showWarnings = FALSE)
#     # ggsave(filename=single_name, plot=tmp, width=256,height=256, units = "px", bg="black") #, bg="black"
#     # Save the modified image
#     writePNG(image, target = single_name)    
#   }
#   
#   lapply(fnames_df_RGB$fnames_Leaves, function(x) rgb_images(filename = x))
#   
#   
# }
