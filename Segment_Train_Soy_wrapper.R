source("/home/kellebea/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/functions/get_between_row_function.R")

########

set.seed(123)

########

setwd("~/Soybean/2022")

foldersAll <- list.dirs(full.names = TRUE)
folders <- foldersAll[grepl("_segmented",foldersAll)]
folders <- folders[grepl("RGB1",folders)]
# folders <- folders[grepl("SB016",folders)]
folders <- folders[!grepl("Mask",folders)]
folders <- folders[!grepl("_parts_segmented",folders)]
folders <- folders[!grepl("_segmented/RGB",folders)]
folders <- folders[!grepl("old",folders)]
folders <- folders[!grepl("lightning_logs",folders)]

folders_growth <- c(folders[grepl("2022_06_",folders)],folders[grepl("2022_05_",folders)])

folders_train <- c(folders[grepl("2022_06_",folders)],folders[grepl("2022_05_",folders)])
folders_train <- folders_train[order(folders_train)]
folders2022 <- folders_train
folders_train2022 <- folders_train[3:7]
folders_train2022

# make training set
get_pixels <- lapply(folders_train2022, function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=T, cut = 750, Row_distance_min=400,LeaveSpecies="Soybean", max_number_of_rows=5 ,min_cluster=3   ))

# check training images
library(png)
library(grid)
library(cowplot) 
###
folder <- folders_train2022[1]

png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
png_files_soy <- png_files[!grepl("asWeed",png_files)]
# # png_files_soy <- png_files_soy[sample(1:length(png_files_soy), 2000)]
# 
# pp <- data.frame(trueID=1:length(png_files_soy) )
# pp$select <- "Not-Selected"
# set.seed(123)
# pp$select[sample(1:length(png_files_soy), 2000)] <- 1:2000
# 
# png_files_soy <- png_files_soy[-addSelection]
# png_files_list_soy <- make_100er_plots(png_files_soy, save=T,  path=paste0(folder,"/Soybean_"))
# 
# 
# addSelection <- pp$trueID[pp$select=="Not-Selected"]                
# png_files_soy <- png_files_soy[addSelection]
# png_files_list_soy <- make_100er_plots(png_files_soy, save=T,  path=paste0(folder,"/Soybean_"))
# 
# pp$exclude <- "not-exclude"
# excluded_first <- c(7,13,15,30,74,75,107,112,117,119,120,124,127,129,133,158:160,173,185,221,240,249,261,265,271,272,276,280,286,291,296,
#                     326,330,385,431,452,479,480,492,500,507,538,546,551,567,570,585,601,613,617,624,626,645,688,730,739,744,797,806,811,817,818,855,877,889,891,869,
#                     901,916,926,927,944,945,965,966,972,974,994,1004,1040,1045,1047,1054,1056,1064,1111,1124,1129,1132,1135,1139,1153,1154,1156,1160,1165,1176,1181,1182,1193,1194,
#                     1203,1214,1223,1224,1225,1245,1258,1261,1309,1322:1324,1326,1343,1349,1357,1371,1372,1398,1407,1427,1429,1431,1439,1444,1459,1465,1488,1496,
#                     1518,1524,1534,1540,1569,1582:1587,1592,1599,1601,1610,1614,1617,1630,1643,1670,1704,1709,1744:1746,1748,1753,1755,1785:1788,1813,1868,1942,1955,1995)
# 
# pp$exclude[pp$select%in%excluded_first] <- "exclude"
# excluded_second <- c(30,31,33,42,60,81,89,90,95:97,114,120,136,149,150,156,158,161,193,218,219,254,271,289,309,319,321,330,331,339,340,342,353,370,372,374,
#                      421,457,458,470,479:481,484,846:484,504,506,507,512,513,517,520,530,534,543,547,560,561,584,592,598,603,611:614,631,633:635,641,653,659,661,666,667,674,677,678,696,
#                      702,722,730,774)
# 
# pp$select_second <- "Not-Selected"
# pp$select_second[addSelection] <-1:785
# pp$exclude[pp$select_second%in%excluded_second] <- "exclude"

# excluded_all <- pp$trueID[pp$exclude=="exclude"]
# png_files_soy <- png_files[!grepl("asWeed",png_files)]
png_files_list_soy <- make_100er_plots(png_files_soy, save=T,  path=paste0(folder,"/Soybean_"))
png_files_exclude_soy <- png_files_list_soy[c(19,75:86,90,91,95,96,101:104,109,128,138,147,221,233,234,273,302,314,318,324,326,330,336,338,339,352,369,396,435,448,453,467,483,
                                              508,512,518,538,553,555,557,568,549,583,593,601,623,674,707,761,779,782,783,789,798,821,900,902,910,920,923,976,1011,1014,1018:1021,1026,1078,1084:1086,1089,1092,1100,
                                              1110,1120,1121,1125,1126,1132,1135,1138,1142,1144,1152,1168,1170,1172:1174,1187,1194,1195,1201,1210,1215,1230:1250,1259,1261:1263,1273,1276,1278,1293,1295,1300,
                                              1313,1314,1321,1325,1326,1330:1334,1352,1354,1355,1357,1362,1370,1378,1399,1409,1414,1423,1436,1462,1484,1486,1506,1508,1573,1580,1593,1595,1597,
                                              1617:1621,1625:1629,1633,1636,1638,1641,1672,1677,1703,1709,1710,1716,1719,1721:1729,1732,1734,1736,1740,1741,1744,1755,1766,1784,1787,1788,1791,1793:1795,
                                              1801,1807,1809,1811,1817,1820:1823,1829,1834,1845,1850,1864,1873,1899,1901,1904,1913,1993,2009,2041,2061,2096,2102,2104,2106,2110,2112,2123,2131,2157,2166,2168,2173,2181,
                                              2222,2227,2239,2242,2243,2247,2252,2255,2260:2262,2267,2268,2271:2275,2300:2301,2304,2306:2308,2314,2316,2326,2337,2343,2347,2352,2355,2359,2360,2428,2429,2457,2473,
                                              2539,2540,2550,2574,2603,2636,2666,2699,2722,2229,2731),]
png_files_exclude_soy$Species <- "Soybean"
p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check

png_files_weed <- png_files[grepl("asWeed",png_files)]
png_files_weed
png_files_list_weed <- make_100er_plots(png_files_weed, save=T)
png_files_exclude_weed <- png_files_list_weed[c(1),]
png_files_exclude_weed$TitleNr <- NA #exclude nothing
png_files_exclude_weed$File <- NA #exclude nothing

# make_plots(png_files_exclude_weed) # check
# png_files_exclude_weed$Species <- "Weed"
# make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check

file_excluded_1 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)


###
folder <- folders_train2022[2]

png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
png_files_soy <- png_files[!grepl("asWeed",png_files)]
png_files_soy
png_files_list_soy <- make_100er_plots(png_files_soy, save=T,  path=paste0(folder,"/Soybean_"))
png_files_exclude_soy <- png_files_list_soy[c(16,35,47,54,56,66,96,100,108),]
png_files_exclude_soy$Species <- "Soybean"
p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check


png_files_weed <- png_files[grepl("asWeed",png_files)]
png_files_weed
png_files_list_weed <- make_100er_plots(png_files_weed, save=T)
png_files_exclude_weed <- png_files_list_weed[c(157,165),]
# make_plots(png_files_exclude_weed) # check
png_files_exclude_weed$Species <- "Weed"
# make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check

file_excluded_2 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)


##
folder <- folders_train2022[3]

png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
png_files_soy <- png_files[!grepl("asWeed",png_files)]
png_files_soy
png_files_list_soy <- make_100er_plots(png_files_soy, save=T)
png_files_exclude_soy <- png_files_list_soy[c(23,48,57,179,180,206,213,220,304,308,313,319,325,327,337,341,345,352,361,365,381,397,428,432,442,470,466,472,498,516,850,522,535,553,566,572,588,595,598,
                                              612,643,657,677,700,723:725,734),]
png_files_exclude_soy$Species <- "Soybean"
p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check

png_files_weed <- png_files[grepl("asWeed",png_files)]
png_files_weed
png_files_list_weed <- make_100er_plots(png_files_weed, save=T)
png_files_exclude_weed<- png_files_list_weed[c(1),] #exclude nothing
png_files_exclude_weed$TitleNr <- NA #exclude nothing
png_files_exclude_weed$File <- NA #exclude nothing

# make_plots(png_files_exclude_weed) # check
# png_files_exclude_weed$Species <- "Weed"
# make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check

file_excluded_3 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)

############



#################################
##### visualize##################
####################################

## requires from above: png_files; png_files_exclude
# make_100er_plots(png_files[!png_files%in%file_excluded_3], save=T)

Species <- "Soybean"
######
folder <- folders_train2022[3]
png_files_exclude <- plyr::rbind.fill(png_files_exclude_weed, png_files_exclude_soy)
# Species <- "Soybean"
png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png", full.names = T)
df_png_files <- data.frame(do.call('rbind', strsplit(png_files,'_x_',fixed=TRUE)))
df_png_files$File <- png_files
df_png_files$FilenameQ90 <- paste0(df_png_files$X1,"_q90.jpg")
df_png_files$FilenameQ90 <- gsub("\\/RGB\\/", "\\/",df_png_files$FilenameQ90  )
df_png_files$Species <- Species
df_png_files$Species[grepl("asWeed",df_png_files$X1)] <- "Weed"
df_png_files$FilenameQ90 <- gsub("_SoybeanasWeed", "",df_png_files$FilenameQ90  )
df_png_files$FilenameQ90 <- gsub("_Soybean", "",df_png_files$FilenameQ90  )


df_png_files <- setDT(df_png_files)[, c("x_coord","y","y_coord") := tstrsplit(X2, "_", fixed=TRUE)]
df_png_files$y <- NULL
df_png_files$y_coord <- as.numeric(gsub("\\.png", "", df_png_files$y_coord ))
df_png_files$x_coord <- as.numeric(df_png_files$x_coord)
df_png_files <- merge(df_png_files,data.frame(File=png_files_exclude$File,color="#FFD700"),by="File",all.x = T)
df_png_files$color[is.na(df_png_files$color)] <- "#00BFFF"
df_png_files$color[df_png_files$Species==Species&df_png_files$color!="#FFD700"] <- "#FF00FF"  
df_png_files <- df_png_files[order(df_png_files$FilenameQ90, df_png_files$x_coord)]
df_png_files[,N:=nrow(.SD),by=.(color,Species,FilenameQ90)]
df_png_files[, TitelNr :=1: nrow(.SD),by=.(Species)]#FilenameQ90

### visualize training set
p_sel <- subset(df_png_files, color!="#FFD700"&Species=="Weed")
ggWeed <- make_plots_ncol(p_sel[1:100,],ncol=10,save = T, Specie = "Weed", Fontsize=5, p_sel$TitelNr)
p_sel <- subset(df_png_files, color!="#FFD700"&Species!="Weed")
p_sel_soy <- p_sel[401:500,]
ggSoybean <-make_plots_ncol(p_sel_soy,ncol=10,save = T, Specie = "Soybean", Fontsize=5, p_sel_soy$TitelNr)


gg_training_set <-plot_grid(do.call(grid.arrange, c(ggSoybean, ncol = 10)),NULL, do.call(grid.arrange, c(ggWeed, ncol = 10)),
                          ncol =  3, rel_widths = c(1,0.05,1),  labels = c("","",""),label_size = 14)
# ggsave(paste0("Trainingset_example",gsub("./SB016/RGB1/Segmentation/","",folder),".png"),  width = 2400, height = 1200, units = "px", dpi = 150, bg="white",gg_training_set)


####

selectedFile <- df_png_files[,list(length(Species[Species!="Weed"]), length(Species[Species=="Weed"]),length(Species[color=="#FFD700"])),by=.(FilenameQ90)]
# selectedFile <- subset(selectedFile, V3>1)
selectedFile <- subset(selectedFile, V2>1&V1>1)

# Ensure imager package is installed and loaded
# install.packages("imager")
library(imager)

# Assuming df is your dataframe with columns FilenameQ90, x_coord, y_coord
# Example: df <- data.frame(FilenameQ90 = c("image1.png", "image2.png"), x_coord = c(50,  100), y_coord = c(50,  100))

# Function to draw rectangles and plot image
# Nweeds<- unique(df_png_files$N[df_png_files$Species=="Weed"])
# Nweeds
selectedFile1 <- selectedFile$FilenameQ90[selectedFile$V2==max(selectedFile$V2)][1]
# selectedFile <- df_png_files$FilenameQ90[df_png_files$Species=="Weed"&df_png_files$N==22][1]
add_row_info <- fread(paste0(folder,"_data_rows.csv"))
add_row_info <- add_row_info[,c("Filename","Plot_slope",paste0("Row_middle_",1:3), "min_y_at_x0", "max_y_at_x0", "min_y_at_xmax", "max_y_at_xmax")]
# add_row_info


df_sub <- subset(df_png_files, FilenameQ90==selectedFile1)
draw_and_plot_middle(df_sub, add_row_info, axis   = TRUE, legend = TRUE)
# third_row_file <- paste0(selectedFile1, "_WeedLocation.png")
# ggSoybeanWeedLocation <-  ggdraw() + draw_image(third_row_file)

df_sub[,list(length(Species[Species!="Weed"]), length(Species[Species=="Weed"])),by=.(FilenameQ90)]

make_plots_ncol(df_sub,ncol=1,save = T, Specie = Species)
ggWeed1 <-make_plots_ncol(df_sub,ncol=1,save = T, Specie = "Weed")

#########
selectedFile2 <- selectedFile$FilenameQ90[selectedFile$V1==max(selectedFile$V1)][1]
df_sub <- subset(df_png_files, FilenameQ90==selectedFile2)
# draw_and_plot(df_sub,legend=F)
draw_and_plot_middle(df_sub, add_row_info, axis = F, legend = TRUE)
third_row_file <- paste0(selectedFile2, "_WeedLocation.png")
ggSoybeanWeedLocation <-  ggdraw() + draw_image(third_row_file)

make_plots_ncol(df_sub,ncol=5,save = T, Specie = Species)
make_plots_ncol(df_sub,ncol=1,save = T, Specie = "Weed")


###
weed_values <- selectedFile$V2

second_highest_sum <- weed_values[order(weed_values, decreasing = TRUE)[2]]

# Filter the rows where the sum of V1 and V2 equals the second highest sum
selected_rows <- selectedFile[weed_values == second_highest_sum, ]

# Select the first FilenameQ90 from the filtered rows
# selectedFile3 <- selected_rows$FilenameQ90[1]

####
####
selectedFile3 <- selectedFile$FilenameQ90[selectedFile$V1+selectedFile$V2==max(selectedFile$V1+selectedFile$V2)][1]

# 
# # Find the second highest sum value
# 
# # Calculate the sum of V1 and V2 for each row
# sum_values <- selectedFile$V1 + selectedFile$V2
# 
# second_highest_sum <- sum_values[order(sum_values, decreasing = TRUE)[2]]
# 
# # Filter the rows where the sum of V1 and V2 equals the second highest sum
# selected_rows <- selectedFile[sum_values == second_highest_sum, ]
# 
# # Select the first FilenameQ90 from the filtered rows
# selectedFile3.2 <- selected_rows$FilenameQ90[1]


library(magick)
library(grid)

plot_q90_vs_mask <- function(
    selectedFile,
    out_suffix = "_split.png",
    plot = TRUE
) {
  
  ## derive files
  f_left  <- selectedFile
  f_right <- gsub("_q90.jpg", "_q90_mask.png", selectedFile)
  
  if (!file.exists(f_right)) {
    stop("Mask file does not exist:\n", f_right)
  }
  
  ## read images
  im_left  <- image_read(f_left)
  im_right <- image_read(f_right)
  
  ## geometry
  info <- image_info(im_left)
  w <- info$width
  h <- info$height
  
  ## crop halves
  left_half  <- image_crop(im_left,  paste0(w/2, "x", h, "+0+0"))
  right_half <- image_crop(im_right, paste0(w/2, "x", h, "+", w/2, "+0"))
  
  ## combine
  combined <- image_append(c(left_half, right_half))
  
  ## output path
  out_file <- sub("_q90\\.jpg$", out_suffix, selectedFile)
  print(paste("Save split image at",out_file))
  ## save
  image_write(combined, out_file)
  
  ## plot (optional)
  if (plot) {
    grid.newpage()
    grid.raster(combined)
  }
  
  invisible(out_file)
}

plot_q90_vs_mask(selectedFile3)


## =========================================================
## 0. libraries
## =========================================================
library(cowplot)
library(grid)
library(magick)

## =========================================================
## 1. subset selected file
## =========================================================
df_sub <- subset(df_png_files, FilenameQ90 == selectedFile3)


## =========================================================
## 2. corrected location plot function
## =========================================================
require(imager)
require(data.table)



make_location_plot <- function(selectedFile,
                               df_png_files,
                               add_row_info,
                               rel.widths,
                               splitImage = FALSE) {
  
  library(cowplot)
  library(imager)
  library(data.table)
  
  ## ---------------------------------------------------------
  ## defaults (self-contained, no globals)
  ## ---------------------------------------------------------
  cut=0
  cut_above <- cut
  cut_below <- cut
  slope1    <- 0
  tile_size <- 128
  
  ## ---------------------------------------------------------
  ## 1. subset data for this image
  ## ---------------------------------------------------------
  df_sub <- subset(df_png_files, FilenameQ90 == selectedFile)
  df_sub <- as.data.table(df_sub)
  
  ## ---------------------------------------------------------
  ## 1b. compute TRUE window origins (x0,y0) from Q90 image
  ## ---------------------------------------------------------
  img_q90 <- load.image(selectedFile)
  img_df  <- as.data.frame(img_q90)
  setDT(img_df)
  
  ## compute middle_x from image width
  middle_x <- max(img_df$x) / 2
  
  ## vertical crop (kept for consistency)
  img_df <- img_df[
    y < max(y) - cut_above & y > cut_below
  ]
  
  ## slope correction (0 by default → no change)
  img_df[, y_corr := y - (x - middle_x) * slope1]
  
  ## define windows exactly as extraction
  img_df[, x_50 := round(x / tile_size) * tile_size]
  img_df[, y_50 := round(y_corr / tile_size) * tile_size]
  
  ## true spatial origins
  window_origins <- img_df[
    ,
    .(x0 = min(x), y0 = min(y)),
    by = .(x_50, y_50)
  ]
  
  ## ---------------------------------------------------------
  ## 1c. reconstruct window keys in df_sub and merge
  ## ---------------------------------------------------------
  df_sub[, x_50 := round(x_coord / tile_size) * tile_size]
  df_sub[, y_50 := round(y_coord / tile_size) * tile_size]
  
  df_sub <- merge(df_sub, window_origins,
                  by = c("x_50", "y_50"),
                  all.x = TRUE)
  
  ## fallback (rare)
  df_sub[is.na(x0), x0 := x_coord]
  df_sub[is.na(y0), y0 := y_coord]
  
  ## IMPORTANT: override anchors used for plotting
  df_sub[, x_coord := x0]
  # df_sub[, y_coord := y0]
  
  ## ---------------------------------------------------------
  ## 2. produce Soybean and Weed image panels
  ## ---------------------------------------------------------
  make_plots_ncol(
    df_sub[Species != "Weed"],
    ncol   = 1,
    save   = TRUE,
    Specie = "Soybean",
    Title  = df_sub[Species != "Weed", TitelNr]
  )
  
  make_plots_ncol(
    df_sub[Species == "Weed"],
    ncol   = 1,
    save   = TRUE,
    Specie = "Weed",
    Title  = df_sub[Species == "Weed", TitelNr]
  )
  
  ## ---------------------------------------------------------
  ## 3. produce WeedLocation image
  ## ---------------------------------------------------------
  weed_loc_png <- gsub("\\.jpg", ".jpg_WeedLocation.png", selectedFile)
  
  if (splitImage) {
    df_sub[, FilenameQ90 := sub("_q90\\.jpg$", "_split.png", FilenameQ90)]
    weed_loc_png <- gsub("_q90\\.jpg", "_split.png_WeedLocation.png", selectedFile)
  }
  
  draw_and_plot_middle(
    df_sub,
    add_row_info,
    legend = TRUE, axis=F,
    crop_top= 750,
    crop_bottom = 750
  )
  
  ## ---------------------------------------------------------
  ## 4. DEFINE image filenames
  ## ---------------------------------------------------------
  soybean_png <- gsub("\\.jpg", "_Soybean.png", selectedFile)
  weed_png    <- gsub("\\.jpg", "_Weed.png", selectedFile)
  
  ## ---------------------------------------------------------
  ## 5. load images
  ## ---------------------------------------------------------
  first_row <- ggdraw() + draw_image(soybean_png)
  first_row <- plot_grid(
    NULL, first_row, NULL,
    ncol = 1,
    rel_heights = c(0.1, 0.9, 0.1)
  )
  
  second_row <- ggdraw() + draw_image(weed_png)
  second_row <- plot_grid(
    NULL, second_row, NULL,
    ncol = 1,
    rel_heights = c(0.1, 0.9, 0.1)
  )
  
  image <- ggdraw() + draw_image(weed_loc_png)
  
  ## ---------------------------------------------------------
  ## 6. combine
  ## ---------------------------------------------------------
  combined_plot <- plot_grid(
    image,
    first_row,
    second_row,
    ncol = 3,
    rel_widths = rel.widths
  )
  
  ## ---------------------------------------------------------
  ## 7. save final figure
  ## ---------------------------------------------------------
  filename <- gsub("\\.png", "_WeedLocation.png", selectedFile)
  filename <- gsub("\\.jpg", "_WeedLocation.png", filename)
  
  ggsave(
    filename = filename,
    plot     = image,
    width    = 160,
    height   = 100,
    units    = "mm",
    dpi      = 300,
    bg       = "white"
  )
  
  filename_combined <- gsub("_WeedLocation.png", "_WeedCombined.png", filename)
  
  ggsave(
    filename = filename_combined,
    plot     = combined_plot,
    width    = 170,
    height   = 100,
    units    = "mm",
    dpi      = 300,
    bg       = "white"
  )
  
  invisible(combined_plot)
}



## create location image once
make_location_plot(
  selectedFile  = selectedFile3,
  df_png_files  = df_png_files,
  add_row_info  = add_row_info,
  rel.widths    = c(1.5, 0.5, 0.2),
  splitImage=T
)

## =========================================================
## 3. make_stack() — FINAL, MEMORY-SAFE VERSION
## =========================================================
make_stack <- function(files,
                       n_tiles   = 10,
                       tile_size = 0.5,
                       seed      = 123,
                       transform = NULL) {
  
  set.seed(seed)
  
  stack_offsets <- data.frame(
    x = seq(0, 0.025, length.out = n_tiles),
    y = seq(0, 0.025, length.out = n_tiles)
  )
  
  x0 <- min(stack_offsets$x)
  y0 <- min(stack_offsets$y)
  x1 <- max(stack_offsets$x) + tile_size
  y1 <- max(stack_offsets$y) + tile_size
  
  x_range <- x1 - x0
  y_range <- y1 - y0
  
  gg <- ggdraw() + theme(plot.margin = margin(0, 0, 0, 0))
  
  for (i in rev(seq_len(n_tiles))) {
    
    f <- files[((i - 1) %% length(files)) + 1]
    img <- image_read(f)
    
    if (!is.null(transform)) {
      img <- transform(img)
    }
    
    img_grob <- rasterGrob(as.raster(img), interpolate = TRUE)
    
    tile <- grobTree(
      img_grob,
      rectGrob(gp = gpar(col = "black", fill = NA, lwd = 1))
    )
    
    gg <- gg +
      draw_grob(
        tile,
        x      = (stack_offsets$x[i] - x0) / x_range,
        y      = (stack_offsets$y[i] - y0) / y_range,
        width  = tile_size / x_range,
        height = tile_size / y_range,
        clip   = "on"
      )
  }
  
  gg
}

## =========================================================
## 4. on-the-fly mask colorization (0 / 100 / 200)
## =========================================================
swap_mask_100_200 <- function(img) {
  img <- image_convert(img, colorspace = "Gray")
  image_fx(img, expression = "u == (100/255) ? (128/255) : u == (200/255) ? (255/255) : u")
}

swap_mask_128_255 <- function(img) {
  img <- image_convert(img, colorspace = "Gray")
  image_fx(img, expression = "u == (128/255) ? (255/255) : u == (255/255) ? (128/255) : u")
}

## =========================================================
## 5. third_row (1×1, NO repetition)
## =========================================================

third_row_file <- gsub("_q90.jpg", "_split.png_WeedLocation.png", selectedFile3)

# third_row <- make_stack(
#   files   = rep(third_row_file, 10),
#   n_tiles = 10
# )
third_row <-  ggdraw() + draw_image(third_row_file)

## =========================================================
## 6. RGB stack (anchored)
## =========================================================
soy_pool  <- subset(df_png_files[401:500,], color != "#FFD700" & Species == "Soybean")
weed_pool <- subset(df_png_files, color != "#FFD700" & Species == "Weed")

soy_anchor  <- subset(df_sub, Species == "Soybean")
weed_anchor <- subset(df_sub, Species == "Weed")

if (nrow(soy_anchor)  > 0) soy_pool  <- rbind(soy_anchor,  soy_pool)
if (nrow(weed_anchor) > 0) weed_pool <- rbind(weed_anchor, weed_pool)

make_stacked_tiles <- function(soy_pool,
                               weed_pool,
                               n_tiles   = 20,
                               seed      = 123,
                               transform = NULL) {
  
  library(cowplot)
  library(grid)
  library(magick)
  
  set.seed(seed)
  
  ## offsets for stacking
  stack_offsets <- data.frame(
    x = seq(0, 0.025, length.out = n_tiles),
    y = seq(0, 0.025, length.out = n_tiles)
  )
  
  tile_size <- 0.5
  
  x0 <- min(stack_offsets$x)
  y0 <- min(stack_offsets$y)
  x1 <- max(stack_offsets$x) + tile_size
  y1 <- max(stack_offsets$y) + tile_size
  
  x_range <- x1 - x0
  y_range <- y1 - y0
  
  gg <- ggdraw() + theme(plot.margin = margin(0, 0, 0, 0))
  
  for (i in rev(seq_len(n_tiles))) {
    
    ## sample images per tile
    if (i == 1) {
      soy_sel  <- soy_pool[1:min(3, nrow(soy_pool)), ]
      weed_sel <- weed_pool[1:min(1, nrow(weed_pool)), ]
    } else {
      soy_sel  <- soy_pool[sample(nrow(soy_pool), min(3, nrow(soy_pool))), ]
      weed_sel <- weed_pool[sample(nrow(weed_pool), min(1, nrow(weed_pool))), ]
    }
    
    p_sel <- rbind(soy_sel, weed_sel)
    
    ## read images (RGB)
    img_grobs <- lapply(p_sel$File, function(f) {
      img <- image_read(f)
      if (!is.null(transform)) {
        img <- transform(img)
      }
      rasterGrob(as.raster(img), interpolate = TRUE)
    })
    
    ## 2×2 tile
    tile <- grobTree(
      arrangeGrob(
        grobs   = img_grobs,
        ncol    = 2,
        nrow    = 2,
        widths  = unit(c(1, 1), "null"),
        heights = unit(c(1, 1), "null")
      ),
      rectGrob(gp = gpar(col = "black", fill = NA, lwd = 1))
    )
    
    gg <- gg +
      draw_grob(
        tile,
        x      = (stack_offsets$x[i] - x0) / x_range,
        y      = (stack_offsets$y[i] - y0) / y_range,
        width  = tile_size / x_range,
        height = tile_size / y_range,
        clip   = "on"
      )
  }
  
  gg
}



## subset for the selected image
df_sub <- subset(df_png_files, FilenameQ90 == selectedFile3)

## prepend anchors so they appear on top of the stack
soy_pool  <- rbind(
  df_sub[df_sub$Species != "Weed", ],
  soy_pool
)

weed_pool <- rbind(
  df_sub[df_sub$Species == "Weed", ],
  weed_pool
)

## clone pools
soy_pool_mask  <- soy_pool
weed_pool_mask <- weed_pool

## replace RGB → Mask in file paths
soy_pool_mask$File  <- gsub("/RGB/", "/Mask/", soy_pool_mask$File)
weed_pool_mask$File <- gsub("/RGB/", "/Mask/", weed_pool_mask$File)


## build mask tile stack (same logic as RGB)
gg_stack_mask <- make_stacked_tiles(
  soy_pool  = soy_pool_mask,
  weed_pool = weed_pool_mask,
  n_tiles   = 20,
  transform = swap_mask_128_255
  
)


gg_stack_rgb <- make_stacked_tiles(
  soy_pool  = soy_pool,
  weed_pool = weed_pool,
  n_tiles   = 20
)

## =========================================================
## 7. mask files (top image enforced)
## =========================================================
mask_dir <- "./SB016/RGB1/Segmentation/2022_05_25_13_43_Lot3_segmented"

mask_files <- list.files(
  mask_dir,
  pattern = "q90_mask\\.png$",
  full.names = TRUE
)

top_mask <- gsub("_q90.jpg", "_q90_mask.png", selectedFile3)
stopifnot(top_mask %in% mask_files)

set.seed(123)
mask_files_ordered <- c(
  top_mask,
  sample(setdiff(mask_files, top_mask))
)

## =========================================================
## 8. rgb and mask stacks
## =========================================================

gg_mask_stack <- make_stack(
  files   = mask_files_ordered,
  n_tiles = 10,
  transform = swap_mask_100_200
)





## =========================================================
## 9. right panel (ALL STACKS)
## =========================================================
right_part <- plot_grid(
  gg_stack_rgb,
  gg_stack_mask,
  # NULL,
  # gg_mask_stack,
  ncol = 1,
  rel_heights = c(1, 1)
)

## =========================================================
## 10. final combined figure
## =========================================================
combined_plot <- plot_grid(
  third_row, NULL,
  right_part, 
  ncol = 3,
  rel_widths = c(2, 0.01, 0.4),
  labels = c("", "", "", "")
)

## =========================================================
## 11. save
## =========================================================
ggsave(
  filename = "Segment_weeds.png",
  plot     = combined_plot,
  width    = 160,
  height   = 50,
  units    = "mm",
  dpi      = 300,
  bg       = "white"
)

## =========================================================
## =========================================================
## =========================================================
## =========================================================
left_part_s1 <- plot_grid(
  gg_stack_rgb,
  gg_stack_mask,
  # NULL,
  # gg_mask_stack,
  ncol = 2)

gg_stack_mask_s1 <- make_stacked_tiles(
  soy_pool  = soy_pool_mask[-3:-1,],
  weed_pool = weed_pool_mask[-1,],
  n_tiles   = 20,
  transform = swap_mask_128_255
  
)


gg_stack_rgb_s1 <- make_stacked_tiles(
  soy_pool = soy_pool[-3:-1,],
  weed_pool = weed_pool[-1,],
  n_tiles   = 20
)


right_part_s1 <- plot_grid(
  gg_stack_rgb_s1,
  gg_stack_mask_s1,
  # NULL,
  # gg_mask_stack,
  ncol = 2)


gg_stack_mask_s1 <- make_stacked_tiles(
  soy_pool  = soy_pool_mask[-6:-1,],
  weed_pool = weed_pool_mask[-2:-1,],
  n_tiles   = 20,
  transform = swap_mask_128_255
  
)


gg_stack_rgb_s1 <- make_stacked_tiles(
  soy_pool = soy_pool[-6:-1,],
  weed_pool = weed_pool[-2:-1,],
  n_tiles   = 20
)


middle_part_s1 <- plot_grid(
  gg_stack_rgb_s1,
  gg_stack_mask_s1,
  # NULL,
  # gg_mask_stack,
  ncol = 2)

mosaic_plot <- plot_grid(
  left_part_s1,  middle_part_s1, right_part_s1,
  ncol = 3,
  labels = c("", "", "", "")
)


library(magick)
library(grid)

plot_mask_cropped <- function(
    selectedFile,
    crop_top = 100,
    crop_bottom = 100,
    out_suffix = "_cropped.png",
    plot = TRUE
) {
  
  ## derive mask file
  f_mask <- gsub("_q90.jpg", "_q90_mask.png", selectedFile)
  
  if (!file.exists(f_mask)) {
    stop("Mask file does not exist:\n", f_mask)
  }
  
  ## read mask
  im_mask <- image_read(f_mask)
  
  ## geometry
  info <- image_info(im_mask)
  w <- info$width
  h <- info$height
  
  ## new cropped height
  new_h <- h - crop_top - crop_bottom
  
  if (new_h <= 0) {
    stop("Cropping removes entire image.")
  }
  
  ## crop full width, trimmed vertically
  cropped <- image_crop(
    im_mask,
    geometry = paste0(w, "x", new_h, "+0+", crop_top)
  )
  
  ## output path
  out_file <- sub("_q90\\.jpg$", out_suffix, selectedFile)
  
  print(paste("Save cropped mask at", out_file))
  
  ## save
  image_write(cropped, out_file)
  
  ## plot
  if (plot) {
    grid.newpage()
    grid.raster(cropped)
  }
  
  invisible(out_file)
}

plot_mask_cropped(
  selectedFile3,
  crop_top = 1100,
  crop_bottom = 900
)

# mask_file <- gsub("_q90.jpg", "_q90_mask.png", selectedFile3)
mask_file <- gsub("_q90.jpg", "_cropped.png", selectedFile3)
ggMask <-  ggdraw() + draw_image(mask_file)

combined_plot <- plot_grid(
  NULL, ggSoybeanWeedLocation, NULL,
  gg_training_set, NULL,
  mosaic_plot, NULL,
  ggMask,
  ncol = 1,
  rel_heights = c(0.05, 0.575, 0.05, 0.8, 0.06, 0.24, 0.06, 0.43),
  labels = c("A","","B","","C","","D","")
)

ggsave(
  filename = "Segment_weeds_workflow.png",
  plot     = combined_plot,
  width    = 160,
  height   = 260,
  units    = "mm",
  dpi      = 150,
  bg       = "white"
)

## =========================================================
## =========================================================
## =========================================================
## =========================================================
## =========================================================
## =========================================================

Data_rows <- fread("~/public/Evaluation/Projects/KP0023_legumes/Scripts/canopy-cover-stats-lab/data/Soybean_CanopyCover_RawMask_data.csv")
####
nrow(unique(Data_rows[,c("Filename")]))

######

Data_rows_exp <- merge(Data_rows, design_all[!is.na(design_all$genotype.id)&!duplicated(design_all$plot.UID),], by = c("plot.UID"))

nrow(unique(Data_rows_exp[,c("plot.UID")]))
# nrow(unique(Data_rows_exp[,c("genotype.id")]))

nrow(unique(Data_rows_exp[,c("Date","plot.UID","Time")]))


make_loop_plots_with_plot_edges <- function(investigate, Pattern = "_segmentation.png", ncol_plot = 3, Title=T) {
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
          
          
          if(Pattern=="_segmentation.png"){img <- image_convert(img, colorspace = "Gray")
          img_data <- image_data(img)
          img_matrix <- as.numeric(img_data)   # Normalize to range 0-1
          img_matrix[round(img_matrix, 1) == 0] <- 1 
          img_matrix[round(img_matrix, 1) == 0.7] <- 0  # Set 0.5555 to 1
          img_matrix[round(img_matrix, 1) == 0.8] <- 0  # Set 0.5555 to 1
          
          # Convert back to magick image
          img <- image_read(img_matrix)}
          if(Pattern=="_RowsMiddle.png"){imagePNG <- img}else{
            
            imagePNG <- image_scale(img, "50%")}
          
          image_info_df <- image_info(img)  # Extract image metadata
          
          maxY <- image_info_df$height[1]  # Correctly extract image height
          maxX <- image_info_df$width[1]   # Correctly extract image width
          
          g <- rasterGrob(imagePNG, interpolate = T)
          
          files_to_plot$i <- 1:nrow(files_to_plot)
          i <- files_to_plot$i[files_to_plot$File==file]
          points_data <- data.frame(
            x = c(0, 0, maxX, maxX),
            y = c(
              maxY - files_to_plot$min_y_at_x0[i], 
              maxY - files_to_plot$max_y_at_x0[i], 
              maxY - files_to_plot$min_y_at_xmax[i], 
              maxY - files_to_plot$max_y_at_xmax[i]
            ),
            Row_middle = c(
              files_to_plot$Row_middle_1[i],
              files_to_plot$Row_middle_2[i],
              files_to_plot$Row_middle_3[i],
              files_to_plot$Row_middle_4[i]
            )
          )
          
          points_data$Row_middle <-  maxY- points_data$Row_middle
          
          ggimage_plot <- ggplot(points_data, aes(x,y),geom="blank") +
            scale_y_continuous(limits=c(0,maxY)) +
            scale_x_continuous(limits=c(0,maxX)) +
            theme(legend.position="none",axis.text = element_blank(), axis.ticks = element_blank(), axis.title= element_blank(),
                  plot.margin=unit(c(0,0,0,0),"line")) +
            annotation_custom(g, xmin=0, xmax=maxX, ymin=0, ymax=maxY) +
            # geom_hline(yintercept = c(points_data$Row_middle))+
            # geom_image(image=file)+
            geom_point(aes(x,y), color="darkred", size=2,shape=13)+
            geom_segment(aes(x = 0, xend = maxX, y =  maxY - files_to_plot$min_y_at_x0[i], yend =  maxY - files_to_plot$min_y_at_xmax[i]),
                         linetype = "dashed", color = "white") +
            geom_segment(aes(x = 0, xend = maxX, y =  maxY - files_to_plot$max_y_at_x0[i], yend =  maxY - files_to_plot$max_y_at_xmax[i]),
                         linetype = "dashed", color = "white") +
            coord_fixed()
          
          return(ggimage_plot)
        }
      })
      
      gg <- plot_grid(plotlist = plots, ncol = ncol_plot, labels = as.character(files_to_plot$TitleNr), label_size = 6, label_colour = "#1965B0")
      title <- ggdraw() + draw_label(paste("Plot:", files_to_plot$UID[1]), fontface = 'bold', size = 10)
      final_plot <- plot_grid(title, gg, ncol = 1, rel_heights = c(0.1, 1))
      return(final_plot)
    }
  }
  
  ToPlot <- investigate#data.table(
  ToPlot$File = paste0("~/public/Evaluation/Projects/KP0023_legumes/Soybean/",year, gsub("\\.\\/","\\/",investigate$Filename_org))
  ToPlot$TitleNr = investigate$Date
  ToPlot$UID = investigate$plot.UID
  # genotype.name = investigate$genotype.name
  # )
  ToPlot$File <- gsub("_segmentation.png",Pattern, ToPlot$File)
  ToPlot <- ToPlot[order(UID, TitleNr)]
  
  if (nrow(ToPlot) == 0) {
    print(paste("Skip", year))
  } else {
    gg_list <- lapply(unique(ToPlot$UID), function(xx) make_plots(files_to_plot=ToPlot[UID == xx], ncol_plot))
    
    NperPlot <- ToPlot[, .(N = .N), by = .(UID)]
    NperPlot[, N_ceiling := ceiling(N / ncol_plot)]
    nrow_plot_overall <- sum(NperPlot$N_ceiling)
    
    final_plot <- grid.arrange(grobs = gg_list, ncol = 1)
    if(Title!=F){    title <- ggdraw() + draw_label(paste("Genotype:", ToPlot$genotype.name[1]), fontface = 'bold', size = 14)
    final_plot <- plot_grid(title, final_plot, ncol = 1, rel_heights = c(0.2, 1))}
    
    ggsave(
      filename = paste0(year, Pattern, ToPlot$genotype.name[1], "_checks.png"),
      plot = final_plot,
      dpi = 150,
      units = "mm",
      height = nrow_plot_overall * 60 + 1,
      width = 87 * ncol_plot
    )
    return(final_plot)
  }
}


investigate <- subset(Data_rows_exp, plot.UID=="FPSB0160014")
investigate <- subset(investigate, plot.UID==investigate$plot.UID[1])
investigate <- investigate[!duplicated(investigate$Filename),]
# ggSeg <- make_loop_plots_with_plot_edges(investigate, Pattern = "_segmentation.png", ncol_plot = 6, Title=F)
p <- investigate
# p$min_y_at_x0 <- NA
# p$max_y_at_x0 <- NA
# p$min_y_at_xmax <- NA
# p$max_y_at_xmax <- NA
# ggQ90 <- make_loop_plots_with_plot_edges(p, Pattern = "_q90.jpg", ncol_plot = 6, Title=T)

ggQ90 <- make_loop_plots_with_plot_edges(p, Pattern = "_q90.jpg", ncol_plot = 8, Title=F)
ggsave("Data_Plot_over_time_FPSB0160014.png",  width = 150, height = 50, units = "mm", ggQ90)
## =========================================================
## =========================================================



# 
# combined_plot <-plot_grid(first_row,second_row,third_row, ncol =  1, labels = "") #c("B","C","D")
# 
# image <-  ggdraw() + draw_image(gsub("\\.jpg",".jpg_WeedLocation.png",selectedFile1))
# 
# combined_plot <-plot_grid(image,  combined_plot,
#                           ncol =  2, rel_widths = c(1,0.25),  labels = c("A",""))
# 
# ggsave("plot_selected_locations.png",  width = 170, height = 100, units = "mm", dpi = 300, bg="white",combined_plot)
# 

# Plot the final co
# Load necessary packages
library(magick)
library(cowplot)
library(ggplot2)
library(imager)

# Read the PNG image
# image <-  ggdraw() + draw_image("./SB016/RGB1/Segmentation/2022_06_07_08_51_Lot3_segmented/FPSB0160051_RGB1_20220607_085840_q90.jpg_WeedLocation.png")
# 
# # first_row <- plot_grid(NULL,  ggWeed1, NULL, nrow =  3, rel_heights  = c(0.1,1,0.1),  labels = c(""))
# # first_row <- plot_grid(NULL,  first_row , ncol =  2, rel_widths =  c(0.1,1),  labels = c(""))
# # # Draw the image and the ggplot object using cowplot
# 
# ggWeed1 <-  load.image("./SB016/RGB1/Segmentation/2022_06_07_08_51_Lot3_segmented/FPSB0160051_RGB1_20220607_085840_q90_weeds.png")
# 
# png("./SB016/RGB1/Segmentation/2022_06_07_08_51_Lot3_segmented/WeedsToPlot.png", width =  3*126, height =  400*2)
# plot(ggWeed1)
# title( xlab = "X Coordinate (Pixel)", ylab = "Y Coordinate (Pixel)")
# dev.off()
# dev.off()
# 
# first_row <-  ggdraw() + draw_image("./SB016/RGB1/Segmentation/2022_06_07_08_51_Lot3_segmented/WeedsToPlot.png")



combined_plot <-plot_grid(first_row,second_row,
                          ncol =  2, rel_widths = c(1,0.35),  labels = c("AUTO"))

# Plot the final combined plot

ggsave("plot_selected_weeds.png",  width = 170, height = 80, units = "mm", dpi = 300, bg="white",combined_plot)

#######

first_row <-  ggdraw() + draw_image("./PXL_20220708_123501419.jpg")
second_row <-  ggdraw() + draw_image("~/public/Evaluation/Projects/KP0023_legumes/Scripts/stats-lab-crops/2022_q90.jpg_row3_plot_extremes.png")

combined_plot <-plot_grid(first_row, NULL, second_row,
                          ncol =  3, rel_widths = c(0.15,0.05,1),  labels = c("A","","B"),label_size = 9)

ggsave("FIP_images_example.png",  width = 170, height = 40, units = "mm", dpi = 300, bg="white",combined_plot)

##################

investigate <- subset(Data_rows_exp, Year == 2022 & genotype.name == "CH22655" )
investigate <- subset(investigate, plot.UID == investigate$plot.UID[1])
investigate <- investigate[!duplicated(investigate$Filename), ]

# Filter to early growth period
investigate <- subset(investigate, Date > as.Date("2022-06-01") & Date < as.Date("2022-06-23"))
investigate$Filename_org <- gsub("_q90_mask.png","_q90.jpg",investigate$Filename_org )
# Add placeholders (required by make_loop_plots_with_plot_edges)
investigate$min_y_at_x0 <- NA
investigate$max_y_at_x0 <- NA
investigate$min_y_at_xmax <- NA
investigate$max_y_at_xmax <- NA

# --- Generate plots ---
ggQ90_CH22650 <- make_loop_plots_with_plot_edges(investigate, Pattern = "_q90.jpg", ncol_plot = 6, Title = TRUE)

investigate$Filename_org <- gsub("_q90.jpg","_mask.png",investigate$Filename_org )
ggSeg_CH22650 <- make_loop_plots_with_plot_edges(investigate, Pattern = "_mask.png", ncol_plot = 6, Title = FALSE)

combined_CH22650 <- plot_grid(
  ggQ90_CH22650, ggSeg_CH22650,
  ncol = 1,
  rel_heights = c(1.15, 1),
  labels = c(""),
  align = "v", axis = "l"
)

# --- Save figure ---
ggsave(
  filename = "CH22655_growth_masks_2022.png",
  plot = combined_CH22650,
  width = 160, height = 240, units = "mm", dpi = 300
)
######

investigate <- subset(Data_rows_exp, Year == 2022 & genotype.name == "Gallec" )
investigate <- subset(investigate, plot.UID == investigate$plot.UID[1])
investigate <- investigate[!duplicated(investigate$Filename), ]

# Filter to early growth period
investigate <- subset(investigate, Date > as.Date("2022-06-01") & Date < as.Date("2022-06-23"))
investigate$Filename_org <- gsub("_q90_mask.png","_q90.jpg",investigate$Filename_org )
# Add placeholders
investigate$min_y_at_x0 <- NA
investigate$max_y_at_x0 <- NA
investigate$min_y_at_xmax <- NA
investigate$max_y_at_xmax <- NA

# --- Generate plots ---
ggQ90_CH22691 <- make_loop_plots_with_plot_edges(investigate, Pattern = "_q90.jpg", ncol_plot = 6, Title = TRUE)

investigate$Filename_org <- gsub("_q90.jpg","_mask.png",investigate$Filename_org )
ggSeg_CH22691 <- make_loop_plots_with_plot_edges(investigate, Pattern = "_mask.png", ncol_plot = 6, Title = FALSE)

combined_CH22691 <- plot_grid(
  ggQ90_CH22691, ggSeg_CH22691,
  ncol = 1,
  rel_heights = c(1.15, 1),
  labels = c(""),
  align = "v", axis = "l"
)

# --- Save figure ---
ggsave(
  filename = "Gallec_growth_masks_2022.png",
  plot = combined_CH22691,
  width = 160, height = 240, units = "mm", dpi = 300
)

library(cowplot)
library(ggplot2)

# # --- Read in both PNGs ---
# img_CH22650 <- png::readPNG("CH22655_growth_masks_2022.png")
# img_CH22691 <- png::readPNG("Gallec_growth_masks_2022.png")
# 
# # Convert to ggplot objects
# gg_CH22650 <- ggdraw() + draw_image(img_CH22650)
# gg_CH22691 <- ggdraw() + draw_image(img_CH22691)

# --- Combine vertically ---
combined_extremes <- plot_grid(
  combined_CH22691,combined_CH22650,
  ncol = 1, labels = c("AUTO"), label_fontface = "bold",
  rel_heights = c(1, 1),
  align = "v", axis = "l"
)

# --- Save combined figure ---
ggsave(
  filename = "Data_PlotMasks_over_time.png",
  plot = combined_extremes,
  width = 170, height = 140, units = "mm", dpi = 150
)
##################

##
folder <- folders_train2022[4]

png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
png_files_soy <- png_files[!grepl("asWeed",png_files)]
png_files_soy
png_files_list_soy <- make_100er_plots(png_files_soy, save=T)
png_files_exclude_soy <- png_files_list_soy[c(2,13,44,60,84,88,116,120,126,129,144,145:148),]
png_files_exclude_soy$Species <- "Soybean"
# make_plots_ncol(png_files_exclude_soy, ncol=10,save=F,Specie="Soybean",Title=png_files_exclude_soy$TitleNr) # check
p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check


png_files_weed <- png_files[grepl("asWeed",png_files)]
png_files_weed
png_files_list_weed <- make_100er_plots(png_files_weed, save=T)
png_files_exclude_weed<- png_files_list_weed[c(2),]
# png_files_exclude_weed$TitleNr <- NA
# png_files_exclude_weed$File <- NA
# make_plots(png_files_exclude_weed) # check
# png_files_exclude_weed$Species <- "Weed"
# make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check
file_excluded_4 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)
############

##
folder <- folders_train2022[5]
png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
png_files_soy <- png_files[!grepl("asWeed",png_files)]
png_files_soy
png_files_list_soy <- make_100er_plots(png_files_soy, save=T)
png_files_exclude_soy <- png_files_list_soy[c(62,69,70,95,99,101,114,119,121,129,134,142,146:148,154),]
png_files_exclude_soy$Species <- "Soybean"
# make_plots_ncol(png_files_exclude_soy, ncol=10,save=F,Specie="Soybean",Title=png_files_exclude_soy$TitleNr) # check
p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check



png_files_weed <- png_files[grepl("asWeed",png_files)]
png_files_weed
png_files_list_weed <- make_100er_plots(png_files_weed, save=T)
png_files_exclude_weed<- png_files_list_weed[c(2),]
# png_files_exclude_weed$TitleNr <- NA
# png_files_exclude_weed$File <- NA
# make_plots(png_files_exclude_weed) # check
png_files_exclude_weed$Species <- "Weed"
# make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
# p <- data.frame(File=png_files_weed, TitleNr=1:length(png_files_weed),Species = "Weed")
# p <- subset(p, !File%in%png_files_exclude_weed$File)
# make_plots_ncol(p, ncol=10,save=F,Specie="Weed",Title=p$TitleNr) # check with TitleNr
p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check
file_excluded_5 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)

############
##
# folder <- folders_train2022[6]
# 
# png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
# png_files_soy <- png_files[!grepl("asWeed",png_files)]
# png_files_soy
# png_files_list_soy <- make_100er_plots(png_files_soy, save=T)
# png_files_exclude_soy <- png_files_list_soy[c(1,19,21,22,24,26,28,103,109,121,261),] #  
# png_files_exclude_soy$Species <- "Soybean"
# # make_plots_ncol(png_files_exclude_soy, ncol=10,save=F,Specie="Soybean",Title=png_files_exclude_soy$TitleNr) # check
# p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
# make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check
# 
# 
# 
# png_files_weed <- png_files[grepl("asWeed",png_files)]
# png_files_weed
# png_files_list_weed <- make_100er_plots(png_files_weed, save=T)
# png_files_exclude_weed <- png_files_list_weed[c(5,6,19,25,27:35,41,43,46:77,79:81,84,85,91,94,97:109,112,114:125,130:143,146,147,150,152,153,155),]
# # make_plots(png_files_exclude_weed) # check
# png_files_exclude_weed$Species <- "Weed"
# # make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
# # p <- data.frame(File=png_files_weed, TitleNr=1:length(png_files_weed),Species = "Weed")
# # p <- subset(p, !File%in%png_files_exclude_weed$File)
# # make_plots_ncol(p, ncol=10,save=F,Specie="Weed",Title=p$TitleNr) # check with TitleNr
# p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
# make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check
# file_excluded_6 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)
#######

file_excluded_2022 <- c(file_excluded_1,file_excluded_2,file_excluded_3,file_excluded_4,file_excluded_5)
file_excluded_2022 <- file_excluded_2022[!is.na(file_excluded_2022)]
# write.csv(file_excluded_2022, "file_excluded_2022.csv", row.names = F,quote = F)

# file_excluded_2022 <- read.csv("file_excluded_2022.csv")


### make collage to train
# create a Collage with RGB and Mask folder inside manually if error
moscais_2022 <- get_training_pictures(folders=folders_train2022[1:5], augment_weed_dir=NA, filename_to_exclude=file_excluded_2022)
# dev.off()
# toSearch <- moscais_2022$fnames_Leaves[moscais_2022$Number_r2==119]
# folder <- folders_train2022[3]
# png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
# c(1:length(png_files))[png_files%in%toSearch]



# p <- correct_collage(getwd())
# hist(p$values)

# lapply(1:4, function(x) get_training_pictures_single(x, folders_train2022[2:5], filename_to_exclude))



### run semantic segmentation in python

##
get_pixels1 <- lapply(folders_growth[1:3], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 750, Row_distance_min=300,LeaveSpecies="Soybean", max_number_of_rows=4 ,min_cluster=3 ))

get_pixels1 <- lapply(folders_growth[4:7], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 750, Row_distance_min=400,LeaveSpecies="Soybean", max_number_of_rows=4 ,min_cluster=3 ))

get_pixels2 <- lapply(folders_growth[8:10], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 750, Row_distance_min=600,LeaveSpecies="Soybean", max_number_of_rows=4,min_cluster=3  ))
get_pixels2 <- lapply(folders_growth[11:12], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=650,LeaveSpecies="Soybean", max_number_of_rows=4,min_cluster=3  ))
get_pixels2 <- lapply(folders_growth[13], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=700,LeaveSpecies="Soybean", max_number_of_rows=4,min_cluster=3  ))

# get_pixels1 <- lapply(folders_train[4:7], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 750, Row_distance_min=400,LeaveSpecies="Soybean", max_number_of_rows=4 ,min_cluster=3 ))
# get_pixels2 <- lapply(folders_train[8:10], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 750, Row_distance_min=600,LeaveSpecies="Soybean", max_number_of_rows=4,min_cluster=3  ))
# get_pixels2 <- lapply(folders_train[11:13], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 750, Row_distance_min=800,LeaveSpecies="Soybean", max_number_of_rows=4,min_cluster=3  ))



p <- rbind(rbindlist(get_pixels1),rbindlist(get_pixels2))
# write.csv(p, "data_rows_All_2022_1-9.csv",  quote=F)

###########################

Plot_borders_mean <- get_plot_borders_mean(pattern="ows.csv")


folders_canopy <- c(folders[grepl("2022_06_",folders)][8:9],folders[grepl("2022_07_",folders)],folders[grepl("2022_08_",folders)])

lapply(folders_canopy, function(x)get_green_canopy_plot_cover(folder_ii = x, Plot_borders_mean))

folders_pods <- c(folders[grepl("2022_09_",folders)])

# folders_pods <- rev(folders_pods)[1:2]

lapply(folders_pods, function(x)get_green_canopy_plot_cover(folder_ii = x, Plot_borders_mean))
#############################


###
data_UID_date <- fread("~/public/Evaluation/Projects/KP0023_legumes/Scripts/stats-lab-crops/data/soybean_pixels_data.csv")
data_UID_date <- subset(data_UID_date, genotype.id==as.integer(10009))
data_UID_date <- unique(data_UID_date[,c("Date","plot.UID","year_site.UID","Year","genotype.id")])
data_UID_date$year <- data_UID_date$Year
data_UID_date$date <- data_UID_date$Date
data_UID_date$UID <- data_UID_date$plot.UID

lapply(unique(data_UID_date$Year), function(x) make_loop_plots(x, data_UID_date, RGB="_q90.jpg",n_row=3) ) 

####


#############################

#############################

setwd("~/Soybean/2021")

foldersAll <- list.dirs(full.names = TRUE)
folders <- foldersAll[grepl("_segmented",foldersAll)]
folders <- folders[!grepl("Mask",folders)]
folders <- folders[!grepl("_segmented/RGB",folders)]
folders <- folders[!grepl("old",folders)]
folders <- folders[!grepl("lightning_logs",folders)]

folders_train <- folders[grepl("2021_06_",folders)]
folders_train <- folders_train[order(folders_train)]
folders_train2021 <- folders_train[1:4]
folders_train2021

get_pixels <- lapply(folders_train2021[1:2], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=T, cut = 500, Row_distance_min=400 ,LeaveSpecies="Soybean", max_number_of_rows=5 ,min_cluster=3 ))
get_pixels <- lapply(folders_train2021[3], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=T, cut = 500, Row_distance_min=600 ,LeaveSpecies="Soybean", max_number_of_rows=5 ,min_cluster=3 ))


##

##
folder <- folders_train2021[1]
png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
png_files_soy <- png_files[!grepl("asWeed",png_files)]
png_files_soy
png_files_list_soy <- make_100er_plots(png_files_soy, save=T)
png_files_exclude_soy <- png_files_list_soy[c(112,166,178,179,269,279,319,320,523,609,1111),]
png_files_exclude_soy$Species <- "Soybean"
# make_plots_ncol(png_files_exclude_soy, ncol=10,save=F,Specie="Soybean",Title=png_files_exclude_soy$TitleNr) # check
p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check

png_files_weed <- png_files[grepl("asWeed",png_files)]
png_files_weed
png_files_list_weed <- make_100er_plots(png_files_weed, save=T)
png_files_exclude_weed <- png_files_list_weed[c(1),]
png_files_exclude_weed$TitleNr <- NA #exclude nothing
png_files_exclude_weed$File <- NA #exclude nothing

# make_plots(png_files_exclude_weed) # check
# png_files_exclude_weed$Species <- "Weed"
# make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
# p <- data.frame(File=png_files_weed, TitleNr=1:length(png_files_weed),Species = "Weed")
# p <- subset(p, !File%in%png_files_exclude_weed$File)
# make_plots_ncol(p, ncol=10,save=F,Specie="Weed",Title=p$TitleNr) # check with TitleNr
p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
make_100er_plots(png_files=p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check
file_excluded_1 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)

##
folder <- folders_train2021[2]
png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
png_files_soy <- png_files[!grepl("asWeed",png_files)]
# set.seed(123)
# png_files_soy <- png_files_soy[sample(1:length(png_files_soy), 600)]
## 1201 lanzelot leaf type
png_files_list_soy <- make_100er_plots(png_files_soy, save=T)
png_files_exclude_soy <- png_files_list_soy[c(4,16,134,135,157,191,196,223,236,246:248,256:259,261,262,301,303,350,409,410,472,480,501,682,700,796,817,818,
                                              1051,1052,1055,1099,1112,1114,1149,1150,1153,1159,1163:1165,1176,1203,1236,1280,1339:1342,1354,1386,1401,1422,1444,
                                              1639:1642,1715,1757,1812,1915),]
png_files_exclude_soy$Species <- "Soybean"
# make_plots_ncol(png_files_exclude_soy, ncol=10,save=F,Specie="Soybean",Title=png_files_exclude_soy$TitleNr) # check
p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check

png_files_weed <- png_files[grepl("asWeed",png_files)]
png_files_weed
png_files_list_weed <- make_100er_plots(png_files_weed, save=T)

# x <- c(201,239:241,245,247)
# y <- 101:length(png_files_weed)
# exclude <- y[-x]

png_files_exclude_weed <- png_files_list_weed[c(394:397),] # 
# make_plots(png_files_exclude_weed) # check
png_files_exclude_weed$Species <- "Weed"
# make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
# p <- data.frame(File=png_files_weed, TitleNr=1:length(png_files_weed),Species = "Weed")
# p <- subset(p, !File%in%png_files_exclude_weed$File)
# make_plots_ncol(p, ncol=10,save=F,Specie="Weed",Title=p$TitleNr) # check with TitleNr
p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check
file_excluded_2 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)

##
# folder <- folders_train2021[3]
# png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
# png_files_soy <- png_files[!grepl("asWeed",png_files)]
# png_files_soy
# png_files_soy <- png_files_soy[1:500]
# png_files_list_soy <- make_100er_plots(png_files_soy, save=T)
# png_files_exclude_soy <- png_files_list_soy[c(31,198,220,281,308,329,341,492),]
# png_files_exclude_soy$Species <- "Soybean"
# # make_plots_ncol(png_files_exclude_soy, ncol=10,save=F,Specie="Soybean",Title=png_files_exclude_soy$TitleNr) # check
# p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
# make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check
# 
# png_files_weed <- png_files[grepl("asWeed",png_files)]
# png_files_weed
# png_files_list_weed <- make_100er_plots(png_files_weed, save=T)
# 
# x <- c(2,16,17,21,24,25,28,29,35,37,59,61:62,66:69,71,72,86:91,107,114,118,125,130,131,139,140,165:167,174,185,186,189,193,194,199)
# y <- 1:length(png_files_weed)
# exclude <- y[-x]
# 
# png_files_exclude_weed <- png_files_list_weed[c(exclude),]
# # make_plots(png_files_exclude_weed) # check
# png_files_exclude_weed$Species <- "Weed"
# make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
# # p <- data.frame(File=png_files_weed, TitleNr=1:length(png_files_weed),Species = "Weed")
# # p <- subset(p, !File%in%png_files_exclude_weed$File)
# # make_plots_ncol(p, ncol=10,save=F,Specie="Weed",Title=p$TitleNr) # check with TitleNr
# p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
# make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check
# file_excluded_3 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)


##
file_excluded_2021 <- c(file_excluded_1,file_excluded_2) #file_excluded_3
file_excluded_2021 <- file_excluded_2021[!is.na(file_excluded_2021)]

# write.csv(file_excluded_2021, "file_excluded_2021.csv", row.names = F,quote = F)
##


##
mosaics_2021<- get_training_pictures(folders_train2021[1:2], augment_weed_dir=NA, filename_to_exclude=file_excluded_2021)


# p <- correct_collage(getwd())
# hist(p$values)
##

folders_growth <- c(folders[grepl("2021_05_",folders)],folders[grepl("2021_06_",folders)])

get_pixels <- lapply(folders_growth[1:7], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=300,LeaveSpecies="Soybean",max_number_of_rows=5,min_cluster=3  ))
get_pixels <- lapply(folders_growth[8:14], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=400,LeaveSpecies="Soybean",max_number_of_rows=5,min_cluster=3  ))
get_pixels <- lapply(folders_growth[15:17], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=600,LeaveSpecies="Soybean",max_number_of_rows=5,min_cluster=3  ))
get_pixels <- lapply(folders_growth[18], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 100, Row_distance_min=700,LeaveSpecies="Soybean",max_number_of_rows=5,min_cluster=3  ))
# get_pixels <- lapply(folders_growth[19:21], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 100, Row_distance_min=750,LeaveSpecies="Soybean",max_number_of_rows=5,min_cluster=3  ))

######

Plot_borders_mean <- get_plot_borders_mean(pattern="ows.csv")


folders_canopy <- c(folders[grepl("2021_07_",folders)],folders[grepl("2021_08_",folders)],folders[grepl("2021_09_",folders)][1])

lapply(folders_canopy, function(x)get_green_canopy_plot_cover(folder_ii = x, Plot_borders_mean))

folders_pods <- c(folders[grepl("2021_09_",folders)])

# folders_pods <- rev(folders_pods)[1:2]

lapply(folders_pods, function(x)get_green_canopy_plot_cover(folder_ii = x, Plot_borders_mean))
#############################

#######################################

setwd("~/Soybean/2015")

foldersAll <- list.dirs(full.names = TRUE)
folders <- foldersAll[grepl("_segmented",foldersAll)]
folders <- folders[!grepl("Mask",folders)]
folders <- folders[!grepl("_segmented/RGB",folders)]
folders <- folders[grepl("RGB1",folders)]


folders_growth <- c(folders[grepl("2015_05_",folders)],folders[grepl("2015_06_",folders)],folders[grepl("2015_07_",folders)][1])
folders_growth <- folders_growth[order(folders_growth)]
folders_growth

estimate_row_distance_pixel(working_dir=folders_growth[3],image_number=1) 

estimate_row_distance_pixel(working_dir=folders_growth[length(folders_growth)],image_number=6) 

#Row_distance_min is here plot width
get_pixels <- lapply(folders_growth[1:4], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 0, Row_distance_min=1100,LeaveSpecies="Soybean",max_number_of_rows=3,min_cluster=1 ))
get_pixels <- lapply(folders_growth[5:6], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=1100,LeaveSpecies="Soybean",max_number_of_rows=3,min_cluster=1 ))


#######
Plot_borders_mean <- get_plot_borders_mean(pattern="ows.csv")


folders_canopy <- c(folders[grepl("2015_07_",folders)],folders[grepl("2015_08_",folders)])

lapply(folders_canopy, function(x)get_green_canopy_plot_cover(folder_ii = x, Plot_borders_mean))

folders_pods <- c(folders[grepl("2015_09_",folders)])

# folders_pods <- rev(folders_pods)[1:2]

lapply(folders_pods, function(x)get_green_canopy_plot_cover(folder_ii = x, Plot_borders_mean))
#############################

#######################################

setwd("~/Soybean/2016")

foldersAll <- list.dirs(full.names = TRUE)
folders <- foldersAll[grepl("_segmented",foldersAll)]
folders <- folders[!grepl("Mask",folders)]
folders <- folders[!grepl("_segmented/RGB",folders)]
folders <- folders[grepl("RGB1",folders)]


folders_growth <- c(folders[grepl("2016_05_",folders)],folders[grepl("2016_06_",folders)],folders[grepl("2016_07_",folders)][1:2])
folders_growth <- folders_growth[order(folders_growth)]
folders_growth

estimate_row_distance_pixel(working_dir=folders_growth[9],image_number=6) 

#Row_distance_min is here plot width
get_pixels <- lapply(folders_growth, function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=1000,LeaveSpecies="Soybean",max_number_of_rows=3 ,min_cluster=1 ))

#######
Plot_borders_mean <- get_plot_borders_mean(pattern="ows.csv")
folders_canopy <- c(folders[grepl("2016_07_",folders)],folders[grepl("2016_08_",folders)],folders[grepl("2016_09_",folders)])
lapply(folders_canopy, function(x)get_green_canopy_plot_cover(folder_ii = x, Plot_borders_mean))
#######################################

setwd("~/Soybean/2017")

foldersAll <- list.dirs(full.names = TRUE)
folders <- foldersAll[grepl("_segmented",foldersAll)]
folders <- folders[!grepl("Mask",folders)]
folders <- folders[!grepl("_segmented/RGB",folders)]
folders <- folders[!grepl("_inc_",folders)]
folders <- folders[!grepl("_parts_",folders)]
folders <- folders[grepl("SB007",folders)]

folders_growth <- c(folders[grepl("2017_06_",folders)],folders[grepl("2017_05_",folders)],folders[grepl("2017_07_",folders)][1:3])

estimate_row_distance_pixel(working_dir=folders_growth[3],image_number=1) 

#Row_distance_min is here plot width
get_pixels <- lapply(folders_growth, function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 0, Row_distance_min=1200,LeaveSpecies="Soybean",max_number_of_rows=3,min_cluster=1  ))
#######
Plot_borders_mean <- get_plot_borders_mean(pattern="ows.csv")
folders_canopy <- c(folders[grepl("2017_07_",folders)],folders[grepl("2017_08_",folders)],folders[grepl("2017_09_",folders)])
lapply(folders_canopy, function(x)get_green_canopy_plot_cover(folder_ii = x, Plot_borders_mean))


#######################################

setwd("~/Soybean/2018")

foldersAll <- list.dirs(full.names = TRUE)
folders <- foldersAll[grepl("_segmented",foldersAll)]
folders <- folders[!grepl("Mask",folders)]
folders <- folders[!grepl("_segmented/RGB",folders)]

folders_growth <- c(folders[grepl("2018_05_",folders)],folders[grepl("2018_06_",folders)])
folders_growth <- folders_growth[order(folders_growth)]

estimate_row_distance_pixel(working_dir=folders_growth[2],image_number=1) 

get_pixels <- lapply(folders_growth[1:4], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=400,LeaveSpecies="Soybean", max_number_of_rows=4 ,min_cluster=3  ))
get_pixels <- lapply(folders_growth[5], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=600,LeaveSpecies="Soybean", max_number_of_rows=4 ,min_cluster=3  ))
# get_pixels <- lapply(folders_growth[6], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=1000,LeaveSpecies="Soybean", max_number_of_rows=4 ,min_cluster=3  ))

#######
Plot_borders_mean <- get_plot_borders_mean(pattern="ows.csv")
folders_canopy <- c(folders_growth[6:8],folders[grepl("2018_07_",folders)],folders[grepl("2018_08_",folders)],folders[grepl("2018_09_",folders)])
lapply(folders_canopy, function(x)get_green_canopy_plot_cover(folder_ii = x, Plot_borders_mean))

#######################################

setwd("~/Soybean/2019")

foldersAll <- list.dirs(full.names = TRUE)
folders <- foldersAll[grepl("_segmented",foldersAll)]
folders <- folders[!grepl("Mask",folders)]
folders <- folders[!grepl("_segmented/RGB",folders)]


folders_growth <- c(folders[grepl("2019_05_",folders)],folders[grepl("2019_06_",folders)],folders[grepl("2019_07_",folders)][1:2])
folders_growth <- folders_growth[order(folders_growth)]


estimate_row_distance_pixel(working_dir=folders_growth[2],image_number=20) 
estimate_row_distance_pixel(working_dir=folders_growth[length(folders_growth)],image_number=6) 


get_pixels <- lapply(folders_growth[1:2], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 750, Row_distance_min=400,LeaveSpecies="Soybean", max_number_of_rows=4 ,min_cluster=3  ))
get_pixels <- lapply(folders_growth[3], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 50, Row_distance_min=600,LeaveSpecies="Soybean", max_number_of_rows=4 ,min_cluster=3  ))
get_pixels <- lapply(folders_growth[4], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 50, Row_distance_min=800,LeaveSpecies="Soybean", max_number_of_rows=4 ,min_cluster=3  ))

#######
Plot_borders_mean <- get_plot_borders_mean(pattern="ows.csv")
folders_canopy <- c(folders[grepl("2019_07_",folders)],folders[grepl("2019_08_",folders)],folders[grepl("2019_09_",folders)])
lapply(folders_canopy, function(x)get_green_canopy_plot_cover(folder_ii = x, Plot_borders_mean))


#######
setwd("~/Soybean/2020")

foldersAll <- list.dirs(full.names = TRUE)
folders <- foldersAll[grepl("_segmented",foldersAll)]
folders <- folders[!grepl("Mask",folders)]
folders <- folders[!grepl("_segmented/RGB",folders)]
folders <- folders[!grepl("old",folders)]


folders_growth <- c(folders[grepl("2020_05_",folders)],folders[grepl("2020_06_",folders)])
folders_growth <- folders_growth[order(folders_growth)]

folders_train2020 <- folders_growth[3:7]

estimate_row_distance_pixel(working_dir=folders_growth[6],image_number=20) 
estimate_row_distance_pixel(working_dir=folders_growth[length(folders_growth)-1],image_number=1) 


get_pixels <- lapply(folders_train2020, function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=T, cut = 500, Row_distance_min=400,LeaveSpecies="Soybean", max_number_of_rows=5 ,min_cluster=3  ))

##

folder <- folders_train2020[1]
png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
png_files_soy <- png_files[!grepl("asWeed",png_files)]
set.seed(123)
png_files_soy <- png_files_soy[sample(1:length(png_files_soy), 500)]
png_files_soy

png_files_list_soy <- make_100er_plots(png_files_soy, save=T)
png_files_exclude_soy <- png_files_list_soy[c(1,122,169,337,439,451,493),]
png_files_exclude_soy$Species <- "Soybean"
# make_plots_ncol(png_files_exclude_soy, ncol=10,save=F,Specie="Soybean",Title=png_files_exclude_soy$TitleNr) # check
p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check

png_files_weed <- png_files[grepl("asWeed",png_files)]
png_files_weed
png_files_list_weed <- make_100er_plots(png_files_weed, save=T)
png_files_exclude_weed <- png_files_list_weed[c(1,7,14,16:57,72:85,87:105,108),]
# make_plots(png_files_exclude_weed) # check
png_files_exclude_weed$Species <- "Weed"
# make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
# p <- data.frame(File=png_files_weed, TitleNr=1:length(png_files_weed),Species = "Weed")
# p <- subset(p, !File%in%png_files_exclude_weed$File)
# make_plots_ncol(p, ncol=10,save=F,Specie="Weed",Title=p$TitleNr) # check with TitleNr
p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check
file_excluded_1 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)

##
folder <- folders_train2020[2]
png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
png_files_soy <- png_files[!grepl("asWeed",png_files)]
set.seed(123)
png_files_soy <- png_files_soy[sample(1:length(png_files_soy), 300)]
## 1201 lanzelot leaf type
png_files_list_soy <- make_100er_plots(png_files_soy, save=T)
png_files_exclude_soy <- png_files_list_soy[c(230),]
png_files_exclude_soy$Species <- "Soybean"
# make_plots_ncol(png_files_exclude_soy, ncol=10,save=F,Specie="Soybean",Title=png_files_exclude_soy$TitleNr) # check
p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check

png_files_weed <- png_files[grepl("asWeed",png_files)]
png_files_weed
png_files_list_weed <- make_100er_plots(png_files_weed, save=T)

# x <- c(201,239:241,245,247)
# y <- 101:length(png_files_weed)
# exclude <- y[-x]

png_files_exclude_weed <- png_files_list_weed[c(14,30),] #
# make_plots(png_files_exclude_weed) # check
png_files_exclude_weed$Species <- "Weed"
make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
# p <- data.frame(File=png_files_weed, TitleNr=1:length(png_files_weed),Species = "Weed")
# p <- subset(p, !File%in%png_files_exclude_weed$File)
# make_plots_ncol(p, ncol=10,save=F,Specie="Weed",Title=p$TitleNr) # check with TitleNr
p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check
file_excluded_2 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)

##
folder <- folders_train2020[3]
png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
png_files_soy <- png_files[!grepl("asWeed",png_files)]
png_files_soy
set.seed(123)
png_files_soy <- png_files_soy[1:500]
png_files_list_soy <- make_100er_plots(png_files_soy, save=T)
png_files_exclude_soy <- png_files_list_soy[c(2,59,126,145,258),]
png_files_exclude_soy$Species <- "Soybean"
# make_plots_ncol(png_files_exclude_soy, ncol=10,save=F,Specie="Soybean",Title=png_files_exclude_soy$TitleNr) # check
p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check

png_files_weed <- png_files[grepl("asWeed",png_files)]
png_files_weed
png_files_list_weed <- make_100er_plots(png_files_weed, save=T)
png_files_exclude_weed <- png_files_list_weed[c(6,10:30,41,42),]
# make_plots(png_files_exclude_weed) # check
png_files_exclude_weed$Species <- "Weed"
make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
# p <- data.frame(File=png_files_weed, TitleNr=1:length(png_files_weed),Species = "Weed")
# p <- subset(p, !File%in%png_files_exclude_weed$File)
# make_plots_ncol(p, ncol=10,save=F,Specie="Weed",Title=p$TitleNr) # check with TitleNr
p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check
file_excluded_3 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)

####

folder <- folders_train2020[4]
png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
png_files_soy <- png_files[!grepl("asWeed",png_files)]
set.seed(123)
png_files_soy <- png_files_soy[sample(1:length(png_files_soy), 200)]
png_files_soy

png_files_list_soy <- make_100er_plots(png_files_soy, save=T)
png_files_exclude_soy <- png_files_list_soy[c(14:16,61,121,122),]
png_files_exclude_soy$Species <- "Soybean"
# make_plots_ncol(png_files_exclude_soy, ncol=10,save=F,Specie="Soybean",Title=png_files_exclude_soy$TitleNr) # check
p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check

png_files_weed <- png_files[grepl("asWeed",png_files)]
png_files_weed
png_files_list_weed <- make_100er_plots(png_files_weed, save=T)
png_files_exclude_weed <- png_files_list_weed[c(14,18,19,26,27,29,31),]
# make_plots(png_files_exclude_weed) # check
png_files_exclude_weed$Species <- "Weed"
# make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check
file_excluded_4 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)

##
folder <- folders_train2020[5]
png_files <- list.files(path=paste0(folder,"/RGB"),pattern = "\\.png$", full.names = T)
png_files_soy <- png_files[!grepl("asWeed",png_files)]
set.seed(123)
png_files_soy <- png_files_soy[sample(1:length(png_files_soy), 100)]
png_files_soy

png_files_list_soy <- make_100er_plots(png_files_soy, save=T)
png_files_exclude_soy <- png_files_list_soy[c(11,121),]
png_files_exclude_soy$Species <- "Soybean"
# make_plots_ncol(png_files_exclude_soy, ncol=10,save=F,Specie="Soybean",Title=png_files_exclude_soy$TitleNr) # check
p <- png_files_list_soy[!png_files_list_soy$File%in%png_files_exclude_soy$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Soybean_")) # check

png_files_weed <- png_files[grepl("asWeed",png_files)]
png_files_weed
png_files_list_weed <- make_100er_plots(png_files_weed, save=T)

x <- c(1:5,7,12:13,100:102,111,115)
y <- 1:length(png_files_weed)
exclude <- y[-x]

png_files_exclude_weed <- png_files_list_weed[c(exclude),]
# make_plots(png_files_exclude_weed) # check
png_files_exclude_weed$Species <- "Weed"
# make_plots_ncol(png_files_exclude_weed, ncol=10,save=F,Specie="Weed",Title=png_files_exclude_weed$TitleNr) # check
p <- png_files_list_weed[!png_files_list_weed$File%in%png_files_exclude_weed$File,]
make_100er_plots(p$File,save=T, Title = p$TitleNr, path=paste0(folder,"/Weed_")) # check
file_excluded_5 <- c(png_files_exclude_weed$File,png_files_exclude_soy$File)

##
file_excluded_2020 <- c(file_excluded_1,file_excluded_2,file_excluded_3,file_excluded_4,file_excluded_5) 
# write.csv(file_excluded_2020, "file_excluded_2020.csv", row.names = F,quote = F)
##


##
mosaics_2020 <- get_training_pictures(folders_train2020, augment_weed_dir=NA, filename_to_exclude=file_excluded_2020)



get_pixels <- lapply(folders_growth[1], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=300,LeaveSpecies="Soybean", max_number_of_rows=5 ,min_cluster=3  ))
get_pixels <- lapply(folders_growth[2:5], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=400,LeaveSpecies="Soybean", max_number_of_rows=5 ,min_cluster=3  ))
get_pixels <- lapply(folders_growth[6], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 400, Row_distance_min=500,LeaveSpecies="Soybean", max_number_of_rows=5 ,min_cluster=3  ))

get_pixels <- lapply(folders_growth[7], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 300, Row_distance_min=600,LeaveSpecies="Soybean", max_number_of_rows=5 ,min_cluster=3  ))
get_pixels <- lapply(folders_growth[8], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 250, Row_distance_min=700,LeaveSpecies="Soybean", max_number_of_rows=5 ,min_cluster=3  ))
get_pixels <- lapply(folders_growth[9], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 250, Row_distance_min=800,LeaveSpecies="Soybean", max_number_of_rows=5 ,min_cluster=3  ))
get_pixels <- lapply(folders_growth[10], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 0, Row_distance_min=800,LeaveSpecies="Soybean", max_number_of_rows=6 ,min_cluster=3  ))
get_pixels <- lapply(folders_growth[11], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 0, Row_distance_min=950,LeaveSpecies="Soybean", max_number_of_rows=5 ,min_cluster=3  ))


Plot_borders_mean <- get_plot_borders_mean(pattern="ows.csv")
folders_canopy <- c(folders[grepl("2020_06_",folders)][6],folders[grepl("2020_07_",folders)],folders[grepl("2020_08_",folders)],folders[grepl("2020_09_",folders)])
lapply(folders_canopy, function(x)get_green_canopy_plot_cover(folder_ii = x, Plot_borders_mean))

#######
