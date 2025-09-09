source("~/functions/get_between_row_function.R")

########

set.seed(123)

########

setwd("~/data/2022")

foldersAll <- list.dirs(full.names = TRUE)
folders <- foldersAll[grepl("_segmented",foldersAll)]
folders <- folders[grepl("RGB1",folders)]
folders <- folders[!grepl("Mask",folders)]
folders <- folders[!grepl("_parts_segmented",folders)]
folders <- folders[!grepl("_segmented/RGB",folders)]

folders <- folders[!grepl("lightning_logs",folders)]

folders_growth <- c(folders[grepl("2022_06_",folders)],folders[grepl("2022_05_",folders)])

folders_train <- c(folders[grepl("2022_06_",folders)],folders[grepl("2022_05_",folders)])
folders_train <- folders_train[order(folders_train)]
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
df_png_files <- merge(df_png_files,data.frame(File=png_files_exclude$File,color="darkred"),by="File",all.x = T)
df_png_files$color[is.na(df_png_files$color)] <- "blue3"
df_png_files$color[df_png_files$Species==Species&df_png_files$color!="darkred"] <- "#DDCC77"
df_png_files <- df_png_files[order(df_png_files$FilenameQ90, df_png_files$x_coord)]
df_png_files[,N:=nrow(.SD),by=.(color,Species,FilenameQ90)]
df_png_files[, TitelNr :=1: nrow(.SD),by=.(Species)]#FilenameQ90

### visualize training set
p_sel <- subset(df_png_files, color!="darkred"&Species=="Weed")
ggWeed <- make_plots_ncol(p_sel[1:100,],ncol=10,save = T, Specie = "Weed", p_sel$TitelNr)
p_sel <- subset(df_png_files, color!="darkred"&Species!="Weed")
p_sel_soy <- p_sel[401:500,]
ggSoybean <-make_plots_ncol(p_sel_soy,ncol=10,save = T, Specie = "Soybean", p_sel_soy$TitelNr)


gg_training_set <-plot_grid(do.call(grid.arrange, c(ggSoybean, ncol = 10)),NULL, do.call(grid.arrange, c(ggWeed, ncol = 10)),
                          ncol =  3, rel_widths = c(1,0.05,1),  labels = c("","",""),label_size = 14)
ggsave(paste0("Trainingset_example",gsub("./SB016/RGB1/Segmentation/","",folder),".png"),  width = 2400, height = 1200, units = "px", dpi = 150, bg="white",gg_training_set)

### 


selectedFile <- df_png_files[,list(length(Species[Species!="Weed"]), length(Species[Species=="Weed"]),length(Species[color=="darkred"])),by=.(FilenameQ90)]
selectedFile <- subset(selectedFile, V3>1)
selectedFile <- subset(selectedFile, V2>1&V1>1)

library(imager)
# Function to draw rectangles and plot image
# Nweeds<- unique(df_png_files$N[df_png_files$Species=="Weed"])
# Nweeds
selectedFile1 <- selectedFile$FilenameQ90[selectedFile$V2==max(selectedFile$V2)][1]
# selectedFile <- df_png_files$FilenameQ90[df_png_files$Species=="Weed"&df_png_files$N==22][1]
add_row_info <- fread(paste0(folder,"_data_rows.csv"))
add_row_info <- add_row_info[,c("Filename","Plot_slope",paste0("Row_middle_",1:3), "min_y_at_x0", "max_y_at_x0", "min_y_at_xmax", "max_y_at_xmax")]
# add_row_info


df_sub <- subset(df_png_files, FilenameQ90==selectedFile1)
draw_and_plot_middle(df_sub, add_row_info, legend = TRUE)


df_sub[,list(length(Species[Species!="Weed"]), length(Species[Species=="Weed"])),by=.(FilenameQ90)]

make_plots_ncol(df_sub,ncol=1,save = T, Specie = Species)
ggWeed1 <-make_plots_ncol(df_sub,ncol=1,save = T, Specie = "Weed")

#########
selectedFile2 <- selectedFile$FilenameQ90[selectedFile$V1==max(selectedFile$V1)][1]
df_sub <- subset(df_png_files, FilenameQ90==selectedFile2)
draw_and_plot_middle(df_sub, add_row_info, legend = TRUE)

make_plots_ncol(df_sub,ncol=5,save = T, Specie = Species)
make_plots_ncol(df_sub,ncol=1,save = T, Specie = "Weed")

####
selectedFile3 <- selectedFile$FilenameQ90[selectedFile$V1+selectedFile$V2==max(selectedFile$V1+selectedFile$V2)][1]
####

df_sub <- subset(df_png_files, FilenameQ90==selectedFile3)
draw_and_plot_middle(df_sub, add_row_info, legend = TRUE)
df_sub[,list(length(Species[Species!="Weed"]), length(Species[Species=="Weed"])),by=.(FilenameQ90)]

make_plots_ncol(df_sub,ncol=4,save = T, Specie = Species,Title=df_sub$TitelNr[df_sub$Species!="Weed"])
make_plots_ncol(df_sub,ncol=1,save = T, Specie = "Weed",Title=df_sub$TitelNr[df_sub$Species=="Weed"])

####

weed_values <- selectedFile$V2

second_highest_sum <- weed_values[order(weed_values, decreasing = TRUE)[2]]

# Filter the rows where the sum of V1 and V2 equals the second highest sum
selected_rows <- selectedFile[weed_values == second_highest_sum, ]

# Select the first FilenameQ90 from the filtered rows
selectedFile1.2 <- selected_rows$FilenameQ90[1]


####

df_sub <- subset(df_png_files, FilenameQ90==selectedFile1.2)
draw_and_plot_middle(df_sub, add_row_info, legend = TRUE)
df_sub[,list(length(Species[Species!="Weed"]), length(Species[Species=="Weed"])),by=.(FilenameQ90)]

make_plots_ncol(df_sub,ncol=2,save = T, Specie = Species)
make_plots_ncol(df_sub,ncol=3,save = T, Specie = "Weed")

#####

make_locatoin_plot <- function(selectedFile1,rel.widths){
first_row <-  ggdraw() + draw_image(paste0(gsub("\\.jpg","",selectedFile1),"_Soybean.png" ))
first_row <-plot_grid(NULL, first_row, NULL, ncol =  1, rel_heights = c(0.1,0.9,0.1), labels = "")

second_row <-  ggdraw() + draw_image(paste0(gsub("\\.jpg","",selectedFile1),"_Weed.png" ))
second_row <-plot_grid(NULL, second_row, NULL, ncol =  1, rel_heights = c(0.1,0.9,0.1), labels = "")

# combined_plot <-plot_grid(first_row,second_row,third_row, ncol =  1, labels = "") #c("B","C","D")

image <-  ggdraw() + draw_image(gsub("\\.jpg",".jpg_WeedLocation.png",selectedFile1))

combined_plot <-plot_grid(image,  first_row, second_row,
                          ncol =  3, rel_widths = rel.widths,  labels = c("AUTO"))

ggsave(gsub("\\.jpg","_locations.png",selectedFile1),  width = 170, height = 100, units = "mm", dpi = 300, bg="white",combined_plot)
}

make_locatoin_plot(selectedFile1,rel.widths = c(2.25,0.2,0.2))
make_locatoin_plot(selectedFile2,rel.widths = c(1.5,0.5,0.2))
make_locatoin_plot(selectedFile1.2,rel.widths = c(1.5,0.5,0.25))
make_locatoin_plot(selectedFile3,rel.widths = c(1.5,0.5,0.1))


first_row <-  ggdraw() + draw_image(gsub("\\.jpg","_locations.png",selectedFile1))
second_row <-  ggdraw() + draw_image(gsub("\\.jpg","_locations.png",selectedFile2))
third_row <-  ggdraw() + draw_image(gsub("\\.jpg","_locations.png",selectedFile3))


# gg_mask <- ggdraw() + draw_image(gsub("_q90.jpg","_q90_mask.png",selectedFile3))

library(magick)
row_info <- subset(add_row_info, Filename==gsub("_q90.jpg","_segmentation.png",selectedFile3))

# Load the image
image_path <- gsub("_q90.jpg", "_q90_mask.png", selectedFile3)
img <- image_read(image_path)
angle <- atan(row_info$Plot_slope) * 180 / pi

# Rotate the image
img <- image_rotate(img, -angle)

# Coor# Coor# Coor# Coor# Coordinates from the data frame
xmin <- 0
xmax <- 5616 # Example from your data
ymin <- row_info$min_y_at_x0
ymax <- row_info$max_y_at_x0

# Compute width and height for cropping
width <- xmax - xmin
height <- ymax - ymin

# Crop the image
cropped_img <- image_crop(img, geometry_area(width = width, height = height, x = xmin, y = ymin))

# Display or save the cropped image
print(cropped_img)
image_write(cropped_img, path = "cropped_image.png")
gg_mask <- ggdraw() + draw_image("cropped_image.png")


third_row <- ggdraw() + draw_image(gsub("\\.jpg",".jpg_WeedLocation.png",selectedFile3))


gg_training_set <- ggdraw() + draw_image( "Trainingset_example2022_06_03_09_27_Lot3_segmented.png") ## keep numbers size small

combined_plot <-plot_grid(third_row, gg_training_set, NULL,gg_mask,
                          ncol =  1, rel_heights = c(1,0.65,0.1,0.34),  labels = c("A","B","","C"))


# first_row <-plot_grid(third_row,gg_training_set,
#                                        ncol =  2,  rel_widths = c(1,1.2), labels = c("AUTO"))
# 
# combined_plot_presentation <-plot_grid(first_row,gg_mask,
#                           ncol =  1, rel_heights  = c(1,0.75),  labels = c("",""))

# Plot the final combined plot

ggsave("Segment_weeds.png",  width = 160, height = 210, units = "mm", dpi = 300, bg="white",combined_plot)

# ggsave("Segment_weeds_long.png",  width = 280, height = 180, units = "mm", dpi = 300, bg="white",combined_plot_presentation)

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

### run semantic segmentation in python

##
get_pixels1 <- lapply(folders_growth[1:3], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 750, Row_distance_min=300,LeaveSpecies="Soybean", max_number_of_rows=4 ,min_cluster=3 ))

get_pixels1 <- lapply(folders_growth[4:7], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 750, Row_distance_min=400,LeaveSpecies="Soybean", max_number_of_rows=4 ,min_cluster=3 ))

get_pixels2 <- lapply(folders_growth[8:10], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 750, Row_distance_min=600,LeaveSpecies="Soybean", max_number_of_rows=4,min_cluster=3  ))
get_pixels2 <- lapply(folders_growth[11:12], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=650,LeaveSpecies="Soybean", max_number_of_rows=4,min_cluster=3  ))
get_pixels2 <- lapply(folders_growth[13], function(x) get_middle_rows_pixel(folder_ii=x ,makeTrainingset=F, cut = 500, Row_distance_min=700,LeaveSpecies="Soybean", max_number_of_rows=4,min_cluster=3  ))


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

####


#############################

#############################

setwd("~/data/2021")

foldersAll <- list.dirs(full.names = TRUE)
folders <- foldersAll[grepl("_segmented",foldersAll)]
folders <- folders[!grepl("Mask",folders)]
folders <- folders[!grepl("_segmented/RGB",folders)]

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


#######
setwd("~/data/2020")

foldersAll <- list.dirs(full.names = TRUE)
folders <- foldersAll[grepl("_segmented",foldersAll)]
folders <- folders[!grepl("Mask",folders)]
folders <- folders[!grepl("_segmented/RGB",folders)]



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

#######################################

setwd("~/data/2015")

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

setwd("~/data/2016")

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

setwd("~/data/2017")

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

setwd("~/data/2018")

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

setwd("~/data/2019")

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

