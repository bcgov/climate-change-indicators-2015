# Copyright 2016 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


library(readr) #read in dataframe
library(dplyr) #data prep
library(reshape2) #for restructuring data frame for plotting
library(ggplot2) #plotting
library(envreportutils) #multiplot function
require(RColorBrewer) # colour scale
library(bcmaps) # for ecoprovinces
library(raster) # intersect
library(rmapshaper) # ms_simplify
library(rgdal) # for spatial projection of ecoprovinces
library(geojsonio) # for writing geojson file


## Load CSV data file from BC Data Catalogue. Data liscensed under the Open Data License-BC
## See metadata record in BC Data Catalogue for details on the data set.
glacier <- read_csv("https://catalogue.data.gov.bc.ca/dataset/89ff86d7-2d04-4c96-b945-ba56688906eb/resource/bf6ba520-dcfd-4a6b-a822-963b77ff7848/download/glacierchange1985-2005.csv",
                    na=c("", "NA"))


## tidying glaciers data frame for data vizualizations
colnames(glacier) <- c("Ecoprovince", "Area_kmsquared_85", "Area_kmsquared_05",
                       "Area_Change_kmsquared", "Percentage_Area_Change", "Volume_Loss_Rate_kmcubicyr")

glacier <- subset(glacier, select = -Area_kmsquared_05) 
colnames(glacier)[colnames(glacier) == "Area_kmsquared_85"] <- "Area_kmsquared"
glacier$Year <- "1985"
glacier <- glacier[c("Ecoprovince","Year", "Area_kmsquared",
           "Area_Change_kmsquared", "Percentage_Area_Change", "Volume_Loss_Rate_kmcubicyr")]


## BAR PLOT

## Data tidy steps
facet_plot <- glacier
facet_plot <- filter(glacier, Ecoprovince != "British Columbia" & Ecoprovince != "TAIGA PLAINS" &
               Ecoprovince != "BOREAL PLAINS") 

##remove unused columns
facet_plot <- subset(facet_plot, select = -Year) 
facet_plot <- subset(facet_plot, select = -Volume_Loss_Rate_kmcubicyr) 


## restructure the data frame for plotting
facet_plot <- melt(facet_plot, id = c("Ecoprovince"))

## re-labelling the ecoprovinces for bar plot
facet_plot$Ecoprovince[facet_plot$Ecoprovince == "COAST AND MOUNTAINS"] <- "Coast and\nMountains"
facet_plot$Ecoprovince[facet_plot$Ecoprovince == "GEORGIA DEPRESSION"] <- "Georgia\nDepression"
facet_plot$Ecoprovince[facet_plot$Ecoprovince == "CENTRAL INTERIOR"] <- "Central\nInterior"
facet_plot$Ecoprovince[facet_plot$Ecoprovince == "SOUTHERN INTERIOR"] <- "Southern\nInterior"
facet_plot$Ecoprovince[facet_plot$Ecoprovince == "SOUTHERN INTERIOR MOUNTAINS"] <- "S. Interior\nMountains"
facet_plot$Ecoprovince[facet_plot$Ecoprovince == "SUB-BOREAL INTERIOR"] <- "Sub-Boreal\nInterior"
facet_plot$Ecoprovince[facet_plot$Ecoprovince == "NORTHERN BOREAL MOUNTAINS"] <- "N. Boreal\nMountains"

## creating data frames to customize value labels on individual facet plots
size_data <- facet_plot[1:7, ]
area_data <- facet_plot[8:14,]
perc_data <- facet_plot[15:21,]

## to individually title the facet plots 
ann_text <- data.frame(Ecoprovince = "Sub-Boreal\nInterior", value = 12500,
                       variable = factor("Area_kmsquared",
                                         levels = c("Area_kmsquared","Area_Change_kmsquared","Percentage_Area_Change")))
ann_text2 <- data.frame(Ecoprovince = "Sub-Boreal\nInterior", value = -1000, 
                        variable = factor("Area_Change_kmsquared",
                                          levels = c("Area_kmsquared","Area_Change_kmsquared","Percentage_Area_Change")))
ann_text3 <- data.frame(Ecoprovince = "Sub-Boreal\nInterior", value = -25,
                        variable = factor("Percentage_Area_Change",
                                          levels = c("Area_kmsquared","Area_Change_kmsquared","Percentage_Area_Change")))
## bar plot
area_charts <- ggplot(facet_plot, aes(reorder(Ecoprovince, value), value, label = value)) + 
  geom_bar(data = subset(facet_plot, variable == "Area_kmsquared"), fill = "grey60",
           stat = "identity", width = .8, alpha = "0.7") +
  geom_bar(data = subset(facet_plot, variable == "Area_Change_kmsquared" |
                           variable == "Percentage_Area_Change"), fill = "#6baed6",
           stat = "identity", width = 0.8, alpha = "0.7") +
  xlab("Ecoprovince") +
  ylab("") +
  geom_text(data = size_data, size = 3.5, vjust = - 0.3, fontface = "bold") +
  geom_text(data = area_data, size = 3.5, vjust = 1.1, fontface = "bold") +
  geom_text(data = perc_data, size = 3.5, vjust = 1.1, fontface = "bold") +
  facet_grid(variable ~ ., scales = "free_y") +
  scale_y_continuous(expand=c(0.1, 0)) +
  geom_text(data = ann_text, label = "Area of ice coverage in 1985 (km2)",
            size = 5, fontface = "bold") +
  geom_text(data = ann_text2, label = "Area of ice coverage lost from 1985 to 2005 (km2)", 
            size = 5, fontface = "bold") +
  geom_text(data = ann_text3, label = "Percent of ice coverage lost from 1985 to 2005 (%)",
            size = 5, fontface = "bold") +
  theme_soe_facet() +
  theme(panel.grid.major.x = (element_blank()), strip.text.y = element_blank(), 
        strip.background = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12))
plot(area_charts)

##print bar plot to PNG
if (!exists("out")) dir.create("out", showWarnings = FALSE)

png(filename = "out/area_barchart.png", width = 650, height = 550, units = "px")
area_charts
dev.off()

svg_px(file = "out/area_barchart.svg", width = 650, height = 550)
area_charts
dev.off()

## Javascript Data Viz & Map Scale

df <- glacier

## Map scale
scale_colours <- c("#979797", brewer.pal(6, "Blues")[2:6]) # Generate 6 and choose last 5 to get darker shades
names(scale_colours) <- c("No data", "-5 to -9%", "-10 to -14%", "-15 to -19%", "-20 to -29%", "-30 to -35%")

df$area_change_category <- as.character(cut(df$Percentage_Area_Change, 
                                            breaks = c(-Inf, -30, -20, -15, -10, -5), 
                                            labels = c(rev(names(scale_colours)[-1])),
                                            include.lowest = TRUE, right = TRUE))


df$fillColour <- scale_colours[df$area_change_category]
df$fillColour[is.na(df$fillColour)] <- scale_colours["No data"]


## get map from bcmaps
ecoprov_clip <- intersect(ecoprovinces, bc_bound)

## Simplify the polygons. Need to disaggregate first so that pieces of 
## multipart polygons are not removed
ecoprov_clip <- disaggregate(ecoprov_clip)
ecoprov_simp <- ms_simplify(ecoprov_clip, keep = 0.02, keep_shapes = TRUE)

## aggregating small polygons into 1 for each ecozone
ecoprov_map <- aggregate(ecoprov_simp, by = "CPRVNCNM")

## changing projection
ecoprov_convert <- spTransform(ecoprov_map, CRS("+init=epsg:4326")) 
names(ecoprov_convert)[1] <- "Ecoprovince"

## merge shapefile with glacier dataframe
ecoprov_convert <- merge(ecoprov_convert, df, by = 'Ecoprovince', all.x = TRUE)

ecoprov_convert$Ecoprovince <- tools::toTitleCase(tolower(ecoprov_convert$Ecoprovince))

spTransform(ecoprov_convert, CRS("+init=epsg:4326")) %>% 
## Write our GeoJson file for web
geojson_write(file = "out/ecoprovinces_glaciers.geojson", precision = 5)

## Add an alpha value to a colour
add_alpha <- function(col, alpha=1){
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

## pring map scale in PNG
#png("out/map_scale.png", width = 460, height = 80)
svg_px("out/map_scale.svg", width = 460, height = 80)
par(mar = c(4,0,0,0), mgp = c(3,1.1,0))
image(1:6, 1, matrix(1:6), col = add_alpha(scale_colours, 0.7), ylab = "", 
      axes = FALSE, xlab = "")
box("plot", col = "grey")
axis(1, at = 1:6, labels = names(scale_colours), tick = FALSE)
mtext(expression(paste("Percent Change in Glacier Area")), 
      side = 1, line = 3)
dev.off()

