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


library(readr) #loading dataframe
library(dplyr) #data prep
library(ggplot2) #plotting
library(envreportutils) #multiplot function, theme_soe
library(RColorBrewer) #colours from RBrewer
library(bcmaps) #using map files
library(raster) #intersect and aggregate functions - keeping attributes
library(maptools) # mapping
library(rgeos) # mapping
library(rmapshaper) #ms_simplify

## Load CSV data file from BC Data Catalogue. Data liscensed under the Open Data License-BC
## See metadata record in BC Data Catalogue for details on the data set.
degree.days <- read_csv("https://catalogue.data.gov.bc.ca/dataset/8f0d304e-161d-42e6-a982-cad13e60bd8f/resource/31d62c2b-ab92-49b5-89af-16ebda42aa98/download/growheatcooldegreedaychange1900-2013.csv",
                        na=c("", "NA"))


## subsetting the degree data
data <- degree.days %>%
  filter(Measure != "Growing_Degree_Days") %>%
  filter(Ecoprovince != "British Columbia")

## Data frame for bar plot
data_plot <- data

## re-labelling the ecoprovinces for bar plot
data_plot$Ecoprovince[data$Ecoprovince == "COAST AND MOUNTAINS"] <- "CM"
data_plot$Ecoprovince[data$Ecoprovince == "GEORGIA DEPRESSION"] <- "GD"
data_plot$Ecoprovince[data$Ecoprovince == "CENTRAL INTERIOR"] <- "CI"
data_plot$Ecoprovince[data$Ecoprovince == "SOUTHERN INTERIOR"] <- "SI"
data_plot$Ecoprovince[data$Ecoprovince == "SOUTHERN INTERIOR MOUNTAINS"] <- "SIM"
data_plot$Ecoprovince[data$Ecoprovince == "SUB-BOREAL INTERIOR"] <- "SBI"
data_plot$Ecoprovince[data$Ecoprovince == "BOREAL PLAINS"] <- "BP"
data_plot$Ecoprovince[data$Ecoprovince == "TAIGA PLAINS"] <- "TP"
data_plot$Ecoprovince[data$Ecoprovince == "NORTHERN BOREAL MOUNTAINS"] <- "NBM"


## Bar chart
## colours from palette RdYlBu, colorbrewer
pal <- c("#fdae61", "#4575b4")

hdd_cdd_barchart <- ggplot(data_plot, aes(Ecoprovince, Trend_DDcentury, fill = Measure)) +
  geom_bar(stat = "identity", position = "identity", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = Trend_DDcentury - Uncertainty_DDcentury,
                    ymax = Trend_DDcentury + Uncertainty_DDcentury, colour = Measure
  ),
  position = "identity", width = 0.2) +
  scale_y_continuous(limits = c(-950, 50), 
                     breaks = c(50, 0, -100, -200, -300, -400, -500, -600, -700, -800, -900, -950)) +
  scale_fill_manual(values = pal, labels = c("Cooling  ", "Heating"),
                    guide = guide_legend(title = NULL)) +
  scale_colour_manual(values = c("#f46d43", "#313695"), guide = "none") +
  xlab("Ecoprovince") +
  ylab(expression(atop("Change in Annual Energy Requirements", "(Degree Days per century)"))) +
  theme_soe() +
  theme(plot.margin = unit(c(5,35,5,5),"mm"), #The legend can then go on the margin outside the chart
        panel.grid.major.x = (element_blank()), legend.position = c(1.1, 0.914))
plot(hdd_cdd_barchart)


## Chloropleth maps

## intersecting the two maps and clipping maps from bcmaps package
ecoprov_clip <- intersect(ecoprovinces, bc_bound)

## Simplify the polygons. Need to disaggregate first so that pieces of 
## multipart polygons are not removed
ecoprov_clip <- disaggregate(ecoprov_clip)
ecoprov_clip <- ms_simplify(ecoprov_clip, keep = 0.05)

## aggregating small polygons into 1 for each ecozone
ecoprov_map <- aggregate(ecoprov_clip,
                         by = "CPRVNCNM")

## creating map for ggplot2 use
ecoprov_df <- fortify(ecoprov_map, region = "CPRVNCNM")
ecoprov_hdd <- left_join(ecoprov_df, data[data$Measure == "Heating_Degree_Days", ], by = c("id" = "Ecoprovince"))
ecoprov_cdd <- left_join(ecoprov_df, data[data$Measure == "Cooling_Degree_Days", ], by = c("id" = "Ecoprovince"))


## creating a map theme
map_theme <- theme(axis.title = element_blank(),
                   axis.text = element_blank(), 
                   axis.ticks = element_blank(),
                   panel.grid = element_blank(),
                   legend.title = element_text(size = 11, face = "bold"),
                   text = element_text(family = "Verdana"), 
                   legend.position = c(0.2, 0.15),
                   legend.direction = ("vertical"),
                   plot.margin = unit(c(0,0,10,0),"mm"),
                   plot.title = element_text(vjust = -1))


## MAPS

## colour palette
hdd_map <- ggplot(ecoprov_hdd, aes(x = long, y = lat, group = group, fill = Trend_DDcentury)) + 
  geom_polygon(alpha = 1) +
  geom_path(colour = "white") + 
  scale_fill_gradient(high = "#ffffbf", low = "#4575b4", na.value = "grey85",
                      guide = guide_colourbar(title = "Degree Days\nper century", 
                                              title.position = "bottom")) +
  coord_fixed() +
  theme_minimal() +
  map_theme +
  annotate("text", x=720000, y=700000,label="Coast &\nMountains\n(CM)",colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1200000, y=1550000,label="Taiga\nPlain\n(TP)",colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=870000, y=1500000,label="N. Boreal\nMountains\n(NBM)",colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1110000, y=1150000, label="S. Boreal\nInterior\n(SBI)", colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1305000, y=1260000, label="Boreal\nPlains\n(BP)", colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1135000, y=820000,label="Central\nInterior\n(CI)",colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1360000, y=610000,label="S. Interior\n(SI)",colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1570000, y=695000,label="S. Interior\nMountains\n(SIM)",colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1320000, y=377000,label="Georgia\nDepression (GD)",colour="black",
           size=3, family = "Verdana") +
  labs(title = "Change in Annual Energy\nRequirements for Heating")
 plot(hdd_map)

cdd_map <- ggplot(ecoprov_cdd, aes(x = long, y = lat, group = group, fill = Trend_DDcentury)) + 
  geom_polygon(alpha = 1) +
  geom_path(colour = "white") + 
  scale_fill_gradient(low = "#ffffbf", high = "#f46d43", na.value = "grey85",
                      guide = guide_colourbar(title = "Degree Days\nper century", 
                                              title.position = "bottom")) +
  coord_fixed() +
  theme_minimal() +
  map_theme +
  annotate("text", x=720000, y=700000,label="Coast &\nMountains\n(CM)",colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1200000, y=1550000,label="Taiga\nPlain\n(TP)",colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=870000, y=1500000,label="N. Boreal\nMountains\n(NBM)",colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1110000, y=1150000, label="S. Boreal\nInterior\n(SBI)", colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1305000, y=1260000, label="Boreal\nPlains\n(BP)", colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1135000, y=820000,label="Central\nInterior\n(CI)",colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1360000, y=610000,label="S. Interior\n(SI)",colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1570000, y=695000,label="S. Interior\nMountains\n(SIM)",colour="black",
           size=3, family = "Verdana") +
  annotate("text", x=1320000, y=377000,label="Georgia\nDepression (GD)",colour="black",
           size=3, family = "Verdana") +
  labs(title = "Change in Annual Energy\nRequirements for Cooling")
 plot(cdd_map)

png(filename = "./out/hdd_cdd_map.png", width=930, height = 450, units="px", type = "cairo-png")
multiplot(hdd_map, cdd_map, cols = 2)
dev.off()

png(filename = "out/hdd_cdd_barchart.png", width = 700, height = 450, units = "px", type = "cairo-png")
hdd_cdd_barchart
dev.off()


