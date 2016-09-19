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
library(rgeos) #mapping
library(rmapshaper) #ms_simplify


## Load CSV data file from BC Data Catalogue. Data liscensed under the Open Data License-BC
## See metadata record in BC Data Catalogue for details on the data set.
degree.days <- read_csv("https://catalogue.data.gov.bc.ca/dataset/8f0d304e-161d-42e6-a982-cad13e60bd8f/resource/31d62c2b-ab92-49b5-89af-16ebda42aa98/download/growheatcooldegreedaychange1900-2013.csv",
                     na=c("", "NA"))


## subsetting the growing degree data
df <- degree.days %>%
  filter(Measure == "Growing_Degree_Days") %>%
  filter(Ecoprovince != "British Columbia")

## dataframe for bar chart
df_plot <- df

## re-labelling the ecoprovinces for bar plot
df_plot$Ecoprovince[df_plot$Ecoprovince == "COAST AND MOUNTAINS"] <- "CM"
df_plot$Ecoprovince[df_plot$Ecoprovince == "GEORGIA DEPRESSION"] <- "GD"
df_plot$Ecoprovince[df_plot$Ecoprovince == "CENTRAL INTERIOR"] <- "CI"
df_plot$Ecoprovince[df_plot$Ecoprovince == "SOUTHERN INTERIOR"] <- "SI"
df_plot$Ecoprovince[df_plot$Ecoprovince == "SOUTHERN INTERIOR MOUNTAINS"] <- "SIM"
df_plot$Ecoprovince[df_plot$Ecoprovince == "SUB-BOREAL INTERIOR"] <- "SBI"
df_plot$Ecoprovince[df_plot$Ecoprovince == "BOREAL PLAINS"] <- "BP"
df_plot$Ecoprovince[df_plot$Ecoprovince == "TAIGA PLAINS"] <- "TP"
df_plot$Ecoprovince[df_plot$Ecoprovince == "NORTHERN BOREAL MOUNTAINS"] <- "NBM"

## orange colour palette
pal <- brewer.pal(9, "YlOrBr")[4:8]

## Bar chart
bar_chart <- ggplot(df_plot, aes(x = reorder(Ecoprovince, -Trend_DDcentury), y = Trend_DDcentury, 
                                 fill = Trend_DDcentury, 
                                 colour = Trend_DDcentury)) +
  geom_bar(stat ="identity", alpha = 1, colour = NA) +
  geom_errorbar(aes(ymin = Trend_DDcentury - Uncertainty_DDcentury, 
                    ymax = Trend_DDcentury + Uncertainty_DDcentury,
                    stat = "identity"),
                width = 0.3, size = 0.5) + 
  scale_fill_gradientn(colours = pal) +
  scale_colour_gradientn(colours = "#662506") +
  xlab("Ecoprovince") +
  ylab("Change in Annual Available\nHeat Energy (GDD per century)") +
  scale_y_continuous(limits = c(0,350), breaks=seq(0, 350, 50),
                     expand=c(0,0)) +
  theme_soe() +
  theme(legend.position = "none",
        plot.margin = unit(c(20,20,60,-40),"mm"),
        panel.grid.major.x = (element_blank()))
#plot(bar_chart)


## Static Map

## intersecting two maps and clipping (from bcmaps package)
ecoprov_clip <- intersect(ecoprovinces, bc_bound)

## Simplify the polygons. Need to disaggregate first so that pieces of 
## multipart polygons are not removed
ecoprov_clip <- disaggregate(ecoprov_clip)
ecoprov_clip <- ms_simplify(ecoprov_clip, keep = 0.05)

## aggregating small polygons into 1 for each ecozone
ecoprov_map <- aggregate(ecoprov_clip,
                         by = "CPRVNCNM")

## creating map for ggplot2 using df dataframe
ecoprov_df <- fortify(ecoprov_map, region = "CPRVNCNM")
ecoprov_df <- left_join(ecoprov_df, df, by = c("id" = "Ecoprovince"))

## creating a map theme
map_theme <- theme(axis.title = element_blank(),
                   axis.text = element_blank(), 
                   axis.ticks = element_blank(),
                   panel.grid = element_blank(),
                   legend.title = element_text(size = 11, face = "bold"),
                   text = element_text(family = "Verdana"))

## MAP
gdd_map <- ggplot(ecoprov_df, aes(x = long, y = lat, group = group, fill = Trend_DDcentury)) + 
  geom_polygon(alpha = 1) +
  geom_path(colour = "white") + 
  scale_fill_gradientn(colours = pal, na.value = "grey85", 
                       guide = guide_colourbar(title = "Change in Annual Available\nHeat Energy (GDD per century)", 
                                               title.position = "bottom")) +
  coord_fixed() +
  theme_minimal() +
  map_theme +
  theme(legend.position = c(0.24, 0.16),
        legend.direction = ("vertical"),
        plot.margin = unit(c(-20,0,-20,0),"mm")) +
  annotate("text", x=720000, y=700000,label="Coast &\nMountains\n(CM)",colour="black",
           size=4, family = "Verdana") +
  annotate("text", x=1200000, y=1550000,label="Taiga\nPlain\n(TP)",colour="black",
           size=4, family = "Verdana") +
  annotate("text", x=870000, y=1500000,label="N. Boreal\nMountains\n(NBM)",colour="black",
           size=4, family = "Verdana") +
  annotate("text", x=1110000, y=1150000, label="S. Boreal\nInterior\n(SBI)", colour="black",
           size=4, family = "Verdana") +
  annotate("text", x=1305000, y=1270000, label="Boreal\nPlains\n(BP)", colour="black",
           size=4, family = "Verdana") +
  annotate("text", x=1135000, y=820000,label="Central\nInterior\n(CI)",colour="black",
           size=4, family = "Verdana") +
  annotate("text", x=1360000, y=610000,label="S. Interior\n(SI)",colour="black",
           size=4, family = "Verdana") +
  annotate("text", x=1570000, y=695000,label="S. Interior\nMountains\n(SIM)",colour="black",
           size=4, family = "Verdana") +
  annotate("text", x=1320000, y=377000,label="Georgia\nDepression\n(GD)",colour="black",
           size=4, family = "Verdana") 
 #plot(gdd_map)


## Print to PNG

## output map
#png(filename = "./out/gdd_map.png", width=836, height=430, units="px", type = "cairo-png")
#plot(gdd_map)
#dev.off()

## output chart 
#png(filename = "./out/gdd_chart.png", width=836, height=430, units="px", type = "cairo-png")
#plot(bar_chart)
#dev.off()

## Combined map and barchart with multiplot
png(filename = "./out/gdd_viz.png", width=930, height=478, units="px", type = "cairo-png")
multiplot(gdd_map, bar_chart, cols=2, widths = c(1.6, 1))
dev.off()


