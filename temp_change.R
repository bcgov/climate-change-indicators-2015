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
require(ggplot2) #plotting
require(envreportutils) #soe theme
require(RColorBrewer) # colour scale
library(bcmaps) # for ecoprovinces
library(rgdal) # for spatial projection of ecoprovinces
library(geojsonio) # for writing geojson file
library(rmapshaper) # simplify geojson


## Load CSV data file from BC Data Catalogue. Data liscensed under the Open Data License-BC
## See metadata record in BC Data Catalogue for details on the data set.

tempdata <- read_csv("https://catalogue.data.gov.bc.ca/dataset/86f93096-8d3d-4b68-ab63-175cc68257e6/resource/2ea26a15-8420-4d85-bba0-742b8c1a4dc2/download/temperaturechange19002013.csv",
                       na=c("", "NA"))


## Create NS column for direct labelling significance
tempdata$sig <- NA
tempdata$sig[tempdata$Stat_Significance == 0] <- "NS"
tempdata$sig[tempdata$Stat_Significance == 1] <- ""

## Renaming Measures
tempdata$Measure[tempdata$Measure == "Mean_Temp"] <- "Average Temperature"
tempdata$Measure[tempdata$Measure == "Min_Temp"] <- "Minimum Temperature"
tempdata$Measure[tempdata$Measure == "Max_Temp"] <- "Maximum Temperature"

## Creating factor for ordering the facets
season.order <- c("Annual", "Winter", "Spring", "Summer", "Fall")
tempdata$Season <- as.factor(tempdata$Season)
tempdata$Season <-  factor(tempdata$Season, levels = season.order)

## Creating factor for ordering the Measures
measure.order <- c("Minimum Temperature", "Average Temperature", "Maximum Temperature")
tempdata$Measure <- as.factor(tempdata$Measure)
tempdata$Measure <-  factor(tempdata$Measure, levels = measure.order)

## Colour palette
colrs <- c("#377eb8", "#4daf4a", "#e41a1c")

dir.create("outtemp", showWarnings = FALSE)

## Looping through Ecoprovinces and creating plots
for (ecoprov in unique(tempdata$Ecoprovince)) {
  
  ## Subset the data for plotting   
  plotdata <- subset(tempdata, tempdata$Ecoprovince == ecoprov)
  
  ##Create png file names
  plotfile <- "plot"
  title_case_title <- tools::toTitleCase(tolower(ecoprov))
  png_name <- sprintf('outtemp/%s_%s.png', title_case_title, plotfile)
  svg_name <- sprintf('outtemp/%s_%s.svg', title_case_title, plotfile)
  
  ## Create chart title
  type <- ifelse(ecoprov == "British Columbia", "", "Ecoprovince")
  plot_title <- paste(title_case_title, type)
  
  ## Bar plot
#  png(file=png_name, width=350, height=500, type = "cairo-png")
 svg_px(file=svg_name, width=350, height=500)
  tempplot <- ggplot(plotdata, aes(x = Measure, y = Trend_Ccentury)) + 
    geom_point(aes(colour = Measure), size = 4) +
    geom_errorbar(aes(ymax = Trend_Ccentury + Uncertainty_Ccentury,
                      ymin= Trend_Ccentury - Uncertainty_Ccentury,
                      colour = Measure), width=.3, size = .5) +
    facet_grid(. ~ Season) +
    scale_y_continuous(limits = c(-1.5,7), breaks=seq(-1, 7, 1),
                       expand=c(0,0)) +
    ylab("Degrees Celcius per century") +
    ggtitle(plot_title) + 
    theme_soe_facet() + 
    theme(plot.title = element_text(size = rel(1.3), hjust = 0.5),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=14),
          axis.line = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_rect(colour = "grey50", fill = NA),
          panel.background = element_rect(colour = "grey50", fill = NA),
          strip.background = element_rect(colour = "grey50"),
          panel.margin.x = unit(0, "lines"), 
          strip.text = element_text(size = rel(1.1)), 
          legend.position = ("bottom"),
          legend.direction = ("vertical"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          plot.margin = unit(c(1,0.1,0,0.5), "lines")) +
    geom_text(aes(y = Trend_Ccentury, label = sig, hjust = 1.2, vjust = 1.8), 
              colour = "grey30", size = 2.8, family = "Verdana") +
    scale_colour_manual(name = "Observed Change In:", values = colrs) +
    geom_hline(aes(yintercept = 0), linetype = 2, colour = "black")  
  #plot(tempplot) 
  
  #Printing png plots
  print(tempplot)
  graphics.off()
  
  #eco_plots[[foo]] <- tempplot
  
}

## Add an alpha value to a colour
add_alpha <- function(col, alpha=1){
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}


scale_colours <- c("#FFFFFF", brewer.pal(5, "Reds"))
names(scale_colours) <- c("No significant\nchange", "0.0 - 0.4", "0.5 - 0.9", "1.0 - 1.4", "1.5 - 1.9", "2.0+")
#png("outtemp/map_scale.png", width = 460, height = 80)
svg_px("outtemp/map_scale.svg", width = 460, height = 80)
par(mar = c(4,0,0,0), mgp = c(3,1.1,0))
image(1:6, 1, matrix(1:6), col = add_alpha(scale_colours, 0.7), ylab = "", axes = FALSE, xlab = "")
box("plot", col = "grey")
axis(1, at = 1:6, labels = names(scale_colours), tick = FALSE)
mtext(expression(paste("Average Annual Temperature Change (", degree, "C per century)")), 
      side = 1, line = 3)
dev.off()

## Categorize change and add colours:
tempdata$trend_category <- as.character(cut(tempdata$Trend_Ccentury, 
                                            breaks = c(-Inf, 0, 0.5, 1.0, 1.5, 2.0, Inf), 
                                            labels = c("<0", names(scale_colours)[-1]),
                                            include.lowest = TRUE, right = FALSE))
tempdata$trend_category[!tempdata$Stat_Significance] <- names(scale_colours)[1]
tempdata$fillColor <- scale_colours[tempdata$trend_category]

# Format ecoprovinces for web for geojson
data("ecoprovinces")
ecoprovinces <- ecoprovinces[ecoprovinces$CPRVNCNM %in% tempdata$Ecoprovince,]
ecoprovinces <- merge(ecoprovinces, 
                      subset(tempdata, Measure == "Average Temperature" & 
                               Season == "Annual"), 
                      by.x = "CPRVNCNM", by.y = "Ecoprovince")

ecoprovinces <- ecoprovinces[,c("CPRVNCNM", "Trend_Ccentury", "Stat_Significance", "fillColor")]
names(ecoprovinces)[1:3] <- c("region", "trend", "sig")
ecoprovinces$region <- tools::toTitleCase(tolower(ecoprovinces$region))

## clips to bc boundaries and simplifies for web map
spTransform(ecoprovinces, CRS("+init=epsg:4326")) %>% 
  ms_clip(bc_bound) %>% 
  ms_simplify(0.01, keep_shapes = TRUE) %>% 
  geojson_write(file = "outtemp/ecoprovinces.geojson", precision = 5)

