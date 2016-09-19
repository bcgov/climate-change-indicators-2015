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

precipdata <- read_csv("https://catalogue.data.gov.bc.ca/dataset/86f93096-8d3d-4b68-ab63-175cc68257e6/resource/31b4473e-819e-4c04-becd-655837f05fb5/download/precipitationchange19002013.csv",
                       na=c("", "NA"))

## Create NS column for direct labelling significance
precipdata$sig <- NA
precipdata$sig[precipdata$Stat_Significance == 0] <- "NS"
precipdata$sig[precipdata$Stat_Significance == 1] <- ""

## Renaming Measures
precipdata$Measure[precipdata$Measure == "Mean_Precip"] <- "Average Precipitation"

## Creating factor for ordering the facets
season.order <- c("Annual", "Winter", "Spring", "Summer", "Fall")
precipdata$Season <- as.factor(precipdata$Season)
precipdata$Season <-  factor(precipdata$Season, levels = season.order)

## Re-labelling measures
plotlabels <- c("Observed Change in Average Precipitation")

## Colour palette
colrs <- c("#377eb8") #blue

## Directory for plots
dir.create("outprecip", showWarnings = FALSE)

## Looping through Ecoprovinces and creating plots
for (ecoprov in unique(precipdata$Ecoprovince)) {
  
  ## Subset the data for plotting   
  plotdata <- subset(precipdata, precipdata$Ecoprovince == ecoprov)
  
  ##Create png file names
  plotfile <- "plot"
  title_case_title <- tools::toTitleCase(tolower(ecoprov))
  png_name <- sprintf('outprecip/%s_%s.png', title_case_title, plotfile)
  
  ## Create chart title
  type <- ifelse(ecoprov == "British Columbia", "", "Ecoprovince")
  plot_title <- paste(title_case_title, type)
  
  ## Bar plot
  png(file=png_name, width=350, height=500, type = "cairo-png")
  tempplot <- ggplot(plotdata, aes(x = Season, y = Trend_percentcentury)) + 
    geom_point(aes(colour = colrs), size = 4) +
    geom_errorbar(aes(ymax = Trend_percentcentury + Uncertainty_percentcentury,
                      ymin= Trend_percentcentury - Uncertainty_percentcentury,
                      colour = colrs), width=.3, size = .5) +
    scale_y_continuous(limits = c(-40,50), breaks=seq(-40, 50, 10),
                       expand=c(0,0)) +
    ylab("Percent per century") +
    ggtitle(plot_title) + 
    theme_soe() + 
    theme(plot.title = element_text(size = rel(1.2)),
          axis.title.y = element_text(size=13),
          axis.title.x = element_blank(),
          axis.line = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_line(),
          panel.border = element_rect(colour = "grey50", fill = NA),
          panel.background = element_rect(colour = "grey50", fill = NA),
          legend.position = ("bottom"),
          legend.direction = ("horizontal"), 
          plot.margin = unit(c(1,0.1,0,0.5), "lines")) +
    geom_text(aes(y = Trend_percentcentury, label = sig, hjust = 1.2, vjust = 1.8), 
              colour = "grey30", size = 2.8, family = "Verdana") +
    scale_colour_manual(name = "", values = colrs,
                        labels = plotlabels) +
    geom_hline(aes(yintercept = 0), linetype = 2, colour = "black")  
  plot(tempplot) 
  
  #Printing png plots
  print(tempplot)
  graphics.off()
  
}

## Add an alpha value to a colour
add_alpha <- function(col, alpha=1){
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}


## Javascript Map Scale
scale_colours <- c("#FFFFFF", brewer.pal(6, "Blues")[2:6]) # Generate 6 and choose last 5 to get darker shades
names(scale_colours) <- c("No significant\nchange","0 - 5", "6 - 10", "11 - 15", "16 - 20", "21+")
png("outprecip/map_scale.png", width = 460, height = 80)
par(mar = c(4,0,0,0), mgp = c(3,1.1,0))
image(1:6, 1, matrix(1:6), col = add_alpha(scale_colours, 0.7), ylab = "", 
      axes = FALSE, xlab = "")
box("plot", col = "grey")
axis(1, at = 1:6, labels = names(scale_colours), tick = FALSE)
mtext(expression(paste("Change in Average Annual Precipitation (% per century)")), 
      side = 1, line = 3)
dev.off()


## Javascript Map
## Categorize change and add colours:
precipdata$trend_category <- as.character(cut(precipdata$Trend_percentcentury, 
                                              breaks = c(-Inf, 0, 6, 11, 16, 21, Inf), 
                                              labels = c("<0", names(scale_colours)[-1]),
                                              include.lowest = TRUE, right = FALSE))
precipdata$trend_category[!precipdata$Stat_Significance] <- names(scale_colours)[1]
precipdata$fillColor <- scale_colours[precipdata$trend_category]

# Format ecoprovinces for web for geojson
data("ecoprovinces")
ecoprovinces <- ecoprovinces[ecoprovinces$CPRVNCNM %in% precipdata$Ecoprovince,]
ecoprovinces <- merge(ecoprovinces, 
                      subset(precipdata, Measure == "Average Precipitation" & 
                               Season == "Annual"), 
                      by.x = "CPRVNCNM", by.y = "Ecoprovince")

ecoprovinces <- ecoprovinces[,c("CPRVNCNM", "Trend_percentcentury", "Stat_Significance", "fillColor")]
names(ecoprovinces)[1:3] <- c("region", "trend", "sig")
ecoprovinces$region <- tools::toTitleCase(tolower(ecoprovinces$region))

## clips to bc boundaries and simplifies for web map
spTransform(ecoprovinces, CRS("+init=epsg:4326")) %>% 
  ms_clip(bc_bound) %>% 
  ms_simplify(0.01, keep_shapes = TRUE) %>% 
  geojson_write(file = "outprecip/ecoprovinces.geojson", precision = 5)

