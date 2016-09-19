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
library(maptools) # map
library(rgeos) 
library(rmapshaper) #ms_simplify
