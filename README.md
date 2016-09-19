<div id="devex-badge">
<a rel="Exploration" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="http://bcdevexchange.org/badge/2.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a>
</div>
---


# Data Visualizations for Indicators of Climate Change for B.C. (2015)

A set of R scripts for creating data visualizations for climate change indicators published on [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=3C233B4F802A4FE186297EC52311E40C) in 2015:

- [Change in Size of B.C. Glaciers (1985-2005)](http://www.env.gov.bc.ca/soe/indicators/climate-change/glaciers.html)
- [Long-term Change in Heat Energy Available for Plant Growth in B.C. (1900-2013)](http://www.env.gov.bc.ca/soe/indicators/climate-change/growing-days.html)
- [Long-term Change in Energy Requirements for Heating & Cooling Buildings in B.C.](http://www.env.gov.bc.ca/soe/indicators/climate-change/heating-cooling-days.html)
- [Long-term Change in Air Temperature in B.C. (1900-2013)](http://www.env.gov.bc.ca/soe/indicators/climate-change/temp.html)
- [Long-term Change in Precipitation in B.C. (1900-2013)](http://www.env.gov.bc.ca/soe/indicators/climate-change/precip.html)


### Usage

#### Data
The data used to develop the indicator data visualizations are available from the [BC Data Catalogue](https://catalogue.data.gov.bc.ca/dataset?download_audience=Public) under the
[Open Government License - BC](http://www2.gov.bc.ca/gov/content/governments/about-the-bc-government/databc/open-data/open-government-license-bc):

- [Change in Size of Glaciers in BC (1985-2005)](https://catalogue.data.gov.bc.ca/dataset/89ff86d7-2d04-4c96-b945-ba56688906eb)
- [Long-term Change in Growing Degree Days and Heating and Cooling Degree Days in BC](https://catalogue.data.gov.bc.ca/dataset/8f0d304e-161d-42e6-a982-cad13e60bd8f)
- [Long-term Change in Air Temperature and Precipitation in BC](https://catalogue.data.gov.bc.ca/dataset/86f93096-8d3d-4b68-ab63-175cc68257e6)


####Code

Most packages used in the code can be installed from CRAN using `install.packages()`, but you will need to install [envreportutils](https://github.com/bcgov/envreportutils) using devtools:


```r
install.packages("devtools") # If you don't already have it installed

library(devtools)
install_github("bcgov/envreportutils")
```

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/<repo-name>/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.


### License

    Copyright 2016 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
    
This repository is maintained by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B). Click [here](https://github.com/bcgov/EnvReportBC-RepoList) for a complete list of our repositories on GitHub.
