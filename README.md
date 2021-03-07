# STA2453-proj2-group6
:rocket:  Project 2 for STA2453 at University of Toronto, 2020 - 2021  

:clipboard: A data dashboard that visualizes various aspects of the COVID-19 pandemic in canada, including prevalence of the disease, status on vaccine rollout, reports on residents' mobility changes, and air travel flow-map. 

Unfortunately this dashboard is not hosted on any external server. <br>
To locally serve the dashboard with the most up-to-date data, simply download and open `covid_dashboard_shiny_app.R` in [RStudio](https://rstudio.com/), and click the "run" button. :sparkles: <br>
Note that you don't need local copies of other files in this repo. `covid_dashboard_shiny_app.R` is all you need.

### Data Source
This dashboard is built using Shiny dashboard and R. 

Our data sources are:
* Government of Canada, ["Coronavirus disease (COVID-19): Locations where you may have been exposed to COVID-19"](https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection/latest-travel-health-advice/exposure-flights-cruise-ships-mass-gatherings.html#wb-auto-5)
* Stanford University, [geometric location of airports](http://www.stanford.edu/~cengel/cgi-bin/anthrospace/wp-content/uploads/2012/03/airports.csv)
* Government of Canada, [Public Hleath Infobase](https://health-infobase.canada.ca/)
* Google, [Community Mobility Report](https://www.google.com/covid19/mobility/)
* [COVID-19 Canada Open Data Working Group](https://opencovid.ca/)

### Data Ingestion
Our sources of data can be broken down into 3 categories:
* Those readily available on the internet as CSV files
* Those that exist on the web in html form (needs to be scraped and processed)
* Those that exist on the web as zip files (google mobility data)

For the first kind, we directly read from the web source. <br>
For the second, scraping and processing is done in our R code. <br>
For the third, the zip file is downloaded to the local folder from which the shiny app is run. However, if there's already an up-to-date zip file existing in the folder, this download is skipped to save time. After download, the zip file is unzipped to extract the csv containing mobility data for Canada. This csv is then read into the shiny app. This process creates 3 files in the local directory: 1) the zip file, 2) the csv containing the mobility data, and 3) a small csv file used to keep track of the most recent download, and to determine whether a new download is needed. 
