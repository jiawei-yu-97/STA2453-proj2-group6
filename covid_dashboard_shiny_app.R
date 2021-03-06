required_libraries = c(
  "geosphere",     
  "rgdal",
  "plotly",        
  "shinydashboard",
  "tibble",        
  "imputeTS",      
  "DT",
  "fs",
  "rvest",
  "sf",
  "shiny",
  "tidyverse",     
  "lubridate",     
  "rcarbon",       
  "dplyr",
  "rmapshaper",
  "leaflet",
  "scales",
  "rworldmap",
  "magrittr",
  "spdplyr",
  "ggplot2"
)

for (lib in required_libraries){
  if (!require(lib, character.only = TRUE, quietly=TRUE)){
    install.packages(toString(lib))
    library(lib, character.only=TRUE)
  }
}


#############
# FUNCTIONS #
#############
movingAverage <- function(vec, window_size){
  na_idx <- is.na(vec)
  if (all(na_idx)){
    vec
  } else {
    vec <- na_interpolation(vec)
    vec <- runMean(vec, window_size)
    vec[na_idx] <- NA
    vec
  }
}


update_name <- Vectorize(function(name){
  if (name == 'British Columbia'){
    'BC'
  } else if (name == 'Newfoundland and Labrador') {
    'NL'
  } else if (name == 'Prince Edward Island') {
    'PEI'
  } else if (name == 'Northwest Territories') {
    'NWT'
  } else if (name == ''){
    'Canada'
  } else{
    name
  }
})


concat_locality <- Vectorize(function(province, region){
  if (region == ''){
    province
  } else {
    paste0(province, ' - ', region)
  }
})


remove_region <- Vectorize(function(name){
  if (!grepl('-', name)){
    name
  } else{
    index <- regexpr('-', name)[[1]]
    substr(name, 1, index - 2)
  }
})


###################
# DATA PROCESSING #
###################
# some metadata and population data
# population estimate from https://en.wikipedia.org/wiki/Population_of_Canada_by_province_and_territory
provinces <- c('Canada','Ontario', 'Quebec', 'BC','Alberta', 'Manitoba', 'Saskatchewan', 'Nova Scotia',
               'New Brunswick', 'NL','PEI','NWT','Nunavut','Yukon')

population <- tibble(
  province = provinces,
  pop = c(38008005, 14733119, 8575779, 5145851, 4428112, 1379584, 1177884, 979115,
          781315, 520998, 159713, 45074, 39285, 42176)
)


# cases, recoveries, deaths, and tests
daily_case_link <- "https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv"

daily_case_ca <- read.csv(daily_case_link)%>% filter(prname != 'Repatriated travellers') %>%
  mutate(province = update_name(prname)) %>%
  right_join(population, by = c('province' = 'province')) %>%  
  group_by(province)%>%
  mutate(death_pc = (numdeaths/pop)* 100, 
         cumulative_case = cumsum(numtoday),
         cumulative_pc = (cumulative_case/pop)* 100, 
         recover_pc = (numrecover/pop) *100,
         death_daily_pc = (numdeathstoday/pop)*100,
         cases_daily_pc = (numtoday/pop)*100,
         tested_pc = (numtested/pop)*100,
         tested_daily_pc = (numtestedtoday/pop)*100,
         date = ymd(date))

# Vaccine data
read_github_data <- function(provincial_link, national_link){
  provincial_data <- read.csv(provincial_link)
  national_data <- read.csv(national_link)
  bind_rows(national_data, provincial_data)
}

administered_pr_link = 'https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_prov/vaccine_administration_timeseries_prov.csv'
administered_national_link = 'https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_canada/vaccine_administration_timeseries_canada.csv'
administered <- read_github_data(administered_pr_link, administered_national_link) %>%
  mutate(date = dmy(date_vaccine_administered)) %>%
  right_join(population) %>%
  mutate(daily_pc = avaccine / pop * 100, cumulative_pc = cumulative_avaccine/pop * 100)

completed_pr_link <- 'https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_prov/vaccine_completion_timeseries_prov.csv'
completed_national_link = 'https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_canada/vaccine_completion_timeseries_canada.csv'
completed <- read_github_data(completed_pr_link, completed_national_link) %>%
  mutate(date = dmy(date_vaccine_completed)) %>%
  right_join(population) %>%
  mutate(daily_pc = cvaccine / pop * 100, cumulative_pc = cumulative_cvaccine/pop * 100)

distributed_pr_link <- 'https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_prov/vaccine_distribution_timeseries_prov.csv'
distributed_national_link <- 'https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_canada/vaccine_distribution_timeseries_canada.csv'
distributed <- read_github_data(distributed_pr_link, distributed_national_link) %>%
  mutate(date = dmy(date_vaccine_distributed)) %>%
  right_join(population) %>%
  mutate(daily_pc = dvaccine / pop * 100, cumulative_pc = cumulative_dvaccine/pop * 100)

cases_pr_link <- 'https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_prov/cases_timeseries_prov.csv'
cases_national_link <- 'https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_canada/cases_timeseries_canada.csv'
cases <- read_github_data(cases_pr_link, cases_national_link) %>%
  mutate(date = dmy(date_report))%>%
  right_join(population) %>%
  mutate(cases_pc = cases / pop * 100, cumulative_pc = cumulative_cases/pop * 100)


# Mobility data
# DOWNLOAD mobility data as a zip file, and unzip to extract the csv for canada
meta_file <- './metadata.csv'
download_mobility <- function(){
  mobility_zip_link = 'https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip'
  download.file(mobility_zip_link, destfile = './mobility.zip')
  # UNZIP
  unzip('./mobility.zip', files = c('2020_CA_Region_Mobility_Report.csv'))
  meta <- tibble(date = c(Sys.Date()))
  write.csv(meta, meta_file)
}

if (file.exists(meta_file)){
  meta <- read.csv(meta_file)
  date <- meta$date[1]
  if (date != Sys.Date() | !file.exists('2020_CA_Region_Mobility_Report.csv')){
    download_mobility()
  }
}else{
  download_mobility()
}

mobility <- read.csv('2020_CA_Region_Mobility_Report.csv') %>% 
  rename(province = sub_region_1, 
         region = sub_region_2,
         retail_and_recreation = retail_and_recreation_percent_change_from_baseline,
         grocery_and_pharmacy = grocery_and_pharmacy_percent_change_from_baseline,
         parks = parks_percent_change_from_baseline,
         transit_stations = transit_stations_percent_change_from_baseline,
         workplaces = workplaces_percent_change_from_baseline,
         residential = residential_percent_change_from_baseline) %>%
  select(-c(country_region_code, country_region, metro_area, iso_3166_2_code, census_fips_code)) %>%
  mutate(date = ymd(date), 
         province = update_name(province),
         province_region = concat_locality(province, region)) %>%
  filter(!is.na(province))

# get list of places (residential, parks, work space, etc.)
places <- colnames(mobility)
places <- places[places != 'province' & places != 'region' & places != 'date' & places != 'province_region' & places != 'place_id']


# Map Settings
canada_map <- st_read('https://raw.githubusercontent.com/jiawei-yu-97/STA2453-proj2-group6/main/canada_provinces.geojson') %>%
  mutate(province = update_name(name))
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

ggline_theme <- theme_bw() + 
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"), 
        panel.grid.minor = element_blank())

theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) +
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank()
    )
}


# Air transport flowmap 
airports <- read.csv("http://www.stanford.edu/~cengel/cgi-bin/anthrospace/wp-content/uploads/2012/03/airports.csv", as.is=TRUE, header=TRUE)
airport_locations <- airports[, c("IATA","longitude", "latitude")]
# Scrape the Data
link = 'https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection/latest-travel-health-advice/exposure-flights-cruise-ships-mass-gatherings.html#wb-auto-5'
webpage <- read_html(link)
tb <- webpage %>% html_nodes("table") 
domestic <- tb[1] %>% html_table(fill=TRUE) %>%.[[1]]
inter <- tb[2] %>% html_table(fill=TRUE) %>%.[[1]]
names(domestic)[5] <- 'flightDate'
names(inter)[5] <- 'flightDate'
domestic[c('DepartSym')] <- apply(domestic[c('Departing')],1,function(x) substr(x,nchar(x) - 3, nchar(x)-1))
domestic[c('DestinationSym')] <- apply(domestic[c('Destination')],1,function(x) substr(x,nchar(x) - 3, nchar(x)-1))
inter[c('DepartSym')] <- apply(inter[c('Departing')],1,function(x) substr(x,nchar(x) - 3, nchar(x)-1))
inter[c('DestinationSym')] <- apply(inter[c('Destination')],1,function(x) substr(x,nchar(x) - 3, nchar(x)-1))
earliest_date = min(min(domestic$flightDate), min(inter$flightDate))
latest_date = min(max(domestic$flightDate), max(inter$flightDate))

#################
#     SHINY     #
#################

status <- 'primary'

ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(title = "COVID-19 Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pandemic Prevalence", tabName = "prevalence", icon = icon("hospital-symbol")),
      menuItem("Vaccine Rollout", tabName = "vaccine", icon = icon("syringe")),
      menuItem("Mobility Report", tabName = "mobility", icon = icon("walking")),
      menuItem("COVID via Air Travel", tabName = "airflow", icon = icon("plane-arrival"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = 'prevalence',
              h2('Canadian COVID-19 Pandemic Prevalence', align="center"),
              fluidRow(
                box(status=status, title='Pandemic Prevalence - Compare Province', solidHeader=TRUE,
                    fluidRow(
                      column(4, selectInput('prevalence_cp_province','Select Province',choices = provinces, multiple = TRUE, selected='Canada')),
                      column(3, radioButtons("prevalence_cp_mode", "Select mode", c('Daily','Cumulative'), selected = 'Cumulative')),
                      column(5, sliderInput('prevalence_cp_smoothing', 'Control window of moving average', value = 1, min = 1, max = 28))
                    ),
                    tabsetPanel(
                      tabPanel('Cases', plotlyOutput("prevalence_time_cum")),
                      tabPanel('Death', plotlyOutput("prevalence_time_death")) ,
                      tabPanel('Tests', plotlyOutput("prevalence_time_test")) 
                    )),
                box(status=status, title='Pandemic Prevalence - Map', solidHeader=TRUE,
                    radioButtons("prevalence_map_mode", "Select mode", c('Cumulative', 'Weekly'), selected = 'Cumulative'),
                    tabsetPanel(
                      tabPanel('Cases', plotOutput("prevalence_map_cum")),
                      tabPanel('Death', plotOutput("prevalence_map_death")) ,
                      tabPanel('Recover', plotOutput("prevalence_map_recover")) 
                    ))
              ),
              fluidRow(
                box(status=status, title='Pandemic Prevalence by Province', solidHeader=TRUE,
                    fluidRow(
                      column(6, selectInput("prevalence_province", "Province",choices = provinces, selected='Canada')),
                      column(6, sliderInput('prevalence_smoothing', 'Control window of moving average', value = 1, min = 1, max = 28))
                    ),
                    plotlyOutput("prevalence_time_series"))
              )
      ),
      
      tabItem(tabName = 'vaccine', 
              h2('Canadian Vaccine Rollout Status', align="center"),
              fluidRow(
                box(status = status, title='Vaccine rollout - Time Series',solidHeader = TRUE,
                    fluidRow(
                      column(4, selectInput('vaccine_province','Select Province',choices = provinces, multiple = TRUE, selected='Canada')),
                      column(3, radioButtons("vaccine_line_mode", "Select mode", c('Daily','Cumulative'), selected = 'Cumulative')),
                      column(5, sliderInput('vaccine_line_smoothing', 'Control window of moving average', value = 1, min = 1, max = 28))
                    ),
                    
                    tabsetPanel(
                      tabPanel('Administered', plotlyOutput("vaccine_administered")),
                      tabPanel('Completed', plotlyOutput("vaccine_completed")),
                      tabPanel('Distributed', plotlyOutput("vaccine_distributed"))
                    )),
                
                box(status = status, title='Vaccine rollout - Map',solidHeader = TRUE,
                    radioButtons("vaccine_map_mode", "Select mode", c('Cumulative', 'Weekly'), selected = 'Cumulative'),
                    tabsetPanel(
                      tabPanel('Administered', plotOutput("map_administered")),
                      tabPanel('Completed', plotOutput("map_completed")),
                      tabPanel('Distributed', plotOutput("map_distributed"))
                    ))
              )
      ),
      
      tabItem(tabName = 'mobility',
              h2("Canadian Residents' Mobility Report", align="center"),
              fluidRow(
                box(title='Mobility Data: Comparing regions',solidHeader = TRUE,status = status,
                    fluidRow(
                      column(6,selectInput('mr_region','Select Region', choices = unique(mobility$province_region), multiple = TRUE, selected='Canada')),
                      column(6,sliderInput('mr_smoothing', 'Control window of moving average', value = 1, min = 1, max = 28))
                    ),
                    selectInput('mr_type', 'Select type of places', choices = places, multiple=FALSE, selected='retail_and_recreation'),
                    plotOutput("mobility_region")),
                
                box(title='Mobility Data: Comparing types of places',solidHeader = TRUE,status = status,
                    fluidRow(
                      column(6,selectInput('mt_region','Select Region', choices = unique(mobility$province_region), multiple = FALSE, selected='Canada')),
                      column(6,sliderInput('mt_smoothing', 'Control window of moving average', value = 1, min = 1, max = 28))
                    ),
                    selectInput('mt_type', 'Select type of places', choices = places, multiple=TRUE, selected=places),
                    plotOutput("mobility_type"))
              )),
      
      tabItem(tabName = 'airflow',
              h2('Flowmap: COVID-19 in Air Travels', align="center"),
              fluidRow(
                column(8, 
                       box(title='Data Selector', status=status,
                           sliderInput("flowmap_dates",
                                       "Dates:",
                                       min = ymd(earliest_date),
                                       max = ymd(latest_date),
                                       value = ymd(earliest_date),
                                       timeFormat = '%m/%d/%y'),
                           selectInput('dDestination','Please Select Domestic Destination:',
                                       choices = unique(domestic$Destination),
                                       selected = 'Toronto (YYZ)'),
                           selectInput('iDestination','Please Select International Destination:',
                                       choices = unique(inter$Destination),
                                       selected = 'Toronto (YYZ)')
                       ))
              ),
              fluidRow(
                box(title='Static Flow Map', status=status, solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("Domestic Flow", plotOutput("domes")),
                      tabPanel("International Flow", plotOutput("inter"))
                    )
                ),
                box(title='Dynamic Flow Map', status=status, solidHeader = TRUE,
                  tabsetPanel(
                    tabPanel("Domestic Flow", leafletOutput("map2")),
                    tabPanel("International Flow", leafletOutput('map1'))
                  )
                )
              )
      )
    )
  )
)


server <- function(input, output, session){
  
  ##########################
  # FUNCTIONS FOR ALL TABS #
  ##########################
  
  get_data_for_map <- function(data, mode, precision=2) {
    # mode is either "Cumulative' or "Weekly"
    # note that the data should be massaged so that there is a dependent column named "y"
    new_data <- data %>% group_by(province)
    if (mode == 'Cumulative'){
      new_data <- new_data %>%
        filter(date == max(date))
    } else{
      data_last_week <- new_data %>%
        filter(date == max(date) - 7) %>%
        mutate(last_week = y) %>%
        select(date, province, last_week)
      new_data <- new_data %>% 
        filter(date == max(date)) %>%
        left_join(data_last_week, by=c('province' = 'province')) %>%
        mutate(y = y - last_week)
    }
    new_data %>%
      mutate(y = round(y, precision)) %>%
      right_join(canada_map, by = 'province') %>%
      unique()
  }
  
  
  get_map_from_data <- function(data){
    ggplot(data = data) +
      geom_sf(aes(fill = y, geometry = geometry), color = "gray60", size = 0.1, show.legend=TRUE) + 
      ggrepel::geom_label_repel(
        aes(label = y, geometry = geometry),
        stat = "sf_coordinates",
        min.segment.length = 0.25) + 
      coord_sf(crs = crs_string) + 
      theme_map() +
      theme(panel.grid.major = element_line(color = "white"),
            legend.key = element_rect(color = "gray40", size = 0.1))
  }
  
  
  plot_time_series_plotly <- function(data, mode, smoothing_handle, province_handle, cumulative_name, daily_name){
    new_data <- data
    if (mode == 'Cumulative'){
      new_data$y <- data[[cumulative_name]]
    } else {
      new_data$y <-  data[[daily_name]]
    }
    new_data <- new_data %>% 
      group_by(province) %>%
      mutate(y = movingAverage(y, smoothing_handle))
    
    plot <- new_data %>% plot_ly()
    for (prov in province_handle){
      partial <- new_data %>% filter(province == prov)
      plot <- add_trace(plot, data = partial, x = ~date, y = ~y, type = "scatter", mode = "lines", name = prov)
    }
    plot <- plot %>% layout(yaxis = list(title = "Percentage of total population"), 
                            xaxis = list(title = "Date"),
                            legend=list(orientation='h', x = 1, y = 1.02, yanchor='bottom', xanchor='right'))
    plot
  }
  
  
  ################################
  # Cases, Deaths and Recoveries #
  ################################
  
  plot_prevalence_map <- function(data, name) {
    if (name == 'Deaths'){
      new_data <- get_data_for_map(data, input$prevalence_map_mode, 6) %>%
        mutate(y = round(y * 10000, 1))
    } else {
      new_data <- get_data_for_map(data, input$prevalence_map_mode, 2)
    }
    
    if (name == 'Deaths'){
      plt <- get_map_from_data(new_data) +
        labs(fill = 'per 1 million', 
             title = paste0(name, ', per 1 million population'))
    } else {
      plt <- get_map_from_data(new_data) +
        labs(fill = '% of population', 
             title = paste0(name, ', as a percentage of population'))
    }
    
    if (name == 'Recover'){
      plt + scale_fill_gradient(low = 'light blue',  high = 'dark blue')
    } else {
      plt + scale_fill_gradient(low = 'yellow',  high = 'red')
    }
  }
  
  
  plot_prevalence_time_series <- function() {
    data <- daily_case_ca %>% 
      filter(province == input$prevalence_province) %>%
      group_by(province)
    
    for (var in c('numtoday', 'numrecoveredtoday', 'numdeathstoday')){
      data[[var]] = movingAverage(data[[var]], input$prevalence_smoothing)
    }
    
    data %>%
      plot_ly(x = ~date, y = ~numtoday, type = "scatter", mode = "lines", color = I("dark green"), name = "Cases") %>% 
      add_trace(x = ~date, y = ~numrecoveredtoday, type = "scatter", mode = "lines", color = I("blue"), name = "Recovered") %>% 
      add_trace(x = ~date, y = ~numdeathstoday, type = "scatter", mode = "lines", color = I("red"), name = "Deaths", yaxis = "y2") %>% 
      layout(yaxis2 = list(overlaying = "y", side = "right", automargin = T), 
             yaxis = list(title = "Cumulative Report Number"), 
             xaxis = list(title = "Date"), 
             legend = list(orientation='h', x = 1, y = 1.02, yanchor='bottom', xanchor='right'))
  }
  
  
  output$prevalence_map_cum <- renderPlot(plot_prevalence_map(mutate(daily_case_ca, y=cumulative_pc), 'Cumulative Cases'), res = 96)
  output$prevalence_map_death <- renderPlot(plot_prevalence_map(mutate(daily_case_ca, y=death_pc), 'Deaths'), res = 96)
  output$prevalence_map_recover <- renderPlot(plot_prevalence_map(mutate(daily_case_ca, y=recover_pc), 'Recover'), res = 96)
  
  output$prevalence_time_series <- renderPlotly({plot_prevalence_time_series()})
  
  output$prevalence_time_cum <- renderPlotly(plot_time_series_plotly(daily_case_ca, 
                                                                     input$prevalence_cp_mode, input$prevalence_cp_smoothing, input$prevalence_cp_province,
                                                                      'cumulative_pc', 'cases_daily_pc'))
  output$prevalence_time_death <- renderPlotly(plot_time_series_plotly(daily_case_ca, 
                                                                       input$prevalence_cp_mode, input$prevalence_cp_smoothing, input$prevalence_cp_province,
                                                                   'death_pc', 'death_daily_pc'))
  output$prevalence_time_test <- renderPlotly(plot_time_series_plotly(daily_case_ca, 
                                                                      input$prevalence_cp_mode, input$prevalence_cp_smoothing, input$prevalence_cp_province,
                                                                     'tested_pc', 'tested_daily_pc'))
  
  
  ########################
  # Vaccine and Mobility #
  ########################
  
  plot_vaccine_map_ggplot <- function(data, name) {
    new_data <- data %>% mutate(y = cumulative_pc)
    new_data <- get_data_for_map(new_data, input$vaccine_map_mode)
    
    plt <- get_map_from_data(new_data) +
      labs(fill = '% of population', 
           title = paste0(input$vaccine_map_mode,' ',name, ' of Vaccine, as a percentage of population'))
    if (input$vaccine_map_mode == 'Cumulative'){
      plt + scale_fill_gradient(low = 'red',  high = 'green', limits=c(0, 60), na.value='green')
    } else{
      plt + scale_fill_gradient(low = 'light blue',  high = 'dark blue')
    }
  }
  
  
  plot_mobility_region_ggplot <- function() {
    region_data <- mobility %>%
      filter(province_region %in% input$mr_region) %>%
      group_by(province_region) %>%
      mutate(selected_place = get(input$mr_type)) %>%
      mutate(ra = movingAverage(selected_place, input$mr_smoothing))
    
    plot <- ggplot(data = region_data) + 
      geom_line(aes(x = date, y = ra, color = province_region), size = 1, alpha = 0.5) + 
      geom_hline(yintercept = 0, color = 'black', size = 1) +
      ggtitle(', for the corresponding day of week') + 
      labs(x = 'Date', y = 'Percentage change from baseline',
           color = 'region',
           title = 'Mobility: percentage change from January 2020',
           subtitle = 'Compared to the corresponding day of week. Certain places may have missing data.')
    plot + ggline_theme
  }
  
  
  plot_mobility_type_ggplot <- function() {
    region_data <- mobility %>%
      filter(province_region %in% input$mt_region)
    
    colors = rainbow(6)
    for (type in input$mt_type) {
      region_data[[type]] = movingAverage(region_data[[type]], input$mt_smoothing)
    }
    
    plot <- ggplot(data = region_data)
    for (type in input$mt_type) {
      plot <- plot + geom_line(aes_string(x = 'date', y = type, color = as.factor(type)), 
                               size = 1, alpha = 0.5)
    }
    
    plot <- plot + 
      geom_hline(yintercept = 0, color = 'black', size = 1) +
      labs(x = 'Date', y = 'Percentage change from baseline',
           color = 'Place type',
           title = 'Mobility: percentage change from January 2020',
           subtitle = 'Compared to the corresponding day of week') +  
      scale_colour_manual(breaks = input$mt_type, values = colors[1:length(input$mt_type)])
    plot + ggline_theme
  }
  
  
  output$vaccine_administered <- renderPlotly(plot_time_series_plotly(administered, 
                                                                      input$vaccine_line_mode, input$vaccine_line_smoothing, input$vaccine_province,
                                                                      'cumulative_pc', 'daily_pc'))
  output$vaccine_completed <- renderPlotly(plot_time_series_plotly(completed, 
                                                                  input$vaccine_line_mode, input$vaccine_line_smoothing, input$vaccine_province,
                                                                  'cumulative_pc', 'daily_pc'))
  output$vaccine_distributed <- renderPlotly(plot_time_series_plotly(distributed, 
                                                                    input$vaccine_line_mode, input$vaccine_line_smoothing, input$vaccine_province,
                                                                    'cumulative_pc', 'daily_pc'))
  
  output$map_administered <- renderPlot(plot_vaccine_map_ggplot(administered, 'Administration'))
  output$map_completed <- renderPlot(plot_vaccine_map_ggplot(completed, 'Completion'))
  output$map_distributed <- renderPlot(plot_vaccine_map_ggplot(distributed, 'Distribution'))
  
  output$mobility_region <- renderPlot(plot_mobility_region_ggplot(), res = 96)
  output$mobility_type <- renderPlot(plot_mobility_type_ggplot(), res = 96)
  
  
  #######################
  # AIR TRAVEL FLOW MAP #
  #######################
  
  plot_flowmap_ggplot <- function(trip, mode='inter') {
    worldMap <- getMap()
    mapworld_df <- fortify( worldMap )
    dest <- input$iDestination
    if (mode == 'domes'){
      dest <- input$dDestination
    }
    
    data <- trip %>% filter((flightDate >= input$flowmap_dates) & (Destination == dest)) %>%
      group_by(Departing, Destination, DepartSym, DestinationSym) %>%
      summarise(trips =n())
    if (nrow(data) == 0){
      stop('No import case detected yet for the selected data range & location')
    }
    
    data <- left_join(data, airport_locations, by=c("DepartSym"="IATA") )
    data <- left_join(data, airport_locations, by=c("DestinationSym"="IATA"))
    
    plt <- ggplot() + 
      geom_polygon(data= mapworld_df, aes(long, lat, group=group), fill="gray30") +
      geom_curve(data = data, aes(x = longitude.x, y = latitude.x, xend = longitude.y, 
                                   yend = latitude.y, color=trips),
                 curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
      scale_colour_distiller(palette="Reds", name="Frequency", guide = "colorbar") 
    
    if (mode == 'domes'){
      plt + coord_cartesian(xlim = c(-130, -50), ylim = c(35, 75))
    } else{
      plt+coord_equal()
    }
  }
  
  
  plot_flowmap_leaflet <- function(trip, mode='inter'){
    dest <- input$iDestination
    if (mode == 'domes'){
      dest <- input$dDestination
    }
    
    data <- trip %>% filter((flightDate >= input$flowmap_dates) & (Destination == dest)) %>%
      group_by(Departing, Destination, DepartSym, DestinationSym) %>%
      summarise(trips =n())
    if (nrow(data) == 0){
      stop('No import case detected yet for the selected data range & location')
    }
    
    data <- left_join(data, airport_locations, by=c("DepartSym"="IATA") )
    data <- left_join(data, airport_locations, by=c("DestinationSym"="IATA"))
    
    df2<- gcIntermediate(as.matrix(data[,c("longitude.x", "latitude.x")]),
                         as.matrix(data[,c("longitude.y", "latitude.y")]),
                         n=100,
                         addStartEnd=TRUE,
                         sp=TRUE,
                         breakAtDateLine=FALSE)
    df2 <- as(df2, "SpatialLinesDataFrame")
    df2.ff <- fortify(df2)
    data$id <-as.character(c(1:nrow(data)))
    gcircles <- merge(df2.ff, data, all.x=T, by="id")
    hover <- paste0(data$Departing, " to ",
                    data$Destination, ': ',
                    as.character(data$trips))
    
    if (data$longitude.x[1] > 0){
      center <- data$longitude.x[1]
    }else{
      center <- data$longitude.x[1] + 360
    }
    
    gcircles$long.recenter <-  ifelse(gcircles$long  < center - 180 , gcircles$long + 360, gcircles$long)
    data$long.ori.recenter <-  ifelse(data$longitude.x  < center - 180 , data$longitude.x + 360, data$longitude.x)
    data$long.dest.recenter <-  ifelse(data$longitude.y  < center - 180 , data$longitude.y + 360, data$longitude.y)
    
    test_line <- sf::st_as_sf(gcircles, coords = c("long.recenter", "lat")) %>%
      dplyr::group_by(id, piece) %>%
      dplyr::summarize(do_union=FALSE) %>%
      sf::st_cast("LINESTRING") %>%
      dplyr::ungroup()
    
    test_line2 <- dplyr::left_join(test_line, data)
    
    leaflet(data =  test_line2) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Carto DB Positron") %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Carto DB dark")%>%
      addPolylines(weight = 1.5, opacity = 0.5, label = ~hover) %>%
      addCircleMarkers(data = data, lng = ~longitude.x, lat = ~latitude.x, radius =0.5, fillOpacity = 0.5,
                       weight = 1, opacity = 0.1, color = "red") %>%
      addCircleMarkers(data = data, lng = ~longitude.y, lat = ~latitude.y, radius =1) %>%
      addLayersControl(baseGroups = c("Carto DB Positron", "Carto DB dark"))
  }
  
  
  output$inter <- renderPlot(plot_flowmap_ggplot(inter, 'inter'))
  output$domes <- renderPlot(plot_flowmap_ggplot(domestic, 'domes'))
  
  output$map1 <- renderLeaflet({plot_flowmap_leaflet(inter, 'inter')})
  output$map2 <- renderLeaflet({plot_flowmap_leaflet(domestic, 'domes')})
}


shinyApp(ui, server)
