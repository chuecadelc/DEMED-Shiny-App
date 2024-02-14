### Required Libraries ###

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(rgdal)
library(ggpubr)
library(countrycode)
library(DT)
library(sf)
library(maps)

### Importing the data set ###

data <- read.csv("demed_cy.csv", header=T, stringsAsFactors = T) 

#str(data)
#summary(data) 
## importing the dataset with indicator point estimates all rescaled to a 0-1 interval

data_standard <- read.csv("demed_resc_cy.csv", header=T, stringsAsFactors = T)

data_std_sub <-
  select(data_standard,
         -contains(
           c("_osp", "codehigh68", "codelow68", "codelow95", "codehigh95", "_sd", "_mean", "_ord")
         ))

## subset without the regions/electoral democracies stuff - too many NAs
data_standard1 <- read.csv("demed_resc_cy_1.csv", header=T, stringsAsFactors = T)

data_std_sub1 <-
  select(data_standard1,
         -contains(
           c("_osp", "codehigh68", "codelow68", "codelow95", "codehigh95", "_sd", "_mean", "_ord")
         )) # %>% rename(country_name = ï..country_name)

world1 <- map_data("world") %>% 
  filter(! long > 180) %>%  select (-c("subregion")) %>% 
  mutate(country_text_id=countrycode(region, origin="country.name", 
                                     destination="iso3c"))


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "black",
  #),
  dashboardHeader(title = "DEMED Shiny App", titleWidth = 500),
  
  dashboardSidebar(
    width = 400,
    sidebarMenu(
      style = "font-size:20px",
      menuItem("Information", tabName = "Info"),
      menuItem("Country graph", tabName = "Indiv"), # 1 country multiple indicators
      menuItem("Variable graph", tabName = "Multiple"), # 1 indicator multiple countries
      menuItem("Interactive Indicator Map", tabName = "map"), 
      menuItem("Bivariate Scatterplots", tabName = "Cross-comparison")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(
      HTML(
        " @import url('https://fonts.googleapis.com/css?family=Neucha|Cabin+Sketch');
                .main-header .logo {
                font-family: 'Times New Roman', Times, serif;
                font-size: 40px;
                color: #212539;
                }
                "
      )
    ),
    
    tags$style(
      HTML(
        "@import url('https://fonts.googleapis.com/css?family=Neucha|Cabin+Sketch');
                .selectize-input {
                font-family: 'Times New Roman', Times, serif;
                font-size: 32px;
                color: #212539;
                }
                "
      )
    )),
    
    
    tabItems(
      tabItem( ## Awaiting Anja's text
        
        tabName = "Info",
        ## From the project description at gla.ac.uk
        tags$img(
          src = "demed_logo.jpg",
          height = 400,
          width = 400
        ),
        p("For more information about this project, visit the",
          a("DEMED homepage.", 
            href = "https://www.gla.ac.uk/research/az/democracyresearch/projectoverview/"), style = "font-family: 'times'; font-size:22px")
      ),
      
      tabItem(tabName = "Indiv",
              fluid = TRUE,
              
              fluidRow(
                box(
                  width = 5,
                  selectInput(
                    "country",
                    "Country:",
                    choices = c("All", levels(data_std_sub$country_name))
                    
                  ),
                  
                  pickerInput("index",
                              "Indexes/Indicators",
                              choices = names(data_std_sub),
                              multiple=T,
                              options=pickerOptions(actionsBox=T, liveSearch=T),
                              selected = "v2edcentcurrlm")
                  
                ),
                
                box(
                  width = 7,
                  
                  mainPanel(
                    p("Summary statistics"),
                    DTOutput("Summary"),
                    
                    sliderInput(
                      "date",
                      "Time Period",
                      min = min(data_std_sub$year),
                      max = max(data_std_sub$year),
                      value = c(1945,2021)
                    ),
                    
                    plotOutput("distPlot"),
                    height = 550,
                    width = 800
                  )
                )
              )
      ),
      
      tabItem(tabName = "Cross-comparison", fluid = TRUE,
              
              p("Select one country and any indexes/indicators you wish to compare"),
              
              fluidRow(
                
                box(
                  width = 5,
                  
                  selectInput("country1", "Country", choices = levels(data_std_sub$country_name), selected = "Albania"),
                  
                  sliderInput(
                    "date1",
                    "Time Period",
                    min = min(data_std_sub$year),
                    max = max(data_std_sub$year),
                    value = c(1945,2021)
                  ),
                  
                  selectInput("cov1", "X-Axis", choices = names(data_std_sub), selected = "v2edcentcurrlm"),
                  selectInput("cov2", "Y-Axis", choices = names(data_std_sub), selected = "v2edcenttxbooks")
                ), 
                
                box(
                  width = 7,
                  mainPanel(
                    title = "Scatterplot",
                    status = "primary",
                    solidHeader = TRUE,
                    plotOutput("compPlot", height = 550, width = 800)
                  )
                )
              )
      ),
      
      tabItem(
        tabName = "Multiple", fluid = TRUE,
        
        p("Select two countries and compare one of the indexes/indicators"),
        
        fluidRow(
          
          box(
            width = 5,
            pickerInput("country2", "Countries",
                        choices= levels(data_std_sub$country_name),
                        multiple=T,
                        options=pickerOptions(actionsBox=T, liveSearch=T)),
            
            sliderInput(
              "date1",
              "Time Period",
              min = min(data_std_sub$year),
              max = max(data_std_sub$year),
              value = c(1945,2021)
            ),
            
            selectInput(
              "cov3",
              "Indexes/Indicators",
              choices = names(data_std_sub),
              selected = "v2edcentcurrlm"
            ),
            
            checkboxInput(inputId = "addconf",
                          label = "Add confidence intervals to the graph?",
                          value = FALSE)
          ),
            
          box(
            width = 10,
            mainPanel(plotOutput("compPlot1")))
        )
      ),
      
      tabItem(
        tabName = "map",
        
        fluidRow(
          width = 7,
          
          tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
          
          
          sidebarPanel(
            selectInput("cov4", "Variables:", choices = names(data_std_sub1), selected= "v2edcentcurrlm"),
            selected = "Population",
            sliderInput(
              "date2",
              "Time Period",
              min = min(data_std_sub$year),
              max = max(data_std_sub$year),
              value = c(1945,2021)
            )#,
          #   selectInput("colors", "Color Scheme",
          #               rownames(subset(
          #                 brewer.pal.info, category %in% c("seq", "div")
          #               )))
          ),
          
         # leafletOutput("my_map", width = "100%", height = 600),
          plotOutput("my_map1")
        )
        
      )
      
    ) 
    
  )
)


server <- function(input, output,session) {
  
  # Individual tab ----------------------------------------------------------
  
  
  selectedCountry <- reactive({
    
    ## Calculates summary stats for All countries
    if (input$country == "All") {
      if (length(input$index) < 2) {
        data_std_sub %>%
          filter(year >= as.numeric(input$date[1]),
                 year <= as.numeric(input$date[2])) %>%
          select(input$index) %>%
          summarise_all(list(
            Mean = mean,
            Median = median,
            Std.dev = sd
          ), na.rm = TRUE)
      }
      
      else{
        data_std_sub %>%
          filter(year >= as.numeric(input$date[1]),
                 year <= as.numeric(input$date[2])) %>%
          select(input$index) %>%
          summarise_all(list(
            Mean = mean,
            Median = median,
            Std.dev = sd
          ), na.rm = TRUE) %>%
          tidyr::pivot_longer(
            cols = everything(),
            names_sep = "_",
            names_to  = c("variable", ".value")
          )
      }
    }
    
    ## Calculates summary stats for a specific country only
    else {
      if (length(input$index) < 2) {
        data_std_sub %>%
          filter(
            country_name %in% input$country,
            year >= as.numeric(input$date[1]),
            year <= as.numeric(input$date[2])
          ) %>%
          select(input$index) %>%
          summarise_all(list(
            Mean = mean,
            Median = median,
            Std.dev = sd
          ), na.rm = TRUE)
      }
      
      else{
        data_std_sub %>%
          filter(
            country_name %in% input$country,
            year >= as.numeric(input$date[1]),
            year <= as.numeric(input$date[2])
          ) %>%
          select(input$index) %>%
          summarise_all(list(
            Mean = mean,
            Median = median,
            Std.dev = sd
          ), na.rm = TRUE) %>%
          tidyr::pivot_longer(
            cols = everything(),
            names_sep = "_",
            names_to  = c("variable", ".value")
          )
      }
      
    }
    
  })
  
  ## prints summary stats either for all countries or the one chosen
 
  output$Summary <-  DT::renderDataTable({
      datatable(
        selectedCountry(), selection = 'none', class = 'cell-border strip hover'
      ) %>% formatStyle(columns =0, cursor = 'pointer')

    
  })
 
  
  draw_chart <- function(df, listv, d){
    
    df <- data_std_sub %>%
      gather("variable", "value", 3:46) %>%
      filter(variable %in% listv)
    
    df1 <- df %>%
      group_by(country_name, year) %>%
      summarise(value = mean(value)) %>%
      mutate(variable = "m")
    
    df3 <- bind_rows(df, df1) 
    
  
    df3 %>%
      group_by(country_name,year) %>% 
      filter(country_name %in% input$country, variable != 'm',
             year >= as.numeric(input$date[1]),
             year <= as.numeric(input$date[2])) %>% 
      ggplot(aes(x = year, y = value ,color = variable)) + 
      geom_line() + 
      scale_color_discrete() 
    
  }
  
  #Render the plot -- currently fixing the plot so it allows multiple values
  
  output$distPlot = renderPlot({
    if (input$country == "All" & length(input$index) < 2) {
     
       data_std_sub %>%
        group_by(year) %>%
        filter(year >= as.numeric(input$date[1]),
               year <= as.numeric(input$date[2])) %>%
        select(input$index) %>%
        summarise_all(list(Mean = mean), na.rm = TRUE) %>%
        ggplot(aes(
          x = year,
          y = Mean,
          color = input$index
        )) +
        geom_line(linewidth = 1.5) +
        scale_color_discrete() +
        labs(y = input$index,
             x = "Years") +
        
        theme(
          axis.text = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 14, face = "bold")
        ) +
        theme(plot.title = element_text(size = 22))
    }
    
    else if (input$country == "All" & length(input$index) > 1) {
      data_std_sub %>%
        group_by(year) %>%
        filter(year >= as.numeric(input$date[1]),
               year <= as.numeric(input$date[2])) %>%
        select(input$index) %>%
        summarise_all(list(Mean = mean), na.rm = TRUE) %>%
        tidyr::pivot_longer(
          -year,
          names_sep = "_",
          names_to  = c("variable", ".value")
        ) %>% 
        ggplot(aes(
          x = year,
          y = Mean,
          color = variable
        )) +
        geom_line(linewidth = 1.5) +
        scale_color_discrete() +
        labs(y = input$index,
             x = "Years") +
        
        theme(
          axis.text = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 14, face = "bold")
        ) +
        theme(plot.title = element_text(size = 22))
    }
    
    else {
      draw_chart(data_std_sub, input$index, input$country)
     
    }
  })
  
  # Cross-country tab -------------------------------------------------------
  
  selectedData1 <- reactive({
    as.numeric(unlist(data_std_sub[, input$cov1]))
  })
  
  selectedData2 <- reactive({
    as.numeric(unlist(data_std_sub[, input$cov2]))
  })
  
  
  ## cross-country comparison w/ 2 indexes/indicators
  
  output$compPlot <-renderPlot({
    
    data_std_sub %>%
      group_by(country_name) %>%
      filter(country_name == as.character(input$country1),
             year >= as.numeric(input$date1[1]),
             year <= as.numeric(input$date1[2])) %>%
      ggplot(aes_string(x=input$cov1, y=input$cov2, na.rm=TRUE))+
      geom_point(
        alpha = 1 / 5,
        position = "jitter",
        size = 3
      ) +
      geom_smooth(method = "lm",
                  se = FALSE,
                  color = "black") +
      ggtitle("Scatterplot") +
      labs(x = input$cov1, y = input$cov2) +
      scale_y_continuous(limits = c(0, max(selectedData2()))) +
      theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      theme(plot.title = element_text(size = 22))
    
  })
  
  # Between-countries tab ---------------------------------------------------
  
  
  ## Scatterplot comparing multiple countries for a given indicator over time - like V-Dem
  output$compPlot1 <- renderPlot({
    
    ## Adds confidence intervals for 
    
    if (input$addconf == "TRUE") {
      
      osp_intervals_low <- data %>% select(c(year,country_name),contains("_osp_codelow68"))
      osp_intervals_high <- data %>% select(c(year,country_name),contains("_osp_codehigh68"))
      
      plot_dat <- data %>%
        group_by(country_name) %>%
        filter(
          country_name %in% input$country2,
          year >= as.numeric(input$date1[1]),
          year <= as.numeric(input$date1[2])
        ) %>%
        select(country_name, year, input$cov3[1], paste0(input$cov3[1],"_codelow68"), paste0(input$cov3[1],"_codehigh68"))
      
      ggplot(plot_dat, aes_string(x = names(plot_dat)[2], y = names(plot_dat)[3], color = names(plot_dat)[1])) +
        geom_line(linewidth = 1.5) +
        scale_x_continuous(breaks = seq(1945, 2020, 5),
                           limits = c(1945, 2021)) +
        ggtitle(paste("Scatterplot of ",  input$cov3 , " over time"))+
        labs(
          y = input$cov3,
          x = "Years") +
        theme(
          axis.text = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 14, face = "bold")
        ) +
        theme(plot.title = element_text(size = 12)) +
        geom_ribbon(aes_string(ymin = names(plot_dat)[4], ymax = names(plot_dat)[5]), ### Add/Remove this line based on whether CI option is True/False
                    alpha = 0.1) 
      
      
    }
     
    
    ## Scatterplot of countries, indicator and year(s) w/out conf.intervals
    else {
      
      
      data_std_sub %>%
        group_by(country_name) %>%
        filter(
          country_name %in% input$country2,
          year >= as.numeric(input$date1[1]),
          year <= as.numeric(input$date1[2])
        ) %>%
        ggplot(aes_string(y = input$cov3)) +
        geom_line(aes(x = year, color = country_name), linewidth = 1.5) +
        scale_x_continuous(breaks = seq(1945, 2020, 5),
                           limits = c(1945, 2021)) +
        
        ggtitle(paste("Scatterplot of ",  input$cov3 , " over time"))+
        labs(
          y = input$cov3,
          x = "Years") +
        
        theme(
          axis.text = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 14, face = "bold")
        ) +
        theme(plot.title = element_text(size = 22))
      
    }
    
    
  })
  
  

# Interactive Map ---------------------------------------------------------
  
  ## below code should work in principle, tested without the reactive statements, but doesn't yet
  
  output$my_map <- renderPlot({ 

  world_map <- map_data("world") %>% 
    filter(! long > 180) %>%  select (-c("subregion")) %>% 
    mutate(country_text_id=countrycode(region, origin="country.name", 
                                       destination="iso3c")) %>%
    rowwise() %>% 
    left_join(.,
              data_std_sub1 %>% filter(year >= as.numeric(input$date2[1]),
                                       year <= as.numeric(input$date2[2])) %>% 
                select(country_text_id, year, input$cov4)) %>%
    filter(!(lat<=13 & long<=-115)) 
  
  
  
  p <-
    ggplot(world_map, aes(
      x = long,
      y = lat,
      fill = input$cov4,
      group = group
    )) +
    geom_polygon(color = "white")
  
  p

})
  
}


# Run the application 
shinyApp(ui = ui, server = server)
