### load libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggwordcloud)

######################

ufo <- read.csv('nuforc_events.csv')

### UI----------------------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("united"),
  
  
  # title
  titlePanel('UFO Sightings'),
  
  # sidebar layout
  sidebarLayout(
    
    # panel inputs
    sidebarPanel(
      
      # Input: year range ----
      sliderInput("range", "Year Range:",
                  min = 1800, max = 2020,
                  value = c(1900,2020)),
      
      # Input: select shape ---
      checkboxGroupInput(inputId = 'UFO_Shape',
                       label = 'Shape of UFO:',
                       choices = c("Other", "Light", "Triangle", "Sphere", "Circle", "Unknown", "Changing",
                                   "Oval", "Cross", "Formation", "Cone", "Teardrop", "Cigar", "Fireball", "Flash",
                                   "Egg", "Diamond", "Disk", "Chevron", "Rectangle", "Cylinder", "Triangular", "Hexagon",
                                   "Flare", "Crescent", "Delta", "Pyramid", "Round", "Changed", "Dome", "NA"),
                       selected = c("Light", "Circle"))
  ),
      
    
    # output - set up navigation tabs
    mainPanel(
      
      tabsetPanel(
        
        # introduction
        tabPanel('*~*Welcome*~*', 
                 br(), 
                 br(), 
                 textOutput('introtext')
        ),
        
        # plots
        tabPanel('Plots',
                 br(), 
                 textOutput('counttext'), 
                 br(), 
                 plotOutput(outputId = 'lineplot'), 
                 br(), 
                 plotOutput(outputId = 'wordcloud'), 
                 br(), 
                 plotOutput(outputId = 'barplot')
        ),
        
        # data table
        tabPanel('Data', 
                 br(), 
                 textOutput('dttitletext'), 
                 DT::dataTableOutput(outputId = 'ufotable')
        ),
        
        # download
        tabPanel('Download', 
                 br(), 
                 br(), 
                 textOutput('downloadtext'), 
                 downloadButton(outputId = 'download', label = 'Download')
        )
        
      )
    )
  )
)

######################

### SERVER ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  ## REACTIVES!
  
  # reactive - filter dataframe based on inputs (date range, shape)
  ufo_filtered <- reactive({
    ufo %>%
      filter((Year >= input$range[1]) & (Year <= input$range[2]) & (Shape %in% input$UFO_Shape))
  })
  
  # reactive - dataset for barchart of sightings per month
  ufo_month_hist <- reactive({
    ufo_filtered() %>% 
      group_by(Month) %>% 
      summarize(n_month = n())
  })
  
  # reactive - dataset for wordcloud by city, keep top 50
  ufo_city_cloud <- reactive({
    ufo_filtered() %>% 
      group_by(City) %>%
      summarize(n_city = n()) %>%
      arrange(desc(n_city)) %>%
      top_n(50)
  })
   
  ## OUTPUT -------------------------------------------------------------------------
  # intro text
  output$introtext <- renderText({'
        *WeLcOMe* earthling. This Shiny App explores UFO Sightings Reported to the National UFO Reporting Center from early history until 2017.
        You can explore trends in sightings over the years, frequency of sightings by month, and the top 50 cities with the most sightings. 
        You can customize the dataset by shape of UFO and year using the controls on the left hand side of the page.
        Selections will be reflected in the data presented throughout the rest of the app.
        Use the tab bar located at the top of the page to navigate through the app.'})
  
  ## PLOTS!
  # request count text
  output$counttext <- renderText({paste('The number of sightings in your current dataset is:', nrow(ufo_filtered()))})
  
  # line plot - sightings over time
  output$lineplot <- renderPlot({
    ggplot(data = ufo_filtered(), 
           aes(x = ufo_filtered()$Year)) +
      geom_line(stat = 'count') +
      labs(x = 'Year', 
           y = 'Number of Sightings', 
           title = 'UFO Sightings Per Year') +
      theme(text = element_text(size = 18))
  })
  
  # Create word cloud based on city
  output$wordcloud <- renderPlot({
  ggplot(data = ufo_city_cloud(), aes(label = City, size = n_city)) +
    geom_text_wordcloud() + 
    labs(title = 'Top 50 Cities for UFO Sightings') +
    scale_size_area(max_size = 10) +
    theme(text = element_text(size = 18))
    
  })
  
  
  # bar plot of requests by month
  output$barplot <- renderPlot({
    ggplot(data = ufo_month_hist(), 
           aes(x = ufo_month_hist()$Month, 
               y = ufo_month_hist()$n_month)) +
      geom_bar(stat = 'identity') + 
      labs(x = 'Month', 
           y = 'Number of Sightings', 
           title = 'Sightings by Month') +
      scale_x_continuous(breaks=seq(0, 12, 1)) +
      theme(text = element_text(size = 18),
            axis.text.x = element_text(angle = 45, hjust = 1)) 
            
  })
  
  ## DATA TABLE!
  # data table text
  output$datatabletitle <- renderText({'UFO Sightings Data Table'})
  
  # data table
  output$ufotable <- DT::renderDataTable({
    DT::datatable(data = ufo_filtered()[, c('Year', 'Month', 'Day', 'Hour', 'Minute', 'City', 'State', 'Shape', 'Summary', 'Event_URL')],
                  options = list(pageLength = 25),
                  rownames = FALSE)
  })
  
  ## DOWNLOAD !
  # download text
  output$download <- renderText({'Click the button below to download the data with all current selections.'})
  
  # download button
  output$download <- downloadHandler(
    filename = function() {
      paste('ufo_sightings_','.csv', sep = '')
    },
    content = function(file) {
      write.csv(ufo_filtered()[, c('Year','Month', 'Day', 'Hour', 'Minute', 'City', 'State', 'Shape', 'Summary', 'Event_URL')], file, row.names = FALSE)
    })
  
}

######################

### run the application 
shinyApp(ui = ui, server = server)