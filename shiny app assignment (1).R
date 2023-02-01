# library for shiny app to run
library(shiny)

# library to create shiny dashboard
library(shinydashboard)

# library used for joins, groupby, summarize, filter, rename etc
library(dplyr)

# library to build types of ggplot graphs like bar, line, scatter plots etc
library(ggplot2)
library(plotly)
library(DT)

# set the working directory where all your files are stored
# we are setting the path from where we are reading the data
setwd('C:/Users/Owner new/Downloads/')


# Read fact_population csv file
fact_population <- read.csv('fact_population.csv')
#View(fact_population)

# Read dim_region csv file
dim_region <- read.csv('dim_region.csv')
#View(dim_region)

# Read dim_age csv file
dim_age <- read.csv('dim_age.csv') 
#View(dim_age)

# Read dim_gender csv file
dim_gender <- read.csv('dim_gender.csv')
#View(dim_gender)

# Merge the two files to get region wise poulation
popRegion <- merge(fact_population, dim_region, by = 'country.id', all.x = TRUE)
popAge <- merge(popRegion , dim_age , by = "age.group" , all.x = T)

#View(popRegion)
#str(popRegion)

# convert population column datatype to numeric
popRegion$population <- as.numeric(popRegion$population)

#---------------------------------Build the shiny application-----------------------------------------------------

#---------------------------------------shiny UI module-----------------------------------------------------

# dasbboardPage is the whole page of the shiny app
ui <- dashboardPage(
  
  # dashboardHeader is the header of the shiny page
  dashboardHeader(title = "Population dashboard",titleWidth = 300),
  
  # dashboardSidebar is the siderbar panel of the shiny page
  dashboardSidebar(width = 300,
                   
     # sidebarMenu is the menu that you want to add like tabs, dropdowns, buttons etc.
     sidebarMenu(
       
       # menuItem is the tab in which you will add your content
       menuItem("Region", tabName = "Region", icon = icon("dashboard")),
       menuItem("Age", tabName = "Age", icon = icon("th"))
     )
  ),
  
  # dashboardBody is the main body page apart from header & siderbar where our graphs or analysis will be displayed
  dashboardBody(
    
    # main tab contents for both the pages               
    tabItems(
      
      # First tab content
      
      tabItem(tabName = "Region",
              
              # fluidRow is a structurized row in which you add contents side by side
              fluidRow(
                
                # valueBoxOutput is the card in which you can display/highlight values
                valueBoxOutput("reg1", width = 6),
                
                valueBoxOutput("reg2", width = 6)
              ),
              
              fluidRow(
                
                # Create a box in which you can display any information like graphs, valuecards, tables etc
                box(title = "Regionwise Population", status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                    
                    # plotOutput is used to display the basic plots in R
                    plotOutput("bargraph")),
                
                box(title = "Region & Yearwise Population", status = "warning", solidHeader = TRUE, collapsible = TRUE, 
                    plotOutput("linegraph"))
              )
      ),
      tabItem(tabName = "Age",
              
              
              fluidRow(
                
                # Create a box in which you can display any information like graphs, valuecards, tables etc
                box(title = "Country Wise Population", status = "primary", solidHeader = TRUE, height = 560, collapsible = TRUE, 
                    
                    # plotOutput is used to display the basic plots in R
                    plotlyOutput("agetreegraph")),
                
                box(title = "Region & Age Category Wise Population,", status = "warning", height = 560, solidHeader = TRUE, collapsible = TRUE, 
                    dataTableOutput("mytable"))
              )
      )
    )
  )
)

#---------------------------------------shiny serve module-----------------------------------------------------

# server is to add the backend logic to display outputs in the UI
server <- function(input, output){ 
  
  #-----------------------------------1st value box & bar graph----------------------------------------------
  
  # Get region wise population
  dfr <- popRegion %>% 
    group_by(region) %>% 
    dplyr::summarise(TotalPopulation = sum(population, na.rm = TRUE))
  
  # convert it to the data frame
  dfr <- data.frame(dfr)
  
  # remove NA values 
  dfr <- subset(dfr, !is.na(dfr$region))
  
  # Get the row with the highest population
  maxp <- dfr[which.max(dfr$TotalPopulation),]
  
  # Add a thousand format to TotalPopulation 
  maxp$TotalPopulation <- format(round(as.numeric(maxp$TotalPopulation), 1), nsmall=1, big.mark=",")
  
  # display the 1st valuebox output, using reg1 ID from UI we render the ValueBox
  output$reg1 <- renderValueBox({
    
    # valueBox function is to display the contents in the valueBox like the value, text, icon & color
    valueBox(
      maxp$TotalPopulation, paste0("Region with Highest Population : ",maxp$region), icon = icon("fas fa-users"),
      color = "light-blue"
    )
  })
  
  # display the 1st graph output, using bargraph ID from UI we render the Plot
  output$bargraph <- renderPlot({
    
    # plot a bar graph using x & y axis variables and other information like labels and main title and bar border color
    barplot(dfr$TotalPopulation, names.arg=dfr$region, xlab="Region", ylab="TotalPopulation", col="lightblue",
            main="",border="black")  
  })
  
  #---------------------------------------------------------------------------------------------------
  
  # Get the year & region wise population
  dfyr <- popRegion %>% 
    group_by(year,region) %>% 
    dplyr::summarise(TotalPopulation = sum(population, na.rm = TRUE))
  
  # convert it to the data frame
  dfyr <- data.frame(dfyr)
  
  # remove NA values  
  dfyr <- subset(dfyr, !is.na(dfyr$region))
  
  # Get the row with the highest population
  maxyr <- dfyr[which.max(dfyr$TotalPopulation),]
  
  # Add a thousand format to TotalPopulation
  maxyr$TotalPopulation <- format(round(as.numeric(maxyr$TotalPopulation), 1), nsmall=1, big.mark=",")
  
  # display the 2nd valuebox output, using reg2 ID from UI we render the ValueBox
  output$reg2 <- renderValueBox({
    
    # valueBox function is to display the contents in the valueBox like the value, text, icon & color
    valueBox(
      maxyr$TotalPopulation, paste0("Year & Region with Highest Population : ", maxyr$year, " - ",maxyr$region), icon = icon("fas fa-users"),
      color = "green"
    )
  })
  
  # display the 2nd graph output, using linegraph ID from UI we render the Plot
  output$linegraph <- renderPlot({
    
    # plot a multi-line graph using x & y axis variables and fill(legend)
    ggplot(dfyr, aes(x = year, y = TotalPopulation, fill = region)) +
      # geom_line to tell the ggplot function that we want a line graph out of the data
      geom_line() +
      # geom_point to tell the ggplot function that we want a points on the line where we are passing the point size and point shape
      geom_point(size = 4, shape = 21)  # point shape is represented by a number
    
  })
  dfpop <- popRegion %>% 
    group_by(country) %>% 
    dplyr::summarise(TotalPopulation = sum(population, na.rm = TRUE))
  
  output$agetreegraph <- renderPlotly({
    
    # plot a multi-line graph using x & y axis variables and fill(legend)
    plot_ly(
      type="treemap",
      labels = dfpop$country,
      parents = "world",
      values = dfpop$TotalPopulation
    )
  })
  
  dftable <- select(popAge , "region", "category" , "population")
  output$mytable <- renderDataTable({
    datatable(dftable)
  })
  
}

# call the shinyApp function to run the App with ui & server components as parameters
shinyApp(ui, server)
