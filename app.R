#devtools::install_github("bwlewis/rthreejs")
library(threejs)
library(shiny)
library(dplyr)
library(shinythemes)
library(shinydashboard)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  
  
  

      # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        tags$div(class = "header", checked = NA,
             tags$b("Select maximum three airlines using their IATA codes"),
             tags$br(),
             tags$em("i.e. Lufthansa: LH, Turkish: TK")
        ),
        
        uiOutput("airSelector"),
        sliderInput(inputId="height",label="Height:", min = 1, max = 5, value=1),
         sliderInput(inputId="width",label="Width:", min = 1, max = 5, value=1),
         sliderInput(inputId="opac",label="Opacity:", min = 0, max = 1, value=0.3),
         checkboxInput(inputId="atmos", label="Atmosphere",value = TRUE, width = "800px"),
         
         tags$div(class = "header", checked = NA,
                  tags$a(href = "http://www.sterlingtravel.co.uk/airline_list.html", "Airline names and corresponding IATA codes"))
         
         

      ),
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
         globeOutput("flightPlot", width = "100%", height = "1000px")
      )
   )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

  airports <- read.csv("./data/airports.dat", header=FALSE, stringsAsFactors=FALSE)
  colnames(airports) <- c("airport_id", "name", "city", "country", "IATA_FAA", "ICAO", "latitude", "longitude", "altitude", "timezone", "DST")
  airports <- subset(airports, IATA_FAA != '')
  flights <- read.csv("./data/routes.dat", header=FALSE, stringsAsFactors=FALSE)
  colnames(flights) <- c("airline", "airline_id", "source_air", "source_air_id", "dest_air", "dest_air_id","code_share", "stops", "equipment")
  
  ### for select options
  output$airSelector <- renderUI({
    selectizeInput(inputId = "selectAirline",
                   label = " ", 
                   multiple = T,
                   options = list(maxItems = 3, placeholder = 'Ex: LH SK TK'),
                   choices=rev(unique(flights$airline))) 
  })
  ###
  
  
   output$flightPlot <- renderGlobe({

     
     flights2 <- flights %>% 
       filter(airline %in% input$selectAirline)

     f1 <- flights2 %>% select(airline, source_air, dest_air)
     a1 <- airports %>% select(IATA_FAA, latitude, longitude )
     
     m <- merge(x = f1, y=a1, by.x = "source_air", by.y = "IATA_FAA")
     m <- merge(x = m,  y=a1, by.x = "dest_air", by.y="IATA_FAA")
     
     m <- m %>% select(airline, latitude.x, longitude.x, latitude.y, longitude.y, airline)
     colnames(m) <- c("airline","origin_lat","origin_long","dest_lat","dest_long")
     
     m <- m %>% 
       arrange(airline) %>% 
       mutate(color=ifelse(airline==input$selectAirline[1],"#02C5FE",
                           ifelse(airline==input$selectAirline[2],"#DE88F78",
                                  ifelse(airline==input$selectAirline[3],"#88F8AB",NA))))
     globejs(arcs=m[,2:5],
             long=airports[,8],
             lat=airports[,7],
             value=0.1,
             arcsHeight=input$height/10,
             arcsLwd=input$width/2,
             arcsColor=m$color,
             color="white",
             arcsOpacity=input$opac,
             emissive="black",
             atmosphere=input$atmos,
             bodycolor='white',
             fov=35, lightcolor="grey",
             rotationlong=42, rotationlat=57,
             bg = "#060606")

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

