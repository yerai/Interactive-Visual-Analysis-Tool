# Load packages ----
library(shiny)

#
library(ggplot2)
#


# Load data ----
movies <- readRDS("data/movies.rds")

# Global variables ----
year_list <- sort(unique(movies$year))
genres_list = colnames(movies)[18:24]

# Define UI  ----
ui <- fluidPage(
  
  # Custom CSS ---
  tags$head(
    tags$style(
      HTML(
        "
        .multicol .shiny-options-group{
            -webkit-column-count: 3; /* Chrome, Safari, Opera */
            -moz-column-count: 3;    /* Firefox */
            column-count: 3;
            -moz-column-fill: balanced;
            -column-fill: balanced;
        }

        .checkbox{
            margin-top: 0px !important;
            -webkit-margin-after: 0px !important; 
        }

        .checkbox-inline, .radio-inline {
            position: relative;
            display: inline-block;
            padding-left: 20px;
            margin-bottom: 0;
            margin-left: 10px;
            font-weight: 400;
            vertical-align: middle;
            cursor: pointer;
        }
        "
      )
    ) 
  ),
  
  title ="Genre Explorer",
  plotOutput("graph"),
  hr(),
  fluidRow(
    column(
      width = 12, 
      h3("Movie genre explorer")
    ),
    column(
      width = 6,
      sliderInput(
        "year_range", 
        label = h4("Year range"), 
        min = year_list[1], 
        max = year_list[length(year_list)-1], 
        value = c(year_list[1], year_list[length(year_list)-1]),
        width = '100%')
    ),
    column(
      width = 4,
      tags$div(align = 'left',
               class = 'multicol',
               checkboxGroupInput(inputId  = 'genres_selected',
                                  label    = h4("Genres displayed"), 
                                  choices  = genres_list,
                                  selected = genres_list,
                                  width = '100%',
                                  inline   = TRUE)
               )
    ),
    column(
      width = 2,
      radioButtons("color_palette", 
                   label = h4("Color palette"), 
                   choices = c("Light", "Dark"), 
                   selected = "Light",
                   inline = FALSE, 
                   width = '100%')
      )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  
  output$graph <- renderPlot({
    
    
    
  })
  
  output$graph <- renderPlot({
    
    start_year <- input$year_range[1]
    end_year <- input$year_range[2]
    
    # Filter year_list ---
    filtered_year_list <- c()
    for(year in year_list){
      if(year >= start_year && year <= end_year){
        filtered_year_list <- c(filtered_year_list, year)
      }
    }
    
    # Filter genre_list ---
    filtered_genres_list <- input$genres_selected
    
    # Get color selected ---
    colors <- c()
    if (input$color_palette == "Light"){
      colors <-  c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628')
    }else{
      colors <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d')
    }
      
    #Get genres total list ---
    lista = matrix (list(), nrow = length(filtered_year_list), ncol = length(filtered_genres_list))
    year_count = 1;
    genre_count = 1;
    
    for(year in filtered_year_list){
      year_movies <- movies[movies[, "year"] == year,]
      for(genre in filtered_genres_list){
        lista[year_count,genre_count] <- length(which(year_movies[[genre]] == 1))
        genre_count = genre_count + 1
      }
      genre_count = 1
      year_count = year_count + 1
    }
    
    t_lista <- t(lista)
    
    # Plot graph ---
    xrange <- range(filtered_year_list)
    yrange <- c(0,max(as.numeric(unlist(t_lista))))

    plot(xrange,
         yrange,
         type="n",
         xlab="Años",
         ylab="Numero de pelis", 
         main="Titulo del grafico",
         sub="Subtitulo del grafico")
    
    genre_count = 1
    for (genre in filtered_genres_list){
      lines(filtered_year_list, t_lista[genre_count,], pch=19, col=colors[genre_count], type="b", lty=1)
    genre_count = genre_count + 1
    }
    legend("topleft", legend=filtered_genres_list, col=colors, lty=1, cex=0.8)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)