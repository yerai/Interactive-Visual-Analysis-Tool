# Load packages ----
library(shiny)
library(ggplot2)
library(plotly)

# Load data ----
movies <- readRDS("data/movies.rds")

# Global variables ----
year_list <- sort(unique(movies$year))
genres_list = colnames(movies)[18:24]
budget_list <- sort(unique(movies$budget))
budget_list <- budget_list[2:length(budget_list)]
rating_list <- sort(unique(movies$rating))

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
  
  navbarPage("Movie Interactive Visual Analysis Tool",
    tabPanel(
       title ="Genre Trends",
       plotlyOutput("graph"),
       hr(),
       fluidRow(
         column(
           width = 12, 
           h3("Controls")
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
        )#FluidRow
             
    ),#TabPanel 1
    tabPanel(
      title ="Movies",
      plotlyOutput("graph2"),
      hr(),
      fluidRow(
        column(
          width = 12, 
          h3("Controls")
        ),
        column(
          width = 6,
          sliderInput(
            "budget_range", 
            label = h4("Budget range"), 
            min = budget_list[1], 
            max = budget_list[length(budget_list)], 
            value = c(budget_list[1], budget_list[length(budget_list)]),
            width = '100%')
        ),
        column(
          width = 6,
          sliderInput(
            "rating_range", 
            label = h4("Rating range"), 
            min = rating_list[1], 
            max = rating_list[length(rating_list)], 
            value = c(rating_list[1], rating_list[length(rating_list)]),
            width = '100%')
        ),
        column(
          width = 4,
          tags$div(align = 'left',
                   class = 'multicol',
                   checkboxGroupInput(inputId  = 'decades_selected',
                                      label    = h4("Decades displayed"), 
                                      choices  = c("1910","1920","1930","1940","1950","1960","1970","1980","1990","2000"),
                                      selected = c("1910","1920","1930","1940","1950","1960","1970","1980","1990","2000"),
                                      width = '100%',
                                      inline   = TRUE)
          )
        ),
        column(
          width = 2,
          radioButtons("color_palette_scatter", 
                       label = h4("Color palette"), 
                       choices = c("Light", "Dark"), 
                       selected = "Dark",
                       inline = FALSE, 
                       width = '100%')
        )
      )#FluidRow
    )#TabPanel 2
  )#Navbar
)#UI

                 
     


# Define server logic ----
server <- function(input, output) {
  
  output$graph <- renderPlotly({
    
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
  #  xrange <- range(filtered_year_list)
  #  yrange <- c(0,max(as.numeric(unlist(t_lista))))

  #  p <-plot(xrange,
  #       yrange,
  #       type="n",
  #       xlab="Años",
  #       ylab="Numero de pelis", 
  #       main="Titulo del grafico",
  #       sub="Subtitulo del grafico")
    
  #  genre_count = 1
  #  for (genre in filtered_genres_list){
  #    lines(filtered_year_list, t_lista[genre_count,], pch=19, col=colors[genre_count], type="l", lty=1, lwd=4)
  #    genre_count = genre_count + 1
  #  }
  #  legend("topleft", legend=filtered_genres_list, col=colors, lty=1, cex=0.8, lwd=6)
    
    
    data <- data.frame(t_lista)
    P <- plot_ly(data, type = "scatter", mode="markers")%>% 
      layout(title = "Plot Title",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Movies")) %>% 
      config(displayModeBar = F)
    
    genre_count = length(filtered_genres_list)
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[1,], type="scatter", mode="lines", name=filtered_genres_list[1])
      genre_count = genre_count - 1
    }
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[2,], type="scatter", mode="lines", name=filtered_genres_list[2])
      genre_count = genre_count - 1
    }
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[3,], type="scatter", mode="lines", name=filtered_genres_list[3])
      genre_count = genre_count - 1
    }
    
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[4,], type="scatter", mode="lines", name=filtered_genres_list[4])
      genre_count = genre_count - 1
    }
    
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[5,], type="scatter", mode="lines", name=filtered_genres_list[5])
      genre_count = genre_count - 1
    }
    
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[6,], type="scatter", mode="lines", name=filtered_genres_list[6])
      genre_count = genre_count - 1
    }
    
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[7,], type="scatter", mode="lines", name=filtered_genres_list[7])
      genre_count = genre_count - 1
    }

    P

  })
  
  output$graph2 <- renderPlotly({
    budget_list <- c()
    title_list <- c()
    rating_list <- c()
    decade_list <- c()
    
    counter = 1;
    for(budget in movies$budget){
      if(!(budget == 0) && !is.na(budget)){
        if(budget >= input$budget_range[1] && budget <= input$budget_range[2]
            && movies$rating[counter] >= input$rating_range[1] && movies$rating[counter] <= input$rating_range[2]){
          decade <- paste(substr(as.character(movies$year[counter]),1,3), sep="", 0)
          if(decade %in% input$decades_selected){
            budget_list <- c(budget_list, budget)
            title_list <- c(title_list, movies$title[counter])
            rating_list <- c(rating_list, movies$rating[counter])
            decade_list <- c(decade_list, paste(substr(as.character(movies$year[counter]),1,3), sep="", 0))
          }
        }
      }
      counter = counter + 1
    }
    
    # Get color selected ---
    colors <- c()
    if (input$color_palette_scatter == "Light"){
      colors <-  c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd')
    }else{
      colors <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')
    }
    
    p <- plot_ly(x = budget_list, 
                 y = rating_list,
                 color = decade_list,
                 colors = colors,
                 type = "scatter",
                 mode = "markers",
                 hoverinfo = "text",
                 hovertext = paste(title_list)
                 
                 #marker = list(size = 10,
                               #color = 'rgba(255, 182, 193, .9)',
                               #line = list(color = 'rgba(152, 0, 0, .8)',
                                #           width = 2))
                 )%>% 
          layout(title = "Plot Title",
                 xaxis = list(title = "Budget"),
                 yaxis = list(title = "Rating")) %>% 
          config(displayModeBar = F)
    
   
    
    #plot(budget_list, rating_list, main="Scatterplot Example", 
    #     xlab="budget", ylab="rating", pch=19)
    
    #text(budget_list, rating_list, labels=title_list, cex= 0.7, pos=3)
    
    
  })
  
  output$graph3 <- renderPlotly({
    
    
    movie_decades <- c()
    for(year in movies$year){
      movie_decades <- c(movie_decades,paste(substr(as.character(year),1,3), sep="", 0))
    }
    
    movies_rating <- data.frame(x = movie_decades, y = movies$rating)
    
    
    p <- movies_rating %>%
      plot_ly(
        x = ~x,
        y = ~y,
        split = ~x,
        type = 'violin',
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        )
      ) %>% 
      layout(
        xaxis = list(
          title = "Decade"
        ),
        yaxis = list(
          title = "Rating",
          zeroline = F
        )
      )
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)