# Load packages ----
library(shiny)
library(plotly)

# Load data ----
movies <- readRDS("data/movies.rds")
decades_rating <- readRDS("data/decades_rating.Rda")
decades_votes <- readRDS("data/decades_votes.Rda")

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
        body{
          color: #2F2F2F;
          font-family: 'Raleway', sans-serif;
          
        }

        h4{
          font-family: 'Lato', sans-serif;
          font-weight: bold;
        }
  
        .navbar-default {
          background-color: #F4F4F4;
          border-color: #e7e7e700;
          border-radius: 10px;
          margin-top: 10px;
        }

        .navbar-default .navbar-brand {
          color: #2F2F2F;
          font-weight: 600;
        }

        #control_panel{
          background-color: #F4F4F4;
          padding:10px;
          margin-top: 20px;
          border-radius: 10px;
        }        
  
        

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
  
  navbarPage(
    title = "Movie Interactive Visual Analysis Tool",
    position = "static-top",
    collapsible = "TRUE",
    windowTitle = "Movie Interactive Visual Analysis Tool",
    tabPanel(
       title ="Genre Trends",
       plotlyOutput("graph"),
       fluidRow(
         id='control_panel',
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
             
    ),
    tabPanel(
      title ="Budget & Rating",
      plotlyOutput("graph2"),
      fluidRow(
        id='control_panel',
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
          width = 3,
          tags$div(align = 'left',
                   class = 'multicol',
                   checkboxGroupInput(inputId  = 'mpaa_selected',
                                      label    = h4("MPAA displayed"), 
                                      choices  = c("NC-17","PG","PG-13","R"),
                                      selected = c("NC-17","PG","PG-13","R"),
                                      width = '100%',
                                      inline   = TRUE)
          )
        ),
        column(
          width = 2,
          radioButtons("color_palette_scatter", 
                       label = h4("Color palette"), 
                       choices = c("Light", "Dark"), 
                       selected = "Light",
                       inline = FALSE, 
                       width = '100%')
        )
      )#FluidRow
    ),
    tabPanel(
      title ="Rating Trends",
      fluidRow(
        column(
          width = 8,
          plotlyOutput("graph3")
        ),
        column(
          width = 4,
          plotlyOutput("graph4")
        )
      ),
      fluidRow(
        id='control_panel',
        column(
          width = 6,
          tags$div(align = 'left',
                   class = 'multicol',
                   checkboxGroupInput(inputId  = 'decades_selected',
                                      label    = h4("Decades displayed"), 
                                      choices  = c("1890", "1900","1910","1920","1930","1940","1950","1960","1970","1980","1990","2000"),
                                      selected = c("1890", "1900","1910","1920","1930","1940","1950","1960","1970","1980","1990","2000"),
                                      width = '100%',
                                      inline   = TRUE)
          )
        ),
        column(
          width = 2,
          radioButtons("color_palette_violin", 
                       label = h4("Color palette"), 
                       choices = c("Light", "Dark"), 
                       selected = "Dark",
                       inline = FALSE, 
                       width = '100%')
        )
      )
    )
  )
)


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
    color_selected <- c()
    if (input$color_palette == "Light"){
      color_selected = c("rgb(228,26,28)","rgb(55,126,184)","rgb(77,175,74)","rgb(152,78,163)","rgb(255,127,0)","rgb(255,255,51)","rgb(166,86,40)")
    }else{
      color_selected = c("rgb(27,158,119)","rgb(217,95,2)","rgb(117,112,179)","rgb(231,41,138)","rgb(102,166,30)","rgb(230,171,2)","rgb(166,118,29)")
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
    
    #Build Data Frame ---
    t_lista <- t(lista)
    data <- data.frame(t_lista)
    
    #Display plot ---
    P <- plot_ly(data,
                 type = "scatter", 
                 mode ="lines"
                 )%>% 
      layout(title = "Number of movies released across the years",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Movies Released")) %>% 
      config(displayModeBar = F)
    
    #Add as many traces as genres selected ---
    genre_count = length(filtered_genres_list)
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[1,], name=filtered_genres_list[1], line = list(color = color_selected[1], width = 3), hoverinfo = "text", hovertext = paste('</br> Number of Movies: ', t_lista[1,], '</br> Year: ', filtered_year_list, '</br> Genre: ', filtered_genres_list[1]))
      genre_count = genre_count - 1
    }
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[2,], name=filtered_genres_list[2], line = list(color = color_selected[2], width = 3), hoverinfo = "text", hovertext = paste('</br> Number of Movies: ', t_lista[2,], '</br> Year: ', filtered_year_list, '</br> Genre: ', filtered_genres_list[2]))
      genre_count = genre_count - 1
    }
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[3,], name=filtered_genres_list[3], line = list(color = color_selected[3], width = 3), hoverinfo = "text", hovertext = paste('</br> Number of Movies: ', t_lista[3,], '</br> Year: ', filtered_year_list, '</br> Genre: ', filtered_genres_list[3]))
      genre_count = genre_count - 1
    }
    
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[4,], name=filtered_genres_list[4], line = list(color = color_selected[4], width = 3), hoverinfo = "text", hovertext = paste('</br> Number of Movies: ', t_lista[4,], '</br> Year: ', filtered_year_list, '</br> Genre: ', filtered_genres_list[4]))
      genre_count = genre_count - 1
    }
    
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[5,], name=filtered_genres_list[5], line = list(color = color_selected[5], width = 3), hoverinfo = "text", hovertext = paste('</br> Number of Movies: ', t_lista[5,], '</br> Year: ', filtered_year_list, '</br> Genre: ', filtered_genres_list[5]))
      genre_count = genre_count - 1
    }
    
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[6,], name=filtered_genres_list[6], line = list(color = color_selected[6], width = 3), hoverinfo = "text", hovertext = paste('</br> Number of Movies: ', t_lista[6,], '</br> Year: ', filtered_year_list, '</br> Genre: ', filtered_genres_list[6]))
      genre_count = genre_count - 1
    }
    
    if(genre_count > 0){
      P <- add_trace(P,x=~filtered_year_list, y = ~t_lista[7,], name=filtered_genres_list[7], line = list(color = color_selected[7], width = 3), hoverinfo = "text", hovertext = paste('</br> Number of Movies: ', t_lista[7,], '</br> Year: ', filtered_year_list, '</br> Genre: ', filtered_genres_list[7]))
      genre_count = genre_count - 1
    }

    P

  })
  
  output$graph2 <- renderPlotly({
    budget_list <- c()
    title_list <- c()
    rating_list <- c()
    mpaa_list <- c()
    
    counter = 1;
    for(budget in movies$budget){
      if(!(budget == 0) && !is.na(budget)){
        if(budget >= input$budget_range[1] && budget <= input$budget_range[2]
            && movies$rating[counter] >= input$rating_range[1] && movies$rating[counter] <= input$rating_range[2]
            && !(movies$mpaa[counter] == "") && !is.na(movies$mpaa[counter])
            && movies$mpaa[counter] %in% input$mpaa_selected){
          
            budget_list <- c(budget_list, budget)
            title_list <- c(title_list, movies$title[counter])
            rating_list <- c(rating_list, movies$rating[counter])
            mpaa_list <- c(mpaa_list, movies$mpaa[counter])
        }
      }
      counter = counter + 1
    }
    
    # Get color selected ---
    color_selected_scatter <- c()
    if (input$color_palette_scatter == "Light"){
      color_selected_scatter = c("#e41a1c","#377eb8","#4daf4a","#984ea3")
    }else{
      color_selected_scatter = c("#1b9e77","#d95f02","#7570b3","#e7298a")
    }
    
    p <- plot_ly(x = budget_list, 
                 y = rating_list,
                 color = mpaa_list,
                 colors = color_selected_scatter,
                 type = "scatter",
                 mode = "markers",
                 hoverinfo = "text", 
                 hovertext = paste('</br> Title: ', title_list, 
                                   '</br> Budget: ', budget_list, 
                                   '</br> Rating: ', rating_list)
                 )%>% 
          layout(title = "Plot Title",
                 xaxis = list(title = "Budget"),
                 yaxis = list(title = "Rating")) %>% 
          config(displayModeBar = F)
    
  })
  
  output$graph3 <- renderPlotly({
    
    rows_to_delete <- c()
    counter = 1
    for(decade in decades_rating$x){
      if(!(is.element(decade, input$decades_selected))){
        rows_to_delete <- c(rows_to_delete,counter)
      }
      counter = counter + 1
    }
    
    if(!is.null(rows_to_delete)){
      movies_rating <- decades_rating[-rows_to_delete, ] 
    }else{
      movies_rating <- decades_rating
    }
    
    # Get color selected ---
    color_selected_violin <- c()
    if (input$color_palette_violin == "Light"){
      color_selected_violin = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f')
    }else{
      color_selected_violin = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
    }
    
    p <- movies_rating %>%
      plot_ly(
        x = ~x,
        y = ~y,
        split = ~x,
        color = ~x,
        colors = color_selected_violin,
        type = 'violin',
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        )
      ) %>% 
      layout(title = "Average Movie Rating per Decade",
             xaxis = list(title = "Decade"),
             yaxis = list(title = "Average Rating")) %>% 
      config(displayModeBar = F)
    
  })
  
  output$graph4 <- renderPlotly({
    
    counter = 1
    rows_to_delete_votes <- c()
   
    for(decade in decades_votes$x){
      if (!(is.element(decade, input$decades_selected))){
        print(!(is.element(decade, input$decades_selected)))
        print(decade)
        print(input$decades_selected)
        rows_to_delete_votes <- c(rows_to_delete_votes, counter)
      }
      counter = counter + 1
    }
    
   
    
  
    
    if(!(is.null(rows_to_delete_votes))){
      decades_voting <- decades_votes[-rows_to_delete_votes, ] 
    }else{
      decades_voting <- decades_votes
    }
    
   
    

    # Get color selected ---
    color_selected_violin <- c()
    if (input$color_palette_violin == "Light"){
      color_selected_violin = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f')
    }else{
      color_selected_violin = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
    }
    
    p <- plot_ly( 
      x = ~decades_voting[,1],
      y = ~decades_voting[,2],
      type = "bar"
      
    )%>% 
      layout(title = "Vote Count per Decade",
             xaxis = list(title = "Decade"),
             yaxis = list(title = "Number of Votes")) %>% 
      config(displayModeBar = F)
    
    
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)