# Load packages ----
library(shiny)

# Load data ----
movies <- readRDS("data/movies.rds")

# Global variables ----
year_list <- sort(unique(movies$year))

# Define UI  ----
ui <- fluidPage(
  titlePanel("Movie Genres"),
  
  sidebarLayout(
    sidebarPanel(
      
      
      
      
      selectInput("start_year",
                  label="Start year",
                  choices = year_list, 
                  selected = year_list[1]),
      
      selectInput("end_year",
                  label ="End year",
                  choices = year_list, 
                  selected = year_list[length(year_list)])
      
    
    ),
    mainPanel(
      plotOutput("graph")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$graph <- renderPlot({
    
    # Filter year_list ----
    filtered_year_list <- c()
    for(year in year_list){
      if(year >= input$start_year && year <= input$end_year){
        filtered_year_list <- c(filtered_year_list, year)
      }
    }
    
    # Get genres List ----
    genres_list = colnames(movies)[18:24]
    
    #Get genres total list ---
    lista = matrix (list(), nrow = length(filtered_year_list), ncol = length(genres_list))
    year_count = 1;
    genre_count = 1;
    
    for(year in filtered_year_list){
      year_movies <- movies[movies[, "year"] == year,]
      for(genre in genres_list){
        lista[year_count,genre_count] <- length(which(year_movies[[genre]] == 1))
        genre_count = genre_count + 1
      }
      genre_count = 1
      year_count = year_count + 1
    }
    
    t_lista <- t(lista)
    
    # Color list ---
    colors <- c("#267278", "#65338D", "#4770B3", "#D21F75", "#3B36B9", "#50AED3", "#48B24F", "#E57438")

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
    for (genre in genres_list){
      lines(filtered_year_list, t_lista[genre_count,], pch=19, col=colors[genre_count-1], type="b", lty=1)
      legend("topleft", legend=c(genre), col=c(colors[genre_count-1]), lty=1, cex=0.8)
      genre_count = genre_count + 1
    }
   
      
  })


  
  
  
  
 
}

# Run the app ----
shinyApp(ui = ui, server = server)