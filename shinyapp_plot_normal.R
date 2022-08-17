if (!require(shiny)) install.packages('shiny')
library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Pintando la distribución normal"),
  numericInput('mu',        #nombre del objeto interativo   
               'Media(μ)',  #Texto que aparece por pantalla
               10),         #Valor inicial
  numericInput('sigma',     
               'Desviación estándar (σ)', 
               1),
  numericInput('xmin',     
               'x min.', 
               7),       
  numericInput('xmax',     
               'x max.', 
               13),
  numericInput('puntos',     
               'Puntos de la curva', 
               500),
  plotOutput('plot')
)


# Define the server code
server <- function(input, output) {
  output$plot <- renderPlot({
    x <- seq(input$xmin,                             #min 
             input$xmax,                             #max
             (input$xmax - input$xmin)/input$puntos) #step
    plot(x,                                     #vector de valores x
         y = dnorm(x, input$mu, input$sigma),   #vector de valores y
         xlab="valor de x",
         ylab="densidad",
         main = "densidad de la función normal",
         col = "red",
         type="l",   # l para líneas, p para puntos, etc.
         lwd= 2,     # grosor de la línea. Por defecto es 1.
         bty="n")    # n para que no haya caja alrededor de la gráfica
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)