if (!require(shiny)) install.packages('shiny')
library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Pintando distribuciones"),
  selectInput('dist', "Distribución:", 
              choices=c(Normal   = 'dnorm',    #Una lista nombrada... 
                        Gamma    = 'dgamma',   # ... muestra los nombres...
                        Cauchy   = 'dcauchy',  # ... por pantalla.
                        TStudent = 'dt')
              ),
  numericInput('mu',        #nombre del objeto interativo   
               'par1', #Texto que aparece por pantalla
               10),         #Valor inicial
  numericInput('sigma',     
               'par2', 
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
    plot(x,                         #vector de valores x
         y =                        #vector de valores y
             do.call(input$dist,    #do.call invoca como función al primer arg.    
                     list (x,       #los params de la función van en una lista
                           input$mu, input$sigma)),
         xlab="valor de x",
         ylab="densidad",
         main = paste("densidad de la función", input$dist, names(input$dist)),
         col = "red",
         type="l",   # l para líneas, p para puntos, etc.
         lwd= 2,     # grosor de la línea. Por defecto es 1.
         bty="n")    # n para que no haya caja alrededor de la gráfica
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)