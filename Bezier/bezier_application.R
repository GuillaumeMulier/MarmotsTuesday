#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(colourpicker)

# Variable globale qui donnera le chemin vers le dossier
Chemin <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/")

# Sourcer le script avec les fonctions
source(paste0(Chemin, "script_anim_bezier.R"))
source(paste0(Chemin, "bezier_application_helpers.R"))

ui <- fluidPage(

    # Titre de l'application
    titlePanel("Courbes de Bézier"),
    h3("Expérimentons avec une marmotte"),

    fluidRow(
      column(2, 
             sliderInput(inputId = "Degre",
                         label = "Degré de la courbe",
                         min = 1,
                         max = 5,
                         value = 2,
                         ticks = FALSE,
                         step = 1)),
      column(10,
             )
    ),
    
    fluidRow(
      tableOutput()
    ),
    
    fluidRow(
      column(12,
             plotOutput(outputId = "PlotBezier"))
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  Donnees <- reactive({
    
  })
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
