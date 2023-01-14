library(shiny)
library(ggplot2)
library(ggridges)
library(dbplyr)

datos <- readRDS("data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Explorador de los datos AhotsApp"),
    
    p("Una sencilla aplicación Shiny para el análisis exploratorio de datos. 
      Se muestra un máximo de 15 filas del conjunto de datos."),
    
    sidebarPanel(
      
      selectInput("variable" , label = "Variable", choices = colnames(datos[-c(1:2)]), selected = "FF_A"),
        
      checkboxGroupInput("group",label = "Grupos",choices = levels(datos$Clase), selected = c("Normal","Organicas")),
        
      radioButtons("sex",label = "Sexos",choices = list("Ambos" = "Ambos", "Hombres" = "Hombre" , "Mujeres" = "Mujer"), selected = c("Ambos"))
        
    ),
      
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Gráfico de densidad", 
                 br(),
                 plotOutput("density")),
        tabPanel("Gráfico de dispersión", 
                 br(),
                 plotOutput("dispersion")),
        tabPanel("Datos", verbatimTextOutput("snippet")),
        tabPanel("Resumen", verbatimTextOutput("summary")),
        
        )
      
      )
    
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataInput <- reactive({
    
    aux_datos <- data.frame()
    
    for ( i in input$group){
      
      if (input$sex != "Ambos"){
        aux_datos <-  rbind(aux_datos, subset(datos,Clase == i & Sexo == input$sex))
      } else {
        aux_datos <-  rbind(aux_datos, subset(datos,Clase == i))
      }
    }
    aux_datos
    
  })
  
  
  output$summary <- renderPrint({
    by(dataInput()[input$variable], dataInput()$Clase,summary)
  })

  output$snippet <- renderPrint({
    head(dataInput(), n=25)
  })
  
  tema <- theme(
    plot.title = element_text( size = (20), hjust = 0.5 ),
    axis.title = element_text( size = (15)),
    axis.text = element_text( size = (10)),
    )
  
  output$density <- renderPlot({
    
    ggplot(data = dataInput(), aes(x=unlist(dataInput()[input$variable]), y=Clase,fill=Clase )) + 
      geom_density_ridges(alpha = 0.75) + 
      xlab(input$variable) + 
      ggtitle(paste(input$variable , "por clase en" , input$sex)) +
      tema + 
      scale_x_continuous(n.breaks = 20)

  })
  
  output$dispersion <- renderPlot({
    
    ggplot(data = dataInput(), aes(x=unlist(dataInput()[input$variable]), y=Clase,fill=Clase )) + 
      geom_boxplot(alpha = 0.75) + 
      xlab(input$variable) + 
      ggtitle(paste(input$variable , "por clase en" , input$sex)) +
      tema + 
      scale_x_continuous(n.breaks = 20)
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
