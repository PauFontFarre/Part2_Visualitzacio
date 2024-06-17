# Vector de títols personalitzats per a les variables
axis_titles <- c(
  "consumo.dia" = "Consum mig diari (g)",
  "consumo.tot" = "Consum Total (kg)",
  "peso.ini" = "Pes Inicial (kg)",
  "peso.fin" = "Pes Final (kg)",
  "GMD" = "Guany Mitjà Diari (g)",
  "TV" = "Nombre mig de visites diaries",
  "TM" = "Nombre mig de menjades diaries",
  "TD" = "Duració total de les menjades diaries (min)",
  "VS" = "Tamany de les ingestes per visita (g)",
  "MS" = "Tamany de les ingestes per menjada (g)",
  "FR" = "Velocitat d'ingesta (g/min)",
  "ganancia" = "Guany Total en l'engreix (kg)",
  "IC" = "Índex de Conversió"
)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      tags$h2("Bienvenido a la Herramienta de Clustering k-means"),
      tags$p("Esta herramienta permite visualizar el resultado del algoritmo k-means sobre un conjunto de datos seleccionados."),
      tags$p("Seleccione dos variables para representar en el gráfico y el número de clústers."),
      selectInput("var1", "Variable 1:", choices = names(fin)[4:16], selected = names(fin)[4]),
      selectInput("var2", "Variable 2:", choices = setdiff(names(fin)[4:16], names(fin)[4]), selected = names(fin)[5]),
      numericInput("clusters", "Nombre de Clústers:", 3, min = 1)
    ),
    mainPanel(
      style = "margin-top: 50px;",
      plotlyOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Actualitzar opcions de var2 quan var1 canvia
  observeEvent(input$var1, {
    current_var2 <- isolate(input$var2)
    available_vars <- setdiff(names(fin)[4:16], input$var1)
    if (current_var2 %in% available_vars) {
      updateSelectInput(session, "var2",
                        choices = available_vars,
                        selected = current_var2)
    } else {
      updateSelectInput(session, "var2",
                        choices = available_vars)
    }
  })
  
  # Actualitzar opcions de var1 quan var2 canvia
  observeEvent(input$var2, {
    current_var1 <- isolate(input$var1)
    available_vars <- setdiff(names(fin)[4:16], input$var2)
    if (current_var1 %in% available_vars) {
      updateSelectInput(session, "var1",
                        choices = available_vars,
                        selected = current_var1)
    } else {
      updateSelectInput(session, "var1",
                        choices = available_vars)
    }
  })
  
  output$plot <- renderPlotly({
    req(input$var1, input$var2)  # Assegurar-se que ambdues variables estan seleccionades
    
    var1 <- input$var1
    var2 <- input$var2
    k <- input$clusters
    
    selected_data <- fin[, c(var1, var2)]
    
    # Estandaritzar les variables seleccionades
    standardized_data <- scale(selected_data)
    
    kmeans_result <- kmeans(standardized_data, centers = k)
    
    plot_ly(
      data = selected_data,
      x = ~get(var1),
      y = ~get(var2),
      type = 'scatter',
      mode = 'markers',
      color = as.factor(kmeans_result$cluster),
      marker = list(size = 10)
    ) %>%
      layout(
        title = paste("K-means Clustering amb", k, "Clústers"),
        xaxis = list(title = axis_titles[[var1]]),
        yaxis = list(title = axis_titles[[var2]]),
        legend = list(title = list(text = 'Cluster'))
      )
  })
}

shinyApp(ui = ui, server = server)