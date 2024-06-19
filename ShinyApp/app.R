library(readxl)
library(plotly)
library(stringr)
library(dplyr)
library(data.table)
library(lubridate)
library(qwraps2)
library(tidyr)
library(shinythemes)
library(here)
library(shinyjs)

fin <- read.csv2("Data/clean_data.csv")  
#fin <- read.csv2(paste0(here(),"/ShinyApp/Data/clean_data.csv")) 
# Vector de títols personalitzats per a les variables
axis_titles <- c(
  "Consumo_diario" = "Consum mig diari (g)",
  "Consumo_total" = "Consum Total (kg)",
  "Peso_inicial" = "Pes Inicial (kg)",
  "Peso_final" = "Pes Final (kg)",
  "Ganancia_media_diaria" = "Guany Mitjà Diari (g)",
  "Número_medio_de_visitas diarias" = "Nombre mig de visites diaries",
  "Número_medio_de_comidas diarias" = "Nombre mig de menjades diaries",
  "Duración_total_de_comidas_diarias" = "Duració total de les menjades diaries (min)",
  "Tamaño_de_ingestas_por_visita" = "Tamany de les ingestes per visita (g)",
  "Tamaño_de_ingestas_por_comida" = "Tamany de les ingestes per menjada (g)",
  "Velocidad_de_ingestión" = "Velocitat d'ingesta (g/min)",
  "Ganancia_total_en_engorde" = "Guany Total en l'engreix (kg)",
  "Índice_de_conversión" = "Índex de Conversió"
)

input_labels <- c(
  "consumo.dia" = "Consumo_diario",
  "consumo.tot" = "Consumo_total",
  "peso.ini" = "Peso_inicial",
  "peso.fin" = "Peso_final",
  "GMD" = "Ganancia_media_diaria",
  "TV" = "Número_medio_de_visitas diarias",
  "TM" = "Número_medio_de_comidas diarias",
  "TD" = "Duración_total_de_comidas_diarias",
  "VS" = "Tamaño_de_ingestas_por_visita",
  "MS" = "Tamaño_de_ingestas_por_comida",
  "FR" = "Velocidad_de_ingestión",
  "ganancia" = "Ganancia_total_en_engorde",
  "IC" = "Índice_de_conversión"
)
colnames(fin) <- sapply(colnames(fin), function(x) {
  if (x %in% names(input_labels)) {
    return(input_labels[x])
  } else {
    return(x)  # Mantener el nombre original si no está en input_labels
  }
})

ui <- fluidPage(
  useShinyjs(), 
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .info-text {
        font-size: 17px;
        text-align: justify;
        color: #333;
        padding: 10px;
        ackground-color: #ffffff;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-top: 20px;
      }
      .machines-img {
        display: block;
        margin: 10px auto;
        width: 80%;
        max-width: 430px;
        border: 2px solid #ccc;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
    "))
  ),
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      tags$h2("Clustering k-means en variables de comportament alimentari porcí"),
      tags$p("Aquesta eina permet visualitzar la clusterització del algoritme ",
             tags$a(href = "https://ca.wikipedia.org/wiki/K-means", "k-means"),
             "segons el parell de variables seleccionades i el nombre de clústers seleccionats. Per tant, permet l'identificació de patrons alimentaris porcins"),
      tags$p("Seleccioneu dues variables per representar en el gràfic i el nombre de clústers."),
      selectInput("var1", "Variable 1:", choices = names(fin)[4:16], selected = names(fin)[4]),
      selectInput("var2", "Variable 2:", choices = setdiff(names(fin)[4:16], names(fin)[4]), selected = names(fin)[5]),
      numericInput("clusters", "Nombre de Clústers:", 3, min = 1)
    ),
    mainPanel(
      style = "margin-top: 20px;",
      plotlyOutput("plot", width = "100%", height = "100%"),
      div(
        class = "info-text", id = "info_text",
        tags$img(src = "icono_interrogacion.png", width = 100, height = 80, style = "float: left; margin-right: 10px;"),
        div(
          style = "text-align: justify;",
          "Les màquines d'alimentació de precisió permeten dosificar de manera individualitzada l'alimentació a cada animal, 
    segons la seva corba de creixement. La màquina recull dades i mètriques mentre l'animal està menjant, com la quantitat 
    de menjar ingerida, el pes, la duració de la menjada i altres paràmetres que poden ser calculats. Això permet conèixer i estudiar 
    els hàbits i comportaments d'aquests animals."
        )
      ),
      img(class = "machines-img", src = "maquinas_alimentacion.png", alt = "Màquines d'alimentació de precisió")
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
    req(input$var1, input$var2)  
    
    var1 <- input$var1
    var2 <- input$var2
    print(paste("Var1:", var1))
    print(paste("Var2:", var2))
    k <- input$clusters

    selected_data <- fin[, c(var1, var2)]

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
        title = paste("Clustering k-means amb", k, "Clústers"),
        xaxis = list(title = axis_titles[[var1]]),
        yaxis = list(title = axis_titles[[var2]]),
        legend = list(title = list(text = 'Clúster'))
      )
  })
}

shinyApp(ui = ui, server = server)
