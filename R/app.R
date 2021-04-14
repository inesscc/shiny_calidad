library(calidad)
library(shiny)
library(haven)
library(labelled)
library(dplyr)
library(openxlsx)
library(sjmisc)
library(readxl)
library(XLConnect)
library(survey)
library(shinyWidgets)
library(rlang)
library(kableExtra)
library(shinycssloaders)
library(readr)
library(shinybusy)
library(shinyalert)
library(writexl)
library(shinyjs)
library(tibble)
library(plotly)

# Cargar funciones ####
source("download_data.R")
source("create_tabulado.R")
source("create_plot.R")
#source("utils.R")

# posibilidad nombres de variables DC"
fact_exp = c("Fact_pers","Fact_Pers","fe","fact_cal", "fact_pers","fact_pers","fe","fact_cal", "fact_cal_esi","FACT_PERS","FACT_PERS","FE","FACT_CAL","FACT_CAL_ESI")
conglomerados = c("Conglomerado", "id_directorio","varunit", "conglomerado","id_directorio","varunit","CONGLOMERADO","ID_DIRECTORIO","VARUNIT")
estratos = c("VarStrat", "estrato", "varstrat","varstrat", "estrato",  "varstrat","VARSTRAT", "ESTRATO",  "VARSTRAT")

testeo = F

# UI ----
ui <- fluidPage(
  useShinyjs(),
  useShinyalert(),
  includeCSS("styles.css"),
  
  tags$style("
              body {
    -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
    zoom: 0.8; /* Other non-webkit browsers */
    zoom: 80%; /* Webkit browsers */
}
              "),
  
  tags$head(
    tags$style(HTML("
     .my_table .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th {
       padding: 10px;
       line-height: 1.42857143;
       vertical-align: top;
       font-weight: 700;
     }
   "))),
  # Agregar el logo del INE    
  tags$img(src="logo_ine.png", width = 150, align="right"),
  
  # Título de la aplicación
  titlePanel(title = "Evaluación de calidad de estimaciones en encuestas de hogares"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
                 ## input de archivo local -----
                 fileInput(inputId = "file", label = h4("Carga una base de datos desde tu computador")),
                 
                 ## input archivo página del INE
                 selectInput("base_web_ine", label = h4("Descarga una base de datos desde la web del INE"),
                             choices = c("epf personas" = "epf", 
                                         "ene (última trimestre)" = "ene",
                                         "enusc", 
                                         "esi",
                                         "enut"),  
                             multiple = F),
                 
                 actionButton("base_ine", label = "Descargar base"),
                 
                 ## render selección de variables de interes, y de cruce
                 uiOutput("seleccion1"),
                 
                 ## render selección variables DC
                 uiOutput("seleccion2"),
                 
                 ## botón generación tabulado 
                 uiOutput("botonTAB")
                 
                 
    ),
    
    ## Main PANEL ----
    mainPanel(width = 9,
              
              ### render titulo tabulado
              uiOutput("tituloTAB"),
              verbatimTextOutput("PRUEBAS2"),
              ### render tabulado
              tags$div(
                class="my_table", # set to custom class
                htmlOutput("tabulado") %>% withSpinner(color="#0dc5c1")),
              uiOutput("PRUEBAS"),
              div(plotOutput('grafico') %>% withSpinner(color="#0dc5c1"), align = "center")
              
              
    )
  )
)

# SERVER ----
server <- function(input, output) {
  ## parametro para subir archivos pesados  
  options(shiny.maxRequestSize=1000*1024^2, scipen=999)
  
  
  # Logo inicial
  observe( {
    #Muestra el cartel cuando inicia la APP
    shinyalert(imageUrl = "logo_ine.png",imageWidth = "200", imageHeight = "200",
               closeOnClickOutside = T, showConfirmButton = F)
  })
  
  ### + I N P U T S + ####
  
  ### CARGA: base local ####
  data_input <- reactive({
    
    
    if(grepl(".sav", input$file$datapath)){
      
      haven::read_sav(input$file$datapath)
      
    } else if(grepl(".rds", tolower(input$file$datapath))){
      
      readRDS(input$file$datapath)
    } else if(grepl(".dta", tolower(input$file$datapath))){
      
      haven::read_dta(input$file$datapath)
    } else if(grepl(".sas", tolower(input$file$datapath))){
      
      haven::read_sas(input$file$datapath)
    }  else if(grepl(".xlsx", tolower(input$file$datapath))){
      
      readxl::read_excel(input$file$datapath)
    }#Pendiente csv, feather
    
  })
  
  
  # DESCARGA: DE DATOS PÁGINA INE ----
  descarga =  eventReactive(input$base_ine, {
    
    # Modal para descarga
    show_modal_spinner() # show the modal window
    
    #Descargar la base de datos en archivo temporal
    temp <- tempfile()
    datos <- download_data(input$base_web_ine)
    
    # Modal para descarga
    remove_modal_spinner() # remove it when done
    
    datos
  }) 
  
  # SWITCH: DESCARGA DATOS WEB INE | COMPUTADOR LOCAL ----
  
  datos <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    new <- data_input() 
    datos(new)
  })
  
  
  observeEvent(input$base_ine, {
    datos(descarga())
  })
  
  ### EXTRACT: names variables input datos ####
  variables_int <- reactive({
    if (!is.null(input$file)) {
      names(data_input())    
    } else if (!is.null(descarga())) {
      names(descarga())      
    }
  })
  
  variable_selected <- reactive({
    if (!is.null(input$file)) {
      names(data_input())    
    } else if (!is.null(descarga())) {
      names(descarga())      
    }
  })
  
  
  
  
  ### + R E N D E R - U I + ####
  
  # ### RENDER: IN SIDE BAR  ####
  ### RENDER: selección variables -----
  output$seleccion1 <- renderUI({
    tagList(## render selección variables DC
      #   varSelectInput("varINTERES", label = h3("Seleccione las variables de interés"),variables_int() , selected = 1, multiple = T),
      selectInput("varINTERES", label = h4("Variable de interés"),choices = variables_int(),  multiple = F),
      uiOutput("denominador"),
      radioButtons("tipoCALCULO", "¿Qué tipo de cálculo deseas realizar?",choices = list("Media","Proporción","Suma variable Continua","Conteo casos"), inline = F ),
      selectInput("varCRUCE", label = h4("Desagregación"), choices = variables_int(), selected = NULL, multiple = T),
      checkboxInput("IC", "¿Deseas agregar intervalos de confianza?",value = F),
      checkboxInput("ajuste_ene", "¿Deseas agregar los ajuste del MM ENE?",value = F),
      uiOutput("etiqueta"),
      selectInput("varSUBPOB", label = h4("Sub Población"), choices = variables_int(), selected = NULL, multiple = T),
      selectInput("varFACT1", label = h4("Variable para factor expansión"), choices = variables_int(), selected =  variables_int()[grep(paste0("^",fact_exp,"$",collapse = "|"), variables_int())], multiple = F),
      selectInput("varCONGLOM", label = h4("Variable para conglomerados"), choices = variables_int(), selected = variables_int()[grep(paste0("^",conglomerados,"$",collapse = "|"), variables_int())], multiple = F),
      selectInput("varESTRATOS",label = h4("Variable para estratos"), choices = variables_int(), selected = variables_int()[grep(paste0("^",estratos,"$",collapse = "|"), variables_int())], multiple = F), 
      disabled(downloadButton("tabla", label = "Descargar")),
      actionButton("actionTAB", label = "Generar tabulado")
    )})

  
output$etiqueta <- renderUI({
  req(input$varCRUCE >= 1)
  req(labelled::is.labelled(datos()[[input$varCRUCE[1]]]))
  checkboxInput("ETIQUETAS", "Sus datos poseen etiquetas, ¿Desea agregregarlas?",value = F)

})

output$denominador <- renderUI({
  req(input$tipoCALCULO == "Proporción")
  selectInput("varDENOM", label = h4("Denominador - Opcional"), choices = variables_int(), selected = NULL, multiple = T)
})

  
  
  ### RENDER: IN MAIN PANEL -----
  ### Render título tabulado 
  output$tituloTAB <- renderUI({
    req(input$actionTAB)
    tagList(
      h2("Tabulados"),
      
    )
  })
  
  #### + O U T P U T S + ####
  
  ### CREATE: tabulados  ----
  
  tabuladoOK =  eventReactive(input$actionTAB,{

    tabulado = create_tabulado(base = datos(),   
               v_interes =  input$varINTERES, 
               denominador = input$varDENOM,
               v_cruce = input$varCRUCE, 
               v_subpob =  input$varSUBPOB, 
               v_fexp1 = input$varFACT1, 
               v_conglom = input$varCONGLOM, 
               v_estratos = input$varESTRATOS, 
               tipoCALCULO = input$tipoCALCULO,
               ci = input$IC,
               ajuste_ene = input$ajuste_ene)
    
    #### opción de etiquetas ###

    # browser()

if(input$ETIQUETAS != FALSE && !is.null(input$varCRUCE) &&  labelled::is.labelled(datos()[[input$varCRUCE[1]]])){ # 

      paste_labels = function(tabla, base, var_cruce){
      
      dt = data.frame(valor = labelled::val_labels(base[[var_cruce]]))
      dt = tibble::rownames_to_column(dt)
      
      tabla[[var_cruce]] =  unlist(lapply(tabla[[var_cruce]] ,function(x) as.character(dt$rowname[dt$valor == x])))
      tabla
    }
    
####  al hacer filtros se eliminan categorias, necesitamos sacar etiquetas de base filtrada

      if(!is.null(input$varSUBPOB)){
        datos2 = datos()[datos()[[input$varSUBPOB]] == 1,]
      }else{
        datos2 = datos()
      }
      
    for(i in input$varCRUCE){
      tabulado = paste_labels(tabla = tabulado, base = datos2, var_cruce = i)
    
      }  
  
  }  
    
tabulado
  })
  
  ### RENDER: Tabulado ####
  output$tabulado  <- renderText({
    calidad::tabla_html(tabuladoOK())
  })
  
  ### RENDER: GRÁFICO DE BARRAS CON PORCENTAJE DE CELDAS POR CATEGORÍA
   
  output$grafico  <- renderPlot({
    create_plot(tabuladoOK())
    
  }, height = 100, width = 500)


  # DESCARGA: DE TABULADO GENERADO ----
  
  # Habilitar botón de descarga
  observeEvent(tabuladoOK(), {
    enable("tabla")
  })
  
  
  output$tabla <- downloadHandler(
    
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(tabuladoOK(), file)
    }
  )
  
  
  ##### * Pruebas de outputs * ####  
  output$textcat <- renderPrint({
    paste(tabuladoOK())
  })      
  
  
  if(testeo == T){   
    output$PRUEBAS =  renderUI({
      verbatimTextOutput("textcat")
    })
  }
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

