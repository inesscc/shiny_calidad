
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


testeo = F

# UI ----
ui <- fluidPage(
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
              uiOutput("PRUEBAS")
              
    )
  )
)

# SERVER ----
server <- function(input, output) {
  ## parametro para subir archivos pesados  
  options(shiny.maxRequestSize=1000*1024^2)
  
  
  # Logo inicial
  observe( {
    #Muestra el cartel cuando inicia la APP
    shinyalert(imageUrl = "logo_ine.png",imageWidth = "200", imageHeight = "200",
               closeOnClickOutside = T, showConfirmButton = F)
  })
  
  
  
  ### abrimos input de datos
  data_input <- reactive({
    
    
    if(grepl(".sav", input$file$datapath)){
      
      haven::read_sav(input$file$datapath)
      
    } else if(grepl(".rds", input$file$datapath)){
      
      readRDS(input$file$datapath)
    } # sas, dta, csv, feather, xlsx,
    
  })
  
  ### Extraemos nombres de variables input datos
  variables_int <- reactive({
    
    
    if (!is.null(input$file)) {
      names(data_input())    
    } else if (!is.null(descarga())) {
      names(descarga())      
    }
    
  })
  
  ### RENDER IN SIDE BAR -----
  ### Render selección 1 -----
  output$seleccion1 <- renderUI({
    tagList(## render selección variables DC
      #   varSelectInput("varINTERES", label = h3("Seleccione las variables de interés"),variables_int() , selected = 1, multiple = T),
      selectInput("varINTERES", label = h4("Variable de interés"),choices = variables_int(),  multiple = F),
      radioButtons("tipoCALCULO", "¿Qué tipo de cálculo deseas realizar?",choices = list("Media","Proporción","Suma variable Continua","Conteo casos"), inline = F ),
      selectInput("varCRUCE", label = h4("Desagregación"), choices = variables_int(), selected = NULL, multiple = T),
      selectInput("varSUBPOB", label = h4("Sub Población"), choices = variables_int(), selected = NULL, multiple = T)
    )})
  
  ### Render selección 2 DC----
  output$seleccion2 <- renderUI({
    req(input$varINTERES)
    tagList(
      selectInput("varFACT1", label = h4("Variable para factor expansión"), choices = variables_int(), selected = "Fact_pers", multiple = F),
      selectInput("varCONGLOM", label = h4("Variable para conglomerados"), choices = variables_int(), selected = "Conglomerado", multiple = F),
      selectInput("varESTRATOS",label = h4("Variable para estratos"), choices = variables_int(), selected = "VarStrat", multiple = F), 
      downloadButton("tabla", label = "Descargar"),
      actionButton("actionTAB", label = "Generar tabulado")
      
    )
  })
  
  ## Botón generación tabuldado ----
  #output$botonTAB <- renderUI({
  #    actionButton("actionTAB", label = "Generar tabulado")
  #    })
  
  
  ### RENDER IN MAIN PANEL -----
  ### Render título tabulado 
  output$tituloTAB <- renderUI({
    req(input$actionTAB)
    tagList(
      h2("Tabulados"),
      
    )
  })
  
  
  ###################################
  # DATOS WEB INE - COMPUTADOR LOCAL
  ##################################
  datos <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    new <- data_input() 
    datos(new)
  })
  
  
  observeEvent(input$base_ine, {
    datos(descarga())
  })
  
  
  ### TABULADO generación ----
  
  tabuladoOK =  eventReactive(input$actionTAB,{
    
    ## base datos
    base_is =  datos()
    ## lista de variables de interes
    v_interes =  input$varINTERES
    ## lista de variables a cruzar
    if(length(input$varCRUCE)>1){
      v_cruce = paste0(input$varCRUCE, collapse  = "+")
    }else{
      v_cruce = input$varCRUCE
    }
    
    #v_cruce =  input$varCRUCE
    ## variable subpoblación
    v_subpob =  input$varSUBPOB
    # variable de factor de expansión
    v_fexp1 = input$varFACT1#
    # variable de id de conglomerado
    v_conglom = input$varCONGLOM 
    # variable de estratos
    v_estratos = input$varESTRATOS 
    
    
    #f_conglom = as.formula(paste0("~",v_conglom))
    #f_fexp1 = as.formula(paste0("~",v_fexp1))
    #f_estratos = as.formula(paste0("~",v_estratos))
    
    #  list(v_conglom, v_estratos,v_fexp1, v_interes)
    
    
    base_is[[v_interes]] = as.numeric(base_is[[v_interes]])
    base_is$unit =  as.numeric(base_is[[v_conglom]])
    base_is$varstrat =  as.numeric(base_is[[v_estratos]])
    base_is$fe =  as.numeric(base_is[[v_fexp1]])
    
    
    ### Diseño complejo ####
    dc <- svydesign(ids = ~unit, strata = ~varstrat,
                    data =  base_is, weights = ~fe)
    
    options(survey.lonely.psu = "certainty")
    
    
    ### listas de funciones CALIDAD ####
    funciones_cal = list(calidad::crear_insumos_media, calidad::crear_insumos_prop, 
                         calidad::crear_insumos_tot_con, calidad::crear_insumos_tot)
    funciones_eval = list(calidad::evaluar_calidad_media, calidad::evaluar_calidad_prop, 
                          calidad::evaluar_calidad_tot_con, calidad::evaluar_calidad_tot)
    
    
    if(input$tipoCALCULO %in% "Media") {
      num = 1
    }else if(input$tipoCALCULO %in% "Proporción"){
      num = 2
    }else if(input$tipoCALCULO %in% "Suma variable Continua"){
      num = 3
    }else if(input$tipoCALCULO %in% "Conteo casos"){
      num = 4
    }
    
    
    if(is.null(input$varCRUCE) && is.null(input$varSUBPOB)) {
      insumos = funciones_cal[[num]](var = !!parse_expr(enexpr(v_interes)), disenio = dc)
      evaluados =  funciones_eval[[num]](insumos)
      
    } else if (is.null(input$varSUBPOB)){
      #    base_is[[v_cruce]] = as.numeric(base_is[[v_cruce]])
      #browser()
      insumos = funciones_cal[[num]](var = !!parse_expr(enexpr(v_interes)),dominios = !!parse_expr(enexpr(v_cruce)) ,disenio = dc)
      evaluados =  funciones_eval[[num]](insumos)
      
    } else if (is.null(input$varCRUCE)){
      base_is[[v_subpob]] = as.numeric(base_is[[v_subpob]])
      insumos = funciones_cal[[num]](var = !!parse_expr(enexpr(v_interes)),subpop = !!parse_expr(enexpr(v_subpob)) ,disenio = dc)
      evaluados =  funciones_eval[[num]](insumos)
      
    } else {
      base_is[[v_subpob]] = as.numeric(base_is[[v_subpob]])
      insumos = funciones_cal[[num]](var = !!parse_expr(enexpr(v_interes)),subpop = !!parse_expr(enexpr(v_subpob)) ,disenio = dc)
      evaluados =  funciones_eval[[num]](insumos)
    }
    evaluados
    
  })
  
  
  ##############################
  # DESCARGA DE DATOS PÁGINA INE
  ##############################
  
  descarga =  eventReactive(input$base_ine, {
    show_modal_spinner() # show the modal window
    #Descargar la base de datos en archivo temporal
    temp <- tempfile()
    
    # Seleccionar la ruta de cada base de datos
    if (input$base_web_ine == "epf") {
      file <- "https://www.ine.cl/docs/default-source/encuesta-de-presupuestos-familiares/bbdd/viii-epf---(junio-2016---julio-2017)/base-personas-viii-epf-(formato-csv).csv?sfvrsn=8cdf62d7_2&download=true"
      datos <-  read_delim(file, delim = ';')
      
    } else if (input$base_web_ine == "ene") {
      file <- "https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/formato-csv/ene-2020-10-son.csv?sfvrsn=35fc8a67_4&download=true"
      datos <-  read_delim(file, delim = ';')
      
    } else if (input$base_web_ine == "enusc") {
      file <- "https://www.ine.cl/docs/default-source/seguridad-ciudadana/bbdd/2019/base-de-datos---xvi-enusc-2019-(csv).csv?sfvrsn=d3465758_2&download=true"
      datos <-  read_delim(file, delim = ';')
      
    } else if (input$base_web_ine == "esi") {
      file <- "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_4&download=true"
      datos <-  read_delim(file, delim = ';')
      
    } else if (input$base_web_ine == "enut") {
      file <- "https://www.ine.cl/docs/default-source/uso-del-tiempo-tiempo-libre/bbdd/documentos/base_datos_enut_csv.zip?sfvrsn=b399edf0_5"
      download.file(file, temp)
      unzip(temp)
      datos <-  read_delim("BASE_USUARIO ENUT 2015.csv", delim = ';')
      
    }
    
    remove_modal_spinner() # remove it when done
    
    #dim(datos)
    datos
  }) 
  
  ### render Tabulado ####
  output$tabulado  <- renderText({
    calidad::tabla_html(tabuladoOK())
  })
  
  
  
  ###################################
  # Descargar la tabla generada ####
  ###################################   
  # 
  output$tabla <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(tabuladoOK(), file)
      #write.csv(tabuladoOK(), file)
    }
  )
  
  
  ##### Pruebas de outputs ####  
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

