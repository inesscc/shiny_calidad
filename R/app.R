### pendiente 
# 1 Cargar en el computador, o descargar pagina INE (desplegue lista) (externos) Klaus
# 2 subpob (si o si) Ricardo (1 semana)

# 3 posibilidad mas un calculo por variable Ricardo (2 semana)
# 3.1 mas variables?    

# 4 Render tabla y evaluación calidad 

# 5 botón de descarga Excel, Csv o Rdata. tabla, PDF Klaus (1 semana)

# 6 Pestaña elaborar propio estandar y comparar.

#### Abril fines 

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


rm(list = ls())


# UI ----
ui <- fluidPage(
    tags$style("
              body {
    -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
    zoom: 0.8; /* Other non-webkit browsers */
    zoom: 80%; /* Webkit browsers */
}
              "),
    # Application title
    titlePanel("Evaluación calidad del dato en encuestas de hogar"),

    # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
            
          ## input de archivo -----
           fileInput(inputId = "file", label = h3("File input") ),
                     
          ## render selección de variables de interes, y de cruce
           uiOutput("seleccion1"),
          
          ## render selección variables DC
           uiOutput("seleccion2"),
          
          ## botón generación tabulado 
          uiOutput("botonTAB")
          
      
        ),
        
        ## Main PANEL ----
    mainPanel(
        
      ### render titulo tabulado
      uiOutput("tituloTAB"),
      
      ### render tabulado
      htmlOutput("tabulado"),
      uiOutput("PRUEBAS")

    )
)
)

# SERVER ----
server <- function(input, output) {
  options(shiny.maxRequestSize=1000*1024^2)
    
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
    req(input$file)
    names(data_input())
    })
    
    
    ### RENDER IN SIDE BAR -----
    ### Render selección 1 -----
    output$seleccion1 <- renderUI({
              tagList(## render selección variables DC
         #   varSelectInput("varINTERES", label = h3("Seleccione las variables de interés"),variables_int() , selected = 1, multiple = T),
            selectInput("varINTERES", label = h3("Variable de interés"),choices = variables_int(), selected = "VP_DC", multiple = T),
           radioButtons("tipoCALCULO", "¿Que tipo de cálculo deseas realizar?",choices = list("Media","Proporción","Suma variable Continua","Conteo casos"),),
           selectInput("varCRUCE", label = h3("Desagregación"), choices = variables_int(), selected = 2, multiple = T)
    )})
    
    ### Render selección 2 DC----
    output$seleccion2 <- renderUI({
        req(input$varINTERES)
      tagList(
    selectInput("varFACT1", label = h3("Variable para factor expansión"), choices = variables_int(), selected = "Fact_pers", multiple = F),
    selectInput("varCONGLOM", label = h3("Variable para conglomerados"), choices = variables_int(), selected = "Conglomerado", multiple = F),
    selectInput("varESTRATOS",label = h3("Variable para estratos"), choices = variables_int(), selected = "VarStrat", multiple = F), 
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
    

    ### TABULADO generación ----
    
    tabuladoOK =  eventReactive(input$actionTAB,{
      
         ## base datos
         base_is =  data_input()
         ## lista de variables de interes
         v_interes =  input$varINTERES
         ## lista de variables a cruzar
         v_cruces = input$varCRUCE 
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
       
        
      # base_is$unit =  base_is[[v_conglom]]
      # base_is$varstrat =  base_is[[v_estratos]]
      # base_is$fe =  base_is[[v_fexp1]]
        
      #  list(base_is$unit,  base_is$varstrat,  base_is$fe,  base_is[[v_interes]])
         
       dc <- svydesign(ids = ~unit, strata = ~varstrat,
                       data =  base_is, weights = ~fe)
       
       options(survey.lonely.psu = "certainty")
    #   dc$call 
     
         #### seleccionando la función a utilizar
         
         #"Media","Proporción","Suma variable Continua","Conteo casos"
         

      if(input$tipoCALCULO %in% "Media") {
      #  message = paste0("media")
        insumos = calidad::crear_insumos_media(var = !!parse_expr(enexpr(v_interes)), disenio = dc)
        evaluados =   calidad::evaluar_calidad_media(insumos)
        
      }else if(input$tipoCALCULO %in% "Proporción"){
       # message = paste0("Proporción")
        insumos = calidad::crear_insumos_prop(var = !!parse_expr(enexpr(v_interes)), disenio = dc)
        evaluados =   calidad::evaluar_calidad_prop(insumos)
        
      }else if(input$tipoCALCULO %in% "Suma variable Continua"){
        #message = paste0("Suma variable Continua")
        insumos = calidad::crear_insumos_tot_con(var = !!parse_expr(enexpr(v_interes)), disenio = dc)
        evaluados =   calidad::evaluar_calidad_tot_con(insumos)
        
      }else if(input$tipoCALCULO %in% "Conteo casos"){
        #message = paste0("Conteo casos")
        insumos = calidad::crear_insumos_tot(var = !!parse_expr(enexpr(v_interes)), disenio = dc)
        evaluados = calidad::evaluar_calidad_tot(insumos)
        
      }
       
         
         ## generamos cálculo 
         
   #     ins_prop = calidad::crear_insumos_prop(var = !!parse_expr(enexpr(v_interes)), disenio = dc)
    #    cal_prop =   calidad::evaluar_calidad_prop(ins_prop)
     
         #message    


    })
  
    
    
   output$tabulado  <- renderText({
   
   #tabuladoOK()
   
  calidad::tabla_html(tabuladoOK())

    
    })
 
   # Bloque para descargar la tabla generada
   output$tabla <- downloadHandler(
     filename = function() {
       paste("data-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(tabuladoOK(), file)
     }
   )
    
    
     output$textcat <- renderPrint({
    
      # tabuladoOK()
       
       paste(tabuladoOK())
       
       
     ### útil para  varios tabulados
     # num = length(input$varINTERES)
      
     # as.list(lapply(1:num, function(i) {
  #  #    input[[paste0("categoriasSELEC",i)]]
     # }))
      
    })      
  
   
 output$PRUEBAS =  renderUI({
   verbatimTextOutput("textcat")
 })

}
# Run the application 
 shinyApp(ui = ui, server = server)
