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
          uiOutput("botonTAB"),
        ),
        
        ## Main PANEL ----
    mainPanel(
        
      ### render titulo tabulado
      uiOutput("tituloTAB"),
      
      ### render tabulado
      uiOutput("tabulado"),
      uiOutput("PRUEBAS")
        
    )
)
)

# SERVER ----
server <- function(input, output) {
  options(shiny.maxRequestSize=70*1024^2)
    
  ### abrimos input de datos
    data_input <- reactive({
        readRDS(input$file$datapath) # ¿datapath es un atributo de file?
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
            selectInput("varINTERES", label = h3("Seleccione las variables de interés"),choices = variables_int(), selected = "VP_DC", multiple = T),
            selectInput("varCRUCE", label = h3("Variables de cruce (una o mas)"), choices = variables_int(), selected = 2, multiple = T)
    )})
    
    ### Render selección 2 DC----
    output$seleccion2 <- renderUI({
        req(input$varINTERES)
      tagList(
    selectInput("varFACT1", label = h3("Variable para factor expansión"), choices = variables_int(), selected = "Fact_pers", multiple = F),
    selectInput("varCONGLOM", label = h3("Variable id. Conglomerado"), choices = variables_int(), selected = "Conglomerado", multiple = F),
    selectInput("varESTRATOS",label = h3("Variable de estratos"), choices = variables_int(), selected = "VarStrat", multiple = F)
    )
    })
    
    ## Botón generación tabuldado ----
    output$botonTAB <- renderUI({
        actionButton("actionTAB", label = "Generar tabulado")
        })
    
  
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
         base_is = isolate({data_input()})
         ## lista de variables de interes
         v_interes = isolate({input$varINTERES})
         ## lista de variables a cruzar
         v_cruces = isolate({input$varCRUCE})
         # variable de factor de expansión
         v_fexp1 = isolate({input$varFACT1})
         # variable de factor de expansión 2
         v_fexp2 = isolate({input$varFACT2})
         # variable de id de conglomerado
         v_conglom = isolate({input$varCONGLOM})
         # variable de estratos
         v_estratos = isolate({input$varESTRATOS}) 
         
       #  tabuladoOK  = list(v_interes, v_cruces, v_fexp1, v_fexp2, v_conglom, v_estratos)
         
         f_conglom = as.formula(paste0("~",v_conglom))
         f_fexp1 = as.formula(paste0("~",v_fexp1))
         f_estratos = as.formula(paste0("~",v_estratos))
    
         #base_is[[v_interes]] = as.numeric(base_is[[v_interes]])
    
         dc <- svydesign(ids = f_conglom, strata = f_estratos,
                              data = base_is, weights = f_fexp1)
         
         options(survey.lonely.psu = "certainty" )
         
         # class(base_is[[v_interes]])
         # eval(parse(text = v_interes))
          v_interes <- rlang::parse_expr(v_interes) 
         
         calidad::crear_insumos_prop(var = v_interes , disenio = dc)
         
        # v_interes
    })
  
    
    
   # output$tabulado  <- renderTable({tabuladoOK()})
    
    
    
     output$textcat <- renderPrint({
    
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
