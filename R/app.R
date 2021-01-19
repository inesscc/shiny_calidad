


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


lista_categorias_t = function(var, total = T){
  if(total == T){
    
    fe = list()
    
    if(is.null(levels(var))){
      
      fe[[1]] <-  unique(var)
      
    }else{
      
      fe[[1]] = levels(var)
    }
    
    names(fe[[1]]) = "Total"
    
    for (i in 1:length(unique(var))){
      
      if(!is.null(names(labelled::val_labels(var)))){
        
        fe[[i+1]] <- sort(unique(var),decreasing = T)[i] 
        names(fe[[i+1]]) <- rev(names(labelled::val_labels(var)))[i] }else{
          
          if(is.null(levels(var))){
            
            fe[[i+1]] <- unique(var)[i]
            
            names(fe[[i+1]]) <- unique(var)[i]
            
          }else{
            
            fe[[i+1]] <- levels(var)[i]
            
            names(fe[[i+1]]) <- levels(var)[i]
            
          }
          
        }
    }
    ##### sin totla
  }else{
    fe = list()
    
    for (i in 1:length(unique(var))){
      
      if(!is.null(names(labelled::val_labels(var)))){
        
        fe[[i]] <- sort(unique(var),decreasing = T)[i] 
        names(fe[[i]]) <- rev(names(labelled::val_labels(var)))[i] }else{
          
          if(is.null(levels(var))){
            
            fe[[i]] <- unique(var)[i]
            
            names(fe[[i]]) <- unique(var)[i]
            
          }else{
            
            fe[[i]] <- levels(var)[i]
            
            names(fe[[i]]) <- levels(var)[i]
            
          }
          
        }
    }
  }
  return(fe)
  
}


#setwd("/home/ricardo/Documents/INE/Mesa de calidad")
#############################################################################################################

#############################################################################################################

#BKish<- haven::read_sav("BBDD/Base de datos - XV ENUSC 2018.sav")
#bkish = BKish[BKish$Kish == 1,]
#rm(BKish)

#saveRDS(bkish,"/home/ricardo/Documents/INE/Mesa de calidad/Generación_tabulados_2018/bkish.rds")

#base <- readRDS("/home/ricardo/Documents/INE/Mesa de calidad/Generación_tabulados_2018/bkish.rds")

#names(base)

#names(base)
vect_var_int = c("Es necesario cargar base de datos")

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$style("
              body {
    -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
    zoom: 0.8; /* Other non-webkit browsers */
    zoom: 80%; /* Webkit browsers */
}
              "),
    # Application title
    titlePanel("Automatización de tabulado de encuestas"),

    # Sidebar with a slider input for number of bins 
   
        sidebarLayout(
            sidebarPanel(
            
           fileInput(inputId = "file", label = h3("File input") ),
                     
           uiOutput("seleccion1"),
           uiOutput("seleccion2"),
           uiOutput("botonCAT"),
           uiOutput("seleccion4"),
           uiOutput("seleccion3")
           
           
        ),
    mainPanel(
        
      uiOutput("tablaFE"),
        uiOutput("tituloCAT"),
        uiOutput("seleccionCAT"),
      uiOutput("categorias")
        
        
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    data_input <- reactive({
        readRDS(input$file$datapath) # ¿datapath es un atributo de file?
    })
    
    
    variables_int <- reactive({
    req(input$file)
    names(data_input())
    })
    
    output$seleccion1 <- renderUI({
        tagList(
         #   varSelectInput("varINTERES", label = h3("Seleccione las variables de interés"),variables_int() , selected = 1, multiple = T),
            selectInput("varINTERES", label = h3("Seleccione las variables de interés"),choices = variables_int(), selected = 1, multiple = T),
            selectInput("varREGIONES", label = h3("Variable de regiones"), choices = variables_int(), selected = 1, multiple = F),
            selectInput("varCRUCE", label = h3("Variables socioeconómicas de cruce"), choices = variables_int(), selected = 2, multiple = T)
    )
        })

    output$seleccion2 <- renderUI({
        req(input$varCRUCE)
      tagList(
    selectInput("varFACT1", label = h3("Variable para factor expansión"), choices = variables_int(), selected = 1, multiple = F),
    selectInput("varCONGLOM", label = h3("Variable id. Conglomerado"), choices = variables_int(), selected = 1, multiple = F),
    selectInput("varESTRATOS", label = h3("Variable de estratos"), choices = variables_int(), selected = 1, multiple = F)
    )
    })
    
    output$seleccion3 <- renderUI({
        req(input$varDISCOMP)
    checkboxInput("checkbox", label = "¿Existe otra variable para factor expansión?", value = F)
    })
    
    output$seleccion4 <- renderUI({
        req(input$checkbox)
        selectInput("varFACT2", label = h3("Segunda variable de factor expansión"), choices = variables_int(), selected = 1, multiple = F)
    })
    

    output$botonCAT <- renderUI({
        actionButton("actionCAT", label = "Seleccionar Categorias")
        })
    
    
    output$tituloCAT <- renderUI({
        req(input$actionCAT)
        tagList(
    h2("Selección de Categorias de variables"),
    
      hr(),    
      br()
    
 
    
          )
    })
    

    
#    output$seleccionCAT <- renderUI({
#      str(input$varINTERES)  
#    })
# #       

   #####################33 generamos el main panel con las posibles categorias ####
    output$seleccionCAT <- renderUI({
        req(input$actionCAT)
        isolate({data_input()}) ->locafe
    
            tagList(
        lapply(seq_along(input$varINTERES), function(i) {
         if (length(input$varINTERES) == 0) return(names(locafe)[1])
          ## variable unica
         locafe  %>%  select(!!!input$varINTERES) %>% select(i) -> fufu 
         
         ### listamos sus categorias
         locafe  %>%  select(!!!input$varINTERES) %>% select(i) %>% unique() %>% labelled::val_labels(prefixed = T) -> fofo
          as.list(names(fofo[[1]])) -> fifi
         tagList(
           h3(paste(names(fufu))),
          
           h5(paste(sjlabelled::get_label(fufu[[1]], attr.only = T))),
          
          prettyCheckboxGroup(inputId = paste0("categoriasSELEC",i),"_", choices = fifi, inline = T, bigger = T, shape = "round", outline = T),
          hr(),    
          br()
          )
   
      
         }),
        #actionLink("Guardar", "Guardar trabajo"),
        actionButton("generar", "Generar tablas"),
        hr(),    
        br(),
        tableOutput("tabulados")
        
        )
    })
    

    output$textcat <- renderPrint({
      num = length(input$varINTERES)
      
      as.list(lapply(1:num, function(i) {
        input[[paste0("categoriasSELEC",i)]]
      }))
    })      
  
   
 output$categorias =  renderUI({
   verbatimTextOutput("textcat")
 })

        
 #########################  comenzamos a calcular las tablas ########
 
 #### isolate general 
   ##  lista de categorias
  list_caeg  <- isolate({
    num = length(input$varINTERES)
    
   as.list(lapply(1:num, function(i) {
     input[[paste0("categoriasSELEC",i)]]
   }))
   })
   
 
tabuladoOK =  eventReactive(input$generar,{
   ## base datos
   base_is = isolate({data_input()})
   ## lista de variables de interes
   v_interes = isolate({input$varINTERES})
   ## variable regiones
   v_regiones = isolate({input$varREGIONES})
   ## lista de variables a cruzar
   v_cruces = isolate({input$varCRUCE})
   # variable de factor de expansión
   v_fexp1 = isolate({input$varFACT1})
   # variable de factor de expansión 2
   v_fexp2 = isolate({input$varFACT2})
   # variable de id de conglomerado
   v_conglom = isolate({input$CONGLOM})
   # variable de estratos
   v_estratos = isolate({input$ESTRATOS}) 
   
   
   dc_pers <- svydesign(ids = ~v_conglom, strata = ~v_estratos,
                        data = base_is, weights = ~v_fexp1)
   
   dc_hog <- svydesign(ids = ~v_conglom, strata = ~estratos,
                       data = base_is, weights = ~v_fexp12)
   
   options(survey.lonely.psu = "certainty" )
   
   #names(labelled::val_labels(bkish$enc_region16FIX))
   
   lista_categorias_t(base_is[[v_regiones]]) -> list_cat_region; list_cat_region
   
   lista_categorias_t(base_is[[v_cruces[1]]]) -> list_cat1; list_cat1
   
   lista_categorias_t(base_is[[v_cruces[2]]]) -> list_cat2; list_cat2
   
   tabulado = data.frame(matrix(" ",1,1))
   
   col = 2
   
   for (i in seq_len(length(list_cat1))) {
     row = 2

          for (j in seq_len(length(list_cat_region))) {
       
       tabulado[row,col] = as.vector(svymean(~base_is[[v_interes[1]]][base_is[[v_regiones]] %in% list_cat_region[[j]] & base_is[[v_cruces[1]]] %in% list_cat1[[i]]], design= dc_pers[base_is[[v_regiones]] %in% list_cat_region[[j]]  & base_is[[v_cruces[1]]] %in% list_cat1[[i]]]))
       
       row = row + 1
     }
     col = col + 1
     
   }
   
   tabuladoOK = tabulado
   
   
 })
   
   
output$tabulado  <- renderTable({tabuladoOK()})
 
    
    
}
# Run the application 
 shinyApp(ui = ui, server = server)
