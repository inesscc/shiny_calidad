
create_tabulado = function(base, v_interes, v_cruce,  v_subpob, v_fexp1, v_conglom,  v_estratos, tipoCALCULO, server = T){
  
  #create_Tabulado = function(base){
  
# base = datos()
# v_interes =  input$varINTERES
# v_cruce = input$varCRUCE
# v_subpob =  input$varSUBPOB
# v_fexp1 = input$varFACT1
# v_conglom = input$varCONGLOM
# v_estratos = input$varESTRATOS
# tipoCALCULO = input$tipoCALCULO
  

  if(length(v_cruce)>1){
    v_cruce1 = paste0(v_cruce, collapse  = "+")
  }else{
    v_cruce1 = v_cruce
  }
  
  base[[v_interes]] = as.numeric(base[[v_interes]])
  base$unit =  as.numeric(base[[v_conglom]])
  base$varstrat =  as.numeric(base[[v_estratos]])
  base$fe =  as.numeric(base[[v_fexp1]])
  
  
  ### Diseño complejo 
  dc <- svydesign(ids = ~unit, strata = ~varstrat,
                  data =  base, weights = ~fe)
  
  options(survey.lonely.psu = "certainty")
  
  
  ### listas de funciones CALIDAD
  funciones_cal = list(calidad::crear_insumos_media, calidad::crear_insumos_prop, 
                       calidad::crear_insumos_tot_con, calidad::crear_insumos_tot)
  funciones_eval = list(calidad::evaluar_calidad_media, calidad::evaluar_calidad_prop, 
                        calidad::evaluar_calidad_tot_con, calidad::evaluar_calidad_tot)
  

  
  
  if(tipoCALCULO %in% "Media") {
    num = 1
  }else if(tipoCALCULO %in% "Proporción"){
    num = 2
  }else if(tipoCALCULO %in% "Suma variable Continua"){
    num = 3
  }else if(tipoCALCULO %in% "Conteo casos"){
    num = 4
  }
  
  
  if(is.null(v_cruce) && is.null(v_subpob)) {
    insumos = funciones_cal[[num]](var = !!parse_expr(enexpr(v_interes)), disenio = dc)
    evaluados =  funciones_eval[[num]](insumos, publicar = TRUE)
    
  } else if (is.null(v_subpob)){
    insumos = funciones_cal[[num]](var = !!parse_expr(enexpr(v_interes)),dominios = !!parse_expr(enexpr(v_cruce1)) ,disenio = dc)
    evaluados =  funciones_eval[[num]](insumos, publicar = TRUE)
    
  } else if (is.null(v_cruce)){
    base[[v_subpob]] = as.numeric(base[[v_subpob]])
    insumos = funciones_cal[[num]](var = !!parse_expr(enexpr(v_interes)),subpop = !!parse_expr(enexpr(v_subpob)) ,disenio = dc)
    evaluados =  funciones_eval[[num]](insumos, publicar = TRUE)
    
  } else {
    base[[v_subpob]] = as.numeric(base[[v_subpob]])
    insumos = funciones_cal[[num]](var = !!parse_expr(enexpr(v_interes)), dominios = !!parse_expr(enexpr(v_cruce1)), subpop = !!parse_expr(enexpr(v_subpob)) ,disenio = dc)
    evaluados =  funciones_eval[[num]](insumos, publicar = TRUE)
  }
  
  return(evaluados)
}
