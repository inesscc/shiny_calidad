
create_tabulado = function(base, v_interes, v_cruce,  v_subpob, v_fexp1, v_conglom,  v_estratos, tipoCALCULO, ci, ajuste_ene,denominador, server = T){
  
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
  funciones_cal = list(calidad::create_mean, calidad::create_prop,
                       calidad::create_tot_con, calidad::create_tot)
  funciones_eval = list(calidad::evaluate_mean, calidad::evaluate_prop, 
                        calidad::evaluate_tot_con, calidad::evaluate_tot)
  

  
  if(tipoCALCULO %in% "Media") {
    num = 1
  }else if(tipoCALCULO %in% "Proporción"){
    num = 2
  }else if(tipoCALCULO %in% "Suma variable Continua"){
    num = 3
  }else if(tipoCALCULO %in% "Conteo casos"){
    num = 4
  }
  
  
  if(is.null(denominador)){
  if(is.null(v_cruce) && is.null(v_subpob)) {
    insumos = funciones_cal[[num]](var = v_interes, disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T)
    evaluados =  funciones_eval[[num]](insumos, publicar = TRUE)
    
  } else if (is.null(v_subpob)){
    insumos = funciones_cal[[num]](var = v_interes,dominios = v_cruce1 ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T)
    evaluados =  funciones_eval[[num]](insumos, publicar = TRUE)
    
  } else if (is.null(v_cruce)){
    base[[v_subpob]] = as.numeric(base[[v_subpob]])
    insumos = funciones_cal[[num]](var = v_interes,subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T)
    evaluados =  funciones_eval[[num]](insumos, publicar = TRUE)
    
  } else {
    base[[v_subpob]] = as.numeric(base[[v_subpob]])
    insumos = funciones_cal[[num]](var = v_interes,subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T)
    evaluados =  funciones_eval[[num]](insumos, publicar = TRUE)
  }
  }
  
  
  if(!is.null(denominador)){
    if(is.null(v_cruce) && is.null(v_subpob)) {
      insumos = funciones_cal[[num]](var = v_interes, denominador = denominador, disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE)
      
    } else if (is.null(v_subpob)){
      insumos = funciones_cal[[num]](var = v_interes,denominador = denominador, dominios = v_cruce1 ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE)
      
    } else if (is.null(v_cruce)){
      base[[v_subpob]] = as.numeric(base[[v_subpob]])
      insumos = funciones_cal[[num]](var = v_interes ,denominador = denominador, subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE)
      
    } else {
      base[[v_subpob]] = as.numeric(base[[v_subpob]])
      insumos = funciones_cal[[num]](var = v_interes, denominador = denominador, subpop = v_subpob ,disenio = dc, ci = ci, ajuste_ene = ajuste_ene, standard_eval = T)
      evaluados =  funciones_eval[[num]](insumos, publicar = TRUE)
    }
  }
  
  return(evaluados)
}
