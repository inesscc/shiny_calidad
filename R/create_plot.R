create_plot <- function(tab) {
  tabulado <- tab
  labels <- unique(tabulado$calidad) %>% 
    sort()
  
  pasted_labels <- labels %>% 
    paste(collapse = " ")
  
  
  colors <- switch(pasted_labels, "no fiable" =  "red",
                   "poco fiable" = "yellow",
                   "fiable" = "green",
                   "fiable no fiable" = c("green", "red"),
                   "fiable no fiable poco fiable" = c("green", "red", "yellow"),
                   "fiable poco fiable" = c("green", "yellow"))
  
  tabulado %>% 
    dplyr::group_by(calidad) %>% 
    dplyr::summarise(suma = n()) %>% 
    dplyr::mutate(porcentaje = suma / sum(suma) * 100,
                  porcentaje_chr = paste0(round(porcentaje), "%"))  %>% 
    dplyr::mutate(row = 1,
                  calidad = forcats::fct_relevel(calidad, labels)) %>% 
    ggplot2::ggplot(aes(x = row, y = porcentaje, fill = calidad)) +
    ggplot2::geom_bar(stat = "identity" ) + 
    ggplot2::scale_fill_manual(values= colors) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(label = porcentaje_chr), 
                       position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()) 
}