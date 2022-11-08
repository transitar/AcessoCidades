#ggplot patchwork
var <- "P002"


func_print_map <- function(var){
  titulo <- label_names %>% filter(variavel == var) %>% pull(legenda)
  dado_print <- data_complete %>% select(var)
  map <- ggplot() +
    geom_raster(data = maptiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() +
    theme_map() +
    geom_sf(data = st_transform(dado_print,3857),aes(fill = !!sym(var)),color = NA) + 
    # facet_wrap(~dado, labeller = labeller_grupos) +
    scale_fill_gradientn(
      name = "Nº de Habitantes",
      colors =colors_orange,
      # colours = hcl.colors(n = 10,palette = "oranges",rev = T),
      # values = NULL,
      space = "Lab",
      na.value = NA,
      # guide = "colourbar",
      aesthetics = "fill"
      # colors
    ) +
    # tema_populacao()
    theme(legend.position = "bottom",
          legend.justification = "center",
          strip.text.x = element_text(size=rel(1.5)),
          strip.background = element_rect(
            color = NA,
            fill = "grey70"
          ),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(), 
          panel.grid = element_blank(),
          plot.margin=unit(c(2,0,0,0),"mm"),
          legend.key.width=unit(2,"line"),
          legend.key.height = unit(.5,"cm"),
          legend.text=element_text(size=rel(1.2)),
          legend.title=element_text(size=rel(1.2),                                   ),
          plot.title = element_text(hjust = 0.5,vjust = -5, size = 14),
          strip.text = element_text(size = 10)
    ) +
    ggtitle(titulo)
  return(map)
  
  
}

homens_brancos <- func_print_map("P001")
homens_pretos <- func_print_map("P002")
homens_amarelos <- func_print_map("P003")
homens_pardos <- func_print_map("P004")
homens_indigenas <- func_print_map("P005")
homens_total <- func_print_map("Ptot_homens")

mulheres_brancas <- func_print_map("P006")
mulheres_pretas <- func_print_map("P007")
mulheres_amarelas <- func_print_map("P008")
mulheres_pardas <- func_print_map("P009")
mulheres_indigenas <- func_print_map("P010")
mulheres_total <- func_print_map("Ptot_mulheres")

(homens_brancos+homens_pretos+homens_amarelos)/(homens_pardos+homens_indigenas+homens_total)
