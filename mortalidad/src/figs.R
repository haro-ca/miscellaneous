# Librerías ====
options(tidyverse.quiet = T)
library(tidyverse)
library(patchwork)

# Funciones ====
lectura <- function(path = NULL, skip = NULL) {
    readxl::read_excel(path, skip = skip) %>%
            janitor::clean_names()
}

limpieza <- function(dataf) {
    dataf %>%
        pivot_longer(cols = c(-clave_inegi, -entidad),
                     names_to = 'anio',
                     values_to = 'mortalidad') %>%
        mutate(anio = parse_number(anio))
}

calcular_topn_mortalidad <- function(dataf, n) {
   dataf %>%
    filter(anio == max(anio)) %>%
    arrange(desc(mortalidad)) %>%
    top_n(n, mortalidad) %>%
    mutate(color = unname(yarrr::piratepal('basel')[1:n]),
           alpha = 1) %>%
    select(entidad, color, alpha)
}

# Inicio ====
mort_por_entidad <- lectura(path = 'data/raw/indice.xlsx', skip = 7) %>%
                    limpieza()

# Control de color y alpha a los top n ====
top <- mort_por_entidad %>%
    calcular_topn_mortalidad(n = 10)

frame <- mort_por_entidad %>%
    left_join(top, by = 'entidad') %>%
    mutate(color = replace_na(color, 'black'),
           alpha = replace_na(alpha, 0.2),
           anio_label = as.character(anio) %>% str_sub(start = 3L) %>% str_c('\'', .)) %>%
    arrange(entidad)

# Gráficas ====
# 'spaguetti' plot ====
gg_mortalidad <- frame %>%
    ggplot() +
    geom_line(aes(x = anio, y = mortalidad, group = entidad, color = entidad),
              alpha = frame$alpha,
              size = 1) +
    scale_y_continuous(breaks = seq(0, 200, by = 25)) +
    scale_x_continuous(breaks = seq(min(frame$anio), max(frame$anio), by = 1),
                       labels = frame$anio_label %>% unique()) +
    scale_color_manual(values = frame %>% distinct(entidad, .keep_all = T) %>% .$color) +
    labs(title = 'Tasa de mortalidad por diabetes e hipertensión, 2001 - 2018\n(por cada 10 mil habitantes)',
         y = NULL,
         x = NULL,
         caption = 'Elaboración propia con datos del INEGI',
         color = 'Estado') +
    guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 22))

# facetted plot ====
gg_mort_faceted <- frame %>%
    ggplot() +
    facet_wrap(vars(entidad)) +
    aes(x = anio, y = mortalidad, color = entidad, label = round(mortalidad, digits = 1)) +
    geom_line(size = 1) +
    geom_text(vjust = -1, color = 'black', size = 7,
              data = frame %>% filter(anio %in% c(min(anio), max(anio)))) +
    scale_x_continuous(breaks = c(2001, 2005, 2010, 2015, 2018),
                       expand = expansion(c(0.1, 0.15)),
                       labels = frame %>%
                                   filter(anio %in% c(2001, 2005, 2010, 2015, 2018)) %>%
                                   .$anio_label %>% unique()) +
    scale_y_continuous(breaks = seq(0, 200, by = 50),
                       expand = expansion(c(0.05, 0.5))) +
    scale_color_manual(values = frame %>% distinct(entidad, .keep_all = T) %>% .$color) +
    labs(title = 'Tasa de mortalidad por diabetes e hipertensión, 2001 - 2018\n(por cada 10 mil habitantes)',
         y = NULL,
         x = NULL,
         caption = 'Elaboración propia con datos del INEGI',
         color = 'Estado') +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 22),
          legend.position = 'none')

# Escritura
write_csv(mort_por_entidad, 'data/processed/tidy_mortalidad.csv')
ggsave(plot = gg_mortalidad,
       filename = 'figs/gg_mort_estatal.png', width = 22, height = 11)
ggsave(plot = gg_mort_faceted,
       filename = 'figs/gg_mort_estatal_facets.png', width = 22, height = 11)
