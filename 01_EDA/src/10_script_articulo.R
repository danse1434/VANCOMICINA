#' --- 
#' title: 'Script generador de perfiles concentración-tiempo vancomicina' 
#' subtitle: 'PK poblacional de Vancomicina' 
#' date: '2022-10-14' 
#' author: Daniel S. Parra G. 
#' email: dsparrag@minsalud.gov.co 
#' institute: FNE - Fondo Nacional de Estupefacientes - Misión PRI 1901
#' abstract: abstract 
#' output:  
#'     pdf_document: default 
#'     html_document: default 
#' always_allow_html: true 
#' --- 

# Carga de paquetes
require(patchwork)
require(gt)
require(scales)
require(tidyverse)
theme_set(theme_bw())

source('src/20_funciones.R')


data <- read_csv('data/data_TAD.csv', na = '.')


ytype_status <- c(
  `1` = 'Microbiológico',
  `2` = 'Quimioluminiscencia'
)

ytype_LLOQ <- tribble( ~ YTYPE, ~ LLOQ,
                       1, NA_real_,
                       2, 3)

data %>% 
  mutate(ID = factor(ID)) %>% 
  filter(EVID == 0 & YTYPE == 2) %>% pull(DV) %>% summary()


gperfil1 <- data %>% 
  mutate(ID = factor(ID)) %>% 
  filter(EVID == 0 & YTYPE == 2) %>% 
  ggplot(., aes(x = TAD, y = DV, group = ID, col = ID)) + 
  geom_point() + geom_line() + 
  xlab('Time After Last Dose (h)') +
  ylab('VAN concentration (mg/L)') + 
  guides(col = guide_legend(nrow=5)) + 
  scale_x_continuous(breaks = breaks_width(2)) + 
  coord_cartesian(xlim=c(0,12), ylim = c(1,60)) +
  geom_hline(data = ytype_LLOQ, aes(yintercept = LLOQ), col='red', lty='dashed') +
  scale_color_viridis_d() + 
  annotate("text", 0, 6, label = "LLOQ", col = "red", hjust = 0) + 
  guides(color = guide_legend(ncol = 5)) + 
  theme(legend.position = c(1, 1), legend.justification = c(1.05,1.05))


ggsave(filename = file.path("output", "figs", "DV_vs_TAD.pdf"), plot = gperfil1, 
       device = 'pdf', width = 8, height = 8 * 0.60, units = 'in')

saveRDS(gperfil1, file.path("output", "figs", "DV_vs_TAD.rds"))

