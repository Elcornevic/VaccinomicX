## DETERMINACIÓN DE COBERTURA POBLACIONAL
setwd('~/Escritorio/Tesis/RESULTADOS/6. Determinación de cobertura poblacional')

library(ggpubr)
library(ggplot2)
library(rstatix)
library(readxl)

only_regiones <- read_excel('Cobertura_resultados.xlsx', sheet = 'Comparaciones', col_names = TRUE)

model <- aov(Porcentaje ~ Países, data = only_regiones)
summary(model)

## 1. Barplot de regiones
only_regiones <- read_excel('Cobertura_resultados.xlsx', sheet = 'Only_regiones', col_names = TRUE)
only_regiones$Porcentaje <- as.numeric(only_regiones$Porcentaje)
d_only_regiones <- density(only_regiones$Porcentaje)
plot(d_only_regiones)

stat.test.only_regiones.all <- only_regiones %>% wilcox_test(Porcentaje ~ Regiones, p.adjust.method = 'fdr')
stat.test.only_regiones.all ## Revisar pares significativamente distintos

o_regiones <- ggbarplot(only_regiones, x = 'Regiones', y = 'Porcentaje', fill = 'Regiones', palette = 'simpsons', ylab = 'Cobertura poblacional (%)', xlab = '') +
  geom_hline(yintercept = 96.22, linetype = 'dashed') +
  coord_flip() +
  annotate('text', x = 3, y = 99, label = 'Mundo (96.22%)', srt = 90) +
  annotate('text', x = 1, y = 83, label = '94.18%', fontface = 'bold', color = 'black') +
  annotate('text', x = 1, y = 19, label = 'Asia Oriental', fontface = 'bold', color = 'black') +
  annotate('text', x = 2, y = 70, label = '81.13%', fontface = 'bold', color = '#92C5DE') + 
  annotate('text', x = 3, y = 77.4, label = '88.11%', fontface = 'bold', color = 'white') + 
  annotate('text', x = 3, y = 18, label = 'Asia del Sur', fontface = 'bold', color = 'white') + 
  annotate('text', x = 4, y = 77.5, label = '88.54%', fontface = 'bold', color = 'white') +
  annotate('text', x = 4, y = 23.5, label = 'Sureste Asiático', fontface = 'bold', color = 'white') + 
  annotate('text', x = 5, y = 74, label = '85.43%', fontface = 'bold', color = '#92C5DE') +
  annotate('text', x = 6, y = 85, label = '98.42%', fontface = 'bold', color = 'black') +
  annotate('text', x = 6, y = 11.5, label = 'Europa', fontface = 'bold', color = 'black') +
  annotate('text', x = 7, y = 69, label = '80.71%', fontface = 'bold', color = '#92C5DE') +
  annotate('text', x = 8, y = 74.5, label='86.42%', fontface = 'bold', color = '#92C5DE') +
  annotate('text', x = 9, y = 72, label = '83.64%', fontface = 'bold', color = '#92C5DE') +
  annotate('text', x = 10, y = 80, label = '91.41%', fontface = 'bold', color = 'black') +
  annotate('text', x = 10, y = 23.5, label = 'África del Norte', fontface = 'bold', color = 'black') +
  annotate('text', x = 11, y = 69.5, label = '80.93%', fontface = 'bold', color = '#92C5DE') +
  annotate('text', x = 12, y = 72.5, label = '84.13%', fontface = 'bold', color = '#92C5DE') +
  annotate('text', x = 13, y = 84.5, label = '97.08%', fontface = 'bold', color = 'black') +
  annotate('text', x = 13, y = 21, label = 'Norteamérica', fontface = 'bold', color = 'black') +
  annotate('text', x = 14, y = 76.5, label = '87.18%', fontface = 'bold', color = 'white') +
  annotate('text', x = 14, y = 21.7, label = 'Centroamérica', fontface = 'bold', color = 'white') +
  annotate('text', x = 15, y = 82.8, label = '94.43%', fontface = 'bold', color = 'black') +
  annotate('text', x = 15, y = 18, label = 'Sudamérica', fontface = 'bold', color = 'black') +
  annotate('text', x = 16, y = 77, label = '88.48%', fontface = 'bold', color = 'white') +
  annotate('text', x = 16, y = 13, label = 'Oceanía', fontface = 'bold', color = 'white') +
  rremove('y.text') +
  geom_segment(aes(x = 15, y = 40, xend = 15, yend = 35),arrow = arrow(length = unit(0.2, 'cm'))) +
  theme(legend.position = 'left')
o_regiones
ggsave('regiones.jpg', height = 7, width = 6, dpi = 400, bg = 'white', device = 'jpg')


## 2. Barplot de países por región
sudamerica <- read_excel('Cobertura_resultados.xlsx', sheet = 'Sudamérica', col_names = TRUE)
sudamerica$Porcentaje <- as.numeric(sudamerica$Porcentaje)
d_sudamerica <- density(sudamerica$Porcentaje)
plot(d_sudamerica)

stat.test.sudamerica.all <- sudamerica %>% wilcox_test(Porcentaje ~ Países, p.adjust.method = 'fdr')
stat.test.sudamerica.all

sudamerica_b <- ggbarplot(sudamerica, x = 'Países', y = 'Porcentaje', fill = 'Países', palette = 'lancet', ylab = 'Cobertura poblacional (%)', xlab = '') +
  geom_hline(yintercept = 94.43, linetype = 'dashed') +
  rremove('x.text') +
  annotate('text', x = 8.5, y = 105, label = 'Sudamérica (94.43%)', fontface = 'bold', color = 'black') +
  annotate('text', x = 3, y = 88, label = '99.54%', fontface = 'bold', color = 'white', size = 4)
sudamerica_b

norteamerica <- read_excel('Cobertura_resultados.xlsx', sheet = 'Norteamérica', col_names = TRUE)
norteamerica$Porcentaje <- as.numeric(norteamerica$Porcentaje)
d_norteamerica <- density(norteamerica$Porcentaje)
plot(d_norteamerica)
stat.test.norteamerica.all <- norteamerica %>% wilcox_test(Porcentaje ~ Países, p.adjust.method = 'fdr')
stat.test.norteamerica.all
norteamerica_b <- ggbarplot(norteamerica, x = 'Países', y = 'Porcentaje', fill = 'Países', palette = 'uchicago', ylab = '', xlab = '') +
  geom_hline(yintercept = 97.08, linetype = 'dashed') +
  rremove('legend') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  annotate('text', x = 2, y = 106, label = 'Norteamérica \n (97.08%)', fontface = 'bold', color = 'black', size = 2.7)
norteamerica_b

europa <- read_excel('Cobertura_resultados.xlsx', sheet = 'Europa', col_names = TRUE)
europa$Porcentaje <- as.numeric(europa$Porcentaje)
d_europa <- density(europa$Porcentaje)
plot(d_europa)
stat.test.europa.all <- europa %>% wilcox_test(Porcentaje ~ Países, p.adjust.method = 'fdr')
stat.test.europa.all
library(RColorBrewer)
nb.cols <- 31
mycolors <- colorRampPalette(brewer.pal(8, 'Set2'))(nb.cols)
europa_b <- ggbarplot(europa, x = 'Países', y = 'Porcentaje', fill = 'Países', palette = mycolors, ylab = 'Cobertura \n poblacional (%)', xlab = '') +
  geom_hline(yintercept = 98.42, linetype = 'dashed') +
  rremove('legend') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  annotate('text', x = 29, y = 105, label = 'Europa (98.42%)', fontface = 'bold', color = 'black', size = 4)
europa_b

oceania <- read_excel('Cobertura_resultados.xlsx', sheet = 'Oceanía', col_names = TRUE)
oceania$Porcentaje <- as.numeric(oceania$Porcentaje)
d_oceania <- density(oceania$Porcentaje)
plot(d_oceania)
oceania_b <- ggbarplot(oceania, x = 'Países', y = 'Porcentaje', fill = 'Países', palette = 'Set3', ylab = 'Cobertura poblacional (%)', xlab = '') +
  coord_flip() +
   geom_hline(yintercept = 88.48, linetype = 'dashed') +
  rremove('legend') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  annotate('text', x = 12.7, y = 71, label = '  Oceanía \n (88.48%)', fontface = 'bold', color = 'black', size = 4)
oceania_b

centroamerica <- read_excel('Cobertura_resultados.xlsx', sheet = 'Centroamérica', col_names = TRUE)
centroamerica$Porcentaje <- as.numeric(centroamerica$Porcentaje)
d_centroamerica <- density(centroamerica$Porcentaje)
plot(d_centroamerica)
centroamerica_b <- ggbarplot(centroamerica, x = 'Países', y = 'Porcentaje', fill = 'Países', palette = 'npg', ylab = 'Cobertura poblacional (%)', xlab = '') +
  geom_hline(yintercept = 87.18, linetype = 'dashed') +
  rremove('legend') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  annotate('text', x = 1.5, y = 95, label = 'Centroamérica \n (87.18%)', fontface = 'bold', color = 'black', size = 2.7)
centroamerica_b

africa <- read_excel('Cobertura_resultados.xlsx', sheet = 'África', col_names = TRUE)
africa$Porcentaje <- as.numeric(africa$Porcentaje)
d_africa <- density(africa$Porcentaje)
plot(d_africa)
africa_b <- ggbarplot(africa, x = 'Países', y = 'Porcentaje', fill = 'Países', palette = 'jco', ylab = '', xlab = '') +
  geom_hline(yintercept = 91.41, linetype = 'dashed') +
  rremove('legend') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  annotate('text', x = 3.5, y = 102.5, label = 'África del Norte \n (91.41%)', fontface = 'bold', color = 'black', size = 2.6)
africa_b

sure_asia <- read_excel('Cobertura_resultados.xlsx', sheet = 'Sureste asiático', col_names = TRUE)
sure_asia$Porcentaje <- as.numeric(sure_asia$Porcentaje)
d_sure_asia <- density(sure_asia$Porcentaje)
plot(d_sure_asia)
sure_asia_b <- ggbarplot(sure_asia, x = 'Países', y = 'Porcentaje', fill = 'Países', palette = 'Set4', ylab = 'Cobertura poblacional (%)', xlab = '') +
  geom_hline(yintercept = 88.54, linetype = 'dashed') +
  rremove('legend') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  annotate('text', x = 6.3, y = 96, label = 'Sureste Asiático (88.54%)', fontface = 'bold', color = 'black', size = 4)
sure_asia_b

asia_sur <- read_excel('Cobertura_resultados.xlsx', sheet = 'Asia del Sur', col_names = TRUE)
asia_sur$Porcentaje <- as.numeric(asia_sur$Porcentaje)
d_asia_sur <- density(asia_sur$Porcentaje)
plot(d_asia_sur)
asia_sur_b <- ggbarplot(asia_sur, x = 'Países', y = 'Porcentaje', fill = 'Países', palette = 'ucscgb', ylab = '', xlab = '') +
  geom_hline(yintercept = 88.11, linetype = 'dashed') +
  rremove('legend') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  annotate('text', x = 2, y = 96, label = 'Asia del Sur \n (88.11%)', fontface = 'bold', color = 'black', size = 2.7)
asia_sur_b

asia_oriental <- read_excel('Cobertura_resultados.xlsx', sheet = 'Asia Oriental', col_names = TRUE)
asia_oriental$Porcentaje <- as.numeric(asia_oriental$Porcentaje)
d_asia_oriental <- density(asia_oriental$Porcentaje)
plot(d_asia_oriental)
asia_oriental_b <- ggbarplot(asia_oriental, x = 'Países', y = 'Porcentaje', fill = 'Países', palette = 'rickandmorty', ylab = '', xlab = '') +
  geom_hline(yintercept = 94.18, linetype = 'dashed') +
  rremove('legend') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  annotate('text', x = 2, y = 103, label = 'Asia Oriental \n (94.18%)', fontface = 'bold', color = 'black', size = 2.7)
asia_oriental_b

### http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
ggarrange(sudamerica_b,                                              
          ggarrange(centroamerica_b, norteamerica_b, africa_b, asia_sur_b, asia_oriental_b, ncol = 5, labels = c('B', 'C', 'D', 'E', 'F')),
          europa_b,
          ggarrange(sure_asia_b, oceania_b, ncol = 2, labels = c('H', 'I')),
          nrow = 4, 
          labels = c('A', '', 'G')) 

ggsave('países.jpg', height = 16, width = 10, dpi = 400, bg = 'white', device = 'jpg')