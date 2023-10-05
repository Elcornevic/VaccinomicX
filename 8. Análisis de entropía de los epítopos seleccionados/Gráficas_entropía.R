# Análisis de entropía 
## Autor: Victor Cornejo Villanueva
library(readxl)
library(ggplot2)

setwd('~/Escritorio/Tesis/RESULTADOS/8. Análisis de entropía de los epítopos seleccionados/Entropy_One')
entropy_list <- read_excel('Entropía_valores.xlsx')

setwd('~/Escritorio/Tesis/RESULTADOS/8. Análisis de entropía de los epítopos seleccionados')
ggplot(data = entropy_list, aes(x = Posición, y = Entropy, group = 1)) +
  geom_line(color = '#B22222') +
  theme_light() +
  scale_color_brewer(palette = 'Paired') +
  geom_vline(xintercept = c(100, 115, 280, 775, 1127, 1345, 1475, 2093, 2220, 2244, 2491)) +
  geom_hline(yintercept = 0.05, color = "blue", linetype = 2, size = 0.8) +
  scale_x_continuous(breaks = seq(1, 3391, by = 125), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 1.1, by = 0.2), expand = c(0,0)) +
  xlab('Posición de aminoácidos') + 
  ylab('Entropía (Hx)') +
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(colour = 'black', size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  annotate('text', x = 50, y = 1.05, label = 'C', fontface = 'bold', color = 'black') +
  annotate('text', x = 51, y = 1, label = '1.4%', color = 'black', size = 3.5) +
  annotate('text', x = 200, y = 1.05, label = 'prM', fontface = 'bold', color = 'black') +
  annotate('text', x = 200, y = 1, label = '2.5%', color = 'black', size = 3.5) +
  annotate('text', x = 530, y = 1.05, label = 'E', fontface = 'bold', color = 'black') +
  annotate('text', x = 536, y = 1, label = '2.3%', color = 'black', size = 3.5) +
  annotate('text', x = 950, y = 1.05, label = 'NS1', fontface = 'bold', color = 'black') +
  annotate('text', x = 953, y = 1, label = '2.4%', color = 'black', size = 3.5) +
  annotate('text', x = 1238, y = 1.05, label = 'NS2A', fontface = 'bold', color = 'black') +
  annotate('text', x = 1241, y = 1, label = '4.6%', color = 'black', size = 3.5) +
  annotate('text', x = 1410, y = 1.05, label = 'NS2B', fontface = 'bold', color = 'black') +
  annotate('text', x = 1413, y = 1, label = '3.1%', color = 'black', size = 3.5) +
  annotate('text', x = 1790, y = 1.05, label = 'NS3', fontface = 'bold', color = 'black') +
  annotate('text', x = 1793, y = 1, label = '1.4%', color = 'black', size = 3.5) +
  annotate('text', x = 2157, y = 1.05, label = 'NS4A', fontface = 'bold', color = 'black') +
  annotate('text', x = 2160, y = 1, label = '3.8%', color = 'black', size = 3.5) +
  annotate('text', x = 2360, y = 1.05, label = 'NS4B', fontface = 'bold', color = 'black') +
  annotate('text', x = 2363, y = 1, label = '2.1%', color = 'black', size = 3.5) +
  annotate('text', x = 2945, y = 1.05, label = 'NS5', fontface = 'bold', color = 'black') +
  annotate('text', x = 2948, y = 1, label = '6.3%', color = 'black', size = 3.5) +
  geom_area(aes(y = Entropy), fill = '#B22222') +
  geom_label(aes(x = 3308, y = 0.084, label = 'Y=0.05'), fill = 'white', color = 'blue', size = 3)
ggsave('all_entropy.jpg', height = 5, width = 14, dpi = 400, bg = 'white', device = 'jpg') 


## Analísis de entropía en proteína NS1
setwd('~/Escritorio/Tesis/RESULTADOS/8. Análisis de entropía de los epítopos seleccionados/Entropy_One')
entropy_NS1 <- read_excel('Entropía_valores.xlsx', sheet = 'NS1')

setwd('~/Escritorio/Tesis/RESULTADOS/8. Análisis de entropía de los epítopos seleccionados')
ggplot(data = entropy_NS1, aes(x = Posición, y = Entropy, group = 1)) +
  geom_line(color = '#00B6FF') +
  theme_light() +
  scale_color_brewer(palette = 'Paired') +
  geom_hline(yintercept = 0.05, color = 'blue', linetype = 2, size = 0.8) +
  scale_x_continuous(breaks = seq(1, 618, by = 50), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), expand = c(0,0)) +
  ggtitle('Proteína No Estructural 1 (NS1)') +
  xlab('Posición de aminoácidos') + 
  ylab('Entropía (Hx)') +
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 14),
        plot.title = element_text(face = 'bold', hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(colour = 'black', size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_segment(aes(x = 192, y = 0.43, xend = 203, yend = 0.43)) +
  annotate('text', x = 197, y = 0.45, label = 'RAVHADMGYWIE', color = 'black', size = 3) +
  geom_area(aes(y = Entropy), fill = '#00B6FF') +
  geom_label(aes(x = 341, y = 0.078, label = 'Y=0.05'), fill = 'white', color = 'blue', size = 3)
ggsave('NS1_entropy.jpg', height = 5, width = 12, dpi = 400, bg = 'white', device = 'jpg')


## Analísis de entropía en proteína NS3
setwd('~/Escritorio/Tesis/RESULTADOS/8. Análisis de entropía de los epítopos seleccionados/Entropy_One')
entropy_NS3 <- read_excel('Entropía_valores.xlsx', sheet = 'NS3')

setwd('~/Escritorio/Tesis/RESULTADOS/8. Análisis de entropía de los epítopos seleccionados')
ggplot(data = entropy_NS3, aes(x = Posición, y = Entropy, group = 1)) +
  geom_line(color = '#31A354') +
  theme_light() +
  scale_color_brewer(palette = 'Paired') +
  geom_hline(yintercept = 0.05, color = 'blue', linetype = 2, size = 0.8) +
  scale_x_continuous(breaks = seq(1, 618, by = 50), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), expand = c(0,0)) +
  ggtitle('Proteína No Estructural 3 (NS3)') +
  xlab('Posición de aminoácidos') + 
  ylab('Entropía (Hx)') +
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 14),
        plot.title = element_text(face = 'bold', hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(colour = 'black', size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_segment(aes(x = 358, y = 0.53, xend = 366, yend = 0.53)) +
  annotate('text', x = 362, y = 0.55, label = 'TVWFVPSIK', color = 'black', size = 3) +
  geom_segment(aes(x = 402, y = 0.44, xend = 413, yend = 0.44)) +
  annotate('text', x = 406, y = 0.46, label = 'WDFVVTTDISEM', color = 'black', size = 3) +
  geom_segment(aes(x = 401, y = 0.63, xend = 412, yend = 0.63)) +
  annotate('text', x = 405, y = 0.65, label = 'DWDFVVTTDISE', color = 'black', size = 3) +
  geom_area(aes(y = Entropy), fill = '#31A354') +
  geom_label(aes(x = 598, y = 0.078, label = 'Y=0.05'), fill = 'white', color = 'blue', size = 3)
ggsave('NS3_entropy.jpg', height = 5, width = 12, dpi = 400, bg = 'white', device = 'jpg')


## Analísis de entropía en proteína NS4B
setwd('~/Escritorio/Tesis/RESULTADOS/8. Análisis de entropía de los epítopos seleccionados/Entropy_One')
entropy_NS4B <- read_excel('Entropía_valores.xlsx', sheet = 'NS4B')

setwd('~/Escritorio/Tesis/RESULTADOS/8. Análisis de entropía de los epítopos seleccionados')
ggplot(data = entropy_NS4B, aes(x = Posición, y = Entropy, group = 1)) +
  geom_line(color = '#BC61F5') +
  theme_light() +
  scale_color_brewer(palette = 'Paired') +
  geom_hline(yintercept = 0.05, color = 'blue', linetype = 2, size = 0.8) +
  scale_x_continuous(breaks = seq(1, 618, by = 50), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), expand = c(0,0)) +
  ggtitle('Proteína No Estructural 4B (NS4B)') +
  xlab('Posición de aminoácidos') + 
  ylab('Entropía (Hx)') +
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 14),
        plot.title = element_text(face = 'bold', hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(colour = 'black', size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_segment(aes(x = 116, y = 0.43, xend = 127, yend = 0.43)) +
  annotate('text', x = 121, y = 0.45, label = 'AHYAIIGPGLQA', color = 'black', size = 3) +
  geom_segment(aes(x = 120, y = 0.34, xend = 128, yend = 0.34)) +
  annotate('text', x = 124, y = 0.36, label = 'IIGPGLQAK', color = 'black', size = 3) +
  geom_segment(aes(x = 117, y = 0.53, xend = 125, yend = 0.53)) +
  annotate('text', x = 121, y = 0.55, label = 'HYAIIGPGL', color = 'black', size = 3) +
  geom_area(aes(y = Entropy), fill = '#BC61F5') +
  geom_label(aes(x = 240, y = 0.078, label = 'Y=0.05'), fill = 'white', color = 'blue', size = 3)
ggsave('NS4B_entropy.jpg', height = 5, width = 12, dpi = 400, bg = 'white', device = 'jpg')


## Analísis de entropía en proteína NS5
setwd('~/Escritorio/Tesis/RESULTADOS/8. Análisis de entropía de los epítopos seleccionados/Entropy_One')
entropy_NS5 <- read_excel('Entropía_valores.xlsx', sheet = 'NS5')

setwd('~/Escritorio/Tesis/RESULTADOS/8. Análisis de entropía de los epítopos seleccionados')
ggplot(data = entropy_NS5, aes(x = Posición, y = Entropy, group = 1)) +
  geom_line(color = 'darkorange') +
  theme_light() +
  scale_color_brewer(palette = 'Paired') +
  geom_hline(yintercept = 0.05, color = 'blue', linetype = 2, size = 0.8) +
  scale_x_continuous(breaks = seq(1, 900, by = 50), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 1.5, by = 0.2), expand = c(0,0)) +
  ggtitle('Proteína No Estructural 5 (NS5)') +
  xlab('Posición de aminoácidos') + 
  ylab('Entropía (Hx)') +
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 14),
        plot.title = element_text(face = 'bold', hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(colour = 'black', size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_segment(aes(x = 765, y = 0.63, xend = 773, yend = 0.63)) +
  annotate('text', x = 768, y = 0.66, label = 'MYFHRRDLR', color = 'black', size = 3) +
  geom_segment(aes(x = 567, y = 0.73, xend = 578, yend = 0.73)) +
  annotate('text', x = 575, y = 0.76, label = 'AIFKLTYQNKVV', color = 'black', size = 3) +
  geom_area(aes(y = Entropy), fill = 'darkorange') +
  geom_label(aes(x = 872, y = 0.080, label = 'Y=0.05'), fill = 'white', color = 'blue', size = 3)
ggsave('NS5_entropy.jpg', height = 5, width = 12, dpi = 400, bg = 'white', device = 'jpg')
