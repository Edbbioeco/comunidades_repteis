# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggdendro)

library(factoextra)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/mestrado/matriz_ambientais.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

# Agrupamento hierárquico ----

## Calculando uma matriz de distância ----

dist_bray <- dados |>
  dplyr::select_if(is.numeric) |>
  vegan::decostand(method = "standardize") |>
  vegan::vegdist(method = "euclidean")

dist_bray

## Calculando o UPGMA ----

upgma <- dist_bray |>
  hclust(method = "average")

upgma

upgma |> plot()

## Visualizando ----

### Criando um dendro data ----

upgma_data <- upgma |>
  as.dendrogram() |>
  ggdendro::dendro_data()

upgma_data

### Tratando os rótulos ----

upgma_data$labels$label <- dados$`Unidade Amostral`

upgma_data$labels

### Gráfico ----

ggplot() +
  geom_segment(data = upgma_data$segments, aes(x = x, y = y,
                                               xend = xend, yend = yend),
               linewidth = 1) +
  labs(x = NULL,
       y = "Euclidean Distance") +
  scale_x_continuous(breaks = 1:11,
                     labels = dados$`Unidade Amostral`) +
  scale_y_continuous(expand = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave(filename = "upgma.png", height = 10, width = 12)

# Agrupamento não-hierárquico ----

## Número ideal de grupos ----

dados |>
  dplyr::select_if(is.numeric) |>
  vegan::cascadeKM(inf.gr = 2,
                   sup.gr = 10) |>
  plot()

## Calculando k-means ----

set.seed(123); dados |>
  dplyr::select_if(is.numeric) |>
  kmeans(centers = 3, nstart = 100) -> kmeans

kmeans

## Visualizando ----

kmeans |>
  factoextra::fviz_cluster(geom = "point",
                           data = dados |> dplyr::select_if(is.numeric)) +
  scale_fill_manual(values = c("orange", "royalblue", "seagreen")) +
  scale_color_manual(values = c("orange", "royalblue", "seagreen")) +
  theme_minimal() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.position = "bottom")

ggsave(filename = "kmeans.png", height = 10, width = 12)

# Agrupamento hierárquico com as informações do agrupamento não-hierárquico ----

## Tratando os dados de dendograma ----

upgma_data$labels$cluster <- kmeans$cluster |> as.character()

## Visualizando ----

limit <- upgma_data$segments$y |> max()

ggplot() +
  geom_segment(data = upgma_data$segments, aes(x = x, y = y,
                                               xend = xend, yend = yend),
               linewidth = 1) +
  geom_text(data = upgma_data$labels,
            aes(x = x, y = y, label = label, color = cluster),
            angle = 270,
            hjust = -0.05,
            fontface = "bold",
            size =  5) +
  labs(x = NULL,
       y = "Euclidean Distance") +
  scale_y_continuous(limits = c(-0.1, 0.5), breaks = seq(0, 0.5, 0.1)) +
  scale_color_manual(values = c("orange", "royalblue", "seagreen")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "top")

ggsave(filename = "upgma_cluster.png", height = 10, width = 12)
