library(plotly)
library(ggfortify)
library(tidyverse)
library(corrplot)

# Create data: note in High school for several students
#inat <- read_csv("/Users/marco/Downloads/observations-247664.csv")
data <- read_csv("/Users/marco/GitHub/iwrw/analysis/lilies/data.csv")

pca_res <- prcomp(data %>% elect(-species, -inat_id, -individuals), scale. = TRUE)

(p <- autoplot(pca_res, data = data, colour = 'species',
              loadings = TRUE, loadings.colour = 'black',
              loadings.label = TRUE, loadings.label.size = 5,
              loadings.label.colour = "black",
              loadings.label.vjust = -1, size = 4) 
  + theme_classic()
  + theme(legend.title = element_text(size=26), 
          legend.text = element_text(size=22),
          axis.title = element_text(size=22),
          axis.text = element_text(size=22)))

ggsave("./figure_1.jpg",
       width = 4000, height = 3000, units = "px")


# Calculate correlation matrix
correlation_matrix <- cor(data %>% filter(species == "Lilium martagon") %>% select("ph", "light", "water", "nutrients", "individuals"))

# Reshape correlation matrix into tidy format
correlation_data <- correlation_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "species") %>%
  pivot_longer(cols = -species, names_to = "variable", values_to = "correlation")

# Create correlation heatmap
ggplot(correlation_data, aes(x = species, y = variable, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(text = element_text(size = 25))

ggsave("./lilies/figure_2_Lilium_martagon.jpg",
       width = 4000, height = 3000, units = "px")

