library(plotly)
library(ggfortify)
library(tidyverse)

# Create data: note in High school for several students
#inat <- read_csv("/Users/marco/Downloads/observations-247664.csv")
data <- read_csv("/Users/marco/GitHub/iwrw/analysis/2024/pinguicula/Pinguicula_niche_data.csv") %>%
  filter(!is.na(Species)) %>%
  select(Species, Elevation, pH, Light, Water, Nutrients)
# %>% mutate(Species = gsub(" ", "_", Species))

pca_res <- prcomp(data[,c(2:6)], scale. = TRUE)
(p <- autoplot(pca_res, data = data, colour = 'Species',
              loadings = TRUE, loadings.colour = 'black',
              loadings.label = TRUE, loadings.label.size = 5,
              loadings.label.colour = "black",
              loadings.label.vjust = -1, size = 4) +
  scale_color_manual(values = c("alpina" = "red", 
                                "leptoceras" = "blue", 
                                "vulgaris" = "orange")) +
  theme_classic() +
  theme(legend.title = element_text(size=26), 
        legend.text = element_text(size=22),
        axis.title = element_text(size=22),
        axis.text = element_text(size=22))
)

ggsave("/Users/marco/GitHub/iwrw/analysis/2024/pinguicula/ping_pca_v2.png",
       width = 3000, height = 2000, units = "px")
 