setwd("/Users/marco/GitHub/iwrw/analysis//")

library(ggplot2)
library(plyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(stringr)
library(plotly)
library(grid)
library(gridExtra)
library(dplyr)
library(ggfortify)

zeiger <- read.csv("flora_indicativa/FI_Plant.csv", sep = ";", header = T)
zeiger <- zeiger[, c(2, 19, 22, 24)]
colnames(zeiger) <- c("scientific_name", "F", "R", "N")

# List all files in the data directory
csv_files <- list.files("./data/", pattern = "*.csv", full.names = TRUE)

# Read in plot description
plots <- read_csv("./../R_files/iwrw-plots.csv")

obs <- map_dfr(csv_files, ~read_csv(.x)) %>%
  filter(area != 0) %>%
  mutate(scientific_name = paste(genus, " ", species, sep = "")) %>%
  mutate(scientific_name = if_else(scientific_name == "Festuca valesiaca aggr.", "Festuca valesiaca", scientific_name)) %>%
  left_join(zeiger, by = "scientific_name") %>%
  filter(genus != "0" | !is.na(genus))

result <- obs %>%
  group_by(plot_id) %>%
  summarize(
    Count = n(),
    Avg_N = round(mean(as.numeric(as.character(N)), na.rm = TRUE), 2),
    Avg_F = round(mean(as.numeric(as.character(F)), na.rm = TRUE), 2),
    Avg_R = round(mean(as.numeric(as.character(R)), na.rm = TRUE), 2),
    Avg_tot_cover = mean(tot_cover, na.rm = TRUE),
    Avg_height_ave = mean(height_ave, na.rm = TRUE),
    Avg_height_max = mean(height_max, na.rm = TRUE)
    ) %>%
  left_join(plots, by = "plot_id")

write_csv(result %>% select(plot_id, Count, Avg_N, Avg_F, Avg_R), "./export/table_for_Luis.csv")

# Create a scatter plot with correlation line
F1A <- ggplot(result, aes(x = management, y = Count)) +
  geom_boxplot() +
  geom_point() +                            # Add scatter points
  #geom_text(aes(label=plot_id), hjust=-0.2, vjust=0, size = 2) +
  labs(x = "Management", y = "Number of species") +    # Set x and y axis labels
  theme_classic() +
  theme(text = element_text(size = 16))

F1B <- ggplot(result, aes(x = management, y = Avg_N)) +
  geom_boxplot() +
  geom_point() +                            # Add scatter points
  labs(x = "Management", y = "Average N values") +    # Set x and y axis labels
  theme_classic() +
  theme(text = element_text(size = 16))

F1C <- ggplot(result, aes(x = Avg_N, y = Count)) +
  geom_point() +                            # Add scatter points
  geom_smooth(method = "lm", se = TRUE, col = "red") +   # Add correlation line
  labs(x = "Average N value", y = "Number of species") +    # Set x and y axis labels
  theme_classic() +
  theme(text = element_text(size = 16))

(F1 <- cowplot::plot_grid(F1A,
                          F1B,
                          F1C,
                          ncol = 3, nrow = 1, byrow = FALSE,
                          labels = c("A", "B", "C"),
                          label_size = 20))

ggsave("./export/figure_1.jpg",
       width = 4000, height = 2000, units = "px")

pca_res <- prcomp(result %>%
                    select(Count, Avg_N, Avg_F, Avg_R, Avg_tot_cover, Avg_height_ave, Avg_height_max, cut_days), 
                  scale. = TRUE)
(p <- autoplot(pca_res, data = result, colour = 'management',
               loadings = TRUE, loadings.colour = 'black',
               loadings.label = TRUE, loadings.label.size = 5,
               loadings.label.colour = "black",
               loadings.label.vjust = -1, size = 4) 
  + theme_classic()
  + theme(legend.title = element_text(size=26), 
          legend.text = element_text(size=22),
          axis.title = element_text(size=22),
          axis.text = element_text(size=22)))

ggsave("./export/figure_2.jpg",
       width = 4000, height = 3000, units = "px")







# Select the columns for PCA
data_for_pca <- select(result, Count, Avg_N, Avg_F, Avg_R, Avg_tot_cover, Avg_height_ave, Avg_height_max)

# Perform PCA
pca_result <- prcomp(data_for_pca, scale. = TRUE)

# Access the PCA results
loadings <- pca_result$rotation
scores <- pca_result$x

# Create a dataframe for variable names and loadings
loading_df <- data.frame(variable = colnames(data_for_pca), loadings)

# Create a scatter plot with labeled arrows
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(data = loading_df, aes(x = PC1, y = PC2, label = variable), color = "black", size = 4, hjust = 0, vjust = 0) +
  geom_segment(data = loading_df, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.3, "cm")), color = "blue") +
  xlim(min(scores[, "PC1"]) - 1, max(scores[, "PC1"]) + 1) +
  ylim(min(scores[, "PC2"]) - 1, max(scores[, "PC2"]) + 1) +
  xlab(paste0("PC1 (", round(pca_result$sdev[1] / sum(pca_result$sdev) * 100, 1), "%)")) +
  ylab(paste0("PC2 (", round(pca_result$sdev[2] / sum(pca_result$sdev) * 100, 1), "%)")) +
  ggtitle("PCA Biplot") +
  theme_minimal()














inat <- read_csv("observations_IWRW2022.csv") %>%
  filter(str_detect(description, "^M")) %>%
  mutate(description = gsub("\\?", "", description)) %>%
  mutate(description = gsub(" ", "", description)) %>%
  mutate(description = gsub("M10C", "M10F", description)) %>%
  mutate(description = gsub("M10M", "M10F", description)) %>%
  mutate(description = gsub("M3M", "M3C", description)) %>%
  mutate(description = gsub("M9C\n", "M9C", description)) %>%
  mutate(description = gsub("M7M", "M7N", description)) %>%
  mutate(description = as.factor(description))


# Adding the indicator values to the iNat dataset
with_zeiger <- merge(inat, zeiger, by = "scientific_name")

# Checking that all plots have a decent number of observations (that the student did make large errors in the attribution of the code)
(number_per_plot <- with_zeiger %>% group_by(with_zeiger$description) %>%
  summarise(Count = n_distinct(scientific_name)) %>%
  as.data.frame() %>%
  rename(localities = "with_zeiger$description") %>%
  rename(count = "Count") %>%
  mutate(localities = as.character(localities)) %>%
  mutate(meadow_locations = substr(localities, 2, nchar(localities)-1)) %>%
  mutate(fertilization_regime = substr(localities, nchar(localities), nchar(localities))) %>%
  mutate(average_N = tapply(as.numeric(with_zeiger$N), with_zeiger$description, mean)[1:12])
  )


# The last line creates a new column named Count with a value calculated by n(), 
# which counts observations (rows) per group.

###############################################
#Biodiversity and fertilizer

# Boxplot for Biodiversity
ggplot(number_per_plot, aes(x=fertilization_regime, y=count)) + 
  geom_boxplot(outlier.size=0) +
  geom_jitter(color="black", size=3, alpha=0.9, width = 0) +
  xlab("Fertilization scheme") +
  ylab("Number of species detected") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#  scale_x_discrete(limits = c("N", "O", "T")) +
  geom_text(aes(label=localities), hjust=-0.2, vjust=0) +
  theme(text = element_text(size = 20), 
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), face="bold"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), face="bold"))

# Boxplot for Indicators
ggplot(number_per_plot, aes(x=fertilization_regime, y=average_N)) + 
  geom_boxplot(outlier.size=0) +
  geom_jitter(color="black", size=3, alpha=0.9, width = 0) +
  xlab("Fertilization scheme") +
  ylab("N indicator value") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  #  scale_x_discrete(limits = c("N", "O", "T")) +
  geom_text(aes(label=localities), hjust=-0.2, vjust=0) +
  theme(text = element_text(size = 20), 
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), face="bold"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), face="bold"))

