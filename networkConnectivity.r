# Load the required libraries
install.packages("patchwork")
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
library(patchwork)

# Read the dataset
data <- read.csv("networkConnectivity.csv")

# Define a common theme for consistency
theme_custom <- theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    plot.margin = margin(10, 10, 10, 10)
  )

# Create scatterplots
scatterplot_ncomp <- ggplot(data, aes(x = tiempo, y = nCompμ)) +
  geom_point(color = "firebrick2", size = 3, alpha = 0.8) +
  geom_line(aes(group = 1), color = "firebrick2", linewidth = 0.7) +
  geom_errorbar(aes(ymin = nCompμ - nCompς, ymax = nCompμ + nCompς), width = 0.2, alpha = 0.6) +
  labs(x = "", y = "Number of connected components", title = "Connected Components") +
  ylim(0, 550) + theme_custom

scatterplot_LCCS <- ggplot(data, aes(x = tiempo, y = LCCSnμ)) +
  geom_point(color = "firebrick2", size = 3, alpha = 0.8) +
  geom_line(aes(group = 1), color = "firebrick2", linewidth = 0.7) +
  geom_errorbar(aes(ymin = LCCSnμ - LCCSnς, ymax = LCCSnμ + LCCSnς), width = 0.2, alpha = 0.6) +
  labs(x = "", y = "Number of nodes", title = "Largest Connected Component") +
  ylim(-10, 1600) + theme_custom

scatterplot_dgr <- ggplot(data, aes(x = tiempo, y = Dgrμ)) +
  geom_point(color = "firebrick2", size = 3, alpha = 0.8) +
  geom_line(aes(group = 1), color = "firebrick2", linewidth = 0.7) +
  geom_errorbar(aes(ymin = Dgrμ - Dgrς, ymax = Dgrμ + Dgrς), width = 0.2, alpha = 0.6) +
  labs(x = "Time (hours+minutes)", y = "Number of connections", title = "Mean Degree") +
  ylim(0, 3) + theme_custom

scatterplot_LCCAPL <- ggplot(data, aes(x = tiempo, y = LCCAPLμ)) +
  geom_point(color = "firebrick2", size = 3, alpha = 0.8) +
  geom_line(aes(group = 1), color = "firebrick2", linewidth = 0.7) +
  geom_errorbar(aes(ymin = LCCAPLμ - LCCAPLς, ymax = LCCAPLμ + LCCAPLς), width = 0.2, alpha = 0.6) +
  labs(x = "Time (hours+minutes)", y = "Number of edges", title = "Average Path Length") +
  ylim(0, 30) + theme_custom

# Normalizar el eje x (LCCSeμ) usando min-max scaling
data <- data %>%
  mutate(LCCSeμ_norm = (LCCSeμ - min(LCCSeμ)) / (max(LCCSeμ) - min(LCCSeμ)))
data <- data %>%  
  mutate(LCCSeς_norm = (LCCSeς - min(LCCSeς)) / (max(LCCSeς) - min(LCCSeς)))
data <- data %>%  
  mutate(Dgrμ_norm = (Dgrμ - min(Dgrμ)) / (max(Dgrμ) - min(Dgrμ)))
data <- data %>%  
  mutate(Dgrς_norm = (Dgrς - min(Dgrς)) / (max(Dgrς) - min(Dgrς)))


# Crear el gráfico con la nueva variable normalizada
scatterplot_dgrxedge <- ggplot(data, aes(x = LCCSeμ_norm, y = Dgrμ_norm)) +
  geom_point(color = "firebrick2", size = 3, alpha = 0.8) +
  geom_line(aes(group = 1), color = "firebrick2", linewidth = 0.5) +
  geom_errorbar(aes(ymin = Dgrμ_norm , ymax = Dgrμ_norm ), width = 0.01, alpha = 0.6) +
  labs(x = "Largest connected component (normalized number of edges)", y = "Normalized connections per node") + ylim(0, 1) +
  theme_minimal() + ggtitle("") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.line = element_line(color = "black"), 
        plot.margin=margin(b = 60)) # Ajustar el margen inferior si es necesario

print(scatterplot_dgrxedge)

scatterplot_dgrxedge <- ggplot(data, aes(x = Dgrμ, y = LCCSeμ_norm)) +
  geom_point(color = "firebrick2", size = 3, alpha = 0.8) +
  geom_line(aes(group = 1), color = "firebrick2", linewidth = 0.5) +
  geom_errorbar(aes(ymin = LCCSeμ_norm , ymax = LCCSeμ_norm ), width = 0.01, alpha = 0.6) +
  labs(x = "Connections per node", y = "Largest connected component (normalized number of edges)") + ylim(0, 1) +
  theme_minimal() + ggtitle("") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.line = element_line(color = "black"), 
        plot.margin=margin(b = 60)) # Ajustar el margen inferior si es 
       
print(scatterplot_dgrxedge)
        
# Guardar la nueva figura
png("scatterplot_dgrxedge_normalized.png", width = 800, height = 600)
print(scatterplot_dgrxedge)
dev.off()

# Arrange plots in a 2x2 grid
combined_plot <- (scatterplot_ncomp + scatterplot_LCCS) /
                  (scatterplot_dgr + scatterplot_LCCAPL) 
                  
# Save and display plots
ggsave("combined_plot.png", plot = combined_plot, width = 20, height = 20, dpi = 300)
print(combined_plot)

# Save additional scatterplot
png("combined_plot.png", width = 800, height = 600, dpi = 300)
print(combined_plot)
dev.off()

