library(ggplot2)
library(dplyr)
library(patchwork)

# Read the dataset
data <- read.csv("trailsNCells.csv")

# Custom theme for aesthetics
custom_theme <- theme_classic() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Scatterplot for cells
scatterplot_celulas <- ggplot(data, aes(x = tiempo, y = celulas)) +
  geom_point(color = "turquoise", size = 4, alpha = 0.8) +
  geom_line(aes(group = 1), color = "turquoise", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  geom_errorbar(aes(ymin = celulas - decelulas, ymax = celulas + decelulas), width = 0.1, color = "gray50") +
  labs(x = "Time (hours+minutes)", y = "Area percentage (%)", title = "Cells' area coverage") +
  ylim(0, 20) +
  custom_theme

# Scatterplot for trails
scatterplot_senderos <- ggplot(data, aes(x = tiempo, y = senderos)) +
  geom_point(color = "gold", size = 4, alpha = 0.8) +
  geom_line(aes(group = 1), color = "gold", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  geom_errorbar(aes(ymin = senderos - desenderos, ymax = senderos + desenderos), width = 0.1, color = "gray50") +
  labs(x = "Time (hours+minutes)", y = "Area percentage (%)", title = "Trails' area coverage") +
  ylim(0, 20) +
  custom_theme

# Scatterplot for trail length
scatterplot_long <- ggplot(data, aes(x = tiempo, y = long)) +
  geom_point(color = "gold", size = 4, alpha = 0.8) +
  geom_line(aes(group = 1), color = "gold", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  geom_errorbar(aes(ymin = long - delong, ymax = long + delong), width = 0.1, color = "gray50") +
  labs(x = "Time (hours+minutes)", y = "Length (mm)", title = "Trails' length") +
  ylim(0, 25) +
  custom_theme

# Save plots
ggsave("scatterplot_celulas.png", scatterplot_celulas, width = 6, height = 6, dpi = 300)
ggsave("scatterplot_senderos.png", scatterplot_senderos, width = 6, height = 6, dpi = 300)
ggsave("scatterplot_long.png", scatterplot_long, width = 6, height = 6, dpi = 300)

# Display plots
print(scatterplot_celulas)
print(scatterplot_senderos)
print(scatterplot_long)

