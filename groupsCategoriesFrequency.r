# Install necessary packages (run only once)
install.packages("ggplot2")
install.packages("dplyr")
install.packages("patchwork")

# Load the required libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# Read the dataset
data <- read.csv("groupsCategories.csv")

scatterplot_cinco <- ggplot(data, aes(x = Tiempo, y = cinco)) +
  geom_point(color = "turquoise", size = 2) +
  geom_line(aes(group = 1), color = "turquoise", size = 0.5) +
  geom_errorbar(aes(ymin = pmax(0, cinco - decinco), ymax = cinco + decinco), width = 0.1) +
  labs(x = "Time (hours + minutes)", y = "") + ylim(-0.25, 6) +
  ggtitle("Small networks (covered area ≥ 1.7998)") + 
  theme_minimal() +
  theme(
    legend.position = "none", plot.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 10, face = "bold"),    
    axis.line = element_line(color = "black"), 
    plot.margin = unit(c(-0.2, 1, -0.2, 1), "cm")
  )

scatterplot_cuatro <- ggplot(data, aes(x = Tiempo, y = cuatro)) +
  geom_point(color = "turquoise", size = 2) +
  geom_line(aes(group = 1), color = "turquoise", size = 0.5) +
  geom_errorbar(aes(ymin = pmax(0, cuatro - decuatro), ymax = cuatro + decuatro), width = 0.1) +
  labs(x = "", y = "") + ylim(-0.25, 25) +
  ggtitle("Large streams (covered area < 1.7998 %)") +
  theme_minimal() +
  theme(
    legend.position = "none", plot.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"), 
    plot.margin = unit(c(-0.2, 1, -0.2, 1), "cm")
  )

scatterplot_tres <- ggplot(data, aes(x = Tiempo, y = tres)) +
  geom_point(color = "turquoise", size = 2) +
  geom_line(aes(group = 1), color = "turquoise", size = 0.5) +
  geom_errorbar(aes(ymin = pmax(0, tres - detres), ymax = tres + detres), width = 0.1) +
  labs(x = "", y = "Average number of cell groups") + ylim(-0.25, 40) +
  ggtitle("Small streams (covered area ≤ 0.1408 %)") +
  theme_minimal() +
  theme(
    legend.position = "none", plot.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 10, face = "bold"),    
    axis.line = element_line(color = "black"), 
    plot.margin = unit(c(-0.2, 1, -0.2, 1), "cm")
  )

scatterplot_dos <- ggplot(data, aes(x = Tiempo, y = dos)) +
  geom_point(color = "turquoise", size = 2) +
  geom_line(aes(group = 1), color = "turquoise", size = 0.5) +
  geom_errorbar(aes(ymin = pmax(0, dos - dedos), ymax = dos + dedos), width = 0.1) +
  labs(x = "", y = "") + ylim(-2, 140) +
  ggtitle("Strings (covered area ≤ 0.0468 %)") +
  theme_minimal() +
  theme(
    legend.position = "none", plot.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"), 
    plot.margin = unit(c(-0.2, 1, -0.2, 1), "cm")
  )

scatterplot_uno <- ggplot(data, aes(x = Tiempo, y = uno)) +
  geom_point(color = "turquoise", size = 2) +
  geom_line(aes(group = 1), color = "turquoise", size = 0.5) +
  geom_errorbar(aes(ymin = pmax(0, uno - deuno), ymax = uno + deuno), width = 0.1) +
  labs(x = "", y = "") + ylim(100, 650) +
  ggtitle("Individual and cell pairs [covered area ≤ 0.0165 %)") +
  theme_minimal() +
  theme(
    legend.position = "none", plot.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"), 
    plot.margin = unit(c(-0.2, 1, -0.2, 1), "cm")
  )

# Arrange the two scatterplots side by side with a common x-axis
library(patchwork)
combined_plot <- scatterplot_uno + scatterplot_dos + scatterplot_tres + scatterplot_cuatro + scatterplot_cinco +
  plot_layout(ncol = 1, nrow = 5) 


# Display the combined plot
print(combined_plot)

ggsave("combined_plot.png", plot = combined_plot, width = 12, height = 10) # Adjust width and height as needed


