library(ggplot2)
library(dplyr)

# Load the data
getwd()
setwd("I:/Helotiales_final_maybe")
df <- read.csv("Table S2.csv", sep = ";")

# Replace spaces with newline characters in the Environment column for multi-word environments
df$Environment <- gsub(" ", "\n", df$Environment)

# Create a color mapping for the plot
my_colors <- c(
  "Mycorrhiza" = "#8B4513",          # Brown
  "Freshwater\nSaprobe" = "#56B4E9",  # Sky Blue
  "Endophyte" = "#009E73",           # Green
  "Phytopathogen" = "#FF0000",       # Red
  "Terrestrial\nSaprobe" = "#F0E442", # Yellow
  "Marine\nSaprobe" = "#00008B"       # Dark Blue
)

# Create the plot for Genome_size
Genome_size_plot <- ggplot(data = df, aes(x = Environment, y = Genome_size, color = Environment)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.15) + 
  scale_color_manual(values = my_colors) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, color = "black"),  # x-axis text size
    axis.title.x = element_text(size = 14, color = "black"), # x-axis title size
    axis.text.y = element_text(size = 20, color = "black"),  # y-axis text size
    axis.title.y = element_text(size = 22, color = "black")  # y-axis title size
  ) +
  labs(x = "Environment", y = "Genome Size (Mbp)")

# Save the Genome_size plot
ggsave('Genome_size_plot.jpg', Genome_size_plot, width = 10, height = 8, dpi = 500)
