library(ggplot2)
library(dplyr)
library(cowplot)
library(ggpubr)
library(multcompView)  # For generating significance letters

# Load the data
getwd()
setwd("I:/Helotiales_final")
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

# Function to create individual plots
create_plot <- function(df, column, color_mapping, y_label) {
  means <- df %>% group_by(Environment) %>% summarise(mean_value = mean(.data[[column]], na.rm = TRUE))
  anova_result <- aov(as.formula(paste(column, "~ Environment")), data = df)
  tukey_result <- TukeyHSD(anova_result)
  significance_letters <- multcompLetters4(anova_result, tukey_result)$Environment
  letters_df <- data.frame(
    Environment = names(significance_letters$Letters),
    Letters = significance_letters$Letters
  )
  
  plot <- ggplot(data = df, mapping = aes_string(x = "Environment", y = column, color = "Environment")) + 
    geom_boxplot() + 
    geom_jitter(width = 0.15) + 
    scale_color_manual(values = color_mapping) +
    theme(plot.margin = margin(5.5, 40, 40, 5.5)) +  # Adjust the margin to add space below the plot
    geom_text(data = means, aes(x = Environment, y = -Inf, label = paste("AVG:", sprintf("%.2f", mean_value))), 
              vjust = -0.5, size = 7, color = "black") +  # Reduced size of AVG label to 4
    geom_text(data = letters_df, aes_string(x = "Environment", y = paste("max(df$", column, ", na.rm = TRUE) + 10"), label = "Letters"), 
              vjust = 0, size = 8, color = "black") +  # Size of significance letters remains at 8
    theme(legend.position = "none", 
          axis.text.x = element_text(size = 20, color = "black"),  # Reduced x-axis text size
          axis.title.x = element_text(size = 22, color = "black"),  # Increase x-axis title size and set to black
          axis.text.y = element_text(size = 20, color = "black"),  # Increase y-axis text size and set to black
          axis.title.y = element_text(size = 22, color = "black")) +  # Increase y-axis title size and set to black
    labs(x = "Environment", y = y_label)
  
  return(plot)
}

# Create all the individual plots
P450_plot <- create_plot(df, "P450", my_colors, "P450 Genes")
DFVF_genes_plot <- create_plot(df, "DFVF_genes", my_colors, "Virulence Factors Genes")
Effector_genes_plot <- create_plot(df, "Effector_genes", my_colors, "Effector Genes")
CAZy_plot <- create_plot(df, "Secreted_CAZy", my_colors, "CAZy Genes")

# Extract the legend from one of the plots
legend <- get_legend(P450_plot + 
                       theme(legend.position = "bottom", 
                             legend.text = element_text(size = 20, color = "black"),  # Increase legend text size and set to black
                             legend.title = element_text(size = 22, color = "black")) +  # Increase legend title size and set to black
                       labs(color = "Environment"))

# Combine all the plots into one
combined_plot <- plot_grid(
  P450_plot, DFVF_genes_plot, Effector_genes_plot, CAZy_plot,
  labels = "AUTO", ncol = 2, align = 'v'
)

# Add the legend to the combined plot
combined_plot_with_legend <- plot_grid(combined_plot, legend, ncol = 1, rel_heights = c(1, 0.1))

# Save the combined plot with the legend
ggsave('combined_plot_with_legend.jpg', combined_plot_with_legend, width = 25, height = 20, dpi = 500)

# Save each individual plot
ggsave('P450_plot_general.jpg', P450_plot, width = 10, height = 8, dpi = 500)
ggsave('DFVF_genes_plot_general.jpg', DFVF_genes_plot, width = 10, height = 8, dpi = 500)
ggsave('Effector_genes_plot_general.jpg', Effector_genes_plot, width = 10, height = 8, dpi = 500)
ggsave('CAZy_plot_general.jpg', CAZy_plot, width = 10, height = 8, dpi = 500)

# If you want to save the legend separately
legend <- get_legend(P450_plot + 
                       theme(legend.position = "bottom", 
                             legend.text = element_text(size = 10, color = "black"),  # Increase legend text size and set to black
                             legend.title = element_text(size = 12, color = "black")) +  # Increase legend title size and set to black
                       labs(color = "Environment"))

# Save the legend
legend_plot <- plot_grid(legend)
ggsave('legend_plot_general.jpg', legend_plot, width = 10, height = 2, dpi = 500)
