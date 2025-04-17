# Install necessary packages
# install.packages("lme4")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("cowplot")
# install.packages("ggpubr")
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggpubr)
library(tidyr)
library(lme4)
getwd()
# Load the data
# Assuming your data is in a CSV file named 'metadata.csv'
setwd("C:/Users/danie/OneDrive/Área de Trabalho/Helotiales_V6")
# Read the data from CSV file
df <- read.csv("Table S2.csv", sep = ";")

# List of feature pairs to process
features <- list(
  P450 = c("Thermo_P450", "Cold_P450"),
  DFVF = c("Thermo_DFVF", "Cold_DFVF"),
  CAZy = c("Thermo_Secreted_CAZy", "Cold_Secreted_CAZy"),
  Effector = c("Thermo_Effector", "Cold_Effector")
)

# Create a color mapping for the plot
my_colors <- c(
  "Thermo" = "#FF9999",  # Light red
  "Cold" = "#99CCFF"  # Light blue
)

# Function to create the plot for a given feature
create_plot <- function(df, feature_name, thermo_col, cold_col, my_colors) {
  # Prepare data in long format for plotting and statistical analysis
  df_long <- df %>% 
    select(Environment, !!sym(thermo_col), !!sym(cold_col)) %>% 
    pivot_longer(cols = c(!!sym(thermo_col), !!sym(cold_col)), names_to = "Condition", values_to = "Value") %>% 
    mutate(Condition = ifelse(Condition == thermo_col, "Thermo", "Cold"))
  
  # Perform paired t-test for each environment
  p_values <- df_long %>% 
    group_by(Environment) %>% 
    summarise(p_value = t.test(Value ~ Condition, paired = TRUE)$p.value)
  
  # Define the position for the paired t-test p-value label
  p_label_y <- max(df_long$Value, na.rm = TRUE) + 0.1
  
  # Determine the y-axis label
  y_label <- switch(
    feature_name,
    "P450" = "P450 Genes",
    "DFVF" = "Virulence Factors Genes",
    "CAZy" = "CAZy Genes",
    "Effector" = "Effector Genes",
    paste(feature_name, "values", sep = " ")
  )
  
  # Create the plot
  plot <- ggplot(data = df_long, mapping = aes(x = Environment, y = Value, fill = Condition)) + 
    geom_boxplot() + 
    geom_jitter(aes(color = Condition), size = 3, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) + 
    scale_fill_manual(name = "Adaptation", values = c("Thermo" = "#FF9999", "Cold" = "#99CCFF")) +  # Light red and light blue for bars
    scale_color_manual(values = c("Thermo" = "#FF0000", "Cold" = "#0000FF")) +  # Dark red and dark blue for dots
    stat_compare_means(aes(group = Condition), method = "t.test", paired = TRUE, label = "p.signif", size = 10) +  # Add paired t-test p-value and set size
    theme(
      plot.margin = margin(5.5, 5.5, 40, 5.5),  # Adjust the margin to add space below the plot
      axis.text.x = element_text(size = 20, color = "black"),  # Increase the font size and color for x-axis labels
      axis.title.x = element_text(size = 20),  # Increase the font size for x-axis title
      axis.text.y = element_text(size = 14),  # Increase the font size for y-axis numbers
      axis.title.y = element_text(size = 16),  # Increase the font size for y-axis title
      legend.text = element_text(size = 14),  # Increase the font size for legend text
      legend.title = element_text(size = 16)  # Increase the font size for legend title
    ) +
    labs(
      x = "Environment",  # Explicitly set the x-axis title
      y = y_label  # Set the y-axis title
    ) +
    # Add vertical lines to separate environments
    geom_vline(xintercept = seq(1.5, length(unique(df$Environment)) - 0.5, by = 1), linetype = "dashed", color = "black")
  
  # Save the plot
  ggsave(paste0(feature_name, '_plot.jpg'), plot, width = 20, height = 10, dpi = 300)
}

# Generate plots for all features
for (feature_name in names(features)) {
  create_plot(df, feature_name, features[[feature_name]][1], features[[feature_name]][2], my_colors)
}
