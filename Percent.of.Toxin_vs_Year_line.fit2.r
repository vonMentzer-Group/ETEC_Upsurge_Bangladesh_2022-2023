Toxin_comparison <- read.csv("Toxin comparison (2012-2023).csv", row.names = 1)
head(Toxin_comparison)
Toxin_comparison <- Toxin_comparison[,-1]
#-------------------------------------------------------------------------------
library(reshape2)
Toxin_comparison <- as.matrix(Toxin_comparison)
df.Toxin_comparison <- melt(Toxin_comparison)
colnames(df.Toxin_comparison) <- c("Toxin", "Year", "Value")
head(df.Toxin_comparison)
df.Toxin_comparison$Year  <- sapply(as.vector(df.Toxin_comparison$Year), function(Z) {strsplit(Z,"X")[[1]][2]})
df.Toxin_comparison$Year  <- as.numeric(df.Toxin_comparison$Year)
df.Toxin_comparison$Value <- as.numeric(df.Toxin_comparison$Value)
#-------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(viridis)
# Perform linear regression for each Toxin and extract p-values
lm_results <- df.Toxin_comparison %>%
  group_by(Toxin) %>%
  summarise(p_value = summary(lm(Value ~ Year))$coefficients[2, 4])

# Create a data frame for p-values annotations
p_value_annotations <- data.frame(
  Toxin = unique(df.Toxin_comparison$Toxin),
  # p_value_label = paste("p =", formatC(lm_results$p_value, format = "e", digits = 3)),
  p_value_label = paste("p =", formatC(lm_results$p_value, format = NULL, digits = 2)),
  x = 2013, # Adjust x position as needed
  y = 11    # Adjust y position as needed
)
#-------------------------------------------------------------------------------
Line_Plot <- ggplot(data = df.Toxin_comparison, aes(y = Value, 
                                       x = Year, 
                                       shape = Toxin, 
                                       color = Toxin)) + 
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = y ~ x) +    
  scale_color_viridis(discrete = TRUE) +  
  scale_shape_manual(values = c("circle", "circle", "circle")) + # using numerical values for shapes
  facet_wrap(~Toxin) + # facet by toxin
  theme_bw(base_size = 10) + # increase font size for the entire plot
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 2, hjust = 1.7),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)
        ) +
  ylim(c(10, 50)) +
  labs(x = "Years", y = "% of  toxin profile",  fill = "Toxin Profile") +
  scale_x_continuous(limits = c(2013, 2023), breaks = seq(2013, 2023, by = 1))+
  geom_text(data = p_value_annotations,
            aes(x = x, y = y, label = p_value_label),
            color = "black",
            size = 3,
            hjust = 0)
ggsave("plot_300dpi.png", plot = Line_Plot, width = 107*2, height = 107/2.5*2, units = "mm", dpi = 300)
# Make 107 mm
# 107 mm = 4.2126 Inc
pdf("Percent.of.Toxin_vs_Year_line.fit2.pdf", height = 3, width = 8)
Line_Plot
dev.off()




