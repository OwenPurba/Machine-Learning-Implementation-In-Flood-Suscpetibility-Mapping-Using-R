#VIF
Model <- lm(training_df$label~training_df$ELEVASI+training_df$SLOPE+training_df$CH+training_df$JTS+training_df$TL+training_df$LD+training_df$MANNNING+training_df$TWI+training_df$JT+training_df$NDBI+training_df$NDVI+training_df$B5+training_df$NDWI+training_df$B1+training_df$B2+training_df$B3+training_df$B4+training_df$B6+training_df$B7+training_df$SAVI+training_df$MSAVI)
summary(Model)
library(car)
vif(Model)
vif_values <- vif(Model)
vif_data <- data.frame(variable = names(vif_values), vif = vif_values)
ggplot(vif_data, aes(x = variable, y = vif)) +
  geom_bar(stat = "identity", fill = "Darkblue", width = 0.5) +
  geom_text(aes(label = round(vif, 2)), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Variance Inflation Factor (VIF) Plot",
       x = "Variables",
       y = "VIF Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("B1","B2","B3","B4","B5","B6","B7", "Curah Hujan", "Ëlevasi", "Jenis Tanah", "Jarak Terhadap Sungai", "Kerapatan Sungai", "Manning's RC","MSAVI", "NDBI", "NDVI","NDWI","SAVI","SLOPE", "Tutupan Lahan", "TWI"))