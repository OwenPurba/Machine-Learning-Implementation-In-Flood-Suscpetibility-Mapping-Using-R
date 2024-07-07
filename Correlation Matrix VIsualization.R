# Matriks Korelasi
datakor <- training_df[,c(-16,-19)]
corr <- cor(datakor, method = "pearson")
rounded_corr <- round(corr, 3)  # Menggunakan metode Pearson
fontsize <- 10
plot <- ggcorrplot(rounded_corr, hc.order = TRUE,
                   outline.color = "black",
                   lab=TRUE,
                   tl.cex = fontsize,
                   lab_size = 3) +
  ggtitle("CORRELATION MATRIX") +  # Mengubah judul plot
  theme(plot.title = element_text(size = 15, hjust = 0.5))
plot