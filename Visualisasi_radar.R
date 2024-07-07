# Memuat paket fmsb
library(fmsb)

# Contoh data untuk 10 model machine learning dengan 5 metrik
data <- data.frame(
  SVM_50  = c(0.9796, 0.9388, 0.9592, 0.9746, 0.9592),
  SVM_60 = c(0.9744, 0.9744, 0.9744, 0.9947, 0.9744),
  SVM_70 = c(1, 0.9655, 0.9828, 1, 0.9828),
  RF_70 = c(0.8621, 1, 0.9310, 0.9893, 0.9310),
  XGB_50 = c(0.9388, 0.9388, 0.9388, 0.96, 0.9388),
  XGB_60 = c(1, 1, 1, 1, 1),
  XGB_70 = c(0.9655, 0.9310, 0.9483, 0.9727, 0.9483),
  KNN_50 = c(0.9184, 0.9184, 0.9184, 0.9431, 0.9184),
  KNN_60 = c(0.8974, 0.9231, 0.9103, 0.9474, 0.9103),
  KNN_70 = c(0.9655, 0.8621, 0.9138, 0.9746, 0.9138)
)

# Menambahkan nama baris untuk metrik
rownames(data) <- c("Specificity", "Sensitivity", "Balanced Accuracy", "AUC", "Accuracy")

# Menambahkan baris max (dengan nilai 1) dan min (dengan nilai 0.85) untuk skala radar chart
data <- rbind(max = rep(1, ncol(data)), min = rep(0.85, ncol(data)), data)

# Warna khusus untuk masing-masing metrik
custom_colors <- c("steelblue", "red", "orange", "black", "darkgreen")

# Plot radar chart tanpa warna poligon
radarchart(data, 
           axistype = 1, 
           pcol = custom_colors, 
           plwd = 2, 
           plty = 1,
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "grey", 
           caxislabels = seq(0.86, 1, 0.035), 
           cglwd = 1,
           vlcex = 1,
           pfcol = NA)  # Menonaktifkan warna poligon

# Menambahkan label sumbu dengan font yang lebih bold
par(font.axis = 5) # Menyetel font axis menjadi bold
axis(1, at = seq(0.86, 1, 0.035), labels = seq(0.86, 1, 0.035), cex.axis = 1.2)

# Menambahkan legenda untuk metrik
legend("topright", legend = rownames(data)[-c(1,2)], col = custom_colors, lty = 1, lwd = 2, bty = "n")
