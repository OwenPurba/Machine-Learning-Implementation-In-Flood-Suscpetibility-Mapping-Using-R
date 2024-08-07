#Density
par(mfrow=c(2,3))
hist(Parameter_N1$ELEVASI, freq=FALSE, col="yellow", main ="Densitas Raster Elevasi", xlab="Elevasi")
polygon(density(Parameter_N1$ELEVASI), border = "steelblue", lwd=2)
hist(Parameter_N1$SLOPE, freq=FALSE, col="yellow", main ="Densitas Raster Slope", xlab="Slope")
polygon(density(Parameter_N1$SLOPE), border = "steelblue", lwd=2)
hist(Parameter_N1$CH, freq=FALSE, col="yellow", main ="Densitas Raster Curah Hujan", xlab="Curah Hujan")
polygon(density(Parameter_N1$CH), border = "steelblue", lwd=2)
#hist(Parameter_N1$CURVATURE, freq=FALSE, col="yellow", main ="Densitas Raster Curvature", xlab="Curvature")
#polygon(density(Parameter_N1$CURVATURE), border = "steelblue", lwd=2)
hist(Parameter_N1$JTS, freq=FALSE, col="yellow", main ="Densitas Jarak Terhadap Sungai", xlab="Jarak Terhadap Sungai")
polygon(density(Parameter_N1$JTS), border = "steelblue", lwd=2)
hist(Parameter_N1$TL, freq=FALSE, col="yellow", main ="Densitas Tutupan Lahan", xlab="Tutupan Lahan")
polygon(density(Parameter_N1$TL), border = "steelblue", lwd=2)
hist(Parameter_N1$LD, freq=FALSE, col="yellow", main ="Densitas Raster Drainage Density", xlab="Drainage Density")
polygon(density(Parameter_N1$LD), border = "steelblue", lwd=2)
hist(Parameter_N1$MANNNING, freq=FALSE, col="yellow", main ="Densitas Raster Manning Roughness Coefficient", xlab="Manning Roughness Coefficient")
polygon(density(Parameter_N1$MANNNING), border = "steelblue", lwd=2)
hist(Parameter_N1$TWI, freq=FALSE, col="yellow", main ="Densitas Raster TWI", xlab="TWI")
polygon(density(Parameter_N1$TWI), border = "steelblue", lwd=2)
hist(Parameter_N1$JT, freq=FALSE, col="yellow", main ="Densitas Raster Jenis Tanah", xlab="JT")
polygon(density(Parameter_N1$JT), border = "steelblue", lwd=2)
#hist(Parameter_N1$NDVI, freq=FALSE, col="yellow", main ="Densitas Raster NDVI", xlab="NDVI")
#polygon(density(Parameter_N1$NDVI), border = "steelblue", lwd=2)
hist(Parameter_N1$NDBI, freq=FALSE, col="yellow", main ="Densitas Raster NDBI", xlab="NDBI")
polygon(density(Parameter_N1$NDBI), border = "steelblue", lwd=2)
hist(Parameter_N1$B5, freq=FALSE, col="yellow", main ="Densitas Raster Near InfraRed", xlab="Near InfraRed")
polygon(density(Parameter_N1$B5), border = "steelblue", lwd=2)