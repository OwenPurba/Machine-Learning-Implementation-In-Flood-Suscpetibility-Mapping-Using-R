# Model SVM & RF
# Instalasi Library
library(sp)
library(rgdal)
library(raster)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(cowplot)
library(e1071)
library(scales)
library(kernlab)
library(ggplot2)
library(lattice)
library(caret)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(ggcorrplot)

# Atur environment
setwd("C:/Users/USER PC/Documents/TA/PASURUAN ML/0FIX/Resampling")

# Persiapan Data
# Untuk dataset tarining dan testing banjir dan non-banjir
floods <- read.csv("./Flood.csv", header = T, sep = ",")
nonfloods <- read.csv("./NonFlood_Revised2.csv", header = T, sep = ",")
floods_x <- floods$X
floods_y <- floods$Y
nonfloods_x <- nonfloods$X
nonfloods_y <- nonfloods$Y

training_data <- data.frame(
  x = c(floods_x, nonfloods_x),
  y = c(floods_y, nonfloods_y),
  label = c(rep(1, length(floods_x)), rep(0, length(nonfloods_x)))
)

# Pendefinisian Raster Input
elv1 <- (raster("./HEIGHT_Resampling3.tif"))
slp1 <- (raster("./SLOPE_Resampling3.tif"))
CH <- (raster("./CH_Resample2rbf.tif"))
Cvtr <- (raster("./CURVATURE_Resample2.tif"))
JTS <- (raster("./JTS_Resample2.tif"))
TL <- (raster("./landcoverage_Resample5.tif"))
LD <- (raster("./linedensity_Resample2.tif"))
manning <- (raster("./manning_Resample6.tif"))
ndbi <- (raster("./NDBIfix1.tif"))
ndvi <- (raster("./NDVIfix1.tif"))
savi <- (raster("./SAVIfix.tif"))
msavi <- (raster("./MSAVIfix.tif"))
gndvi <- (raster("./GNDVIfix.tif"))
evi <- (raster("./EVIfix.tif"))
ndwi <- (raster("./NDWIfix1.tif"))
ndmi <- (raster("./NDMIfixed.tif"))
b1 <- (raster("./toal1_B1.tif"))
b2 <- (raster("./toal1_B2.tif"))
b3 <- (raster("./toal1_B3.tif"))
b4 <- (raster("./toal1_B4.tif"))
b5 <- (raster("./toal1_B5.tif"))
b6 <- (raster("./toal1_B6.tif"))
b7 <- (raster("./toal1_B7.tif"))
TWI <- (raster("./TWI_Resampling3.tif"))
JT<- (raster("./JT4.tif"))

#Resampling
Elevasi <- resample(elv1,TL, resample='bilinear')
Slope <- resample(slp1,TL, resample='bilinear')
CH <- resample(CH,TL, resample='bilinear')
Cvtr <- resample(Cvtr,TL, resample='bilinear')
JTS <- resample(JTS,TL, resample='bilinear')
TL <- resample(TL, TL, resample='nearest')
LD <- resample(LD,TL, resample='bilinear')
manning <- resample(manning,TL, resample='nearest')
TWI <- resample(TWI,TL, resample='bilinear')
ndbi <- resample(ndbi,TL, resample='nearest')
ndvi<- resample(ndvi,TL, resample='nearest')
savi <- resample(savi,TL, resample='nearest')
msavi <- resample(msavi,TL, resample='nearest')
gndvi <- resample(gndvi,TL, resample='nearest')
evi <- resample(evi,TL, resample='nearest')
ndwi <- resample(ndwi,TL, resample='nearest')
ndmi <- resample(ndmi,TL, resample='nearest')
b1 <- resample(b1,TL, resample='nearest')
b2 <- resample(b2,TL, resample='nearest')
b3 <- resample(b3,TL, resample='nearest')
b4 <- resample(b4,TL, resample='nearest')
b5 <- resample(b5,TL, resample='nearest')
b6 <- resample(b6,TL, resample='nearest')
b7 <- resample(b7,TL, resample='nearest')
JT <- resample(JT,TL, resample='nearest')
stackinput <- stack(Elevasi, Slope, CH, Cvtr, JTS, TL, LD, manning,TWI,JT,ndbi,ndvi,b5,savi,msavi,gndvi,evi,ndwi,ndmi,b1,b2,b3,b4,b6,b7)

# Ekstrak input raster (23 parameter) dan titik training
# Ekstrak nilai piksel dari raster
raster_values <- raster::extract(stackinput, training_data[,c("x", "y")], df = TRUE)
# Merge nilai piksel dan label training ke dalam data frame
training_df <- cbind.data.frame(raster_values, label=training_data$label)
training_df <- subset(training_df, select = -1)
colnames(training_df) <- c("ELEVASI","SLOPE","CH","CURVATURE","JTS","TL","LD","MANNNING","TWI","JT","NDBI","NDVI","B5","SAVI","MSAVI","GNDVI","EVI","NDWI","NDMI","B1","B2","B3","B4","B6","B7","label")
# Save dataframe format csv
write.csv(training_df, "C:/Users/USER PC/Documents/TA/PASURUAN ML/0FIX/Resampling/TRAINING_PARAMETERsvm11.csv", row.names = FALSE) ##directory penyimpanan csv nya mau dimana


# Parameter Terseleksi Berdasarkan VIF
training_df <- training_df[,c(-4,-12,-14:-25)] #remove kolom dengan vif>5

# Mengubah data kategorik menjadi numerik
# Tutupan Lahan
TL.r <- cut(training_df$TL, seq(0,5,1), right=FALSE, 
            label=c("veg","air","blt","swh","ksg"))
table(TL.r)
class(TL.r)
TL.r <- factor(TL.r)
flags1 = data.frame(Reduce(cbind,lapply(levels(TL.r), function(x){TL.r==x}*1)))
names(flags1) = levels (TL.r)
training_df <- training_df[,-5] # Hapus Tutupan Lahan & 
training_df1 <- training_df[,c(-6)] # Hapus Manning's RC

# Jenis Tanah
JT.r <- cut(training_df$JT, seq(1,6,1), right=FALSE, 
            label=c("andsl","ltsl","mdtn","ncal","alvl"))
table(JT.r)
class(JT.r)
JT.r <- factor(JT.r)
flags2 = data.frame(Reduce(cbind,lapply(levels(JT.r), function(x){JT.r==x}*1)))
names(flags2) = levels (JT.r)
training_df1 <- training_df1[,-7] #Hapus Jenis Tanah

#Scalling
maxs <- apply(training_df1, 2, max) 
mins <- apply(training_df1, 2, min)
scaled_t <- as.data.frame(scale(training_df1, center = mins, scale = maxs - mins))
scaled_t$label <- ifelse(scaled_t$label == 1, "yes","no")

#Attach kolom pasca scalling
scaled_t <- cbind(scaled_t[, 1:which(colnames(scaled_t) == "TWI")],
                  flags1,
                  flags2,
                  scaled_t[, (which(colnames(scaled_t) == "TWI") + 1):ncol(scaled_t)])
scaled_t <- cbind(scaled_t[, 1:which(colnames(scaled_t) == "LD")],
                  training_df[,c(6)],
                  scaled_t[, (which(colnames(scaled_t) == "LD") + 1):ncol(scaled_t)])

# Penamaan Kolom Dataset Training
colnames(scaled_t) <- c("ELEVASI","SLOPE","CH","JTS","LD","MANNNING","TWI","VEG","AIR","BLT","SWH","KSG","ANDSL","LTSL","MDTN","NCAL","ALVL","NDBI","B5","label")

#Splitting ratio for SVM 50:50
set.seed(1234)
intrain1 <- createDataPartition(y = scaled_t$label, p=0.5, list = FALSE)
training1 <- scaled_t[intrain1,]
testing1 <- scaled_t[-intrain1,]
colnames(training1) <- c("ELEVASI","SLOPE","CH","JTS","LD","MANNNING","TWI","VEG","AIR","BLT","SWH","KSG","ANDSL","LTSL","MDTN","NCAL","ALVL","NDBI","B5","label")
colnames(testing1) <- c("ELEVASI","SLOPE","CH","JTS","LD","MANNNING","TWI","VEG","AIR","BLT","SWH","KSG","ANDSL","LTSL","MDTN","NCAL","ALVL","NDBI","B5","label")
dim(training1)
dim(testing1)
summary(training1)
summary(testing1)

# Model SVM
# Pendefinisian Kontrol
control <- trainControl(method="repeatedcv",
                        number = 8,
                        repeats= 15,
                        classProbs= TRUE)
print(control)
# Mencari nilai optimum dari tuning parameter
sigDist1 <- sigest(label~., data = training1, frac = 1)
# Membuat grid dari 2 tuning parameter, dimulai dari .sigma untuk mencari yang terbaik
svmTuneGrid1 <- data.frame(.sigma = sigDist1[1], .C = 2^(-5:20))

training1$label = factor(training1$label)
set.seed(897)
fit.svm1 <- train(label~.,
                  data = training1,
                  method= "svmRadial",
                  preProc = c("center", "scale"),
                  tuneGrid = svmTuneGrid1,
                  trControl = control,
                  importance = TRUE);
print(fit.svm1)
plot(fit.svm1)
fit.svm1$finalModel
fit.svm1$results
svm1_varImp <- caret::varImp(fit.svm1)
svm1_varImp
ggplot(svm1_varImp)

# Prediksi dan confusion matrix model SVM
pred_svm1 <- predict(fit.svm1, newdata = testing1)
cm_svm1 <- caret::confusionMatrix(pred_svm1, as.factor(testing1$label), positive = "yes")
cm_svm1
cm_svm1$overall[1]


# Splitting ratio SVM 60:40
intrain2 <- createDataPartition(y = scaled_t$label, p=0.6, list = FALSE)
training2 <- scaled_t[intrain2,]
testing2 <- scaled_t[-intrain2,]
colnames(training2) <- c("ELEVASI","SLOPE","CH","JTS","LD","MANNNING","TWI","VEG","AIR","BLT","SWH","KSG","ANDSL","LTSL","MDTN","NCAL","ALVL","NDBI","B5","label")
colnames(testing2) <- c("ELEVASI","SLOPE","CH","JTS","LD","MANNNING","TWI","VEG","AIR","BLT","SWH","KSG","ANDSL","LTSL","MDTN","NCAL","ALVL","NDBI","B5","label")
dim(training2)
dim(testing2)
summary(training2)
summary(testing2)

# Model SVM
# Pendefinisian Kontrol
control1 <- trainControl(method="repeatedcv",
                         number = 8,
                         repeats= 15,
                         classProbs= TRUE)
# Mencari nilai optimum dari tuning parameter
sigDist2 <- sigest(label~., data = training2, frac = 1)
# Membuat grid dari 2 tuning parameter, dimulai dari .sigma untuk mencari yang terbaik
svmTuneGrid2 <- data.frame(.sigma = sigDist2[1], .C = 2^(-2:20))

training2$label = factor(training2$label)
set.seed(897)
fit.svm2 <- train(label~.,
                  data = training2,
                  method= "svmRadial",
                  preProc = c("center", "scale"),
                  tuneGrid = svmTuneGrid2,
                  trControl = control,
                  importance = TRUE);
print(fit.svm2)
plot(fit.svm2)
fit.svm2$finalModel
fit.svm2$results
svm1_varImp2 <- caret::varImp(fit.svm2)
svm1_varImp2
ggplot(svm1_varImp2)

# Prediksi dan confusion matrix model SVM
pred_svm2 <- predict(fit.svm2, newdata = testing2)
cm_svm2 <- caret::confusionMatrix(pred_svm2, as.factor(testing2$label), positive = "yes")
cm_svm2

# Splitting ratio for SVM 70:30
set.seed(1234)
intrain3 <- createDataPartition(y = scaled_t$label, p=0.7, list = FALSE)
training3 <- scaled_t[intrain3,]
testing3 <- scaled_t[-intrain3,]
colnames(training3) <- c("ELEVASI","SLOPE","CH","JTS","LD","MANNNING","TWI","VEG","AIR","BLT","SWH","KSG","ANDSL","LTSL","MDTN","NCAL","ALVL","NDBI","B5","label")
colnames(testing3) <- c("ELEVASI","SLOPE","CH","JTS","LD","MANNNING","TWI","VEG","AIR","BLT","SWH","KSG","ANDSL","LTSL","MDTN","NCAL","ALVL","NDBI","B5","label")
dim(training3)
dim(testing3)
summary(training3)
summary(testing3)

# Model SVM
# Pendefinisian Kontrol
control2 <- trainControl(method="repeatedcv",
                        number = 8,
                        repeats= 15,
                        classProbs= TRUE)
print(control2)
# Mencari nilai optimum dari tuning parameter
sigDist3 <- sigest(label~., data = training3, frac = 1)
# Membuat grid dari 2 tuning parameter, dimulai dari .sigma untuk mencari yang terbaik
svmTuneGrid3 <- data.frame(.sigma = sigDist3[1], .C = 2^(-5:20))

training3$label = factor(training3$label)
set.seed(897)
fit.svm3 <- train(label~.,
                  data = training3,
                  method= "svmRadial",
                  preProc = c("center", "scale"),
                  tuneGrid = svmTuneGrid3,
                  trControl = control2,
                  importance = TRUE);
print(fit.svm3)
plot(fit.svm3)
fit.svm3$finalModel
fit.svm3$results
svm1_varImp3 <- caret::varImp(fit.svm3)
svm1_varImp3
ggplot(svm1_varImp3)

# Prediksi dan confusion matrix model SVM
pred_svm3 <- predict(fit.svm3, newdata = testing3)
cm_svm3 <- caret::confusionMatrix(pred_svm3, as.factor(testing3$label), positive = "yes")
cm_svm3
cm_svm3$overall[1]

#Model Random Forest
#Splitting Rasio Training dan Testing 
inTraining<- createDataPartition(scaled_t$label, p = .7, list = FALSE) 
TR<- scaled_t[ inTraining,] 
TS  <- scaled_t[-inTraining,]
summary(TR)
summary(TS)


# Define the control
trControl <- trainControl(method='repeatedcv', 
                          repeats=8,
                          number = 15,
                          search = "grid")

#Let's try the build the model with the default values.

set.seed(1234)
# Run the model
rf_defaultN <- train(label~., 
                     data=TR,
                     method = "rf",
                     metric = "Accuracy",
                     trControl = trControl)
# Print the results
print(rf_defaultN)     
plot(rf_defaultN)
rf_defaultN$finalModel         # Results mtry=12 Number of trees: 500
rf_defaultN$results 


# Step 2) Search best mtry

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 22))
rf_mtry <- train(label~., 
                 data=TR,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 3,
                 ntree = 650)
print(rf_mtry)
rf_mtry$bestTune$mtry
#You can store it and use it when you need to tune the other parameters.
max(rf_mtry$results$Accuracy)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry

# Step 3) Search the best maxnodes SKIP

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 30)) {
  set.seed(1234)
  rf_maxnode <- train(label~., 
                      data=TR,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 3,
                      maxnodes = maxnodes,
                      ntree = 650)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

fit_rf_final <- train(label~., 
                      data=TR,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE
)

fit_rf_final
print(fit_rf_final)
Var<-caret::varImp(fit_rf_final)
Var
ggplot2::ggplot(caret::varImp(fit_rf_final))
plot(caret::varImp(fit_rf_final), main="RF tuned model")



# Step 5) Evaluate the model
p1_final<-predict(fit_rf_final, TS[,c(-22)], type = "raw")
confusionMatrix(p1_final, as.factor(TS$label), positive = "yes")

# AUC model SVM
library(pROC)
#50:50
predicted1 <- predict(fit.svm1, testing1, type="prob", probability = TRUE)
roc1 <- roc(testing1$label, predicted1[,2])
auc1 <- auc(roc1)
print(roc1)
#60:40
predicted2 <- predict(fit.svm2, testing2, type="prob", probability = TRUE)
roc2 <- roc(testing2$label, predicted2[,2])
auc2 <- auc(roc2)
print(roc2)
#70:30
predicted3 <- predict(fit.svm3, testing3, type="prob", probability = TRUE)
roc3 <- roc(testing3$label, predicted3[,2])
auc3 <- auc(roc3)
print(roc3)
# RF 70:30
predictions1 <- as.data.frame(predict(fit_rf_final, TS, type = "prob"))
predictions1$predict <- names(predictions1)[1:2][apply(predictions1[,1:2], 1, which.max)]
predictions1$observed <- as.factor(TS$label)
head(predictions1)
roc.yes <- roc(ifelse(predictions1$observed=="yes","yes-yes","yes"), as.numeric(predictions1$yes))
roc.no <- roc(ifelse(predictions1$observed=="no","no-no", "no"), as.numeric(predictions1$no))
auc4 <- auc(roc.yes)
print(auc4)
 
# Plot SVM ROC-AUC Curve
par(mfrow=c(1,1))
plot(roc1, col ="green", main = "Kurva ROC-AUC Pemodelan SVM")
plot(roc2, col ="blue", add = TRUE)
plot(roc3, col ="orange", add = TRUE)

legend("bottomright",
       c("SVM 50:50 = 0.9746", "SVM 60:40 = 0.9947", "SVM 70:30 = 1.0000"),
       fill=c("green", "blue", "orange"),
       box.lwd=0,
       title="AUC")
# Plot RF ROC-AUC Curve
plot(roc.yes,print.auc=FALSE, col = "darkgreen", main="Kurva ROC-AUC Pemodelan RF", xlim=c(0.44,0.2))
lines(roc.yes, col = "darkgreen")
legend("bottomright",
       c("RF 70:30 = 0.9893"),
       fill="darkgreen",
       box.lwd=0,
       title="AUC")
#Konversi Raster ke dataframe
#Convert raster to dataframe with Long-Lat
Parameter_N <- as.data.frame(stackinput, xy = TRUE, na.rm = TRUE)
saveRDS(Parameter_N,file="Parameter_Ncbsemuatanah.Rda")
head(stackinput,1)
colnames(Parameter_N) <- c("ELEVASI","SLOPE","CH","CURVATURE","JTS","TL","LD","MANNNING","TWI","JT","NDBI","NDVI","B5","SAVI","MSAVI","GNDVI","EVI","NDWI","NDMI","B1","B2","B3","B4","B6","B7","X","Y")

#VIsualisasi data densitas dan frekuensi
Parameter_N1<-Parameter_N[,c(-4,-12,-14:-25)]
dt<-data.frame(Parameter_N1[1:1642205,1:11])
colnames(dt)<- "series"
head(dt)
tail(dt)

# Parameter Terseleksi Berdasarkan VIF
Parameter_N <- Parameter_N[,c(-4,-12,-14:-25)] 

# %% Mengubah data kategorik menjadi numerik
# Tutupan Lahan
TL1.r <- cut(Parameter_N$TL, seq(0,5,1), right=FALSE,
            label=c("veg","air","blt","swh","ksg"))
table(TL1.r)
class(TL1.r)
TL1.r <- factor(TL1.r)
flags11 = data.frame(Reduce(cbind,lapply(levels(TL1.r), function(x){TL1.r==x}*1)))
names(flags11) = levels (TL1.r)
Parameter_N <- Parameter_N[,-5]

# Jenis Tanah
JT1.r <- cut(Parameter_N$JT, seq(1,6,1), right=FALSE, 
             label=c("andsl","ltsl","mdtn","ncal","alvl"))
table(JT1.r)
class(JT1.r)
JT1.r <- factor(JT1.r)
flags22 = data.frame(Reduce(cbind,lapply(levels(JT1.r), function(x){JT1.r==x}*1)))
names(flags22) = levels (JT1.r)

Parameter_N <- Parameter_N[,-8] # remove JT

# Memproses data kontinu untuk scalling
Parameter_1 <- Parameter_N[,c(-6,-10,-11)] # remove manning, x, y
Parameter1 <-Parameter_1

maxs2 <- apply(Parameter1, 2, max)
mins2 <- apply(Parameter1, 2, min)
scaled_data <- scale(Parameter1, center = mins2, scale = maxs2 - mins2)
scaled_df2 <- as.data.frame(scaled_data)

# Attach kolom pasca saclling
scaled_df2 <- cbind(scaled_df2[, 1:which(colnames(scaled_df2) == "TWI")],
                  flags11,
                  flags22,
                  scaled_df2[, (which(colnames(scaled_df2) == "TWI") + 1):ncol(scaled_df2)])
scaled_df2 <- cbind(scaled_df2[, 1:which(colnames(scaled_df2) == "LD")],
                    Parameter_N[,c(6)],
                    scaled_df2[, (which(colnames(scaled_df2) == "LD") + 1):ncol(scaled_df2)])

scaled_df2 = cbind(scaled_df2, Parameter_N[,c(10,11)]) #x dan y

colnames(scaled_df2) <- c("ELEVASI","SLOPE","CH","JTS","LD","MANNNING","TWI","VEG","AIR","BLT","SWH","KSG","ANDSL","LTSL","MDTN","NCAL","ALVL","NDBI","B5","x","y")
