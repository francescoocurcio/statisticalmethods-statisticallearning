library(glmnet)

file_path <- "/Users/francesco/programmi_R/Progetto Finale MS-SL/Datasets/gt_2011.csv"

dati <- read.csv(file_path, header = TRUE, dec = ".", sep = ",")
set.seed(100)

head(dati)

summary(dati)

AT_STD <- scale(dati$AT)
AP_STD <- scale(dati$AP)
AH <- dati$AH #Non c'è bisogno di standardizzare poiché è un valore percentuale
AFDP_STD <- scale(dati$AFDP)
GTEP_STD <- scale(dati$GTEP)
TIT_STD <- scale(dati$TIT)
TAT_STD <- scale(dati$TAT)
TEY<- dati$TEY
CDP_STD <- scale(dati$CDP)
CO_STD <- scale(dati$CO)
NOX_STD <- scale(dati$NOX)

matriceDati_STD <- cbind(AT_STD,AP_STD,AH,AFDP_STD,GTEP_STD,TIT_STD,TAT_STD,TEY,CDP_STD,CO_STD,NOX_STD)
colnames(matriceDati_STD) <- c("AT_STD","AP_STD","AH","AFDP_STD","GTEP_STD","TIT_STD","TAT_STD","TEY","CDP_STD","CO_STD","NOX_STD")

dati_STD <- as.data.frame(matriceDati_STD)

head(dati_STD)

#Stima del primo modello di regressione lineare
modello_lineare <- lm(TEY~AT_STD+AP_STD+AH+AFDP_STD+GTEP_STD+TIT_STD+TAT_STD+CDP_STD+CO_STD+NOX_STD, data = dati_STD)
summary(modello_lineare) #N.B: è stato ottenuto lo stesso modello di stima nella fase di inferenza

#REGOLARIZZAZIONE

X_Regressori <- as.matrix(dati_STD[,-8])
Y_Tey <- dati_STD[,8]
nRighe <- nrow(dati_STD)

valoriLambda <- 10^seq(8,-4,length = 100)

calcola_CV_MSE_Modulare <- function(X,Y,k,a){
  
  lambdaValues <- 10^seq(8,-4,length = 100)
  
  modelliRidge_CV <- cv.glmnet(X,Y,lambda = lambdaValues,alpha = a, nfolds = k)
  
  if (a == 0){
    plot(modelliRidge_CV)
    titolo <- sprintf("Ridge Regression: K-Fold (K=%d). STD", k)
    title(main = titolo, line = 2.5)
  }
  if (a == 1){
    plot(modelliRidge_CV)
    titolo <- sprintf("Lasso Regression: K-Fold (K=%d). STD", k)
    title(main = titolo, line = 2.5)
  }
  if (a > 0 && a < 1){
    plot(modelliRidge_CV)
    titolo <- sprintf("Elastic Net (alpha = %g): K-Fold (K=%g). STD",a,k)
    title(main = titolo, line = 2.5)
  }
  
  
  lmin <- modelliRidge_CV$lambda.min
  
  modelloRidge_CV_LMIN <- glmnet(X,Y,lambda = lmin, alpha = a, standardize = FALSE)
  
  coef_modelloRidge_CV_LMIN <- coef(modelloRidge_CV_LMIN)[,1]
  
  MSE_min <- modelliRidge_CV$cvm[modelliRidge_CV$lambda == modelliRidge_CV$lambda.min]
  
  risultato <- list(modello = modelloRidge_CV_LMIN, mse = MSE_min, lam = lmin, coefficenti = coef_modelloRidge_CV_LMIN)
  
  return(risultato)
  
}

#----RIDGE REGRESSION----

all_modelli_ridge <- glmnet(X_Regressori,Y_Tey,lambda = valoriLambda, alpha = 0, standardize = FALSE)
coef(all_modelli_ridge)

plot(all_modelli_ridge,xvar = "lambda",label = TRUE)
title(main = "Ridge Regression. STD", line = 2.5)

#----CV_RIDGE REGRESSION (K-FOLD)----

all_modelli_ridge_CVK10 <- cv.glmnet(X_Regressori,Y_Tey,lambda = valoriLambda, alpha = 0) # K=10
plot(all_modelli_ridge_CVK10)
title(main = "Ridge Regression: K-Fold (K=10)", line = 2.5)

#Lambda min
lmin_K10 <- all_modelli_ridge_CVK10$lambda.min
lmin_K10

#Costruisco il modello di previsione in funzione del lambda min
modello_ridge_lminK10 <- glmnet(X_Regressori,Y_Tey,lambda = lmin_K10,alpha = 0,standardize = FALSE)

#Parametri/Coefficenti di regressione del modello
coef_modello_ridge_lminK10 <- coef(modello_ridge_lminK10)[,1]
coef_modello_ridge_lminK10

#MSE_min del modello
mseMin_lminK10 <- all_modelli_ridge_CVK10$cvm[all_modelli_ridge_CVK10$lambda == all_modelli_ridge_CVK10$lambda.min]
mseMin_lminK10

#--RIDGE REGRESSION
# K=10
listaRes_RIDGE_K10 <- calcola_CV_MSE_Modulare(X_Regressori,Y_Tey,10,0)
mse_RIDGE_K10 <- listaRes_RIDGE_K10$mse
lmin_RIDGE_K10 <- listaRes_RIDGE_K10$lam
coefs_RIDGE_K10 <- listaRes_RIDGE_K10$coefficenti

# K=5
listaRes_RIDGE_K5 <- calcola_CV_MSE_Modulare(X_Regressori,Y_Tey,5,0)
mse_RIDGE_K5 <- listaRes_RIDGE_K5$mse
lmin_RIDGE_K5 <- listaRes_RIDGE_K5$lam
coefs_RIDGE_K5 <- listaRes_RIDGE_K5$coefficenti

#--LASSO REGRESSION
#Comando principale x REGOLARIZZAZIONE (LASSO, alpha = 1)
all_modelli_lasso = glmnet(X_Regressori,Y_Tey,lambda = valoriLambda,alpha = 1,standardize = FALSE)
#Plot dei coefficenti di regressione
plot(all_modelli_lasso, xvar = "lambda", label = TRUE)
title(main = "Lasso. STD", line = 2.5)

# K=10
listaRes_LASSO_K10 <- calcola_CV_MSE_Modulare(X_Regressori,Y_Tey,10,1)
mse_LASSO_K10 <- listaRes_LASSO_K10$mse
mse_LASSO_K10
lmin_LASSO_K10 <- listaRes_LASSO_K10$lam
lmin_LASSO_K10
coefs_LASSO_K10 <- listaRes_LASSO_K10$coefficenti
coefs_LASSO_K10

# K=5
listaRes_LASSO_K5 <- calcola_CV_MSE_Modulare(X_Regressori,Y_Tey,5,1)
mse_LASSO_K5 <- listaRes_LASSO_K5$mse
mse_LASSO_K5
lmin_LASSO_K5 <- listaRes_LASSO_K5$lam
lmin_LASSO_K5
coefs_LASSO_K5 <- listaRes_LASSO_K5$coefficenti
coefs_LASSO_K5

#--ELASTIC NET (a = 0.2)
all_modelli_elasticNet_02 = glmnet(X_Regressori,Y_Tey,lambda = valoriLambda,alpha = 0.2,standardize = FALSE)
#Plot dei coefficenti di regressione
plot(all_modelli_elasticNet_02, xvar = "lambda", label = TRUE)
title(main = "Elastic-Net (alpha = 0.2). STD", line = 2.5)


# K=10
listaRes_EN_A02_K10 <- calcola_CV_MSE_Modulare(X_Regressori,Y_Tey,10,0.2)
mse_EN_A02_K10 <- listaRes_EN_A02_K10$mse
mse_EN_A02_K10
lmin_EN_A02_K10 <- listaRes_EN_A02_K10$lam
lmin_EN_A02_K10
coefs_EN_A02_K10 <- listaRes_EN_A02_K10$coefficenti
coefs_EN_A02_K10

# K=5
listaRes_EN_A02_K5 <- calcola_CV_MSE_Modulare(X_Regressori,Y_Tey,5,0.2)
mse_EN_A02_K5 <- listaRes_EN_A02_K5$mse
mse_EN_A02_K5
lmin_EN_A02_K5 <- listaRes_EN_A02_K5$lam
lmin_EN_A02_K5
coefs_EN_A02_K5 <- listaRes_EN_A02_K5$coefficenti
coefs_EN_A02_K5

#--ELASTIC NET (a = 0.4)
all_modelli_elasticNet_04 = glmnet(X_Regressori,Y_Tey,lambda = valoriLambda,alpha = 0.4,standardize = FALSE)
#Plot dei coefficenti di regressione
plot(all_modelli_elasticNet_04, xvar = "lambda", label = TRUE)
title(main = "Elastic-Net (alpha = 0.4). STD", line = 2.5)

# K=10
listaRes_EN_A04_K10 <- calcola_CV_MSE_Modulare(X_Regressori,Y_Tey,10,0.4)
mse_EN_A04_K10 <- listaRes_EN_A04_K10$mse
mse_EN_A04_K10
lmin_EN_A04_K10 <- listaRes_EN_A04_K10$lam
lmin_EN_A04_K10
coefs_EN_A04_K10 <- listaRes_EN_A04_K10$coefficenti
coefs_EN_A04_K10

# K=5
listaRes_EN_A04_K5 <- calcola_CV_MSE_Modulare(X_Regressori,Y_Tey,5,0.4)
mse_EN_A04_K5 <- listaRes_EN_A04_K5$mse
mse_EN_A04_K5
lmin_EN_A04_K5 <- listaRes_EN_A04_K5$lam
lmin_EN_A04_K5
coefs_EN_A04_K5 <- listaRes_EN_A04_K5$coefficenti
coefs_EN_A04_K5

#--ELASTIC NET (a = 0.6)
all_modelli_elasticNet_06 = glmnet(X_Regressori,Y_Tey,lambda = valoriLambda,alpha = 0.6,standardize = FALSE)
#Plot dei coefficenti di regressione
plot(all_modelli_elasticNet_06, xvar = "lambda", label = TRUE)
title(main = "Elastic-Net (alpha = 0.6). STD", line = 2.5)

# K=10
listaRes_EN_A06_K10 <- calcola_CV_MSE_Modulare(X_Regressori,Y_Tey,10,0.6)
mse_EN_A06_K10 <- listaRes_EN_A06_K10$mse
mse_EN_A06_K10
lmin_EN_A06_K10 <- listaRes_EN_A06_K10$lam
lmin_EN_A06_K10
coefs_EN_A06_K10 <- listaRes_EN_A06_K10$coefficenti
coefs_EN_A06_K10

# K=5
listaRes_EN_A06_K5 <- calcola_CV_MSE_Modulare(X_Regressori,Y_Tey,5,0.6)
mse_EN_A06_K5 <- listaRes_EN_A06_K5$mse
mse_EN_A06_K5
lmin_EN_A06_K5 <- listaRes_EN_A06_K5$lam
lmin_EN_A06_K5
coefs_EN_A06_K5 <- listaRes_EN_A06_K5$coefficenti
coefs_EN_A06_K5

#--ELASTIC NET (a = 0.8)
all_modelli_elasticNet_08 = glmnet(X_Regressori,Y_Tey,lambda = valoriLambda,alpha = 0.8,standardize = FALSE)
#Plot dei coefficenti di regressione
plot(all_modelli_elasticNet_08, xvar = "lambda", label = TRUE)
title(main = "Elastic-Net (alpha = 0.8). STD", line = 2.5)

# K=10
listaRes_EN_A08_K10 <- calcola_CV_MSE_Modulare(X_Regressori,Y_Tey,10,0.8)
mse_EN_A08_K10 <- listaRes_EN_A08_K10$mse
mse_EN_A08_K10
lmin_EN_A08_K10 <- listaRes_EN_A08_K10$lam
lmin_EN_A08_K10
coefs_EN_A08_K10 <- listaRes_EN_A08_K10$coefficenti
coefs_EN_A08_K10

# K=5
listaRes_EN_A08_K5 <- calcola_CV_MSE_Modulare(X_Regressori,Y_Tey,5,0.8)
mse_EN_A08_K5 <- listaRes_EN_A08_K5$mse
mse_EN_A08_K5
lmin_EN_A08_K5 <- listaRes_EN_A08_K5$lam
lmin_EN_A08_K5
coefs_EN_A08_K5 <- listaRes_EN_A08_K5$coefficenti
coefs_EN_A08_K5

#Ricalcolare il minimoMSE tra tutti quelli dei modelli

vettore_Mse <- cbind(mse_RIDGE_K10,mse_RIDGE_K5,
                     mse_LASSO_K10,mse_LASSO_K5,
                     mse_EN_A02_K10,mse_EN_A02_K5,
                     mse_EN_A04_K10,mse_EN_A04_K5,
                     mse_EN_A06_K10,mse_EN_A06_K5,
                     mse_EN_A08_K10,mse_EN_A08_K5)
vettore_Mse

mseMin <- min(vettore_Mse)
mseMin #MSE min è quello ottenuto con ELASTIC-NET alpha = 0.6 e CV K-FOLD (K=5)

mseMax <- max(vettore_Mse)
mseMax #MSE max è quello ottenuto con RIDGE REGR. K-FOLD=10

#----SEZIONE PREVISIVA CON MODELLO STIMATO 

file_path2 <- "/Users/francesco/programmi_R/Progetto Finale MS-SL/Datasets/gt_2012.csv"

dati_test <- read.csv(file_path2, header = TRUE, dec = ".", sep = ",")

head(dati_test)

#Bisogna standardizzare i dati in modo da eliminare l'unità di misura e rendere i dati tra loro comparabili

AT_STD_test <- scale(dati_test$AT)
AP_STD_test <- scale(dati_test$AP)
AH_test <- dati_test$AH #Non c'è bisogno di standardizzare poiché è un valore percentuale
AFDP_STD_test <- scale(dati_test$AFDP)
GTEP_STD_test <- scale(dati_test$GTEP)
TIT_STD_test <- scale(dati_test$TIT)
TAT_STD_test <- scale(dati_test$TAT)
TEY_test <- dati_test$TEY
CDP_STD_test <- scale(dati_test$CDP)
CO_STD_test <- scale(dati_test$CO)
NOX_STD_test <- scale(dati_test$NOX)

dati_STD_test <- cbind(AT_STD_test,AP_STD_test,AH_test,AFDP_STD_test,GTEP_STD_test,TIT_STD_test,TAT_STD_test,CDP_STD_test,CO_STD_test,NOX_STD_test)
colnames(dati_STD_test) <- c("AT_STD_test","AP_STD_test","AH_test","AFDP_STD_test","GTEP_STD_test","TIT_STD_test","TAT_STD_test","CDP_STD_test","CO_STD_test","NOX_STD_test")
head(dati_STD_test)

#Previsioni effettuate con il modello ELASTIC-NET alpha = 0.6 e CV K-FOLD (K=5) (migliore assoluto)
modello_EN_A06_K5 <- listaRes_EN_A06_K5$modello
previsioni_mod_EN_A06_K5 <- predict(modello_EN_A06_K5, newx = dati_STD_test)
confronto_osservazioni_previsioni1 <- cbind(TEY_test,previsioni_mod_EN_A06_K5)
head(confronto_osservazioni_previsioni)
residui_mod_EN_A06_K5 <- TEY_test - previsioni_mod_EN_A06_K5
MSE_previsioni_mod_EN_A06_K5 <- mean(residui_mod_EN_A06_K5^2)
MSE_previsioni_mod_EN_A06_K5


#Previsioni effettuate con il modello RIDGE_K10 (peggiore assoluto)
modello_RIDGE_K10 <- listaRes_RIDGE_K10$modello
previsioni_mod_RIDGE_K10 <- predict(modello_RIDGE_K10,newx = dati_STD_test)
confronto_osservazioni_previsioni2 <- cbind(TEY_test,previsioni_mod_RIDGE_K10)
head(confronto_osservazioni_previsioni2)
residui_mod_RIDGE_K10 <- TEY_test - previsioni_mod_RIDGE_K10
MSE_previsioni_mod_RIDGE_K10 <- mean(residui_mod_RIDGE_K10^2)
MSE_previsioni_mod_RIDGE_K10


#Previsioni effettuate con il modello EN_A04_K10 (uno a caso)
modello_EN_A02_K10 <- listaRes_EN_A02_K10$modello
previsioni_mod_EN_A02_K10 <- predict(modello_EN_A02_K10,newx = dati_STD_test)
residui_mod_EN_A02_K10 <- TEY_test - previsioni_mod_EN_A02_K10
MSE_previsioni_mod_EN_A02_K10 <- mean(residui_mod_EN_A02_K10^2)
MSE_previsioni_mod_EN_A02_K10
