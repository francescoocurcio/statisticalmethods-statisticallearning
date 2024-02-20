file_path <- "/Users/francesco/programmi_R/Progetto Finale MS-SL/Datasets/gt_2015.csv"

dati <- read.csv(file_path, dec = ".", header = TRUE, sep = ",")

summary(dati)

AT <- dati$AT
AP <- dati$AP
AH <- dati$AH
AFDP <- dati$AFDP
GTEP <- dati$GTEP
TIT <- dati$TIT
TAT <- dati$TAT
TEY <- dati$TEY
CDP <- dati$CDP
CO <- dati$CO
NOX <- dati$NOX

regressori_nonCompleti <- dati[,-8]

X <- as.matrix(cbind(rep(1,7384),regressori_nonCompleti))

head(X)

# AT: temperatura ambientale
# AP: pressione ambientale
# AH: umidità ambintale
# AFDP: differenza di pressione del filtro dell'aria
# GTEP: pressione di scarico della turbina a gas
# TIT: temperatura di ingresso della turbina
# TAT: temperatura di uscita dalla turbina
# TEY: rendimento energetico della turbina
# CDP: pressione di scarico del compressore
# CO: monossido di carbonio
# Nox: ossido d'azoto

#----CHECK MULTICOLLINEARITA

#Matrice di correlazione
cor(dati)

#Dalla matrice di correlazione si evidenzia una forte correlazione tra:
# 1) AFDP/GTEP: 0.84395757
# 2) AFDP/TIT: 0.91512777
# 3) AFDP/TEY: 0.88495380
# 4) AFDP/CDP: 0.92299064
# 5) GTEP/TIT: 0.89285131
# 6) GTEP/TAT: -0.80953841
# 7) GTEP/TEY: 0.93233682
# 8) GTEP/CDP: 0.93814162
# 9) TIT/TEY: 0.95181259
# 10) TIT/CDP: 0.95159003
# 11) TAT/CDP: -0.80121541
# 12) TEY/CDP: 0.99120733

#Determinante della matrice dei regressori
detX <- det(t(X)%*%X)
detX #4.756515e+49

#Calcolo del condition number
autoV <- eigen(t(X)%*%X)
autovalori <- autoV$values

autovaloreMin <- min(autovalori)
autovaloreMax <- max(autovalori)

condition_Number <- sqrt(autovaloreMax/autovaloreMin)
condition_Number #2.13787e-06

#Stima del primo modello di regressione lineare (COMPLETO)
modello_lineare_1 <- lm(TEY~., data = dati)
summary(modello_lineare_1)

#Fattore di accrescimento della varianza
VIF <- vif(modello_lineare_1)
VIF #TIT: 414.476920, CDP: 581.428160

#Tolleranza
TOLLERANCE <- 1/VIF
TOLLERANCE #TIT: 0.002412680, CDP: 0.001719903

#Indice di determinazione (spiego ogni regressore in funzione degli altri per valutare la presenza di correlazione)
INDICIR2 <- (VIF-1)/VIF
INDICIR2 #TIT: 0.9975873, TAT: 0.9853919, CDP: 0.9982801

#Arriviamo ad una contraddizione tra il summary del modello_lineare_1 in cui tutti i regressori sono stat. significativi
#e gli indicatori di possibile multicollinearità. Per la sciiienza si prova ad eliminare questi regressori associati agli indicatori

