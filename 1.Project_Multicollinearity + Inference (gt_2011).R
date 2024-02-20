file_path <- "/Users/francesco/programmi_R/Progetto Finale MS-SL/Datasets/gt_2011.csv"

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

X <- as.matrix(cbind(rep(1,7411),regressori_nonCompleti))

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
# 1) AFDP/GTEP: 0.89063332
# 2) AFDP/TIT: 0.79000368
# 3) AFDP/TEY: 0.9049759
# 4) AFDP/CDP: 0.89953479
# 5) GTEP/TIT: 0.88343819
# 6) GTEP/TAT: -0.80953841
# 7) GTEP/TEY: 0.9775101
# 8) GTEP/CDP: 0.99455598
# 9) TIT/TEY: 0.9052405
# 10) TIT/CDP: 0.89309999
# 11) TAT/CDP: -0.80121541
# 12) TEY/CDP: 0.98844573

#Determinante della matrice dei regressori
detX <- det(t(X)%*%X)

detX #5.110622e+48

#Calcolo del condition number
autoV <- eigen(t(X)%*%X)
autovalori <- autoV$values

autovaloreMin <- min(autovalori)
autovaloreMax <- max(autovalori)

condition_Number <- sqrt(autovaloreMax/autovaloreMin)
condition_Number #2.157977e-06

#Stima del primo modello di regressione lineare (COMPLETO)

modello_lineare_1 <- lm(TEY~., data = dati)
summary(modello_lineare_1)

#Fattore di accrescimento della varianza
VIF <- vif(modello_lineare_1)
VIF #GTEP: 562.630282, TIT: 353.229299, TAT:235.054163, CDP: 314.860238

#Tolleranza
TOLLERANCE <- 1/VIF
TOLLERANCE #GTEP: 0.001777366, TIT: 0.002831022, TAT: 0.004254339, CDP: 0.003176012

#Indice di determinazione (spiego ogni regressore in funzione degli altri per valutare la presenza di correlazione)
INDICIR2 <- (VIF-1)/VIF
INDICIR2 #GTEP: 0.9982226, TIT: 0.9971690, TAT: 0.9957457, CDP: 0.9968240

#Arriviamo ad una contraddizione tra il summary del modello_lineare_1 in cui tutti i regressori sono stat. significativi
#e gli indicatori di possibile multicollinearità. Per la sciiienza si prova ad eliminare questi regressori associati agli indicatori

modello_lineare_2 <- lm(TEY~AT+AP+AH+AFDP+CO+NOX, data = dati)
summary(modello_lineare_2)

#Come ci si aspettava, dando priorità alla significatività dei regressori abbiamo ottenuto un modello il cui indice di
#determinazione corretto è diminuito significativamente.
logTEY <- log(TEY)

modello_lineare_3 <- lm(logTEY~AT+AP+AH+AFDP+GTEP+TIT+TAT+CDP+CO+NOX,data = dati)
summary(modello_lineare_3)


