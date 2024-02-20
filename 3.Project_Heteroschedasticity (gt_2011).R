file_path <- "/Users/francesco/programmi_R/Progetto Finale MS-SL/Datasets/gt_2011.csv"

dati <- read.csv(file_path, header = TRUE, dec = ".", sep = ",")

head(dati)

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

calcola_testBP_regressore <- function(datiF,regressoreF){
  
  ATF <- datiF$AT
  APF <- datiF$AP
  AHF <- datiF$AH
  AFDPF <- datiF$AFDP
  GTEPF <- datiF$GTEP
  TITF <- datiF$TIT
  TATF <- datiF$TAT
  TEYF <- datiF$TEY
  CDPF <- datiF$CDP
  COF <- datiF$CO
  NOXF <- datiF$NOX
  
  TEY_reg <- TEYF/regressoreF
  int <- 1/regressoreF
  ATF_reg <- ATF/regressoreF
  AHF_reg <- AHF/regressoreF
  AFDPF_reg <- AFDPF/regressoreF
  GTEPF_reg <- GTEPF/regressoreF
  TITF_reg <- TITF/regressoreF
  TATF_reg <- TATF/regressoreF
  CDPF_reg <- CDPF/regressoreF
  COF_reg <- COF/regressoreF
  NOXF_reg <- NOXF/regressoreF
  
  modello_regressoreF <- lm(TEY_reg ~ int+ATF_reg+AHF_reg+AFDPF_reg+GTEPF_reg+TITF_reg+TATF_reg+CDPF_reg+COF_reg+NOXF_reg-1, data = datiF)
  
  res_modello_regressoreF <- resid(modello_regressoreF)
  resQuadrato_modello_regressoreF <- res_modello_regressoreF^2
  
  modello_BP_regressoreF <- lm(resQuadrato_modello_regressoreF ~ int+ATF_reg+AHF_reg+AFDPF_reg+GTEPF_reg+TITF_reg+TATF_reg+CDPF_reg+COF_reg+NOXF_reg-1, data = dati)
  
  return(modello_BP_regressoreF)
  
}

primo_modello_regressione <- lm(TEY ~ AT+AP+AH+AFDP+GTEP+TIT+TAT+CDP+CO+NOX, data = dati)
summary(primo_modello_regressione)

#Correlazione tra ordinate stimate e residui
residui_primo_modello_regressione <- resid(primo_modello_regressione)
yStimate_primo_modello_regressione <- fitted(primo_modello_regressione)
plot(yStimate_primo_modello_regressione,residui_primo_modello_regressione,
     xlab = "Ordinate stimate",
     ylab = "Residui stimati", 
     main = "Relazione tra ordinate stimate e residui stimati",
     col = "#8856a7")

#Correlazioni tra i regressori e i residui
plot(AT,residui_primo_modello_regressione,
     xlab = "Temperatura ambientale",
     ylab = "Residui stimati", 
     main = "Relazione tra temperatura ambientale e residui stimati",
     col = "#8856a7")

plot(AP,residui_primo_modello_regressione,
     xlab = "Pressione ambientale",
     ylab = "Residui stimati", 
     main = "Relazione tra pressione ambientale e residui stimati",
     col = "#8856a7")

plot(AH,residui_primo_modello_regressione,
     xlab = "Umidità ambientale",
     ylab = "Residui stimati", 
     main = "Relazione tra umidità ambientale e residui stimati",
     col = "#8856a7")

plot(AP,residui_primo_modello_regressione,
     xlab = "Pressione ambientale",
     ylab = "Residui stimati", 
     main = "Relazione tra pressione ambientale e residui stimati",
     col = "#8856a7")

plot(AFDP,residui_primo_modello_regressione,
     xlab = "Differenza di pressione del filtro dell'aria",
     ylab = "Residui stimati", 
     main = "Relazione tra differenza di pressione del filtro dell'aria e residui stimati",
     col = "#8856a7")

plot(GTEP,residui_primo_modello_regressione,
     xlab = "Pressione di scarico della turbina a gas",
     ylab = "Residui stimati", 
     main = "Relazione tra pressione di scarico della turbina a gas e residui stimati",
     col = "#8856a7")

plot(TIT,residui_primo_modello_regressione,
     xlab = "Temperatura di ingresso della turbina",
     ylab = "Residui stimati", 
     main = "Relazione tra temperatura di ingresso della turbina e residui stimati",
     col = "#8856a7")

plot(TAT,residui_primo_modello_regressione,
     xlab = "Temperatura d'uscita della turbina",
     ylab = "Residui stimati", 
     main = "Relazione tra temperatura d'uscita della turbina e residui stimati",
     col = "#8856a7")

plot(CDP,residui_primo_modello_regressione,
     xlab = "Pressione di scarico del compressore",
     ylab = "Residui stimati", 
     main = "Relazione tra pressione di scarico del compressore e residui stimati",
     col = "#8856a7")

plot(CO,residui_primo_modello_regressione,
     xlab = "Monossido di carbonio",
     ylab = "Residui stimati", 
     main = "Relazione tra monossido di carbonio e residui stimati",
     col = "#8856a7")

plot(NOX,residui_primo_modello_regressione,
     xlab = "Ossido di azoto",
     ylab = "Residui stimati", 
     main = "Relazione tra ossido di azoto e residui stimati",
     col = "#8856a7")

resQuadrato_primo_modello_regressione <- residui_primo_modello_regressione^2
modello_BP <- lm(resQuadrato_primo_modello_regressione ~ AT+AP+AH+AFDP+GTEP+TIT+TAT+CDP+CO+NOX, data = dati)
summary(modello_BP)

#Esito TEST BP: ipotesi nulla rifiuata in quanto il p-value della statistica test F di Fisher è minore del livello di significatività (0.05)

yStimateQuadrato_primo_modello_regressione <- yStimate_primo_modello_regressione^2
modello_W <- lm(resQuadrato_primo_modello_regressione ~ yStimate_primo_modello_regressione + yStimateQuadrato_primo_modello_regressione)
summary(modello_W)

#Esito TEST W: ipotesi nulla rifiuata in quanto il p-value della statistica test F di Fisher è minore del livello di significatività (0.05)

#A questo punto bisogna trasformare il modello e rieffettuare i test 

reciproco_YStimate <- 1/yStimate_primo_modello_regressione

AT_T <- AT/yStimate_primo_modello_regressione
AP_T <- AP/yStimate_primo_modello_regressione
AH_T <- AH/yStimate_primo_modello_regressione
AFDP_T <- AFDP/yStimate_primo_modello_regressione
GTEP_T <- GTEP/yStimate_primo_modello_regressione
TIT_T <- TIT/yStimate_primo_modello_regressione
TAT_T <- TAT/yStimate_primo_modello_regressione
TEY_T <- TEY/yStimate_primo_modello_regressione
CDP_T <- CDP/yStimate_primo_modello_regressione
CO_T <- CO/yStimate_primo_modello_regressione
NOX_T <- NOX/yStimate_primo_modello_regressione

mod_Tr <- lm(TEY_T ~ reciproco_YStimate+AT_T+AP_T+AH_T+AFDP_T+GTEP_T+TIT_T+TAT_T+CDP_T+CO_T+NOX_T-1, data = dati)
summary(mod_Tr)

resid_mod_Tr <- resid(mod_Tr)

residQuadrato_mod_Tr <- resid_mod_Tr^2
modello_Tr_BP <- lm(residQuadrato_mod_Tr ~ reciproco_YStimate+AT_T+AP_T+AH_T+AFDP_T+GTEP_T+TIT_T+TAT_T+CDP_T+CO_T+NOX_T-1, data = dati)
summary(modello_Tr_BP)

#Trasformazione logaritmica del modello: passo da un modello lin-lin ad un modello log-lin

log_TEY <- log(TEY)
modLog <- lm(log_TEY ~ AT+AP+AH+AFDP+GTEP+TIT+TAT+CDP+CO+NOX, data = dati)
summary(modLog)

yStimate_modLog <- fitted(modLog)
reciproco_YStimate_modLog <- 1/yStimate_modLog
resid_modLog <- resid(modLog)
residQuadrato_modLog <- resid_modLog^2

#Test BP sul modello logaritmico
modello_log_BP <- lm(residQuadrato_modLog ~ AT+AP+AH+AFDP+GTEP+TIT+TAT+CDP+CO+NOX, data = dati)
summary(modello_log_BP)

#Trasformazione del modello log-lin

AT_Tlog <- AT/yStimate_modLog
AP_Tlog <- AP/yStimate_modLog
AH_Tlog <- AH/yStimate_modLog
AFDP_Tlog <- AFDP/yStimate_modLog
GTEP_Tlog <- GTEP/yStimate_modLog
TIT_Tlog <- TIT/yStimate_modLog
TAT_Tlog <- TAT/yStimate_modLog
TEY_Tlog <- TEY/yStimate_modLog
CDP_Tlog <- CDP/yStimate_modLog
CO_Tlog <- CO/yStimate_modLog
NOX_Tlog <- NOX/yStimate_modLog

modLogBP <- lm(residQuadrato_modLog ~ reciproco_YStimate_modLog+AT_Tlog+AP_Tlog+AH_Tlog+AFDP_Tlog+GTEP_Tlog+TIT_Tlog+TAT_Tlog+CDP_Tlog+CO_Tlog+NOX_Tlog-1, data = dati)
summary(modLogBP)

#A questo punto provo a trasformare il modello in funzione di tutti i regressori

mod_BP_AT <- calcola_testBP_regressore(dati,AT)
summary(mod_BP_AT)
mod_BP_AP <- calcola_testBP_regressore(dati,AP)
summary(mod_BP_AP)
mod_BP_AH <- calcola_testBP_regressore(dati,AH)
summary(mod_BP_AH)
mod_BP_AFDP <- calcola_testBP_regressore(dati,AFDP)
summary(mod_BP_AFDP)
mod_BP_GTEP <- calcola_testBP_regressore(dati,GTEP)
summary(mod_BP_GTEP)
mod_BP_TIT <- calcola_testBP_regressore(dati,TIT)
summary(mod_BP_TIT)
mod_BP_TAT <- calcola_testBP_regressore(dati,TAT)
summary(mod_BP_TAT)
mod_BP_CDP <- calcola_testBP_regressore(dati,CDP)
summary(mod_BP_CDP)
mod_BP_CO <- calcola_testBP_regressore(dati,CO)
summary(mod_BP_CO)
mod_BP_NOX <- calcola_testBP_regressore(dati,NOX)
summary(mod_BP_NOX)

#Nessuno dei modelli trasformati è riuscito a scardinare la condizione di eteroschedasticità

#Prova modello log-log

AT_log <- log(AT)
AP_log <- log(AP)
AH_log <- log(AH)
AFDP_log <- log(AFDP)
GTEP_log <- log(GTEP)
TIT_log <- log(TIT)
TAT_log <- log(TAT)
TEY_log <- log(TEY)
CDP_log <- log(CDP)
CO_log <- log(CO)
NOX_log <- log(NOX)

modello_log_log <- lm(TEY_log ~ AT_log+AP_log+AH_log+AFDP_log+GTEP_log+TIT_log+TAT_log+CDP_log+CO_log+NOX_log, data = dati)
summary(modello_log_log)

resid_mod_log_log <- resid(modello_log_log)
residQuadrato_mod_log_log <- resid_mod_log_log^2

modelloBP_log_log <- lm(residQuadrato_mod_log_log ~ AT_log+AP_log+AH_log+AFDP_log+GTEP_log+TIT_log+TAT_log+CDP_log+CO_log+NOX_log, data = dati)
summary(modelloBP_log_log)