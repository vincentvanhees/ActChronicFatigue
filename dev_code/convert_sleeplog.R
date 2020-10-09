rm(list=ls())
path = "/media/vincent/DATA/actometer_nkcv/sleepdiary/Logboek Vincent_def.xlsx"

idcol = 1

library(readxl)

LB = as.data.frame(read_excel(path, sheet = "Blad1"))

cn = colnames(LB)

firstnight = which(cn == "op1")


LB = LB[,c(idcol,firstnight:ncol(LB))]

for (i in 1:nrow(LB)) {
  NAT = is.na(LB[i,])
  
  LB[i,NAT] = ""
  
  S = LB[i,2:ncol(LB)]
  NAT2= which(S != "")
  S  = S[NAT2]
  if (length(NAT2) > 0) {
    S = as.numeric(S)
    HRS = as.character(floor(S))
    v1 = which(nchar(HRS) == 1)
    if (length(v1) > 0) HRS[v1] = paste0("0",HRS[v1])
    MIN = as.character(floor(floor(S-floor(S)) * 60))
    v2 = which(nchar(MIN) == 1)
    if (length(v2) > 0) MIN[v2] = paste0("0",MIN[v2])
    SEC = as.character(floor(floor(S-floor(S)) * 60) - 
                         (floor(S-floor(S)) * 60) * 3600)
    v3 = which(nchar(SEC) == 1)
    if (length(v3) > 0) SEC[v3] = paste0("0",SEC[v3])

    LB[i,2:ncol(LB)[NAT2]] = paste0(HRS,":",MIN,":",SEC)
  }
}