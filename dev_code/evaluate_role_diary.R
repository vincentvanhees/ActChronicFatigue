rm(list=ls())


P4Z = read.csv("/media/vincent/DATA/actometer_nkcv/output_nkcv_wrist/results/QC/part4_nightsummary_sleep_full.csv")
P4M = read.csv("/media/vincent/DATA/actometer_nkcv/output_nkcv_wrist/results_guided_diary/QC/part4_nightsummary_sleep_full.csv")


P4 = merge(P4Z, P4M, by = c("ID","night", "calendar_date"), suffixes = c("Z","M"))
P4 = P4[which(P4$guiderM == "sleeplog"),]

x11()
plot(P4$sleeponsetZ - P4$sleeponsetM, type="p", pch=20, ylab="zonder - met dagboek", main="slaap onset")