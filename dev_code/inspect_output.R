rm(list=ls())

pathnew = "D:/Dropbox/Work/sharedfolder/DATA/NKCV/actometer_nkcv_data/output_nkcv_wrist/results/part2_summary.csv"
pathold = "D:/Dropbox/Work/sharedfolder/DATA/NKCV/actometer_nkcv_data/resultsApril2023/results/part2_summary_old.csv"

Snew = read.csv(pathnew)
Sold = read.csv(pathold)

M = merge(x = Sold, y = Snew, by = "ID", suffixes = c("old", "new"))

x11()
plot(M$ENMO_fullRecordingMeannew, M$ENMO_fullRecordingMeanold, type="p", pch = 20)

# 
# x11()
# plot(M$ENMO_fullRecordingMeannew, M$ENMO_fullRecordingMeanold, type="p", pch = 20)