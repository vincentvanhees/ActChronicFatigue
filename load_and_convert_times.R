rm(list=ls())

activitylog ="/media/vincent/DATA/actometer_nkcv/new_info_Manon_25June2020/Actometer_tijden_volledig.csv"
D <- read.csv(activitylog, header = F)


blocks = grep(pattern = "ID", x = D[,1])

AL = matrix("", length(blocks), (ncol(D)-1)*3)


for (i in 1:length(blocks)) {
  AL[i,1] = unlist(strsplit(D[blocks[i],1],"D = "))[2]
  tmp = D[blocks[i]+1:4,2:ncol(D)]
  tmp = as.matrix(tmp[1:3,which(tmp[4,] == "JA")])
  dim(tmp) = c(1,length(tmp))
  tmp = gsub(pattern = "[(+)]", replacement = "",x = tmp)
  tmp[grep(pattern = "[(+)]|24:00|0:00", x = tmp)] = "23:55"
  AL[i,2:(length(tmp)+1)] = tmp
}

AL = as.data.frame(AL)

CN = c("ID", rep(c("date","start","end"), times=floor((ncol(AL)-1) / 3)))
colnames(AL)[1:length(CN)] = CN

remove = which(colnames(AL) %in% c("ID","date","start","end") == FALSE)
AL = AL[,-remove]

write.csv(AL, file = "/media/vincent/DATA/actometer_nkcv/new_info_Manon_25June2020/Actometer_tijden_volledig2.csv")