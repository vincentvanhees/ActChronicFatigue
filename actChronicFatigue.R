rm(list=ls())

# Klik op [Source] hier rechtsboven om het programa te starten.

development.mode = TRUE

#=========================================
# Install code if not available:
verbose.install = TRUE
if (development.mode == TRUE) {
  roxygen2::roxygenise()
  do.gt3x.conversion = FALSE
  locationRcode = "/home/vincent/projects/ActChronicFatigue/R" 
  ffnames = dir(locationRcode) # creating list of filenames of scriptfiles to load
  for (i in 1:length(ffnames)) {
    source(paste(locationRcode,"/",ffnames[i],sep=""))
  }
  locationRcode = "/home/vincent/GGIR/R" 
  ffnames = dir(locationRcode) # creating list of filenames of scriptfiles to load
  for (i in 1:length(ffnames)) {
    source(paste(locationRcode,"/",ffnames[i],sep=""))
  }
} else { # install code from GitHub
  do.gt3x.conversion = TRUE
  install_again = FALSE
  Q1b = 1
  if ("ActChronicFatigue" %in% rownames(installed.packages()) == TRUE) {
    cat(paste0(rep('_',options()$width),collapse=''))
    Q1 = menu(c("Ja", "Nee"), title="\nWil je de software opnieuw installeren?")
    if (Q1 == 1) {
      install_again = TRUE
      cat(paste0(rep('_',options()$width),collapse=''))
      Q1b = menu(c("Ja", "Nee"), title="\nWil je ook all dependencies opnieuw installeren?")
    }
  }
  if ("ActChronicFatigue" %in% rownames(installed.packages()) == FALSE | install_again == TRUE) {
    if ("devtools" %in% rownames(installed.packages()) == FALSE) {
      install.packages("devtools", verbose = verbose.install)
    }
    library("devtools")
    if (install_again == TRUE) {
      if("ActChronicFatigue" %in% (.packages())){
        detach("package:ActChronicFatigue", unload=TRUE)
      }
    }
    if (Q1b == 1) {
      depe = TRUE
    } else {
      depe = FALSE
    }
    install_github("vincentvanhees/ActChronicFatigue", dependencies=depe, 
                   verbose = verbose.install, force = depe)
  }
  library(ActChronicFatigue)
}

#=========================================
# Obtain data locations
datalocaties = ActChronicFatigue::optain_folder_paths() # Obtain folder paths from user
replaceslash = function(x) {
  return(gsub(replacement = "/", pattern = "\\\\",x=x))
}
gt3xdir = replaceslash(datalocaties$gt3xdir)
datadir = replaceslash(datalocaties$datadir)
outputdir = replaceslash(datalocaties$outputdir)
cat(paste0(rep('_',options()$width),collapse=''))
cat("\nBestand locaties:\n")
cat(paste0("\nLocatie ",length(dir(datalocaties$gt3xdir))," gt3x bestanden = ",datalocaties$gt3xdir))
cat(paste0("\nLocatie ",length(dir(datalocaties$datadir))," csv bestanden = ",datalocaties$datadir))
cat(paste0("\nLocatie resultaten =  ",datalocaties$outputdir,"\n"))

chunksize = 0.5

if (length(dir(datalocaties$gt3xdir)) == 0 & length(dir(datalocaties$datadir)) == 0) {
  cat("\n")
  stop("\nGeen data gevonden. Controleer data folders.")
}

#=============================================================================
# Converteer all gt3x bestanden naar csv bestanden en plaats die in datadir
if (do.gt3x.conversion ==  TRUE) {
  gt3x_files_to_convert = dir(gt3xdir, full.names = T)
  if (length(gt3x_files_to_convert ) > 0) {
    for (gt3xfile in gt3x_files_to_convert ) {
      cat(paste0(rep('_',options()$width),collapse=''))
      cat(paste0("\nConverteer ",basename(gt3xfile)," -> .csv"))
      ActChronicFatigue::gt3x_to_csv(path = gt3xfile, outpath =datadir, gzip=T)
    }
  }
}
#=============================================================================
# If sleeplog exists convert sleeplog to expected format
# sleeplog = "/media/vincent/DATA/actometer_nkcv/sleepdiary/Logboek Vincent_def.xlsx"
# sleeplogfile = ActChronicFatigue::convert_sleeplog(sleeplog)
sleeplogfile = c()

filenames = dir(datadir, full.names = FALSE)
if (length(filenames) == 0) {
  stop(paste0("\nGeen data bestanden gevonden in ", datadir))
} else {
  cat("\nStart met verwerken van volgende bestand(en):\n")
  print(filenames)
  Q3 = menu(c("Ja", "Nee"), title=paste0("\nDoorgaan?"))
  if (Q3 == 2) {
    stop("\nProgramma gestopt door gebruiker")
  }
}

#=============================================================================
# Start processing of raw accelerometer data with GGIR
cat(paste0(rep('_',options()$width),collapse=''))
cat("\nStart analyse met GGIR...\n")
# ActChronicFatigue::runGGIR(datadir=datadir, outputdir = outputdir, mode = c(1:4),
#                            do.report = c(2,4), overwrite=FALSE, do.visual = FALSE,
#                            visualreport=FALSE, acc.metric = "BFEN", chunksize = chunksize,
#                            loglocation = sleeplogfile, testbatch = FALSE,  do.parallel=TRUE)
# 
# # always overwrite part 5 to be sure BFEN is used
# ActChronicFatigue::runGGIR(datadir=datadir, outputdir = outputdir, mode = c(5), 
#                            do.report=c(5), overwrite=TRUE, do.visual = FALSE,
#                            visualreport=FALSE, acc.metric = "BFEN",
#                            loglocation = sleeplogfile)

#=============================================================================
cat(paste0(rep('_',options()$width),collapse=''))
cat("\nKlassficeer of de persoon al dan niet laag actief is ...\n")

# Add extra variables, specifically needed for classification
tmp = unlist(strsplit(datadir,"/|\")"))
outputdir_backup = outputdir
outputdir = paste0(outputdir,"/output_",tmp[length(tmp)])
ActChronicFatigue::addVariables5(outputdir=outputdir)


# Load the resulting part5_personsummary.csv bestand
part5_summary_file = grep(dir(paste0(outputdir,"/results"), full.names = TRUE),
                          pattern = "part5_personsummary_WW_", value = T)
part5_summary = read.csv(file=part5_summary_file, sep=",")

# If all went well this part2_summary object will have a column act90, gradient_mean, towards the end.

# Load the trained model
if (development.mode == TRUE) {
  load("./inst/extdata/final_model_wrist.Rdata")
} else {
  load(system.file("extdata/final_model_wrist.Rdata", package = "ActChronicFatigue"))
}

# Make predictions by applying the model
prop_perv_passive = stats::predict(object=final_model_wrist, newdata=part5_summary, type="response")
part5_summary = cbind(part5_summary, prop_perv_passive)

CF = coef(final_model_wrist)
model_threshold = -CF[1]/CF[2]


# Note that we assumed that all data in part2_summary comes from accelerometer
# worn on the hip, because that is what the model was trained for.
# If all went well object prop_perv_passive will have the predictions.

# Check that estimate is matches with labels in our traininge data
# labels = read.csv("/media/vincent/DATA/actometer_nkcv/labels.csv")
##part5_summary part5_summary = part5_summary[which(part5_summary$ID2 %in% labels$ID[which(labels$loc == "wrist")]),]
# EV = merge(labels, part5_summary, by.x = "ID", by.y = "ID2")
# EV = EV[which(EV$loc == "wrist"),]
# EV$pp = 0
# EV$pp[which(EV$label == "pp")] = 1
# x11();plot(EV[,c("pp","prop_perv_passive")],type="p", pch=20)


#=============================================================================
# Summarise and show on screen
cat("\n Samenvatting van resultaten\n")
SUM = ActChronicFatigue::summarise(outputdir, part5_summary, Nmostrecent = 10,
                                   model_threshold=model_threshold)

recent_results_file = paste0(outputdir,"/results/samenvatting_",
                             as.character(as.Date(Sys.time())),".csv")
write.csv(SUM, file = recent_results_file, row.names = FALSE)
cat(paste0("\nSamenvatting van resultaten is ook opgeslagen in", recent_results_file, "\n"))
kkk
#=============================================================================
# Reprocess with ENMO
cat(paste0(rep('_',options()$width),collapse=''))
cat("\nBezig met aanvullende analyses...\n")
ActChronicFatigue::runGGIR(datadir = datadir, outputdir = outputdir_backup, mode = c(5),
                           do.report = c(5), overwrite = TRUE, do.visual = FALSE, visualreport=TRUE,
                           acc.metric = "ENMO", loglocation = sleeplogfile)
part5_summary = read.csv(file=part5_summary_file, sep=",")
# Add BFEN predictions
part5_summary = cbind(part5_summary, prop_perv_passive) # voeg BFEN predictions toe
# Store
write.csv(part5_summary, file = part5_summary_file, row.names = F)
cat(paste0("\nAanvullende resultaten staan in ",outputdir))