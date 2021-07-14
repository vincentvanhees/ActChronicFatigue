#===========
#
# Klik op [Source] hier rechtsboven om het programa te starten.
#
rm(list=ls())

gebruik_slaap_dagboek = TRUE
referentiewaarden = c(30,8) # gemiddelde en standaard deviatie
sleeplogidnum =TRUE # TRUE als patient ID een nummer is, FALSE als het ook letters bevat


# hoeveel uren data wil je negeren aan het begin en aan het einde van de meting?
hrs.del.start = 0 # aantal uren te negeren aan het begin
hrs.del.end = 0 # aantal uren te negeren aan het einde


sleepwindowType="TimeInBed"
# #=========================================
# # Install code if not available:
development.mode =FALSE # laat op FALSE staan, TRUE is alleen voor onderhoud
testbatch = FALSE
# if (development.mode == TRUE) {
# roxygen2::roxygenise()
# do.gt3x.conversion = TRUE
# locationRcode = "/home/vincent/projects/ActChronicFatigue/R"
# ffnames = dir(locationRcode) # creating list of filenames of scriptfiles to load
# for (i in 1:length(ffnames)) {
#   source(paste(locationRcode,"/",ffnames[i],sep=""))
# }
# locationRcode = "/home/vincent/GGIR/R"
# ffnames = dir(locationRcode) # creating list of filenames of scriptfiles to load
# for (i in 1:length(ffnames)) {
#   source(paste(locationRcode,"/",ffnames[i],sep=""))
# }
# } else { # install code from GitHub
# do.gt3x.conversion = TRUE

# Q1 = 1
# if ("ActChronicFatigue" %in% rownames(installed.packages()) == TRUE) {
#   cat(paste0(rep('_',options()$width),collapse=''))
#   Q1 = menu(c("Ja", "Nee"), title="\nWil je de software opnieuw installeren?")
# } 
# if (Q1 == 1 | "ActChronicFatigue" %in% rownames(installed.packages()) == FALSE | install_again == TRUE) {
#   if ("remotes" %in% rownames(installed.packages()) == FALSE) {
#     cat("\nremotes installeren...")
#     install.packages("remotes")
#   }
#   library("remotes")
#   if (install_again == TRUE) {
#     if("ActChronicFatigue" %in% (.packages())){
#       detach("package:ActChronicFatigue", unload=TRUE)
#     }
#   }
if ("GGIR" %in% rownames(installed.packages()) == FALSE) {
  cat("\nGGIR installeren...")
  remotes::install_github("wadpac/GGIR", dependencies=TRUE, ref="issue418_sibreport")
}
#   cat("\nActChronicFatigue installeren...")
#   remotes::install_github("vincentvanhees/ActChronicFatigue", dependencies=TRUE)
#   
# }
library(ActChronicFatigue)
library(GGIR)
# }

#=========================================
# Obtain data locations
datalocaties = ActChronicFatigue::obtain_folder_paths() # Obtain folder paths from user
replaceslash = function(x) {
  return(gsub(replacement = "/", pattern = "\\\\",x=x))
}
# gt3xdir = replaceslash(datalocaties$gt3xdir)
datadir = replaceslash(datalocaties$datadir)
outputdir = replaceslash(datalocaties$outputdir)
sleeplog = replaceslash(datalocaties$dagboekdir)
cat(paste0(rep('_',options()$width),collapse=''))
cat("\nBestand locaties:\n")
# cat(paste0("\nLocatie ",length(dir(datalocaties$gt3xdir))," gt3x bestanden = ",datalocaties$gt3xdir))
cat(paste0("\nLocatie ",length(dir(datalocaties$datadir))," csv bestanden = ",datalocaties$datadir))
cat(paste0("\nLocatie resultaten =  ",datalocaties$outputdir,"\n"))

chunksize = 0.5

if (length(dir(datalocaties$datadir)) == 0) { #length(dir(datalocaties$gt3xdir)) == 0 &
  cat("\n")
  stop("\nGeen data gevonden. Controleer data folders.")
}

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
ActChronicFatigue::runGGIR(datadir=datadir, outputdir = outputdir, mode = c(1:3),
                           do.report = c(2), overwrite=FALSE,
                           visualreport=FALSE, acc.metric = "BFEN", chunksize = chunksize,
                           testbatch = testbatch ,  do.parallel=TRUE, hrs.del.start = hrs.del.start,
                           hrs.del.end = hrs.del.end)

part2resultsfile = paste0(outputdir,"/output_",basename(datadir),"/results/part2_summary.csv")

#=============================================================================
# If sleeplog exists convert sleeplog to expected format
if (gebruik_slaap_dagboek == TRUE) {
  sleeplogfile = ActChronicFatigue::convert_sleeplog(sleeplog, part2resultsfile)
  # sleeplogfile = convert_sleeplog(sleeplog, part2resultsfile)
} else {
  sleeplogfile = c()
}

ActChronicFatigue::runGGIR(datadir=datadir, outputdir = outputdir, mode = c(4:5),
                           do.report = c(4, 5), overwrite=FALSE, do.visual = TRUE,
                           visualreport=FALSE, acc.metric = "BFEN", chunksize = chunksize,
                           loglocation = sleeplogfile, testbatch = testbatch ,
                           do.parallel=TRUE, sleeplogidnum = TRUE, sleepwindowType = sleepwindowType)

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
# worn on the wrist, because that is what the model was trained for.
# If all went well object prop_perv_passive will have the predictions.

#=============================================================================
# Summarise and show on screen
cat("\n Samenvatting van resultatenL\n")
SUM = ActChronicFatigue::summarise(outputdir, part5_summary, 
                                   model_threshold=model_threshold, referentiewaarden =referentiewaarden,
                                   sleepwindowType=sleepwindowType)


