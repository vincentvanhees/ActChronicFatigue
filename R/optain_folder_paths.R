#' optain_folder_paths
#'
#' @return no object is returned, GGIR writes all its outputs to files
#' @importFrom utils menu
optain_folder_paths = function() {
  datalocaties = "log_ActChronicFatigue.RData"
  if (file.exists(datalocaties) == TRUE) {
    D = load(datalocaties)
    gt3xdir = gt3xdir
    datadir = datadir
    outputdir = outputdir
    Q1 = menu(c("Ja", "Nee"), title=paste0("Wil je de bestandspaden opnieuwe instellen?"))
    if (Q1 == 1) {
      Q2 = menu(c("Ja", "Nee"), title=paste0("Gebruik je gt3x data?"))
      if (Q2 == 2) {
        Q3 = menu(c("Ja", "Nee"), title=paste0("Is ",gt3xdir," nog steeds de locatie van de gt3x bestanden? Zo nee, specificeer de nieuwe locatie in het volgende scherm"))
        if (Q3 == 2) {
          gt3xdir = easycsv::choose_dir()
        }
      }
      Q4 = menu(c("Ja", "Nee"), title=paste0("Is ",datadir," nog steeds de locatie van de csv bestanden? Zo nee, specificeer de nieuwe locatie in het volgende scherm"))
      if (Q4 == 2) {
        datadir = easycsv::choose_dir()
      }
      Q5 = menu(c("Ja", "Nee"), title=paste0("Is ",outputdir," nog steeds de locatie waar je de resultaten wilt hebben? Zo nee, specificeer de nieuwe locatie in het volgende scherm"))
      if (Q5 == 2) {
        datadir = easycsv::choose_dir()
      }
    }
  } else {
    readline("Geef aan waar de gt3x bestanden opgeslagen zijn. Klik eerst [enter] om verder te gaan.")  
    gt3xdir = easycsv::choose_dir()
    readline("Geef aan waar de csv bestanden opgeslagen zijn. Klik eerst [enter] om verder te gaan.")  
    datadir = easycsv::choose_dir()
    readline("Geef aan waar de resultaten opgeslagen mogen worden. Klik eerst [enter] om verder te gaan.")  
    outputdir = easycsv::choose_dir()
  }
  
  save(gt3xdir, datadir, outputdir, file = datalocaties)
  invisible(list(gt3xdir=gt3xdir, datadir=datadir,outputdir=outputdir))
}