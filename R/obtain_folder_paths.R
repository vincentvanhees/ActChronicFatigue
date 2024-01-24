#' obtain_folder_paths
#'
#' @return no object is returned, GGIR writes all its outputs to files
#' @importFrom utils menu
#' @export

obtain_folder_paths = function() {
  datalocaties = "log_ActChronicFatigue.RData"
  if (file.exists(datalocaties) == TRUE) {
    dagboekdir = datadir = outputdir = "" #gt3xdir =
    load(datalocaties)
    
    cat(paste0(rep('_',options()$width),collapse=''))
    Q1 = menu(c("Ja", "Nee"), title=paste0("\nWil je de bestandspaden opnieuw instellen?"))
    if (Q1 == 1) {
      # cat(paste0(rep('_',options()$width),collapse=''))
      # Q2 = menu(c("Ja", "Nee"), title=paste0("\nGebruik je gt3x data?"))
      # if (Q2 == 1) {
      #   cat(paste0(rep('_',options()$width),collapse=''))
      #   Q3 = menu(c("Ja", "Nee"), title=paste0("\nIs ",gt3xdir," nog steeds de locatie van de gt3x bestanden? \nZo nee, specificeer de nieuwe locatie in het volgende scherm"))
      #   if (Q3 == 2) {
      #     gt3xdir = easycsv::choose_dir()
      #   }
      # }
      cat(paste0(rep('_',options()$width),collapse=''))
      Q4 = menu(c("Ja", "Nee"), title=paste0("\nIs ",datadir," nog steeds de locatie van de gt3x bestanden? \nZo nee, specificeer de nieuwe locatie in het volgende scherm"))
      if (Q4 == 2) {
        datadir = easycsv::choose_dir()
      }
      cat(paste0(rep('_',options()$width),collapse=''))
      Q5 = menu(c("Ja", "Nee"), title=paste0("\nIs ",outputdir," nog steeds de locatie waar je de resultaten wilt hebben? \nZo nee, specificeer de nieuwe locatie in het volgende scherm"))
      if (Q5 == 2) {
        outputdir = easycsv::choose_dir()
      }
      
      cat(paste0(rep('_',options()$width),collapse=''))
      Q6 = menu(c("Ja", "Nee"), title=paste0("\nGebruik je een slaapdagboek?"))
      if (Q6 == 1) {
        cat(paste0(rep('_',options()$width),collapse=''))
        Q7 = menu(c("Ja", "Nee"), title=paste0("\nIs ",dagboekdir," nog steeds de locatie van het slaapdagboek? \nZo nee, specificeer de nieuwe locatie in het volgende scherm"))
        if (Q7 == 2) {
          dagboekdir = file.choose() #easycsv::choose_dir()
        }
      }
      
    }
  } else {
    cat(paste0(rep('_',options()$width),collapse=''))
    # readline("\nGeef aan waar de gt3x bestanden opgeslagen zijn.\n Klik eerst [enter] om verder te gaan.")  
    # gt3xdir = easycsv::choose_dir()
    readline("\nGeef aan waar de csv bestanden opgeslagen zijn.\n Klik eerst [enter] om verder te gaan.")  
    datadir = easycsv::choose_dir()
    readline("\nGeef aan waar de resultaten opgeslagen mogen worden.\n Klik eerst [enter] om verder te gaan.")  
    outputdir = easycsv::choose_dir()
    readline("\nGeef aan in welk bestand het slaapdagboek (.xlsx) is opgeslagen.\n Klik eerst [enter] om verder te gaan.")  
    dagboekdir = file.choose()
    
    
  }
  
  save(datadir, outputdir,  dagboekdir, file = datalocaties) #gt3xdir, 
  invisible(list(datadir=datadir,outputdir=outputdir,  dagboekdir= dagboekdir)) #gt3xdir=gt3xdir, 
}