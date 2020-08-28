#' addVariables5
#'
#' @param outputdir ...
#' @return no object is returned, only a new file is created in the output directory
#' @export
#' @importFrom stats aggregate sd quantile aggregate.data.frame
#' @importFrom utils read.csv write.csv
#' 
addVariables5 = function(outputdir=c()) {
  #==========================================================
  # Input needed:
  # outputdir = "/media/vincent/DATA/actometer_nkcv/output_rawactigraph_nkcv" # specify output directory
  separator = "," # Note: replace by "\t" if you are working with tab seperated data
  
  #==========================================================
  # load data
  part5_daysummary_file = grep(dir(paste0(outputdir,"/results"), full.names = TRUE),pattern = "part5_daysummary_WW_", value = T)
  part5_summary_file = grep(dir(paste0(outputdir,"/results"), full.names = TRUE),pattern = "part5_personsummary_WW_", value = T)
  P5 = read.csv(part5_daysummary_file, stringsAsFactors = FALSE, sep=separator)
  # Change ID to numeric:
  convertID = function(idValues) {
    if (class(idValues) == "character") {
      idValues = as.character(sapply(idValues, FUN=function(x) unlist(strsplit(x," "))[1]))
      idValues = as.integer(idValues)
    }
    return(idValues)
  }
  P5$ID = convertID(P5$filename)
  # select valid days only
  P5 = P5[which(P5$nonwear_perc_day_spt < 33.33),]
  # select subset of potentially relevant variables:
  P5 = P5[,c("ID", "ACC_day_mg")]
  colnames(P5) = c("ID","act")
  missing = which(is.na(P5$act) == TRUE)
  if (length(missing) > 0) P5 = P5[-missing,]
  # Calculate the 91.67th percentile of the day level variables
  D = aggregate(x = P5, by = list(P5$ID), FUN = function(x) {quantile(x,11/12, na.rm = TRUE) })
  D = D[,-1]
  colnames(D) = c("ID","act9167")
  
  # Add the new variables to the person level output calculated by GGIR part 2:
  P5summary = read.csv(part5_summary_file, stringsAsFactors = FALSE, sep=separator)
  
  # Change ID to numeric
  P5summary$ID2 = convertID(P5summary$filename)
  
  # Check whether data already has the expected variables
  existingvars = which(colnames(P5summary) %in% c("ID","act9167"))
  if (length(existingvars) > 0) P5summary = P5summary[,-existingvars]
  P5summary_updated = merge(P5summary,D,by.x="ID2",by.y="ID")
  # Save changes
  write.csv(P5summary_updated,file=part5_summary_file,row.names = FALSE)
  
}