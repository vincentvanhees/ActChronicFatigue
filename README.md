## Actometer behavioral scoring from wrist- or hip-worn Actigraph data

### To train a new model based on existing training data:

1. Export the raw Actigraph data to .csv format and place them in one data folder.
2. Install R and RStudio
3. Install R package GGIR, e.g. RStudio toolbar: Tools -> Install package -> search for GGIR and click install.
4. Open [runGGIR.R](/runGGIR.R) in RStudo and update the directories at the top to match you file structure.
Note that folder paths are separated by / and not by \.
5. Click on 'Source' button. If all goes well this will results after a while (depends on how many files you are processing) in a new folder structure in the output directory you specified with subfolder 'results' and inside "part2_summary.csv" en "part2_daysummary.csv".

6. Create labels.csv file with one column for id, one column for label (holding character values for "pp" and "fa") and one column loc specifying the body location ("wrist" and "hip").
7. Run script addVariables.R after updating the info at the top to match your situation.
8. Run script fitmodel.R after updating the info at the top to match your situation.

### To apply previously trained model on new data:

See script howtoapplymodel.R, where step 1 is the same as step 1-5 above.
