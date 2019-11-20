## Actometer behavioral scoring from wrist- or hip-worn Actigraph data

### To train a new model based on existing training data:

1. Use ActiLife software to export the raw Actigraph data to .csv format
2. Place the exported .csv files in a new data folder that does not contain non-accelerometer files or epoch level accelerometer files.
3. Install R and RStudio
4. Install R package GGIR, e.g. RStudio toolbar: Tools -> Install package -> search for GGIR and click install.
5. Open [runGGIR.R](/runGGIR.R) in RStudo and update the directories at the top to match you file structure.
Note that folder paths are separated by / and not by \.
6. Click on 'Source' button. If all goes well this will results after a while (depends on how many files you are processing) in a new folder structure in the output directory you specified with subfolder 'results' and inside "part2_summary.csv" en "part2_daysummary.csv".

7. Create labels.csv file with one column for id, one column for label (holding character values for "pp" and "fa") and one column loc specifying the body location ("wrist" and "hip").
8. Run script [addVariables.R](/addVariables.R) after updating the info at the top to match your situation.
9. Run script [fitmodel.R](/fitmodel.R) after updating the info at the top to match your situation.

### To apply previously trained model on new data:

See script [howtoapplymodel.R](howtoapplymodel.R), where step 1 is the same as step 1-6 above.
