## Actometer behavioral scoring from wrist- or hip-worn Actigraph data


### To apply previously trained model on new data:

1. Use ActiLife software to export the raw Actigraph data to .gt3x format
2. Install R and RStudio
3. Op runActChronicFatigue.R from this repository and click the Source button in RStudio.

- The first time you do the software will be installed, which may take a while. The second time you do this you will be asked (in Dutch) whether you want to install the software again. 
- Next, the software will ask you to specify the locations of your data.
- Once that is done the software will continue with processing the data.

### To train a new model based on existing training data:

1. Follow steps above
2. Create a file with the waking times, and convert this to a specific format with R script [load_and_convert_times.R](/dev_code/load_and_convert_times.R)
3. Create labels.csv file with one column for id, one column for label (holding character values for "pp" and "fa") and one column loc specifying the body location ("wrist" and "hip").
4. Run script [fitmodel.R](/dev_code/fitmodel.R) after updating the info at the top to match your situation.

### Model interpretation

The models are logistic regression models, which can be interpretted as follows:

```
If the coefficients are 4.75861402 and -0.05916747, then
x = 4.75861402 + (-0.05916747 * act9167 )
probability_pp = 1/(1+ exp(-x))
```

In other words: a lower value of act9167 (more inactive person) will result in higher x, which will increase the probability of being pp (pervasively passive), while a higher value of act9167 (more active person) will result in a lower value of x and result in a lower probability of being pp (pervasively passive).