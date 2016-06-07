# r-usefull-scripts

binning_finction.R conteins function for binning continuous variables with rpart for further using in logistic regression.

Input
* training - data frame on which the model will be trained (and features will be binned)
* vars - feature names at training data frame
* target - the name of target variable at training data frame
* newSet - data frame on which the model will be tested (and binning on training sample will be applyed)

Output in class Res
* training - data frame on which the model will be trained with binned variables instead of original, the names of binned variable start form 'bin_'
* vars - updated feature names
* not_binned - the names of features that were not binned
* excluded_vars - the names of features that were excluded because of one value rate more than 95%
* newSet - data frame on which the model will be tested with binned variables instead of original, the names of binned variable start form 'bin_'
