Dynamic-Customer-Targeting-in-R
===============================
##Description
This is a demo of dynamic customer targeting. Treating the marketing customer targeting as stream process, we can find out potential buyes
with less marketing expenses.

##Result
With a simple Naive Bayes model, we can save 25% - 30% calls in a marketing campaign.

##Structure
- data_preparation.R
    for pre processing the raw data
- modeling_xxx.R
    for fitting the model
- Summary.R
    for model comparation
- Incremental_model.R
    a recursive method to do customer targeting

##Data Source:
[Moro et al., 2014] S. Moro, P. Cortez and P. Rita. A Data-Driven Approach to Predict the Success of Bank Telemarketing. Decision Support Systems, Elsevier, 62:22-31, June 2014
https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
