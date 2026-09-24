# Final-Thesis-Code of NCDs and labour force participation in Nepal

This is my codebase for my thesis on how non-communicable disease (NCD) affects labour
force participation, using the Nepal Living Standards Survey IV (2022-23).

## Note on this repository
This is my working thesis code, so style and naming are not fully
standardised. The survey data are not included, and file paths in the
scripts point to my local machine. I didn't use R projects when doing the
thesis so there's set working directory code in every file. 

## Data
NLSS IV Stata files, published by Nepal's National Statistics Office.


## Repository structure

```
Final-Thesis-Code/
├── README.md
├── initialcode.R           # clean survey sections, build IDs
├── durables.R              # durable-asset indicators
├── housingquality_PCA.R    # housing-quality indicators
├── utilities.R             # utility indicators
├── calculatingpca.R        # wealth index (PCA)
├── merging.R               # merge and construct variables
└── analysis.R              # descriptives and model
```

