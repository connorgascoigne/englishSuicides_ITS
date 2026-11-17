# Evaluating England’s 2012 suicide prevention strategy: An interrupted time series evaluating national and local trends in suicide rates, 2002–2022 

This repository contains the code used for a study examining the effect of the UKs 2012 suicide prevention scheme on suicide trends in England. It is intended to support the accompanying paper published in **BMJ Mental Health DOI**.

## What this study does

England’s 2012 suicide prevention strategy transferred responsibility for designing and delivering locally tailored suicide prevention plans from central government to local authorities. Implementation was gradual and varied between areas.

This study examines whether suicide rates changed after the strategy was introduced, and whether the size or timing of any change differed between local authorities. It uses a spatially varying interrupted time-series (ITS) design. In simple terms, an ITS compares suicide trends before and after a policy or event. This analysis extends that approach by allowing the post-strategy changes to vary geographically, reflecting differences in local implementation.

The analysis asks whether there was:

1.  An immediate or sustained effect of national suicide rates due to the policy?
2.  Did the transfer of responsbility to local authorities create any differences in the immediate or sustained effects of the policy?

The code in this repository documents the analysis used in the paper. The paper and supplementary material should be used for the full definitions, methods, results and interpretation.

## Relationship to the previous study

This repository is a follow-up study and shares substantial code with the earlier repository:

- [Previous study: `englishSuicides`](https://github.com/connorgascoigne/englishSuicides)
- [This study: `englishSuicides_ITS`](https://github.com/connorgascoigne/englishSuicides_ITS)

The data-simulation code and most covariate-processing code are shared with the previous study. The main additional covariate-processing step is `10_sort_mentalHealth.R`, which prepares mental-health information from Understanding Society / the UK Household Longitudinal Study.

## Repository structure

``` text
00_simulatingData/
01_processCovariateData/
02_modelFitting/
```

### `00_simulatingData`

This folder contains the simulation code used to generate suicide data for testing and development. The same folder and files are used in the related study. Simulated data are not a substitute for the study data; they provide a way to check that the analysis pipeline runs and behaves as expected.

### `01_processCovariateData`

This folder prepares the covariates used by the models. It follows the processing workflow in the previous study, with the addition of:

- `10_sort_mentalHealth.R` — processes the mental-health covariate using data from [Understanding Society](https://www.understandingsociety.ac.uk/).

Understanding Society data require registration and approval through the study's data-access process. The raw UKHLS data are therefore not included in this repository. Users who need to reproduce this step must obtain authorised access independently and follow the terms of use for the data.

### `02_modelFitting`

This folder contains the modelling, robustness and sensitivity-analysis pipelines:

- `01_spatial_pipeline.R` — spatial-analysis pipeline.
- `02_main_pipeline.R` — main analysis pipeline.
- `02a_main_dataprocessing.R` — data processing for the main analysis.
- `02b_main_modelfitting.R` — fitting of the main models.
- `02c_main_modelresults.R` — extraction and preparation of main-model results.
- `02d_main_additionalresults.R` — additional main-analysis results.
- `03_robustness_pipeline.R` — robustness-analysis using alternative years as the 'intervention' pipeline.
- `03a_robustness_dataprocessing.R` — data processing for robustness analyses.
- `03b_robustness_modelfitting.R` — fitting of robustness models.
- `03c_robustness_modelresults.R` — extraction and preparation of robustness results.
- `04_sensitivity_pipeline.R` — sensitivity-analysis on the definition of suicide from ICD-10 codes pipeline.
- `04a_sensitivity_dataprocessing.R` — data processing for sensitivity analyses.
- `04b_sensitivity_modelfitting.R` — fitting of sensitivity models.
- `04c_sensitivity_modelresults.R` — extraction and preparation of sensitivity results.

The exact interpretation of each analysis is given in the paper and its supplementary material.

## Data availability

This repository contains analysis code and selected supporting files.

Because restricted data are not provided, there is code to simulate all data (outcome, denominator, covarites) that is based on the real data.

## Running the analysis

The scripts are numbered to indicate the intended order. A typical workflow is:

1.  Obtain the required data and confirm that you are permitted to use them.
2.  Run the scripts in `00_simulatingData` if you want to test the workflow with simulated data.
3.  Run the covariate-processing scripts in `01_processCovariateData` only if you are using the real data. For simulated data you can omit this step.
4.  Run the relevant pipeline in `02_modelFitting`.

## Citation

If you use this code, please cite the accompanying BMJ Mental Health paper and this repository.
