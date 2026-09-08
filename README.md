README
================

# MicroplasticERA_water

This repository contains data and code used for RIVM letter report
2026-0069: Screening level ecological risk assessment of Microplastics
in Dutch surface waters, DOI:
[10.21945/RIVM-2026-0069](https://doi.org/10.21945/RIVM-2026-0069).

## Use

The markdown files provided contain descriptions and code for the
different steps of the analysis:

1.  Structuring gathered monitoring data

2.  Fitting a power law to the particle data per water body

3.  Rescaling reported microplastics concentrations to the 1 to 5000
    micrometres size range.

The required data is partly made available in the folder
[rawdata](/rawdata).

### Requirements

- **R** ([R Core Team 2025](#ref-rcoreteam2025)), **version** 4.5.2 was
  used.

- **Packages:** See the script `00_Install_dependencies.R` for a
  complete list, important ones are the tidyverse, ggplot, powerLaw and
  related packages.

- **`paths.R`**: Script to define your local input and output
  directories.  
  It should set the following variables:

  - `project_dir`: main output directory (e.g.,
    `"Documents/ERAwater_output/"`)

  - `Input_dir`: directory containing the raw measurement data (e.g.,
    `"Documents/ERAwater_rawdata"`)

  - `output_dir`: subdirectory of project_dir, which will contain the
    output data, tables and figures (e.g., `"Output/"`)

### Overview of files and folders

- [**`01_ParticleDataPrep.Rmd`**](01_ParticleDataPrep.md "01_ParticleDataPrep"):
  Make a harmonized dataset for microplastic particle number
  measurements using raw particle counting data, reported number
  concentrations and other studies specific metadata.
- [**`02_ParticleDataPowerlawFit.Rmd`**](02_ParticleDataPowerlawFit.md "02_ParticleDataPowerlawFit"):
  Fit power laws on particle measurement data prepared in
  `01a_ParticleDataPrep.Rmd` using a parallel computing approach
  specific to RIVM.
- **`02b_HPCrunPowerlawFit.R`**: converts
  `02a_ParticleDataPowerlawFit.Rmd` to an R-script, making it possible
  to run the script on the High Performance Cluster at RIVM.
- [**`03_NumberConcentrationRescaling.Rmd`**](03_NumberConcentrationRescaling.md "03_NumberConcentrationRescaling"):
  Corrects the particle number concentrations using the alphas fitted in
  `02_ParticleDataPowerlawFit.Rmd`, grouped per water body.
- [**`04_FiguresReportedMeasurements.Rmd`**](04_FiguresReportedMeasurements.md "04_FiguresReportedMeasurements"):
  Script to make figures for measurement data.
- [**`05_ReportTables.Rmd`**](05_ReportTables.md "05_ReportTables"):
  Script to make tables for measurement data.
- [**`06_MassDataPrep.Rmd`**](06_MassDataPrep.md "06_MassDataPrep"):
  Make a harmonized dataset for microplastic mass concentrations from
  reported mass concentration and study specific metadata as required to
  derive surfacewater microplastic particle number concentrations.
- [**`07_MassConcentrationAlignment.Rmd`**](07_NumberFromMassConcentration.md "07_NumberFromMassConcentration"):
  Converts mass concentrations to aligned particle concentrations using
  the alphas fitted in `02_ParticleDataPowerlawFit.Rmd.Rmd`, grouped per
  water body.
- **`references.bib`**
- **Folders**:
  - [**`rawdata`**](rawdata): Contains raw data to be published on
    GitHub as part of an external export from this repo.
  - **`R`**: contains specific functions used in various places.
  - [**`Output`**](Output): folder for output of data and figures. Here
    it contains the main dataset of reported and rescaled microplastics
    concentrations along with the output of the alpha fits as was used
    for RIVM report 2026-0069.
  - **`env`**: yaml files for creating HPC conda environment.
  - And the folders with figures for the different .md files.

### Parallel computing approach

For step 2 (`02_ParticleDataPowerlawFit.Rmd`) a high performance
computing cluster (HPC) was used.

#### HPC integration

It is advisable to run the code in the `02_ParticleDataPowerlawFit.Rmd`
script on a HPC. Note that for this we use a workaround, pointing the
HPC to the `02b_RunParallelPowerlawFitHPC.R` script. This script will
first extract the R code from the `02_ParticleDataPowerlawFit.Rmd`
script using purl. Then run the script.

The Parallel script uses the amount of cores in two ways:

- Total cores/ rivers for parallel computing
- Total cores split over all separate datasets in parallel computing
  inside a for loop

Make sure that the amount of cores in the script is the same as the
amount of cores in the HPC job.

Output is written to project_dir (as defined in `paths.R`) /Output

#### Example of running a HPC job

1.  load conda environment from the env folder by terminal command:

> `conda env create -f env/enverawater.yml -n erawater`

1.  Activate the environment

> `conda activate erawater`

1.  Run the HPC job Ensure you are in the project folder root.

> `bsub -q bio -n22 -W 54 -M 24GB -R "rusage[mem=24000]" -o ./hpcmessage/Output_%J.out -e ./hpcmessage/Output_%J.err Rscript 02c_RunParallelPowerlawFitHPC.R`

> Max time is usually around 15 minutes for 5 simulations using 4 cores
> per dataset. Adjust -W parameter accordingly

## Output of data and figures

Harmonised measurement datasets are saved to output_dir (as defined in
`paths.R`). Data and

figures are saved to output_dir (as defined in `paths.R`), organized by
topic in subfolders:

- Rescaled: output of the `03_NumberConcentrationRescaling.Rmd` script.
  Figures on rescaled concentrations.
- Alphas: output of the `03_NumberConcentrationRescaling.Rmd` script.
  Figures on the fitted alpha’s and correction factors.
- Measurements: output of the `04_FiguresReportedMeasurements.Rmd`
  script
- MassConversion: output of the `07_MassConcentrationAlignment.Rmd`
  script.

Tables are saved to output_dir (as defined in paths.R) /Tables.

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-rcoreteam2025" class="csl-entry">

R Core Team. 2025. *R: A Language and Environment for Statistical
Computing*. R foundation for statistical computing.

</div>

</div>
