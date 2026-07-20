06_MassDataPrep
================

This script contains the code to prepare the data for conversion of mass
concentrations to particle number concentrations as described in RIVM
letter report 2026-0069.

# TED GC-MS data

The mass based data measured using TED-GC-MS are provided by
Rijkswaterstaat (RWS) for surface waters in the Netherlands. The data is
partly described in (Freriks, Oversteeg van, and Zemmelink 2023) and
contains the concentrations of microplastics in SPM as sampled using a
sediment box or centrifuge at Eijsden and Lobith.

``` r
# Read in raw RWS data
RWS_raw <- read_excel(path = file.path(Input_dir, "RWS_2025/20250520 TED GC-MS data voor RIVM.xlsx"),
  skip = 12,
  col_names = TRUE) 
```

``` r
# Add metadata per sample
MetaData <- read_excel(file.path(Input_dir, "RWS_2025/readme.xlsx"))

RWS_raw <- 
  RWS_raw |>
  left_join(MetaData) |>
  #Make sure data is date format
  mutate(
    Sampling_date_from = as.Date(Sampling_date_from),
    Sampling_date_to = as.Date(Sampling_date_to)
  )
```

# Suspended matter concentrations

Suspended matter concentrations in mg/L are read in Lobith and Eijsden.

``` r
suspended_matter <- 
  rbind(
    read.csv(
      file.path(Input_dir, "RWS_2025/zwevend stof gehalte Eijsden 2020-2024.csv"), sep = ";") |>
      mutate(Location_name = "Maas Eijsden"), 
    read.csv(
      file.path(Input_dir, "RWS_2025/zwevend stof gehalte Lobith 2020-2024.csv"), sep = ";") |>
      mutate(Location_name = "Rijn Lobith")) |>
  select(paroms, ehdcod, cpmoms, Location_name, datum, tijd, waarde) |>
  mutate(
    datum = case_when(
      nchar(datum) == 8 ~ sub("-(\\d)$", "-200\\1", datum),   
      nchar(datum) == 9 ~ sub("-(\\d{2})$", "-20\\1", datum), 
      TRUE ~ datum
    ),
    datum = as.Date(datum, format = "%d-%m-%Y")
  )

summary(suspended_matter)
```

    ##     paroms             ehdcod             cpmoms          Location_name     
    ##  Length:3637        Length:3637        Length:3637        Length:3637       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##      datum                tijd               waarde         
    ##  Min.   :2020-01-01   Length:3637        Min.   :1.000e+00  
    ##  1st Qu.:2021-02-15   Class :character   1st Qu.:5.000e+00  
    ##  Median :2022-04-04   Mode  :character   Median :1.100e+01  
    ##  Mean   :2022-04-03                      Mean   :1.375e+09  
    ##  3rd Qu.:2023-05-20                      3rd Qu.:2.000e+01  
    ##  Max.   :2024-07-09                      Max.   :1.000e+12

# Combine data

``` r
# Keep all suspended matter concentrations where the location is the same, and where date is between Sampling_date_from and Sampling_date_to
matched_suspended_matter_concentrations <- fuzzy_left_join(
  RWS_raw,
  suspended_matter,
  by = c(
    "Location_name" = "Location_name",
    "Sampling_date_from" = "datum",      
    "Sampling_date_to" = "datum"         
  ),
  match_fun = list(`==`, `<=`, `>=`)
)

# Calculate average suspended matter content per sample
matched_suspended_matter_concentrations <- matched_suspended_matter_concentrations |>
  group_by(across(-c(datum,tijd,  waarde))) |>
  summarise(SPM_mg_L_avg = mean(waarde),
            SPM_mg_L_median = median(waarde),
            SPM_mg_L_min = min(waarde),
            SPM_mg_L_max = max(waarde),
            SPM_n = n()) |>
  ungroup() 
```

The columns needed for calculating concentrations are not all numeric.
Below these columns are cleaned up.

``` r
# Remove all characters from columns that should be numeric
# RWS_data_with_SM_clean <- RWS_data_with_SM |>
#   mutate(across(where(is.character), ~ gsub("<", "", .)),
#          across(where(is.character), ~ gsub("blanco", NA, .)),
#          across(where(is.character), ~ gsub("LOD", NA, .)),
#          across(where(is.character), ~ gsub("ND", NA, .)),
#          across(where(is.character), ~ gsub("n.d.", NA, .))
#          ) 

# Above could be run, but code below throws warnings, but also returns NA when the value is not a number as it should.

to_numeric_cols <- c("NR", "PA6/66", "PE", "PET", "PMMA", "PP", "PS", "SBR", "Som")
matched_suspended_matter_concentrations <-
  matched_suspended_matter_concentrations |> 
  # convert to NA when not a number
  mutate(across(all_of(to_numeric_cols), as.numeric)) |> 
  # calculate own sum.
  mutate(
    Polymer_sum_calculated = rowSums(across(all_of(c("NR", "PA6/66", "PE", "PET", "PMMA", "PP", "PS", "SBR"))), na.rm = TRUE)
  ) |> 
  select(-c("Bemonsterd", "van:", "tot:", "Locatie:","Omschrijving",
             "paroms", "ehdcod", "cpmoms", "Location_name.y")) |> 
  rename(Location_name = Location_name.x)
```

# Sampling recovery

The following recovery percentages are reported:

- Sediment box: 71% - 94% (Harhash et al. 2023)

- Centrifuge: 86% - 100% (Harhash et al. 2023)

``` r
## remove rows with no PP or no SPM data
matched_suspended_matter_concentrations <-
  matched_suspended_matter_concentrations |> 
  filter(!(is.na(SPM_mg_L_avg) | is.na(PP)))

RecoverySampling <- tibble(
  Sampling_method = "sediment kist",
  Recovery_min = 0.1, 
  Recovery_max = 0.9 
) |> rbind(tibble(
  Sampling_method = "centrifuge",
  Recovery_min = 0.86, 
  Recovery_max = 1.0 
))

matched_suspended_matter_concentrations <-
  matched_suspended_matter_concentrations |> 
  left_join(RecoverySampling)

PP_SPM_Concentrations <- matched_suspended_matter_concentrations |>
  pivot_longer(cols=c("NR", "PA6/66", "PE", "PET", "PMMA", "PP", "PS", "SBR", "Som", "Polymer_sum_calculated"),
               names_to = "Polymer",
               values_to = "Concentration_g_kg") |> 
  filter(Polymer == "PP")
```

The combined data is saved for use in
07_NumberFromMassConcentration.Rmd.

``` r
writexl::write_xlsx(PP_SPM_Concentrations, 
                    path = file.path(output_dir, "Data",paste0(Sys.Date(),
                                     "-PP_SPM_Concentrations.xlsx") ))
write_rds(PP_SPM_Concentrations, 
                    file = file.path(output_dir, "Data",paste0(Sys.Date(),
                                     "-PP_SPM_Concentrations.rds")))
```

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-freriks2023" class="csl-entry">

Freriks, I, C Oversteeg van, and H Zemmelink. 2023. “Op Weg Naar
Microplastics Monitoring in Rivieren - Deel 2: Analyse van
Microplastics.”
<https://open.rijkswaterstaat.nl/open-overheid/@259816/weg-microplastics-monitoring-rivieren/>.

</div>

<div id="ref-harhash2023" class="csl-entry">

Harhash, Mohamed, Henning Schroeder, Alexander Zavarsky, Jan Kamp,
Annika Linkhorst, Tim Lauschke, Georg Dierkes, Thomas A. Ternes, and
Lars Duester. 2023. “Efficiency of Five Samplers to Trap Suspended
Particulate Matter and Microplastic Particles of Different Sizes.”
*Chemosphere* 338 (October): 139479.
<https://doi.org/10.1016/j.chemosphere.2023.139479>.

</div>

</div>
