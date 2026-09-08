library(knitr)
library(tidyverse)
library(poweRlaw)
library(ggplot2)
library(readxl)
library(stringr)
library(dplyr)

dir.create("purl", recursive = TRUE, showWarnings = FALSE)

knitr::purl("02_ParticleDataPowerlawFit.Rmd", output = "./purl/02_ParticleDataPowerlawFit.R")
source("./purl/02_ParticleDataPowerlawFit.R")
