library(fst)
library(tidyverse)
library(ipumsr) # needed to pull ipums data which is the source of income for fusionRECS
library(utils) # Extract files from or list the contents of a tar archive. Public release of the fusionACS pseudo-data (https://github.com/ummel/fusionACS/releases) (with the variables listed in the fusionACS data dictionary???) are available in .tar format -> would not open. I was able to just "extract all" in file explorer
library(arrow) # to open the parquet files from the .tar folder
library(survey)
library(srvyr)