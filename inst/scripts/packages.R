#
# This file is highly specific and only works with my file structure
# 
require(conflicted)
require(duckdb)
require(plibr)
require(knitr)
require(tidyverse)
require(dbplyr)
require(ggplot2)
require(DT)
require(kableExtra)

#
#
#
conflicted::conflicts_prefer(DT::JS)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::sql)
conflicted::conflicts_prefer(dplyr::lag)
