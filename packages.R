# Install pacman if it does not already exist
if(!require(pacman)){
  install.packages("pacman")
}

# Use pacman to load/install packages

# Workflow/organization
pacman::p_load(conflicted, 
               dotenv, 
               targets, 
               tarchetypes)

# Data wrangling
pacman::p_load(tidyverse, 
               here, 
               readxl, 
               janitor, 
               magrittr,
               openxlsx)

# Visualization
pacman::p_load(ggthemes, 
               lattice,
               visNetwork,
               ggpubr, 
               ggcorrplot, 
               patchwork,
               extrafont, 
               GGally, 
               scales, 
               DiagrammeRsvg, 
               DiagrammeR, 
               rsvg)

# Analysis
pacman::p_load(metan,
               agricolae, 
               lme4, 
               emmeans,
               agricolae)

# Writeup
pacman::p_load(rmdformats, 
               kableExtra, 
               pander, 
               kableExtra, 
               rmarkdown)

# Function conflict preferences
conflicted::conflict_prefer("filter", "dplyr", "base")

