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
pacman::p_load(dplyr,
               purrr, 
               stringr,
               tidyr,
               forcats,
               readr,
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
               rsvg, 
               cowplot)

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
               flextable,
               kableExtra, 
               rmarkdown, 
               rticles, 
               officer)

# Function conflict preferences
conflicted::conflict_prefer("filter", "dplyr", "base")

library(rmarkdown)
