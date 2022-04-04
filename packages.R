# Install pacman if it does not already exist
if(!require(pacman)){
  install.packages("pacman")
}

# Use pacman to load/install packages
pacman::p_load(conflicted, 
               dotenv, 
               targets, 
               tarchetypes, 
               tidyverse, 
               here, 
               readxl, 
               janitor, 
               metan, 
               rmdformats, 
               openxlsx, 
               ggthemes, 
               lattice, 
               ggpubr, 
               ggcorrplot, 
               patchwork,
               extrafont, 
               GGally, 
               agricolae, 
               lme4, 
               emmeans, 
               kableExtra, 
               pander, 
               visNetwork,
               kableExtra, 
               agricolae, 
               magrittr)

# Function preference
conflicted::conflict_prefer("filter", "dplyr", "base")

