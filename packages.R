# Install pacman if it does not already exist
if(!require(pacman)){
  install.packages("pacman")
}

# Use pacman to load/install packaged
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
               extrafont, 
               GGally, 
               agricolae, 
               lme4, 
               emmeans, 
               kableExtra, 
               pander)

# Function preference
conflicted::conflict_prefer("filter", "dplyr", "base")

