# Custom contrasts example
# Quentin Read
# 27 April 2022

# Example run on a single model
# I don't know emmeans support for gge and those other classes of models, so I am doing this for just the simple lm in your anova_two_years.

# Also I did this example all with base R code to the extent possible. It could also be done in tidyverse way with purrr if you want.

source('packages.R')
tar_make()
tar_load(anova_two_years)

# Get a single example model and find the emmeans by genotype, ignoring interactions at the moment.
mod <- anova_two_years[[1]][[2]][[2]]$model
emm <- emmeans(mod, ~ GEN)

# We want to contrast each test cultivar individually with the mean of the checks.
# Looks like the checks are Dunphy, Dilday, and NC Raleigh.
# Extract the vector of GEN names from the emmeans object and find the indexes of the checks.
# Assign 1 to each of these indexes then divide by the number of checks so the vector sums to 1 (thus it has all zeroes except for 1/3 in three places)

checks <- c('Dilday', 'Dunphy', 'NC-Raleigh')
gen_names <- emm@grid[['GEN']]
check_mean <- as.numeric(gen_names %in% checks)
check_mean <- check_mean/sum(check_mean)

# Now create a vector for the mean of each of the test cultivars.
# This is just a vector of all zeroes except for a 1 in the appropriate location.

test_means <- lapply(gen_names[!gen_names %in% checks], function(n) as.numeric(gen_names %in% n))
names(test_means) <- gen_names[!gen_names %in% checks]

# The contrast vectors are created by subtracting the check mean vector from each of the test mean vectors.

contrast_vectors <- lapply(test_means, function(x) x - check_mean)

# Now do the custom contrast on the emmGrid object created earlier.
# We can do a multiple comparison adjustment in there that uses the appropriate number of tests 
# In this case it's 17 as we are comparing each of the 17 test cultivars to 3 check cultivars.
contrast(emm, method = contrast_vectors, adjust = 'sidak')

## Side note: I originally tried this with mod = anova_two_years[[1]][[2]][[1]]$model but NC Raleigh did not have an estimated marginal mean for that model. 
# So in that case the contrasts could not be estimated by averaging the mean of the three checks, as there were only two and one with a NA.
# I can't really diagnose this issue without really delving into the data.
# So in that case you'd probably want to get rid of NC Raleigh from the checks vector.