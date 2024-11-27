# Fitting and criticising an environmental niche model

# 1. Read in some real presence-absence data, a nice clean subset of the *old*
# Vector Atlas dataset, restricted to Kenya, collections of indoor resting
# mosquitoes, and combining all records of members of the Anopheles gambiae
# complex together.

# 2. Read in some environmental covariates for Kenya.

# 3. Visualise the environmental covariates, and how much they vary across
# Kenya.

# 4. Calculate the values of the environmental covariates at the locations where
# the occurrence data were collected, and visualise how they compare with
# presence and absence status

# 5. Fit a presence-absence environmental niche model.

# 6. Use the environmental niche model to make a spatial prediction of the
# species in Kenya.

# 7. Criticise the model to see if it is any good.

# load the tidyverse R package for data manipulation and visualisation
library(tidyverse)

# load the terra package, which we will use for manipulating rasters
library(terra)

# and load the tidyterra package for nice visualisation of rasters
library(tidyterra)


# OK, let's get started!


# 1. Read in some real presence-absence data, a nice clean subset of the *old*
# Vector Atlas dataset, restricted to Kenya, collections of indoor resting
# mosquitoes, and combining all records of members of the Anopheles gambiae
# complex together.
pa_data <- read_csv("data/ke_gambiae_old_va_data.csv")

# look at the first 6 rows
head(pa_data)

# scroll through all the data
View(pa_data)

# summarise the column types and values
summary(pa_data)

# 2. Read in some environmental covariates for Kenya.

# We'll use the 'rast' function from the terra package, to load the bioclim
# covariate layers that describe the average climate of Kenya
bioclim <- rast("data/rasters/bc_kenya_lores.tif")

# You can read more about these layers here:
# https://www.worldclim.org/data/bioclim.html

# NOTE: this is a lower-resolution version of the original Bioclim data, and
# what Gerry used yesterday. We reduced the resolution to make the downloads
# easier.

# lets plot the occurrence data over the top of the first layer: annual mean
# temperature
ggplot() +
  geom_spatraster(data = bioclim$bio_1) +
  scale_fill_gradient(
    low = grey(0.6),
    high = grey(0.9),
    na.value = "transparent",
    guide = "none") +
  geom_point(
    aes(
      y = latitude,
      x = longitude,
      colour = present
    ),
    data = pa_data,
    shape = 16
  ) +
  scale_colour_manual(
    name = "An. gambiae present",
    values = list(
      "TRUE" = "black",
      "FALSE" = "white"
    )
  ) +
  theme_minimal()


# What do you think of our amazing dataset for vector mapping?!

# It's not ideal for mapping the distributions over all the climatic conditions
# in Kenya is it? We'll discuss this later.

# 3. Visualise the environmental covariates, and how much they vary across
# Kenya.

# plot one raster at a time
layer_to_plot <- 1
ggplot() +
  geom_spatraster(data = bioclim[[layer_to_plot]]) +
  scale_fill_gradient(
    name = names(bioclim)[layer_to_plot],
    low = "green",
    high = "purple",
    na.value = "transparent") +
  theme_minimal()

# plot several rasters together, for a subset of the bioclim layers
several_layers_to_plot <- 1:4
ggplot() +
  geom_spatraster(data = bioclim[[several_layers_to_plot]]) +
  facet_wrap(~lyr) +
  scale_fill_gradient(
    name = "value",
    low = "green",
    high = "purple",
    na.value = "transparent") +
  theme_minimal()

# it is a bit hard to see the differences in some plots, because the layers all
# have different ranges of values

# A common solution is to 'scale' the covariates, to create new covariates (by
# subtracting the mean and dividing by the standard deviation) with the same
# pattern, but where the average cell value is 0 and the variance is 1

bioclim_scaled <- scale(bioclim)

ggplot() +
  geom_spatraster(data = bioclim_scaled[[several_layers_to_plot]]) +
  facet_wrap(~lyr) +
  scale_fill_gradient(
    name = "value (scaled)",
    low = "green",
    high = "purple",
    na.value = "transparent") +
  theme_minimal()


# 4. Calculate the values of the environmental covariates at the locations where
# the occurrence data were collected, and visualise how they compare with
# presence and absence status

# First, we need to 'extract' the values of the covariates at the latitude and
# longitude of our sampling locations, using the terra 'extract' function
?terra::extract

# we need a two-column matrix of our longitude and latitude (in that order!)

occurrence_coordinates <- pa_data %>%
  select(x = longitude,
         y = latitude) %>%
  as.matrix()

nrow(pa_data)
nrow(occurrence_coordinates)

head(occurrence_coordinates)

# we'll use the scaled covariates, since it will be a little bit easier to
# interpret the model coefficients

covariate_values <- extract(bioclim_scaled,
                            occurrence_coordinates)
head(covariate_values)
nrow(covariate_values)

# we can add these on as columns to our dataset, sop we are ready to compare
# columns and do modelling
pa_data_with_covariates <- bind_cols(
  pa_data,
  covariate_values
)
head(pa_data_with_covariates)
nrow(pa_data_with_covariates)
view(pa_data_with_covariates)


# lets plot presence and absence against each covariate
ggplot() +
  # points, with 'present' (TRUE/FALSE) on the y axis, and change the bioclim
  # layer on the x axis
  geom_point(
    aes(
      x = bio_1,
      y = present
    ),
    # let's make the points big, but transparent, so we can see them even when
    # they overlap
    size = 3,
    alpha = 0.25,
    data = pa_data_with_covariates
  ) +
  theme_minimal()

# now let's plot presence/absence against two covariates at once
ggplot() +
  # points, with 'present' (TRUE/FALSE) on the y axis, and change the bioclim
  # layer on the x axis
  geom_point(
    aes(
      x = bio_9,  # Avg temperature in driest quarter (3 months)
      y = bio_13, # Rainfall in wettest month
      colour = present
    ),
    # let's make the points big, but transparent, so we can see them even when
    # they overlap
    size = 3,
    alpha = 0.75,
    data = pa_data_with_covariates
  ) +
  # fix the X and Y axes to have the same scale (because we already scaled the
  # covariates)
  coord_fixed() +
  theme_minimal()


# 5. Fit a presence-absence environmental niche model.

# If you want to change the model, you can change the covariates that are named
# in the formula argument below

enm_model <- glm(
  # the formula lists (on the left) the column with the presence/absence
  # information and (on the right) which covariates we want to include. These
  # have to be separated by a '+'
  formula = present ~ bio_9 + bio_13,
  # we set family to 'binomial' to tell the glm function that we want to fit a
  # presence-absence (binary) model, this uses the ilogit() link function trick,
  # like we used earlier
  family = binomial,
  # we tell it the data object where it can find the things named in the formula
  data = pa_data_with_covariates)

# we can find the model coefficients like this:
enm_model$coefficients
# The '(Intercept)' term is the first number in our formula, and the other two
# are the coefficients for each covariate

# E.g. if you see:
# (Intercept)       bio_9      bio_13
# 0.4052588   2.7023826   0.7165039

# then we have the model:
# p(Vector_present) = ilogit(0.4052588 + 2.7023826 * bio_9 + 0.7165039 * bio_13)

# You shouldn't need this, but in case you want to try something on your own, in
# R the ilogit function is: plogis(). Or you can create your own like this:
#   ilogit <- function(x) {1 / (1 + exp(-x))}

# 6. Use the environmental niche model to make a spatial prediction of the
# species in Kenya.

# We could use the formula above to calculate the value in each pixel for
# ourselves. But fortunately, the terra R package makes it really easy for us,
# we can use the predict function, like this:

p_vector_present <- predict(
  # first we pass as 'object' the raster object that has all the covariates we
  # used, with the same names. Remember we extracted the values from the
  # *scaled* raster, so we must use this here or we will break the mathematics
  # of our model.
  object = bioclim_scaled,
  # next we give it the model object
  model = enm_model,
  # finally, we tell it we want predictions on the 'response' scale, this means
  # *after* squishing it through the ilogit() function. If we left it at the
  # default 'link' scale, it would predict the values of the linear model before
  # squishing
  type = "response")

# let's give this raster a better name than the default one
names(p_vector_present) <- "p(Vector_present)"

# Let's plot it!
ggplot() +
  geom_spatraster(data = p_vector_present) +
  scale_fill_gradient(
    name = "p(Vector_present)",
    low = grey(0.9),
    high = "blue",
    na.value = "transparent") +
  theme_minimal()

# You can try changing the covariates used in the model above to see how the
# prediction changes

# 7. Let's criticise the model, to see if it is any good.

# First, we can summarise the model coefficients. The sign can tell us whether
# presence is associated with a higher value of the covariate (positive sign),
# or a lower value of the covariate (negative sign). Because we have
# *scaled* the covariates, the relative sizes of the coefficients are a little
# interpretable in terms of how much the model relies on each covariate to
# explain spatial variation)

enm_model$coefficients[-1]

# if you like p-values, you can get those too - but do you know how to interpret
# them?
summary(enm_model)

# plot the fitted values versus the data. The original data (transparent dots)
# are presence (1) and absence (0), but we can compare against the fitted values
# of the proportion of sites with presence by aggregating those data over some
# intervals (fitted values 0-0.25, 0.25-0.5, 0.5-0.75, 0.75-1, the vertical
# lines) and calculate what proportion of records actually have presences in
# those intervals. We plot these as the pink squares. They align quite well.
bin_breaks <- seq(0, 1, length.out = 5)
pa_data_with_covariates %>%
  mutate(
    observed_p_present = as.numeric(present),
    fitted_p_present = enm_model$fitted.values
  ) %>%
  ggplot(
    aes(
      x = fitted_p_present,
      y = observed_p_present
    )
  ) +
  stat_summary_bin(
    fun = "mean",
    geom = "point",
    shape = 15,
    size = 3,
    colour = "hotpink",
    breaks = bin_breaks
  ) +
  geom_vline(
    xintercept = bin_breaks,
    col = grey(0.6)
  ) +
  geom_point(
    size = 3,
    alpha = 0.25
  ) +
  theme_minimal()


# Validation statistics

# There are lots of different validation metrics available. To understand which
# will be best for your application (e.g. presence/absence versus presence-only
# modelling), I suggest referring to this paper: Lawson et al.
# https://doi.org/10.1111/2041-210X.12123 (see e.g. Table 2).

# For now we will use MSE: 'mean squared error', which is implemented in the
# mse() function, in the cv package. Smaller numbers of MSE indicate a better
# fit. However, we can't use the number to tell us whether the fit is 'good
# enough'.

library(cv)

# The easiest *but the most misleading* validation approach is internal
# validation: compare the predictions against the data the model was fitted too.

mse_internal_orig <- mse(
  # y = the presence absence data we are comparing to
  y = pa_data_with_covariates$present,
  # yhat = the predicted probability of presence
  yhat = enm_model$fitted.values)
mse_internal_orig

# This internal validation metric lets us measure how close the model fit is to
# the data it was trained on. But this does not tell us how well we can predict
# to new locations. The whole point of mapping is to predict to new locations,
# so this is a problem!

# Note that we can always make the *internal* validation better by including
# more covariates (and therefore more coefficients) and making the model more
# complex, but this does not normally make our predictions better - often it
# makes our predictions worse.

# Let's create a new model that's really complicated, using all the possible
# covariates:
enm_model_complex <- glm(formula = present ~ bio_1 + bio_2 + bio_3 + bio_4 +
                           bio_5 + bio_6 + bio_7 + bio_8 + bio_9 + bio_10 +
                           bio_11 + bio_12 + bio_13 + bio_14 + bio_15 + bio_16 +
                           bio_17 + bio_18 + bio_19,
                         family = binomial,
                         data = pa_data_with_covariates)

# We get a warning that the model is so closely fitting to the training data
# that it predicts a probability of 0 for some absence datapoints and/or 1 for
# some absence points! It thinks it is impossible to observe a different outcome
# at these locations

# Warning message:
#   glm.fit: fitted probabilities numerically 0 or 1 occurred

# let's see how the map looks, by predicting back to the whole raster:
p_vector_present_complex <- predict(
  object = bioclim_scaled,
  model = enm_model_complex,
  type = "response")

ggplot() +
  geom_spatraster(data = p_vector_present_complex) +
  scale_fill_gradient(
    name = "p(Vector_present)",
    low = grey(0.9),
    high = "blue",
    na.value = "transparent") +
  theme_minimal()

# now we compute the internal validation metric
mse_internal_complex <- mse(
  y = pa_data_with_covariates$present,
  # use the predicted probability of presence from the complex model
  yhat = enm_model_complex$fitted.values)

mse_internal_orig
mse_internal_complex

# the complex model has a better validation statistic (lower MSE), as expected

# Let's see how well these two models do at predicting to new data. Since we
# already used all of our data, we'll split it in two and re-fit the models to
# half, and keep the remaining half for testing.

# We'll *randomly* select half of the data for training, and use the rest
# for testing.

n_total <- nrow(pa_data_with_covariates)
n_training <- round(n_total / 2)

# sample some datapoints at random, making sure we will all get the same random
# fraction
set.seed(12321)
training_index <- sample.int(n_total, n_training, replace = FALSE)

training_data <- pa_data_with_covariates[training_index, ]
testing_data <- pa_data_with_covariates[-training_index, ]

nrow(training_data)
head(training_data)
summary(training_data$present)

nrow(testing_data)
summary(testing_data$present)

# now we refit our two models
enm_model_external <- glm(
  # trick: we can re-use the formula from the earlier model
  formula = enm_model$formula,
  family = binomial,
  # amke sure we use the training data
  data = training_data)

enm_model_complex_external <- glm(
  formula = enm_model_complex$formula,
  family = binomial,
  data = training_data)
# more warnings about overfitting!

# now we predict from these to with the testing data (we can't cheat by using
# the fitted values this time, because we are comparing to a 'new' dataset)

enm_model_external_preds <- predict(enm_model_external,
                                    newdata = testing_data,
                                    type = "response")

enm_model_complex_external_preds <- predict(enm_model_complex_external,
                                            newdata = testing_data,
                                            type = "response")

# let's compute the external, or out-of-sample validation statistics, comparing
# these predictions against the real outcomes in the testing data
mse_external_orig <- mse(y = testing_data$present,
                         yhat = enm_model_external_preds)

mse_external_complex <- mse(y = testing_data$present,
                            yhat = enm_model_complex_external_preds)

mse_external_orig
mse_external_complex
# we can see that the simpler model is much better at predicting to new
# locations (it has a lower MSE)


# In practice, it's normally a good idea to do this external validation lots of
# times with different random samples, to make sure the result isn't just a
# glitch of the random training/test split we chose. This is called
# cross-validation, and there are nice R packages to help with it (e.g. the cv
# package)

# cross-validation
mse_cv_orig <- cv::cv(
  # just give it the original model - it will re-fit to the new datasets for us
  model = enm_model,
  # give it the full dataset
  data = pa_data_with_covariates,
  # how many different 'folds' or replicates should we split into?
  K = 10,
  # which metric should we use
  criterion = mse,
  # we'll make sure both models are evaluated with the same random partitions
  seed = 123)

# now for the overfitted model
mse_cv_complex <- cv::cv(
  model = enm_model_complex,
  data = pa_data_with_covariates,
  K = 10,
  criterion = mse,
  # we'll make sure both models are evaluated with the same random partitions
  seed = 123)

mse_cv_orig
mse_cv_complex
# again, the simpler model performs better


