# Fitting a spatial smoothing model

# 1. Read in some real insecticide susceptibility bioassay data, a nice clean
# subset of the the bioassay data from Moyes at al. (2019)
# https://doi.org/10.5061/dryad.dn4676s, restricted to Kenya, pyrethroids, and
# all records of members of the Anopheles gambiae complex together.

# 2. Read in some environmental covariates for Kenya, and plot the IR data over
# it.

# 3. Crop the spatial areas down to the region with sufficient IR data

# 4. Fit and plot a spatial-only model

# 5. Fit and plot a model with covariates and a spatial smooth


# load the tidyverse R package for data manipulation and visualisation
library(tidyverse)

# load the terra package, which we will use for manipulating rasters
library(terra)

# load the tidyterra package for nice visualisation of rasters
library(tidyterra)

# load the patchwork package for combining multiple plots together
library(patchwork)

# and load the mgcv package for spatial smoothing
library(mgcv)

# 1. Read in some real presence-absence data, a nice clean subset of the *old*
# Vector Atlas dataset, restricted to Kenya, collections of indoor resting
# mosquitoes, and combining all records of members of the Anopheles gambiae
# complex together.
ir_data_raw <- read_csv("data/ke_moyes_ir_data.csv")

# add on a calculation of mortality
ir_data <- ir_data_raw %>%
  mutate(
    mortality = died / tested
  )

head(ir_data)
view(ir_data)

# 2. Read in some environmental covariates for Kenya.

# We'll use the same low resolution bioclim covariate layers for Kenya as for
# the ENM practical
bioclim <- rast("data/rasters/bc_kenya_lores.tif")

# You can read more about these layers here:
# https://www.worldclim.org/data/bioclim.html

# lets plot the occurrence data over the top of the first layer: annual mean
# temperature.
ggplot() +
  geom_spatraster(data = bioclim$bio_1) +
  scale_fill_gradient(
    low = grey(0.9),
    high = grey(0.9),
    na.value = "transparent",
    guide = "none") +
  geom_point(
    aes(
      y = latitude,
      x = longitude,
      colour = mortality,
      size = tested
    ),
    data = ir_data,
    shape = 16
  ) +
  scale_colour_gradient(
    labels = scales::label_percent(),
    high = "blue",
    low = "orange"
  ) +
  theme_minimal()

# We can see that the datapoints are almost all in Western Kenya, except for
# two large samples in the same point NE of Nairobi
ir_data %>%
  filter(longitude > 36) %>%
  view()


# 3. Crop the spatial areas down to the region with sufficient IR data

# We know our models won't be able to predict across the whole country from
# these data, so we'll zoom in on Western Kenya

# We will crop the rasters, and the ir data, to a bounding box including Western
# Kenya
western_kenya_box <- c(xmin = 33.54,
                       xmax = 36.08,
                       ymin = -1.28,
                       ymax = 1.31)

bioclim_west <- terra::crop(bioclim,
                            western_kenya_box)

# and filter the data to the same region
ir_data_west <- ir_data %>%
  filter(
    latitude > western_kenya_box["ymin"],
    latitude < western_kenya_box["ymax"],
    longitude > western_kenya_box["xmin"],
    longitude < western_kenya_box["xmax"]
  )

# Let's plot again for this region.

# This time though, we'll assign the plot to an object. Then we can plot it, but
# also save the object to reuse later.

# We'll also define a theme to reuse - don't plot the x and y axes, or the
# gridlines
clean_theme <- theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank())

west_data_plot <- ggplot() +
  geom_spatraster(data = bioclim_west$bio_1) +
  scale_fill_gradient(
    low = grey(0.9),
    high = grey(0.9),
    na.value = "transparent",
    guide = "none") +
  geom_point(
    aes(
      y = latitude,
      x = longitude,
      colour = mortality
    ),
    data = ir_data_west,
    shape = 16,
    alpha = 0.75,
    size = 4
  ) +
  scale_colour_gradient(
    labels = scales::label_percent(),
    high = "blue",
    low = "orange"
  ) +
  ggtitle("Bioassays") +
  clean_theme

# Now plot it. Either of these will do the same thing:
# print(west_data_plot)
west_data_plot

# because we saved the ggplot object, we can add extra details to the plot, like
# splitting out years:
west_data_plot_years <- west_data_plot +
  facet_wrap(~year)

west_data_plot_years

# There are only two years with data, and they are close enough in time. So we
# can probably ignore temporal variation for now

# 4. Fit and plot a spatial-only model

# we can fit a spatial smoothing model using mgcv like this:
spatial_only <- mgcv::gam(
  formula = mortality ~ s(latitude, longitude, bs = "sos", k = 15),
  family = binomial,
  weights = ir_data_west$tested,
  data = ir_data_west)

# To do prediction from this model, we need to pass in sets of longitude and
# latitude values to the model, to predict with. The easiest way to do this is
# to to create rasters with each cell giving the values of the longitude and
# latitude

# make a raster of longitude and latitude, for prediction prediction
coords_raster <- c(terra::init(bioclim_west, "x"),
                   terra::init(bioclim_west, "y"))
coords_raster <- mask(coords_raster, bioclim_west[[1]])
names(coords_raster) <- c("longitude", "latitude")

# predict to this raster, and return both the prediction and the uncertainty
spatial_only_pred <- predict(coords_raster,
                             spatial_only,
                             se.fit = TRUE,
                             type = "response")

# let's plot the predictions
spatial_only_pred_plot <- ggplot() +
  geom_spatraster(data = spatial_only_pred$fit) +
  scale_fill_gradient(
    name = "mortality",
    labels = scales::label_percent(),
    high = "blue",
    low = "orange",
    limits = c(0, 1),
    na.value = "transparent") +
  ggtitle("Prediction") +
  clean_theme

spatial_only_pred_plot

# and the uncertainty

# fix the scale for the plot, to make uncertainty comparable between them
max_se_plot <- 2

spatial_only_pred_uncertainty_plot <- ggplot() +
  geom_spatraster(data = spatial_only_pred$se.fit) +
  scale_fill_gradient(
    name = "standard error",
    low = "lightgreen",
    high = "darkgreen",
    limits = c(0, max_se_plot),
    na.value = "transparent") +
  ggtitle("Uncertainty") +
  clean_theme

spatial_only_pred_uncertainty_plot

# combine these together into a multipanel plot
spatial_only_multi <- west_data_plot +
  spatial_only_pred_plot +
  spatial_only_pred_uncertainty_plot +
  plot_annotation(
    "Spatial only model"
  )

spatial_only_multi

# 5. Fit and plot a model with covariates and a spatial smooth

# first, we need to extract the covariate values again, and add them to our
# training data

ir_coordinates <- ir_data_west %>%
  select(x = longitude,
         y = latitude) %>%
  as.matrix()

# we'll use the scaled covariates, since it will be a little bit easier to
# interpret the model coefficients

covariate_values <- extract(bioclim,
                            ir_coordinates)

ir_data_west_covariates <- bind_cols(
  ir_data_west,
  covariate_values
)

# include annual rainfall as covariate in the model
spatial_env <- mgcv::gam(
  formula = mortality ~ bio_12 +
    s(latitude, longitude, bs = "sos", k = 15),
  family = binomial,
  # select = TRUE,
  weights = ir_data_west_covariates$tested,
  data = ir_data_west_covariates)

# to make predictions, we now need a raster with both the environmental
# covariates, and the coordinates
spatial_env_pred <- predict(c(bioclim_west, coords_raster),
                             spatial_env,
                             se.fit = TRUE,
                             type = "response")

# and make the equivalent prediction plots
# let's plot the predictions
spatial_env_pred_plot <- ggplot() +
  geom_spatraster(data = spatial_env_pred$fit) +
  scale_fill_gradient(
    name = "mortality",
    labels = scales::label_percent(),
    high = "blue",
    low = "orange",
    limits = c(0, 1),
    na.value = "transparent") +
  ggtitle("Prediction") +
  clean_theme

# and the uncertainty
spatial_env_pred_uncertainty_plot <- ggplot() +
  geom_spatraster(data = spatial_env_pred$se.fit) +
  scale_fill_gradient(
    name = "standard error",
    low = "lightgreen",
    high = "darkgreen",
    limits = c(0, max_se_plot),
    na.value = "transparent") +
  ggtitle("Uncertainty") +
  clean_theme

# combine these together into a multipanel plot
spatial_env_multi <- west_data_plot +
  spatial_env_pred_plot +
  spatial_env_pred_uncertainty_plot +
  plot_annotation(
    "Spatial + environment model"
  )

spatial_env_multi

# combine these two, plotting one above the other, and re-adding plot annotation
spatial_only_multi / spatial_env_multi +
  plot_annotation("Spatial (top) and spatial + environment (bottom) models")


