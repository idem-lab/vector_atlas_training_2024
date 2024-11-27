# Spatial data and visualisation in R

# We need some packages - if the following lines fail, select and run the
# commented out line to install the package.
library(terra)
# install.packages("terra")
library(geodata)
# install.packages("geodata")
library(tidyterra)
# install.packages("tidyterra")


###############################################
#######
####### Part 1: Visualisation and ggplot
#######
###############################################

# let's bring back a non-spatial dataset to plot
library(tidyverse)

raw_data_2 <- read_csv(
  file = "data/example_vector_survey_data_20230601.csv"
)

# you already met the base plotting function plot()

plot(
  x = raw_data_2$count
)

hist(x = raw_data_2$count) # histogram
hist(x = raw_data_2$count, breaks = 20)
hist(x = raw_data_2$count, breaks = 100)

plot(
  x = factor(raw_data_2$species),
  y = raw_data_2$count
)

# you can use model-like expression
plot(raw_data_2$count~factor(raw_data_2$species))

# infinitely customisable...
plot(
  x = factor(raw_data_2$species),
  y = raw_data_2$count,
  xlab = "Species",
  ylab = "Abundance",
  las = 1
)

boxplot(
  count ~ species,
  data = raw_data_2,
  las = 1
)

# however, we are going to suggest the ggplot2 package
library(ggplot2)
# https://ggplot2.tidyverse.org

ggplot()

ggplot(data = raw_data_2)

ggplot(data = raw_data_2) +
  geom_boxplot(
    aes(
      x = species,
      y = count
    ) # aes is for aesthetic
  )

# default outputs often attractive

ggplot(data = raw_data_2) +
  geom_boxplot(aes(species, count)) +
  facet_grid(village~.) +
  scale_y_log10()

# do you remember the pipe (%>%)?

ggplot(
  data = raw_data_2 |>
    filter(count != 0)
) +
  geom_boxplot(
    aes(
      x = species,
      y = count
    )
  ) +
  facet_grid(village~.) +
  scale_y_sqrt()

my_plot <- ggplot(data = raw_data_2) +
  geom_boxplot(
    aes(species, count)
  ) +
  facet_grid(village~method) +
  scale_y_sqrt()

ggsave(my_plot, filename = "save_this_plot.png")


# ggplot is particularly good for easy layering and
ggplot(data = raw_data_2) +
  geom_boxplot(
    aes(species, count)
  ) +
  geom_jitter( # new 'geom' or layer
    aes(species, count)
  ) +
  facet_grid( # introduce another factor to subdivide data
    village ~ method
  ) +
  scale_y_sqrt()

#install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)

ggplot(
  data = raw_data_2 |>
    mutate(date = as.factor(date_yyyymmdd)) |>
    filter(method == "cdc_light_trap")
) +
  geom_col(
    aes(
      x = species,
      y = sqrt(count),
      fill = date
    ),
    position = "dodge"
  ) +
  facet_grid( # introduce another factor to subdivide data
    village ~.
  ) +
  scale_fill_excel_new() +
  theme_excel()


###############################################
#######
####### Part 2: Basic Rasters and Vectors
#######
###############################################

m1 <- matrix(
  data = c(
    0,0,0,1,
    0,0,1,2,
    0,1,1,1,
    0,0,0,1
  ),
  ncol = 4,
  nrow = 4,
  byrow = TRUE # also hash out byrow and look at the difference
)

m1

library(terra)
# https://rspatial.org

r1 <- rast(m1)
r1

plot(r1)

vals1 <- values(r1)

vals1


t1 <- tibble(
  x = c(  1,   2, 2.2, 3.7),
  y = c(0.2, 1.2, 1.1, 2.9)
)

t1

v1 <- vect(
  x = t1,
  geom = c("x", "y")
)

v1

plot(v1)

plot(r1)
points(v1, col = "deeppink", cex = 3)

###############################################
#######
####### Part 3: Reading in Rasters and Vectors
#######
###############################################

r2 <- rast("data/rasters/bc_kenya_123.tif")

r2

plot(r2)

names(r2)
ext(r2)
crs(r2)
res(r2)


r2$wc2.1_30s_bio_1

r2[["wc2.1_30s_bio_1"]]

plot(r2$wc2.1_30s_bio_1)

plot(r2[[c("wc2.1_30s_bio_1", "wc2.1_30s_bio_2")]])


t2 <- read_csv("data/example_vector_survey_data_20230601.csv")

t2

v2 <- vect(
  x = t2,
  geom = c("longitude_wgs84", "latitude_wgs84")
)

v2
crs(v2)



###############################################
#######
####### Part 4: Manipulating Spatial Data
#######
###############################################

# v2 has no coordinate reference system - let's give it one
crs(v2) <- crs(r2)
crs(v2)

# projections ?!?!?!?!?
# https://omniatlas.com/blogs/stray-maps/football-projections/

# install.packages("geodata")
library(geodata)

# let's download Kenya county boundaries
kenya_admin <- gadm(
  country = "KEN",
  level = 1,
  path = "data/gadm",
  resolution = 2
)

kenya_admin

plot(kenya_admin)

# give bioclimactic data clearer names
bioclim_kenya <- r2
names_bioclim_old <- names(bioclim_kenya)

# remove the unnecessary
better_names <- sub(
  pattern = "wc2\\.1_30s_",
  replacement = "",
  names_bioclim_old
)

better_names

names(bioclim_kenya) <- better_names

library(tidyterra)

# get just the turkana county
turkana <- kenya_admin |>
  filter(NAME_1 == "Turkana")

# extract (mask) the bioclim data only to turkana
bioclim_turkana <- mask(
  x = bioclim_kenya,
  mask = turkana
)

bioclim_turkana


plot(bioclim_turkana[[1]])

# look at it side-by-side
plot(c(bioclim_kenya[[1]], bioclim_turkana[[1]]))

# show the admin boundary
plot(c(bioclim_kenya[[1]], bioclim_turkana[[1]]))
plot(turkana, add = TRUE)

plot(bioclim_kenya[[1]])
plot(turkana, add = TRUE)

plot(bioclim_turkana[[1]])

#look at extents
ext(turkana)
ext(bioclim_turkana)
ext(bioclim_kenya)

bioclim_turkana_cropped <- crop(
  x = bioclim_turkana,
  y = turkana
)

ext(bioclim_turkana_cropped)
ext(bioclim_turkana)

plot(bioclim_turkana_cropped[[1]])

bioclim_turkana_piped <- bioclim_kenya |>
  mask(
    mask = turkana
  ) |>
  crop(
    y = turkana
  )

bioclim_turkana_piped
plot(bioclim_turkana_piped[[1]])


bioclim_turkana_low_resolution <- aggregate(
  bioclim_turkana_piped[[1]],
  fact = 20,
  fun = "mean"
)

plot(bioclim_turkana_low_resolution)

plot(
  c(
    bioclim_turkana_piped[[1]],
    bioclim_turkana_low_resolution
  )
)

ext(bioclim_turkana_low_resolution)
ext(bioclim_turkana_piped)

###############################################
#######
####### Part 5: Visualising Spatial Data
#######
###############################################

# let's just work with one layer
annual_mean_temp <- bioclim_kenya$bio_1

plot(annual_mean_temp)


# base plot gives lots of utiliy

?plot

plot(
  annual_mean_temp,
  col = rainbow(12),
  grid = TRUE
)



# tidyterra gives ggplot utility to terra spatial data
library(tidyterra)
# https://dieghernan.github.io/tidyterra/index.html

ggplot() +
  geom_spatraster(
    data = annual_mean_temp
  )
# still pretty ugly

ggplot() +
  geom_spatraster(
    data = annual_mean_temp
  ) +
  scale_fill_viridis_c(
    option = "D"
  )


ggplot() +
  geom_spatraster(
    data = annual_mean_temp
  ) +
  scale_fill_viridis_c(
    option = "D",
    na.value = "white"
  )

ggplot() +
  geom_spatraster(
    data = annual_mean_temp
  ) +
  scale_fill_viridis_c(
    option = "B",
    na.value = "white",
    guide = guide_colorbar(title = "Degrees\nCelsius")
  ) +
  theme_void() +
  labs(
    title = "Kenya Annual Mean Temperature"
  )

ggplot() +
  geom_spatraster(
    data = annual_mean_temp
  ) +
  scale_fill_viridis_c(
    option = "B",
    na.value = "white",
    guide = guide_colorbar(title = "Degrees\nCelsius")
  ) +
  theme_void() +
  labs(
    title = "Kenya Annual Mean Temperature"
  ) +
  geom_spatvector(
    data = kenya_admin
  )

ggplot() +
  geom_spatraster(
    data = annual_mean_temp
  ) +
  scale_fill_viridis_c(
    option = "B",
    na.value = "white",
    guide = guide_colorbar(title = "Degrees\nCelsius")
  ) +
  theme_void() +
  labs(
    title = "Kenya Annual Mean Temperature",
    caption = "County boundaries outlines in grey"
  ) +
  geom_spatvector(,
    data = kenya_admin,
    fill = "transparent",
    col = "grey40"
  )

