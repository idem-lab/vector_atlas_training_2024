# Spatial data and visualisation in R


###############################################
#######
####### Part 1: Basic Rasters and Vectors
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
####### Part 2: Reading in Rasters and Vectors
#######
###############################################

r2 <- rast("data/rasters/bc_kenya.tif")

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
####### Part 3: Manipulating Spatial Data
#######
###############################################

crs(v2) <- crs(r2)
crs(v2)

# projections ?!?!?!?!?
# https://omniatlas.com/blogs/stray-maps/football-projections/




###############################################
#######
####### Part 4: Visualisation with ggplot
#######
###############################################

###############################################
#######
####### Part 5: Visualising Spatial Data
#######
###############################################
