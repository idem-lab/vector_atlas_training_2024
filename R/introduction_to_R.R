# Introduction to R


###############################################
#######
####### Part 1: The basics and base
#######
###############################################

# this is a comment
# anything written after a # will be ignored
# it is a useful way of storing notes

# This document is a script. It appears in the source window

# you should also see four other windows: console, files, and environment.

# the orientation might be different to this. You can modify this in:
# tools > global options > pane layout


# let's write some code:
1 + 1

# we can also enter it directly into the console

# we can also create an "object" to store this information using the
# assign function <-

new_object <- 1 + 1

new_object

new_object * 3

# R is an "object oriented" language
# so objects are how we move information around in R

# objects can be as simple as a number or much more complicated in structure

mtcars
LakeHuron

# Objects can have a variety of structures and "classes"

falciparum <- "falciparum"
falciparum
class(falciparum)

true_thing <- TRUE
class(true_thing)

one_to_ten <- 1:10
one_to_ten
class(one_to_ten)

pi
class(pi)

is.numeric(one_to_ten)
is.integer(one_to_ten)

is.integer(pi)

# most objects are in some way made of "vectors"
# i.e. concatenated sequences of numbers or characters of the same class

is.vector(1:10)

vec_1 <- c(1, 4, 55, 66, 777)

vec_2 <-  c(1,4,55,66,777)

vec_1 == vec_2


letters
class(letters)
sapply(letters, class)

# combinations of different class objects will be coerced to the
# most complex class
vec_3 <- c(TRUE, "A", 1)
vec_3
class(vec_3)
sapply(vec_3, class)


c(2:4, 5.11)
class(2:4)
class(5.11)
sapply(c(2:4, 5.11), class)
class(2)


# lists can contain different classes of object

list_1 <- list(TRUE, "A", 1)
list_1
sapply(list_1, class)


list_2 <- list(
  location = "icipe",
  temperature_degrees_c = 25,
  team = c("Lorna", "Dorcas", "Cindy"),
  daytime = TRUE
)

list_2

# missing data is handled by NA
NA

vec_4 <- c(1, NA, 3, NA, 1)

sum(vec_4)

sum(vec_4, na.rm = TRUE)

# R uses functions to do most of the work
# we have already used some functions above:
# sapply, class, is.vector, etc.

# we can also write our own functions

add_and_divide <- function(x, y, z){
  (x + y)/z
}

add_and_divide(1, 2, 3)

add_and_divide(
  x = 2,
  y = 4,
  z = 2
)

add_and_divide(x = 10, y = 20, z = 3)

# get information about a function by preceding it with ?
?sapply

# see what a function does by entering it in console
# without parentheses ()
add_and_divide
summary.data.frame





###############################################
#######
####### Part 2: The basics beyond base
#######
###############################################

# the extensible nature of R means there is (free!) software written
# to help you do virtually anything
# this can be functions, code in a paper, or packages

# e.g. tibble is an improvement on data.frame

install.packages("tibble")
library(tibble)

InsectSprays
class(InsectSprays)

insect_sprays <- as_tibble(InsectSprays)

insect_sprays


# the pipe operator |> allows for chains of stuff operations
# and is generally makes for more readable code
InsectSprays |>
  as_tibble()


# install.packages("tidyverse")
library(tidyverse)

diamonds

# we want to know the mean and sd of the per-carat price
# of diamonds for each combination of cut and color
# and show them all in order from worst to best quality


# this is how to do it with no pipes using tidyverse
print(
  arrange(
    summarise(
      group_by(
        mutate(
          diamonds,
          dollars_per_carat = price / carat
        ),
        cut,
        color
      ),
      avg_per_carat = mean(dollars_per_carat),
      sd_per_carat = sd(dollars_per_carat),
      .groups = "drop"
    ),
    cut,
    desc(color)
  ),
  n = 35
)

# this is the same code but written with a piped workflow:
diamonds |>
  mutate(
    dollars_per_carat = price / carat
  ) |>
  group_by(cut, color) |>
  summarise(
    avg_per_carat = mean(dollars_per_carat),
    sd_per_carat = sd(dollars_per_carat),
    .groups = "drop"
  ) |>
  arrange(
    cut, desc(color)
  ) |>
  print(
    n = 35
  )

# pipes are helpful and make working clearer!

# exercise: live code the above in base R


## Indexing and accessing parts of objects

mt <- mtcars |>
  mutate(
    model = rownames(mtcars)
  ) |>
  as_tibble() |>
  select(
    cyl,
    gear,
    hp,
    qsec,
    model
  ) |>
  nest(
    sub_table = c(
      model,
      qsec,
      hp
    )
  )

mt


mt$cyl

mt$sub_table

# indexing is via row, column, and starts at 1

mt[1,1]

mt[1,]
mt[,1]

mtcars[2:3,4:5]

# double brackets index list elements
# data frames are a special type of list where each element has the same length
mt[[1]]
mt$cyl

mt$sub_table[[1]]
mt[[3]][[1]]
mt[1,3]
mt[1,3][[1]][[1]]

mt[,"cyl"]

list_2

list_2$team
list_2[[3]]
list_2[[3]][[2]]


###############################################
#######
####### Part 3: Bringing in data
#######
###############################################

# reading in as a csv is easiest
raw_data_1 <- read.csv(
  file = "data/example_vector_survey_data_20230601.csv"
)

raw_data_1

class(raw_data_1)

head()

View(raw_data_1)

library(readr)
raw_data_2 <- read_csv(
  file = "data/example_vector_survey_data_20230601.csv"
)

raw_data_2

glimpse(raw_data_2)

#install.packages("readxl")
library(readxl)

raw_data_3 <- read_excel(
  path = "data/FIELD DATA EXAMPLE 2023 gerry and marianne villages.xlsx",
  sheet = "vill_gerry"
)

raw_data_3

raw_data_3 |>
  print(n = 99)


###############################################
#######
####### Part 4: doing things with data
#######
###############################################


table(raw_data_2$species, raw_data_2$count)

# Let's interrogate the results of this function
tapply(
  X = raw_data_2$count,
  INDEX = raw_data_2$species,
  fun = mean
)

# hide the answer below
#
#
#
#
#
#


tapply(
  X = raw_data_2$count,
  INDEX = raw_data_2$species,
  FUN = mean
)


iris3

apply(
  X = iris3,
  MARGIN = c(2,3),
  FUN = mean
)


model_1 <- glm(
  formula = count ~ species + village + method,
  data = raw_data_2,
  family = poisson
)

model_1

summary(model_1)

plot(model_1)

plot(raw_data_2$longitude_wgs84, raw_data_2$count)

plot(
  x = iris$Sepal.Length,
  y = iris$Sepal.Width,
  col = iris$Species
)
