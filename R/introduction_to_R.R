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
  students = c("Cindy", "Dorcas", "Chelsea"),
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

# exercise: live code the above in base R


# pipes are helpful and make working clearer!



# indexing and $ referring
