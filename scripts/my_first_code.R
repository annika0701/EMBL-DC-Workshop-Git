3 + 4

weight_kg <- 55

mickey <- 27
x2 <- 3

weight_kg <- 34
if
for

my_weight_kg <- 50

(my_weight <- 50)

my_weight

2.2 * weight_kg

weight_kg <- 57
2.2 * weight_kg

weight_lb <- 2.2 * weight_kg
# I have to update the value of the weight_kg object

weight_kg <- 100


weight_lb
mass <- 47.5
age <- 122


mass <- mass * 2.0
age <- age - 20
mass_index <- mass/age
weight_kg <- sqrt(9)
sqrt(9)

round(3.14159)
round(3.15159, digits = 2)
round (3.14159, 2)
round (digits =2, x=3.14159)
round (2, 3.14159)
round(4.1458694, 4)
mean(1,2,3)

weight_g <- c(50, 60, 65, 82)
animals <- c("mouse","rat", "dog")

#get the length of the vector
length(animals)
length(weight_g)

# get the type of data contained in the vector
class(animals)
class(weight_g)

#structure of the object
str(animals)

# how to add an element to the beginning of a vector
animals <- c("cincilla", animals)

animals <- c(animals, "frog")

typeof(animals)

num_char <- c(1,2, 3, "a")
class(num_char)
num_logical <- c(1, 2, 3, TRUE)
class(num_logical)
tricky <- c(1,2,3,"4")
tricky
# logical -> numeric -> character
# logical  -> character


# subsetting a vector
animals[3]

animals [c(1,3)]

more_animals <- animals[c(1,3,4,3,1,5)]

weight_g
weight_g[c(FALSE, FALSE, TRUE, TRUE)]
weight_g > 63
weight_g [weight_g > 63]
weight_g [weight_g > 63 & weight_g < 80]
weight_g [weight_g < 58 | weight_g > 80]
weight_g==65
# <, >, ==, !=, <=, >=

animals[animals =="rat" | animals == "frog"]

# %in% helps us find all elements corresponding to a vector of elements of our choice
animals %in% c("rat", "frog", "cat", "duck","dog")
animals [animals %in% c("rat", "frog", "cat", "duck","dog")]

# An example of a vector with missing data
heights <- c(2,4,4,NA,6)
mean(heights)
mean(heights, na.rm = T)
max(heights, na.rm =T)
min(heights, na.rm =T)

# identify which are the missing data (aka NA)
is.na(heights)
heights[!is.na(heights)]
na.omit(heights)
#extract the complete cases
complete.cases(heights)


#challenge
heights <- c(63, 69, 60, 65, NA, 68, 61)
heights_no_na <- na.omit(heights)
heights_no_na <- heights[!is.na(heights)]

median(heights, na.rm = T)
median(heights_no_na)
heights_no_na[heights_no_na > 67]
length(heights_no_na[heights_no_na > 67])
sum(heights_no_na > 67)
