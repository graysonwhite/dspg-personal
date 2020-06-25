library(tidyverse)

head(diamonds)

lm_res <- lm(price ~ carat + cut + carat:cut, data = diamonds)

# -------------------------------------------
# aside: formulas in R

f <- gamma ~ alpha + beta + delta:beta
print(f)
str(f)
class(f)

all.vars(f)  
# formula: a way of describing how
# variables (character vectors under the hood)
# should relate to one another
# "storage box" of var names, along with some
# syntax that can be used to understand them

# --------------------------------------------

print(lm_res)

anova_res <- anova(lm_res)
anova_res


str(lm_res)
str(anova_res)


pvals <- anova_res$`Pr(>F)`
pvals

class(lm_res)
class(anova_res)


# the print fcn is known as a "generic" fcn. 
# looks at the class attr, and if that is set, it calls another fcn
# specific to that class

# so print(lm_res) calls print.lm(lm_res)
# and print(anova_res) calls print.anova(anova_res)
  # if there was not print.anova() fcn, it would check the next class attr
  # which is data.frame, and it would try to run print.data.frame()

# methods("print") shows all print.* fcns

# class attrs determine the set of specialized fcns that are specific to that type of data
# those are called "methods"
# so, methods("print") shows all the specialized fcns that print() can be dispatched to

methods("print")


print(methods(class = "lm"))


plot(lm_res)



# writing our own class

person <- function(firstname, lastname, age, pets, favorite_nums) {
  l <- list()
  l$firstname <- firstname
  l$lastname <- lastname
  l$age <- age
  l$pets <- pets
  l$numpets <- length(pets)
  l$favorite_nums <- favorite_nums
  class(l) <- "human"
  return(l)
}

joe <- person("Joe", "Bob", 37, "Spot", c(13, 666, 91))

print(joe) # looks for print.human(), doesn't exist, so uses list print output
str(joe)

print.human <- function(l) {
  print("A Human")
  print(paste("Name:", l$firstname, l$lastname))
  print(paste("Age:", l$age))
  print(paste("Pets:", l$pets))
  print(paste("Favorite Numbers:", l$favorite_nums))
}

print(joe)



mean.human <- function(l) {
  return(mean(l$favorite_nums))
}

mean(joe)



sayhi <- function(l) {
  cat("Hi my name is", l$firstname, "nice to meet you!")
}

print(sayhi(joe))



feline <- function(firstname, lastname, color) {
  l <- list()
  l$firstname <- firstname
  l$lastname <- lastname
  l$color <- color
  class(l) <- "feline"
  return(l)
}

sterling <- feline("sterling", "cooper", "black")
sterling

print.feline <- function(l) {
  print(paste("A cat!", l$firstname, l$lastname, "they are", l$color))
}

print(sterling)

# this is the S3 system in R


# there is also an S4 object system
# works like S3, but sytax is more like python

# also S6 or "reference" classes. very much like python
# instance variables are by reference - that is they are changable

