library(stringr)
library(magrittr)

# str_extract(): extract pattern matches from character vecs
# str_detect(): id pattern matches from ch vecs
# str_replace(): replace pattern matches 

cars <- rownames(mtcars)
cars

cars %>%
  str_extract(pattern = "Merc")

cars %>%
  str_replace("Merc",  "####")


# escaping ---------------------
cat("hi\tthere")

cat("hi\\tthere")
# ------------------------------

cars %>%
  str_replace(".o",  "#o") # . matches any character

cars %>%
  str_replace_all(".o",  "#o") # . matches any character


cars <- c(cars, "my.specialcar")

cars

cars %>%
  str_replace_all(".\\.",  "#.")

cars %>%
  str_replace_all("[yv]o",  "#o") # yv matches y or v

cars %>%
  str_replace_all("[0-9][A-Z]",  "#@")

cars %>%
  str_replace_all("\\d",  "#")

cars %>%
  str_replace_all("\\s",  "") # whitespace

cars %>%
  str_replace_all("[^yv]o",  "#o") # negation


