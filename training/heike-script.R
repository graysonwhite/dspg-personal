attack3 <- function() {
  attack <- sort(sample(6, 3, replace = TRUE), decreasing = TRUE)
  defend <- sort(sample(6, 3, replace = TRUE), decreasing = TRUE)
  diff <- attack - defend
  return(
    sum(diff >= 0) - sum(diff < 0)
  )
}

rep <- replicate(100, attack3())
sum(rep > 0) / 100




library(discreteRV)
doublesix <- RV(c("0", "1"), probs = c(35/36, 1/36))
rolls <- SofIID(doublesix, n = 24, fractions = FALSE)
P(rolls >= 1)


x <- runif(n = 1000, min = 0, max = 10)
summary(x)
summary(qunif(p = c(0, .25, .5, .75, 1), min = 0, max = 10))


x <- matrix(c(315, 135, 1145, 452, 533, 269), byrow = TRUE, nrow = 3)
chisq.test(x)


