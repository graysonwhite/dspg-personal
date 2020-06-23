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

qnorm(p = .95)
qnorm(p = 0.975)
qnorm(p = .995)

library(tidyverse)
data(barley, package = "lattice")
b31 <- subset(barley, year == 1931)

b31 <- b31 %>%
  mutate(lyield = log(yield))

ggplot(b31, aes(x = yield)) + 
  geom_dotplot()

ggplot(b31, aes(x = lyield)) + 
  geom_dotplot()

ci_summary <- b31 %>%
  summarize(mean = mean(lyield),
            sd = sd(lyield))
ci <- ci_summary$mean + c(-1, 1)*2*ci_summary$sd/sqrt(60)
ci

yield_ci <- exp(ci)
yield_ci




crabs <- read.csv("http://ggobi.org/book/data/australian-crabs.csv")


ggplot(crabs,
       aes(x = CL, y = RW)) +
  geom_point()

m1 <- lm(data = crabs, formula = RW ~ CL)
m1_resid <- resid(m1)


ggplot(crabs,
       aes(x = BD, y = `m1_resid`, color = sex)) + 
  geom_point() +
  theme_bw()

m2 <- lm(data = crabs, formula = RW ~ CW + sex)
m2_resid <- resid(m2)

ggplot(crabs,
       aes(x = BD, y = `m2_resid`, color = sex)) + 
  geom_point() +
  theme_bw()


# * is for the interaction term
m3 <- lm(data = crabs, formula = RW ~ CL * sex)
summary(m3)



m4 <- lm(data = crabs, formula = RW ~ CL:sex + sex)
summary(m4)

m4fit <- fitted(m4)
m3fit <- fitted(m3)

equalish <- function(a, b, epsilon = 0.0000001) {
  result <- abs(a - b) < epsilon
  return(result)
}

equalish(m4fit, m3fit)
