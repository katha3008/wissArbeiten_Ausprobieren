## meine Funktionen fuer die diskrete Gleichverteilung

my_dunifdis <- function(x, a, b) {
  d <- rep(1/(b-a+1), length(x))
  return(d)
}

my_punifdis <- function(q, a, b) {
  p <- (floor(q)-a+1)/(b-a+1)
  return(p)
}

my_qunifdis <- function(p, a, b) {
  q <- ceiling(p*(b-a+1))+(a-1)
  return(q)
}

my_runifdis <- function(n, a, b) {
  s <- runif(n, 0, 1)
  r <- my_qunifdis(s, a, b)
  return(r)
}

b <- 12
a <- 3

my_dunifdis(3:12, a, b)
my_punifdis(3:12, a, b)
my_qunifdis((1:20)/20, a, b)
y <- my_runifdis(100000, a, b)
table(y)
