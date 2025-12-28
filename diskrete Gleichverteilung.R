## meine Funktionen fuer die diskrete Gleichverteilung

my_dunifdis <- function(x, a, b) {
  if(!is.numeric(a) | !is.numeric(b) | !is.numeric(x)) {stop("x, a und b muessen numerische Argumente sein")}
  if(length(a) != 1 | length(b) != 1) {stop("a und b sollen keine Vektoren sein")}
  if(round(a) != a | round(b) != b) {stop("b und a muessen ganze Zahlen sein")}
  if(a >= b) {stop("b muss echt groesser a sein.")}
  d <- ifelse(x %in% a:b, 1/(b-a+1), 0)
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

#b <- 12
#a <- 3

# my_dunifdis(3:12, a, b)
# my_punifdis(3:12, a, b)
# my_qunifdis((1:20)/20, a, b)
# y <- my_runifdis(100000, a, b)
# table(y)
