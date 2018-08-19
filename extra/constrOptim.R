

## Figuring out how to do the constrained optimization
##
## k: constraints
## p: parameters
##
## In our case we'll have p+1 constraints, where p is number of segments, and
## the +1 is the requirement that the sum of all segments adds up to 1.
##
##


f <- function(p) sum(abs(diff(diff(c(0, p, 1)))))

n <- 11
p.init <- cumsum(rep(1/(n * 2), n))
# p.init <- seq(0, 1, len=13)[-c(1, 13)]
res <- optim(p.init, f, control=list(trace=2, maxit=3e4, reltol=1e-14))
res

ui <- rbind(diag(n), -diag(n))
ci <- c(integer(n), integer(n) - 1)
ui %*% p.init - ci
constrOptim(p.init, f, NULL, ui, ci)

n <- 8
tol <- 1e-6
p.init <- c(rep(1/(n * 2), n - 1), 1 - (n - 1)/(n * 2))
p.init[length(p.init)] <- p.init[length(p.init)] + tol / 2

ui <- rbind(
  diag(n),
  -diag(n),
  integer(n) + 1,
  integer(n) + 1
)
ci <- c(
  integer(n),
  integer(n) - 1,
  1 - tol,
  -1 + tol
)
# ui %*% p.init - ci > 0

opt <- constrOptim(p.init, f, NULL, ui, ci)
