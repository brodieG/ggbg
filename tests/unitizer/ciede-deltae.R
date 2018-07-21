library(unitizer)
library(ggbg)

unitizer_sect("sharma tests", {

  Labstd <- as.matrix(sharma.deltae.2000[, 1:3])
  Labsample <- as.matrix(sharma.deltae.2000[, 4:6])
  deltaE2000.vals <- sharma.deltae.2000[, 7]

  round(deltaE2000(Labstd, Labsample) - deltaE2000.vals, 8)
})
