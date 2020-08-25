week_ending <- "2020-08-16"
indir <- dirname(covid_19_path)
raw_data <- readRDS(
  glue::glue("{indir}/model_inputs/data_{week_ending}.rds")
)

si_distrs <- map2(
  raw_data[["si_mean"]],
  raw_data[["si_std"]],
  function(mu, sigma) {
    reparams <- epitrix::gamma_mucv2shapescale(
      mu = mu, cv = sigma / mu
    )
    miss_at_most <-0.001
    cutoff <- ceiling(
      qgamma(
        1 - miss_at_most,
        shape = reparams$shape,
        scale = reparams$scale
      )
    )
    EpiEstim::discr_si(k = 0:cutoff, mu = mu, sigma = sigma)
  }
)

names(si_distrs) <- c("si_1", "si_2")

saveRDS(si_distrs, "si_distrs.rds")
#####################################################################
#####################################################################
############### CFR Parameters
#####################################################################
#####################################################################

CFR_esti <- c(1.38, 1.23, 1.53)/100
# function to get parameters
f1 <- function(shape){
  res <- c(
    shape[1]/(shape[1]+shape[2]),
    qbeta(.025, shape1 = shape[1], shape2 = shape[2]),
    qbeta(.975, shape1 = shape[1], shape2 = shape[2])
  )
  res <- sum((res*100-CFR_esti*100)^2)
  return(res)
}

n <- 5e2
Shape1 <- rep(seq(300,350,length.out = n),each = n )
Shape2 <- rep(seq(22500,23500,length.out = n), n )
res <- rep(NA,n*n)
for (i in 1:(n*n)){
  res[i] <- f1(c(Shape1[i],Shape2[i]))
}
f <- which(res == min(res))
shape <- c(Shape1[f], Shape2[f])

saveRDS(shape, "cfr_shape_params.rds")
