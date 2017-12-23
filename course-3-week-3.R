# Task 1
median_cl_boot <- function(x) {
  M = median(x)
  medians = lapply((1:1000), function(n) median(sample(x, length(x), replace=T)))
  diffs = lapply(medians, function(m) M - m)
  quantile(unlist(diffs), c(.05, 0.95)) + M
}

# Task 2
slope_cl_boot <- function(d){
  slope = lm(y ~ x, d)$coefficients['x']
  slopes = lapply((1:1000), function(n) lm(y ~ x, d[sample(nrow(d), nrow(d), replace=T),])$coefficients['x'])
  diffs = lapply(slopes, function(s) slope - s)
  quantile(unlist(diffs), c(.025, 0.975)) + slope
}