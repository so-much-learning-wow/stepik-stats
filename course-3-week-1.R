hetero_test <- function(data) {
  fit = lm(data)
  resid_fit = lm(fit$residuals^2 ~ as.matrix(data[-1]))
  
  summary(resid_fit)$r.squared
}

VIF <- function(data) {
  X = data[-1]
  vifs = unlist(lapply(colnames(X), function(c) summary(lm(as.matrix(X[c]) ~ as.matrix(X[colnames(X) != c])))$r.squared))
  vifs = 1/(1-vifs)
  names(vifs) = colnames(X)
  
  vifs
}

smart_model <- function(data) {
  d = data
  
  while(TRUE) {
    vifs = VIF(d)
    
    if (max(vifs) > 10) {
      worst_x = which.max(vifs)
      d = d[-(worst_x + 1)]
      
      if (ncol(d) == 2) {
        break;
      }
    } else {
      break;
    }
  }
  
  lm(d)$coefficients
}
