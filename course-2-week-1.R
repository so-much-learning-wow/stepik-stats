# Task 1
NA_position  <- function(x, y){
  all(is.na(x) == is.na(y))
}

# Task 2
smart_test <- function(df) {
  tbl = table(df)

  if (min(tbl) < 5) {
    fisher.test(tbl)[1]
  } else {
    chisq.test(tbl)[1:3]
  }
}

# Task 3
most_significant <- function(df) {
  p_vals = lapply(colnames(df), function(v) chisq.test(table(df[v]))[3])

  colnames(df)[which(unlist(p_vals) == min(unlist(p_vals)))]
}

# Task 4
means = colMeans(iris[,1:4])
iris$important_cases <- factor(apply(iris[1:4], 1, function(r) if(sum(r > means) >= 3) "Yes" else "No"))

# Task 5
get_important_cases <- function(df) {
  means = colMeans(df)
  values = apply(df, 1, function(r) if(sum(r > means) > length(df)/2) "Yes" else "No")

  df$important_cases <- factor(values, levels = c("Yes", "No"))

  df
}

# Task 6
stat_mode <- function(v) {
  strtoi(names(which(table(v) == max(table(v)))))
}

# Task 7
max_resid <- function(df) {
  residuals = chisq.test(table(df))['stdres']
  index = which.max(residuals[[1]])

  # Stupid indices...
  drug = paste('drug', ifelse(index > 3, index - 3, index), sep='_')
  effect = ifelse(index > 3, "negative", "positive")

  c(drug, effect)
}

# Task 8
obj <- ggplot(diamonds, aes(x = color, fill = cut)) + geom_bar(position='dodge')

