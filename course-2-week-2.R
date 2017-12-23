# Task 1
get_coefficients <- function(df) {
  model = glm(formula = df$y ~ df$x, family = "binomial")
  exp(model$coefficients)
}

# Task 2
centered <- function(df, var_names) {
  means = apply(df[var_names], 2, function(c) mean(c))

  for(i in 1:nrow(df)) {
    df[i, var_names] = df[i, var_names] - means
  }

  df
}

# Task 3
get_features <- function(df) {
  model <- glm(df$is_prohibited ~ ., family="binomial", df)
  result <- anova(model, test = "Chisq")
  features = rownames(result[which(result[,5] < 0.05),])

  if (length(features) > 0) {
    features
  } else {
    'Prediction makes no sense'
  }
}

# Task 4
most_suspicious <- function(train, test) {
  model = glm(train$is_prohibited ~ ., family="binomial", data=train)
  result = test[which.max(predict.glm(model, test)),][,5]
  levels(result)[as.integer(result)]
}

# Task 5
normality_test <- function(df) {
  unlist(sapply(df[sapply(df, is.numeric)], function(c) shapiro.test(c)$p.value))
}

# Task 6
smart_anova <- function(df) {
  shapiro_p_vals = lapply(split(df, df$y), function(g) shapiro.test(g$x)[2][[1]])
  bartlett_p_val = bartlett.test(df$x, df$y)[3][[1]]

  if (bartlett_p_val < 0.05 || any(shapiro_p_vals < 0.05)) {
    fit <- kruskal.test(x ~ y, data = df)

    name <- 'KW'
    value <- fit[3][[1]]
  } else {
    fit <- aov(x ~ y, df)

    name <- 'ANOVA'
    value <- summary(fit)[[1]]$'Pr(>F)'[1]
  }

  setNames(value, name)
}

# Task 7
normality_by <- function(df) {
  # install.packages("dplyr")
  # library(dplyr)

  df %>% group_by(y, z) %>% summarise(p_value = shapiro.test(x)$p.value)
}

# Task 8
obj <- ggplot(data=iris, aes(x=Sepal.Length, group=Species, fill=Species)) + geom_density(alpha=0.2)
