# Task #1
library(ggplot2)

exp_data <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
exp_data$scenario = as.factor(exp_data$scenario)

ggplot(exp_data, aes(x = scenario, y = frequency, fill = attitude)) + geom_boxplot()

# Task #2
# exp_data$frequency = as.factor(exp_data$frequency)
# exp_data$subject = as.factor(exp_data$subject)
ggplot(exp_data, aes(x = frequency, fill=subject)) + geom_density(alpha=0.2) + facet_grid(gender~., scale = "free_y")

# Task #3
install.packages("lme4")
library(lme4)
fit_1 = lmer(frequency ~ attitude + (1|subject) + (1|scenario), exp_data)

# Task #4
fit_2 = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), exp_data)

# Task #5
fit_3 = lmer(frequency ~ attitude + gender + (attitude|subject) + (attitude|scenario), exp_data)
