#### Question 1.a.
# Data
upper_class <- c(14, 6, 7)
lower_class <-c(7, 7, 1)

# Table
table_of_study <- rbind(
  "Upper class" = upper_class,
  "Lower class" = lower_class
)

colnames(table_of_study) <- c(
  "Not stopped",
  "Bribe requested",
  "Stopped/given warning"
)

## Calculate the totals
# Calculate row totals
row_totals <- rowSums(table_of_study)

# Calculate column totals
column_totals <- colSums(table_of_study)

# Calculate grand total
grand_total <- sum(table_of_study)

table_of_study

#### State Hypotheses
# H₀ : The variables are statistically independent (If H₀ is true, then we would expect fo = fe)
# H₁ : The variables are statistically dependent

### Expected frequency for one cell
# Frequency observed (fo) = number of observed sample
# Frequency expected (fe) = what we would expect for independent samples
fe <- outer(row_totals, column_totals) / grand_total
fo <- table_of_study

# Calculate the chi statistic
chi_test_statistic <- sum((fo - fe)^2 / fe)
chi_test_statistic

# Check expected frequencies (Frequency >= 5 for all cells, but this is not true for our table)
if (any(fe < 5)) {
  warning("Some expected frequencies are below 5. Chi-squared approximation may be unreliable.")
}

### Calculating p-values for Chi-squared tests
df <- (nrow(table_of_study) - 1) *(ncol(table_of_study) - 1)
df

p_value <- pchisq(chi_test_statistic, df, lower.tail=FALSE)
p_value
# Fail to reject H0 at 0.05 significance level

### The chi-squared statistic, df, and p-value
test_result <- chisq.test(table_of_study, correct = FALSE)
test_result

## The calculated chi-squared test statistic is χ2=3.7912, with 2 degrees of freedom and a p-value of approximately 0.1502.
# There is insufficient statistical evidence to conclude that social class and the outcome category are associated.
### Formal Statistical Conclusion
### Address the Small Sample Size Warning

# Question 1.b.
p_value <- pchisq(
  chi_test_statistic,
  df = df,
  lower.tail = FALSE
)

p_value
# p=0.1502>α=0.1

### Better independence test for small sample size
fisher.test(table_of_study)
# Therefore, we fail to reject the null hypothesis.
# There is insufficient statistical evidence at the 10% significance level to conclude that social class and the outcome category are associated.

### Question 1.c.
## Adjusted standardized residuals - How far away is each observed value from "expectation"
SR_adjusted <- (fo - fe) /
  sqrt(fe * outer(
    1 - row_totals / grand_total,
    1 - column_totals / grand_total
  ))
SR_adjusted

# Standardized (Pearson) residuals
SR_pearson <- test_result$residuals
SR_pearson

# Scatterplot of standardized residuals
# Increase bottom margin for long labels
par(mar = c(10, 4, 4, 2) + 0.1)
# Observation names
observation_names <- c(
  "Upper: Not stopped",
  "Upper: Bribe requested",
  "Upper: Stopped/Warning",
  "Lower: Not stopped",
  "Lower: Bribe requested",
  "Lower: Stopped/Warning"
)

# Plot adjusted standardised residuals
plot(
  as.vector(SR_adjusted),
  pch = 19,
  col = "blue",
  xlab = "Cell",
  ylab = "Standardised Residual",
  main = "Adjusted Standardised Residuals",
  ylim = range(c(SR_adjusted, SR_pearson), na.rm = TRUE),
  xaxt = "n"
)

# Add observation names to x-axis
axis(1, at = 1:6, labels = observation_names, las = 2, cex.axis = 0.7)

# Add best-fit line
abline(
  lm(as.vector(SR_adjusted) ~ seq_along(as.vector(SR_adjusted))),
  col = "red",
  lwd = 2
)

# Add reference line at zero
abline(h = 0, col = "black", lty = 2)

# How might the standardized residuals help you interpret the results?
plot(
  as.vector(SR_adjusted),
  pch = 19,
  col = "blue",
  xlab = "Cell",
  ylab = "Standardised Residual",
  main = "Adjusted vs Pearson Standardised Residuals",
  ylim = range(c(SR_adjusted, SR_pearson), na.rm = TRUE)
)

# Add best-fit line
abline(lm(as.vector(SR_adjusted) ~ seq_along(as.vector(SR_adjusted))),
       col = "red",
       lwd = 2)

# Add reference line at zero
abline(h = 0, col = "black", lty = 2)

# Add Pearson residuals
points(
  as.vector(SR_pearson),
  pch = 17,
  col = "darkgreen"
)

# Add reference lines
abline(h = 0, col = "black", lty = 1)
abline(h = c(-1.96, 1.96), col = "red", lty = 2)

# Add legend
legend(
  "topright",
  legend = c("Adjusted residuals", "Pearson residuals"),
  col = c("blue", "darkgreen"),
  pch = c(19, 17)
)

# Scatterplot: Adjusted vs Pearson residuals
plot(
  as.vector(SR_pearson),
  as.vector(SR_adjusted),
  pch = 19,
  col = "blue",
  xlab = "Pearson Standardised Residuals",
  ylab = "Adjusted Standardised Residuals",
  main = "Adjusted vs Pearson Residuals"
)

# Add line of best fit
abline(
  lm(as.vector(SR_adjusted) ~ as.vector(SR_pearson)),
  col = "red",
  lwd = 2
)

# Add reference lines
abline(h = 0, col = "grey40", lty = 2)
abline(v = 0, col = "grey40", lty = 2)

# Add legend
legend(
  "topleft",
  legend = c("Residuals", "Line of best fit"),
  col = c("blue", "red"),
  pch = c(19, NA),
  lty = c(NA, 1),
  lwd = c(NA, 2)
)

# Assumptions
# We assume the reservation policy was randomly assigned, making treatment and control groups comparable on average
# It also assumes the observations are independent and the status of one village does not influence the status of another; however, outcomes may be correlated within councils.
# Two villages are associated with each GP, which could affect the standard errors.
# Random assignment helps identify the causal effect, but it does not guarantee independent observations or eliminate every possible issue with statistical inference.
# Unit of random assignment: The GP, not the individual village.
# The sample size is 322 observations (villages), but 161 villages. Each GP has two villages, so there are approximately 161 GPs. Treatment group: Villages in GPs reserved for female leaders (reserved = 1). Control group: Villages in GPs not reserved for female leaders (reserved = 0).

# H₀ : β1=0 The reservation policy has no effect on the average number of drinking-water facilities
# H₁ : β1≠0 The reservation policy has an effect on the average number of drinking-water facilities (increase or decrease)
# β1 represents the population regression coefficient

# Create the variables table
variables_table <- data.frame(
  Variable = c("GP", "village", "reserved", "female",
               "irrigation", "water"),
  Data_Type = c(
    "Categorical identifier for the GP",
    "Categorical identifier for the village within a GP",
    "Binary (0 = not reserved, 1 = reserved)",
    "Binary (0 = male leader, 1 = female leader)",
    "Quantitative, discrete count",
    "Quantitative, discrete count (outcome variable)"
  )
)

# Display the table
knitr::kable(variables_table, caption = "Description of Variables")

women <- read.csv(url(
  "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
))

summary(women)

str(women)

# Run the bivariate regression
model <- lm(water ~ reserved, data = women)

# Display the regression results
summary(model)
