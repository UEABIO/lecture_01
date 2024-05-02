# Simulate data
set.seed(123)  # For reproducibility


# Define parameters
alpha <- 0.05  # Significance level
n_sim <- 1000  # Number of simulations

# Sample size
n <- 58  # This could be adjusted to match your desired sample size


# Function to simulate data and test interaction effect
simulate_and_test <- function(effect_size = 0.16, n = 58) {

  # Simulating sex variable
  sex <- factor(sample(c("Females", "Males"), n, replace = TRUE))

  # Simulating jun_mean variable
  mean_jun_mean <- 14.1  # You might choose the mean value provided in the summary
  sd_jun_mean <- 1.1     # Standard deviation can be chosen based on the range of values in your real data

  # Generating jun_mean values from a normal distribution
  jun_mean <- rnorm(n, mean = mean_jun_mean, sd = sd_jun_mean)

  # Simulating rain_jun variable
  mean_rain_jun <- 52.1   # Mean value provided in the summary
  sd_rain_jun <- 22.1     # Standard deviation can be chosen based on the range of values in your real data

  # Generating rain_jun values from a normal distribution
  rain_jun <- rnorm(n, mean = mean_rain_jun, sd = sd_rain_jun)

  # Simulating forewing_length variable
  intercept <- 12.326411  # Intercept provided in the summary
  coeff_sex_males <- -3.562694  # Coefficient for sexMales provided in the summary
  coeff_jun_mean <- 0.132600   # Coefficient for jun_mean provided in the summary
  coeff_rain_jun <- 0.002015   # Coefficient for rain_jun provided in the summary
  coeff_interaction <- effect_size  # Coefficient for sexMales:jun_mean provided in the summary

  # Generating forewing_length values based on the linear model equation
  forewing_length <- intercept +
    coeff_sex_males * (sex == "Males") +
    coeff_jun_mean * jun_mean +
    coeff_rain_jun * rain_jun +
    coeff_interaction * (sex == "Males") * jun_mean +
    rnorm(n)

  # Combining simulated variables into a data frame
  simulated_data <- data.frame(sex, jun_mean, rain_jun, forewing_length)

  # Fit the model
  model <- lm(forewing_length ~ sex * jun_mean + rain_jun, data = simulated_data)

  # Test interaction effect
  interaction_p <- summary(model)$coefficients["sexMales:jun_mean", "Pr(>|t|)"]
  power <- ifelse(interaction_p < alpha, 1, 0)

  return(power)
}

sum(replicate(n_sim, simulate_and_test())/n_sim)
