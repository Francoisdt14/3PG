

#######################

# Initialize vectors to store the results
co2_values <- seq(350, 700, by = 5)
fCalpha_values <- numeric(length(co2_values))
fCg_values <- numeric(length(co2_values))

fcalpha700 <- 1.4
fcg700 <- 0.7

for (i in 1:length(co2_values)) {
    co2 <- co2_values[i]

    fcalphax <- (fcalpha700 / (2 - fcalpha700))
    fcg0 <- (fcg700 / (2 * (fcg700 - 1)))

    fCalpha <- fcalphax * co2 / (350 * (fcalphax - 1) + co2)
    fCg <- fcg0 / (1 + (fcg0 - 1) * co2 / 350)

    # Store the results in vectors
    fCalpha_values[i] <- fCalpha
    fCg_values[i] <- fCg
}

##################################################################################

alphaCx = 0.047 #canopy quantum efficiency (PARAMETER)
fNutr = 0.88
fT = 0.980882468
fFrost = 0.966666666666667
#fCalpha = fCalpha_values[1]
PhysMod = 0.741921238213433


# alphaC = alphaCx * fNutr * fT * fFrost * fCalpha * PhysMod
# alphaC goes directly into NPP calculation

# Initialize a vector to store alphaC values
alphaC_values <- numeric(length(co2_values))

# Calculate alphaC for each fCalpha value
for (i in 1:length(co2_values)) {
    alphaC_values[i] <- alphaCx * fNutr * fT * fFrost * fCalpha_values[i] * PhysMod
}


# Create separate plots for fCalpha and fCg
par(mfrow = c(3, 1))  # Set the layout to have 2 rows and 1 column of plots

# Plot for fCalpha
plot(co2_values, fCalpha_values, type = "l", col = "blue", xlab = "CO2", ylab = "fCalpha")
title("fCalpha vs. CO2")

# Plot for fCg
plot(co2_values, fCg_values, type = "l", col = "red", xlab = "CO2", ylab = "fCg")
title("fCg vs. CO2")

# Create a plot
plot(co2_values, alphaC_values, type = "l", col = "forestgreen", xlab = "CO2", ylab = "alphaC")
title("alphaC vs. CO2 for Varying fCalpha")

par(mfrow = c(1, 1))  # Reset the layout to the default

####

fCalpha <- 1  # Set fCalpha to a constant value

# Create a dataframe to store the results
result_df <- data.frame(CO2 = co2_values, Required_alphaCx = numeric(length(co2_values)))

# Calculate required alphaCx values
for (i in 1:length(co2_values)) {
    #alphaC <- alphaCx * fNutr * fT * fFrost * fCalpha * PhysMod
    Required_alphaCx <- alphaC_values[i] / (fNutr * fT * fFrost * fCalpha * PhysMod)

    result_df$Required_alphaCx[i] <- Required_alphaCx
}

# Print the dataframe
print(result_df)

# Calculate alphaC_new using Required_alphaCx values
alphaC_new <- result_df$Required_alphaCx * fNutr * fT * fFrost * PhysMod

# Create a plot with alphaC and alphaC_new
plot(co2_values, alphaC_values, type = "n", xlab = "CO2", ylab = "alphaC")
lines(co2_values, alphaC_values, type = "l", col = "forestgreen")
lines(co2_values, alphaC_new, type = "l", col = "blue", lty = 4)

legend("topright", legend = c("alphaC", "alphaC_new"), col = c("forestgreen", "blue"), lty = c(1, 2))
title("alphaC and alphaC_new vs. CO2")


# Create a plot with alphaC and alphaC_new
plot(co2_values, result_df$Required_alphaCx, type = "n", xlab = "CO2", ylab = "Required alphaCx")
lines(co2_values, result_df$Required_alphaCx, type = "l", col = "forestgreen")
title("Required alphaCx to curve match vs. CO2")



#######################################################################################

# BUT - we know that PhysMod and AlphaC change over time...
# Trying to deal with that:
# Constants
co2_levels <- c(350, 500, 700)  # Include all relevant CO2 levels
times <- c(1, 2, 3)  # Example time points in years
fCalpha <- 1  # Constant for fCalpha
fNutr <- 0.88
fT <- 0.980882468
fFrost <- 0.966666667

# Observed data for alphaC and PhysMod over time and for different CO2 levels
# Define observed_alphaC and observed_PhysMod matrices or data frames
# The dimensions should match the number of CO2 levels and times.

# Example:
observed_alphaC <- matrix(c(
    0.029095917, 0.021712018, 0.02343216, # CO2 = 350, time = 1, 2, 3
    0.035115762, 0.034594885, 0.028280193, # CO2 = 500, time = 1, 2, 3
    0.040734284, 0.040130067, 0.032805024 # CO2 = 700, time = 1, 2, 3
), nrow = length(co2_levels), byrow = TRUE)

observed_PhysMod <- matrix(c(
    0.741921238, 0.553638062, 0.597500232,  # CO2 = 350, time = 1, 2, 3
    0.741921238, 0.730916229, 0.597500232,  # CO2 = 500, time = 1, 2, ...
    0.741921238, 0.730916229, 0.597500232   # CO2 = 700, time = 1, 2, ...
), nrow = length(co2_levels), byrow = TRUE)

# Function to calculate Required_alphaCx
calculate_required_alphaCx <- function(alphaC, PhysMod) {
    Required_alphaCx <- alphaC / (fNutr * fT * fFrost * fCalpha * PhysMod)
    return(Required_alphaCx)
}

# Calculate Required_alphaCx for different CO2 levels and times
result_df2 <- data.frame(CO2 = numeric(0), Time = numeric(0), Required_alphaCx = numeric(0))

for (i in 1:length(co2_levels)) {
    co2 <- co2_levels[i]
    for (j in 1:length(times)) {
        time <- times[j]
        alphaC <- observed_alphaC[i, j]
        PhysMod <- observed_PhysMod[i, j]

        # Calculate Required_alphaCx using observed values
        Required_alphaCx <- calculate_required_alphaCx(alphaC, PhysMod)

        # Store the results in a dataframe
        result_df2 <- rbind(result_df2, data.frame(CO2 = co2, Time = time, Required_alphaCx = Required_alphaCx))
    }
}

# Print the dataframe
print(result_df2)

# Does this show it doesn't matter whether it changes with time?
