# c4_photosynthesis
# By: Bryan Petersen
# Date: 2024-08-05
# Purpose: This script is used to calculate the leaf-level photosynthesis rate of C4 plants.

# Load libraries
library(tidyverse)

## User input ##
q10 <- 20 # Q10 value, unitless
tl <- 274:340 # temperature of lower canopy leaves and stems, Kelvin
f1 <- 0.6 # constant used in tempvm equations, unitless
f2 <- 0.27 # constant used in tempvm equations, unitless
lotemp <- 5 # low temperature threshold in tempvm equation, Celsius
hitemp <- 45 # high temperature threshold in tempvm equation, Celsius
stresstl <- 1 # sum of soil moisture stress factor for the lower canopy over all 6 soil layers, unitless
drought <- 2 # crop sensitivity to drought parameter, unitless
stressn <- 1 # stress factor applied to vmax based on leaf nitrogen content in crops, unitless
vmax_pft <- 18.0e-6 # maximum rate of Rubisco carboxylation at 15 C at top of canopy, mol CO2 m-2 s-1

# Calculate the parameter values which are a function of temperature
tleaf <- tl - 273.15 # temperature of lower canopy leaves and stems, Celsius
rwork <- 3.47e-3 - (1/tl) # not sure what this is


# Calculate the temperature response of Vmax
tempvm <- exp(3500*rwork) / ((1 + exp(f1 * (lotemp - tleaf))) * (1 + exp(f2 * (tleaf - hitemp))))

# Plot tempvm
plot(tl, tempvm, type = "l", xlab = "Temperature (K)", ylab = "Temperature response of Vm")

# Calculate drought stress
stressc4c <- max(c(0.35, min(c(1, stresstl * drought))))

# Calculate the vmax
vmax <- vmax_pft * tempvm * min(c(stressc4c, stressn))

# Plot vmax
plot(tl, vmax, type = "l", xlab = "Temperature (K)", ylab = "Vmax")

