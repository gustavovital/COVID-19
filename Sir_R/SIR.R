# Simulating COVID-19 interventions with R 

# install.packages('deSolve')
library(deSolve) # for edo

sir <-
  function(time, state, parameters, ...) {
    
    with(
      as.list(c(state, parameters)), {
        
        dS <- -beta * S * I
        dI <-  beta * S * I - gamma * I
        dR <-                 gamma * I
        
        return(list(c(dS, dI, dR)))
      })
  }

# Set parameters ----
# Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0 ----

init <- c(S = 1-1e-6, I = 1e-6, R = 0.0)

# Time frame ----

times <- seq(0, 70, by = 1)

## beta: infection parameter; gamma: recovery parameter ----

parameters <- c(beta = 1.4247, gamma = 0.14286)

# Solve using ode (General Solver for Ordinary Differential Equations) ----

out <- ode(y = init, times = times, func = sir, parms = parameters)
out <- as.data.frame(out)

out$time <- NULL
head(out, 10)

# Plot ----

matplot(x = times, y = out, type = "l",
        xlab = "Time", ylab = "Susceptible and Recovered", main = "SIR Model",
        lwd = 1, lty = 1, bty = "l", col = 2:4)
legend(40, 0.7, c("Susceptible", "Infected", "Recovered"), pch = 1, col = 2:4, bty = "n")
