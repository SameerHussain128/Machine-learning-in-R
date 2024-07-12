# Load the plumber library
library(plumber)

# Create a new plumber router
pr <- plumb("api.R")

# Run the API on port 8000
pr$run(port = 8000)
