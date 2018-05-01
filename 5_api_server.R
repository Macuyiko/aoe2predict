# Deploy as API

library(plumber)
r <- plumb("_model_api.R")
r$run(port=8000)

