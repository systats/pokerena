### Serve API
plumber::plumb("jobs/server_api.R")$run(host="0.0.0.0", port=8000)
# pr$run(port = 3838, host = "192.168.178.47", swagger = F)