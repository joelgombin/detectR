#!/usr/local/bin/r


library(plumber)
r <- plumber::plumb(system.file("API/APIs.R", package = "detectR"))
r$run(port = 6666)
