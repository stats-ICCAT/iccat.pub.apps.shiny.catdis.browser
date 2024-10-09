library(devtools)

GITHUB_AUTH_TOKEN = Sys.getenv("GITHUB_AUTH_TOKEN")

devtools::install_github("stats-ICCAT/iccat.pub.base", auth_token = GITHUB_AUTH_TOKEN, dependencies = FALSE)
devtools::install_github("stats-ICCAT/iccat.pub.data", auth_token = GITHUB_AUTH_TOKEN, dependencies = FALSE)
devtools::install_github("stats-ICCAT/iccat.pub.aes",  auth_token = GITHUB_AUTH_TOKEN, dependencies = FALSE)
devtools::install_github("stats-ICCAT/iccat.pub.maps", auth_token = GITHUB_AUTH_TOKEN, dependencies = FALSE)

q(save = "no")
