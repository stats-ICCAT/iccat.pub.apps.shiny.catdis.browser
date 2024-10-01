library(devtools)

GITLAB_AUTH_TOKEN = Sys.getenv("GITLAB_AUTH_TOKEN")

devtools::install_gitlab("iccat-r-tests/libs/public/iccat.pub.base", auth_token = GITLAB_AUTH_TOKEN, dependencies = FALSE)
devtools::install_gitlab("iccat-r-tests/libs/public/iccat.pub.data", auth_token = GITLAB_AUTH_TOKEN, dependencies = FALSE)
devtools::install_gitlab("iccat-r-tests/libs/public/iccat.pub.aes",  auth_token = GITLAB_AUTH_TOKEN, dependencies = FALSE)
devtools::install_gitlab("iccat-r-tests/libs/public/iccat.pub.maps", auth_token = GITLAB_AUTH_TOKEN, dependencies = FALSE)

q(save = "no")
