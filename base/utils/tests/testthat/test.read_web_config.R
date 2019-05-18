context("Read web config")

pecan_root <- file.path("..", "..")
php_config_example <- file.path(pecan_root, "web", "config.example.php")
php_config_docker <- file.path(pecan_root, "docker", "web", "config.docker.php")
stopifnot(file.exists(php_config_example), file.exists(php_config_docker))

cfg_example <- read_web_config(php_config_example)
cfg_docker <- read_web_config(php_config_docker)
php.config <- php_config_docker
