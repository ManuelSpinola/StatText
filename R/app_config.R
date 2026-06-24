#' Access files in the current app
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "StatText")
}

#' Read App Config
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv("R_CONFIG_ACTIVE", "default")
  ),
  use_parent = TRUE,
  file       = app_sys("golem-config.yml")
) {
  config::get(
    value      = value,
    config     = config,
    file       = file,
    use_parent = use_parent
  )
}
