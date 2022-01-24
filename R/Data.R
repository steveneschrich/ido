#' Retrieve Data Object
#'
#' Analagous to \code{\link[utils]{data}}, but supporting indirection when loading via
#' configuration files.
#'
#' @details
#' The goal of this function is to provide a replacement for the \code{\link[utils]{data}}
#' command in R. Or more accurately, to wrap the command in a layer of indirection such
#' that even when using the command it is possible to retrieve a remote dataset.
#'
#' The current strategy for retrieving data is to use the format `pkg::data`. This function
#' will search for a `conf` subdirectory of an installed package (`pkg`) and retrieve the
#' file `config.yml` using the \code{\link{config}{get}} method. This config file should
#' contain an entry `d`, which matches the argument of this function. The entry should
#' point to a URI suitable for executing the \code{\link[utils]{data}} function.
#'
#' @param d A dataset to load. Must be in the form `pkg::data`.
#' @param which One of c("wd","user","system"): current working directory (wd),
#' user config directory (user) as determined by \code{\link[rappdirs]{user_config_dir}},
#' or system package directory (system) as determined by \code{\link[base]{system.file}}.
#' @param verbose logical. Should extra information be printed during execution.
#' @param envir environment. The environment to load data into (default is .GlobalEnv).
#'
#' @return A character vector of the names of objects created, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' Data("pgreportr::grants")
#' }
Data <- function(d, which = c("wd","user", "system"), verbose=TRUE, envir = .GlobalEnv) {

  which <- match.arg(which, c("wd","user","system"))

  dsplit<-stringr::str_split(d, "::")[[1]]
  if ( length(dsplit) < 2)
    cli::cli_abort("Dataset reference {d} must be in the form pkg::data.")

  pkg <- dsplit[1]
  dataset <- dsplit[2]
  config_file <- "config.yml"

  # Three locations: system, user config and working directory.
  conf <- list(
    system = system.file("conf", config_file, package=pkg),
    user = file.path(rappdirs::user_config_dir(appname="pgreportr"), config_file),
    wd = file.path(getwd(), config_file)
  )

  # Priority for access: which argument, wd, user and then system.
  locations <- unique(c(which,"wd","user","system"))
  dataset_path <- NULL
  for ( loc in locations ) {
    if (verbose) cli::cli_alert_info("Checking {.strong {loc}} location: {.path {conf[[loc]]}}.")
    if ( file.exists(conf[[loc]]) ) {
      if (verbose) cli::cli_alert_success("File {.path {conf[[loc]]}} exists.")
      dataset_path <- config::get(value = dataset, file = conf[[loc]])
      if ( !is.null(dataset_path) ) {
        if (verbose) cli::cli_alert_success("Path {.path {dataset_path}} found in {.path {conf[[loc]]}}.")
        break
      } else {
        if (verbose) cli::cli_alert_warning("File {.path {conf[[loc]]}} does not contain {.strong {dataset}} key.")
      }
    } else {
      if (verbose) cli::cli_alert_warning("File {.path {conf[[loc]]}} does not exist.")
    }
  }

  # At this point, we either short-circuited with a dataset_path or we ran out of
  # options.
  if ( is.null(dataset_path))
    cli::cli_abort(c(
      "Could not find entry for {.strong {dataset}}.",
      "i" = "Search path: {.path {conf}}",
      "x" = "Config files do not exist or no {.strong {dataset}} entry in search path."
    ))

  load(dataset_path, verbose=verbose, envir = envir)
}


