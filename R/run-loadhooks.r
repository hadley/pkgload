#' Run user and package hooks.
#'
#'
#' @inheritParams ns_env
#' @param hook hook name: one of "load", "unload", "attach", or "detach"
#' @keywords internal
run_pkg_hook <- function(package, hook) {
  trans <- c(
    "load"   = ".onLoad",
    "unload" = ".onUnload",
    "attach" = ".onAttach",
    "detach" = ".onDetach")
  hook <- match.arg(hook, names(trans))
  f_name <- trans[hook]

  metadata <- dev_meta(package)
  if (isTRUE(metadata[[f_name]])) return(FALSE)

  # Run hook function if defined, and not already run
  nsenv <- ns_env(package)
  ns_path <- ns_path(package)

  if (!exists(f_name, nsenv, inherits = FALSE)) return(FALSE)

  if (hook %in% c("load", "attach")) {
    nsenv[[f_name]](dirname(ns_path), package)
  } else {
    nsenv[[f_name]](dirname(ns_path))
  }
  metadata[[f_name]] <- TRUE

  TRUE
}

#' @rdname run_pkg_hook
run_user_hook <- function(package, hook) {
  nsenv <- ns_env(package)

  trans <- c(
    "load"   = "onLoad",
    "unload" = "onUnload",
    "attach" = "attach",
    "detach" = "detach")
  hook <- match.arg(hook, names(trans))
  hook_name <- trans[hook]

  metadata <- dev_meta(package)
  if (isTRUE(metadata[[hook_name]])) return(FALSE)

  hooks <- getHook(packageEvent(package, hook_name))
  if (length(hooks) == 0) return(FALSE)

  for(fun in rev(hooks)) try(fun(package))
  metadata[[hook_name]] <- TRUE
  invisible(TRUE)
}
