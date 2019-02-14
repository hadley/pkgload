# Insert shim objects into a package's imports environment
#
# @param pkg A path or package object
insert_imports_shims <- function(package) {
  imp_env <- imports_env(package)
  imp_env$system.file <- shim_system.file
  imp_env$library.dynam.unload <- shim_library.dynam.unload
  imp_env$library.dynam <- shim_library.dynam
}

# Create a new environment as the parent of global, with devtools versions of
# help, ?, and system.file.
insert_global_shims <- function() {
  # If shims already present, just return
  if ("devtools_shims" %in% search()) return()

  e <- new.env()

  e$help <- shim_help
  e$`?` <- shim_question
  e$system.file <- shim_system.file

  base::attach(e, name = "devtools_shims", warn.conflicts = FALSE)
}

#' Replacement version of system.file
#'
#' This function is meant to intercept calls to [base::system.file()],
#' so that it behaves well with packages loaded by devtools. It is made
#' available when a package is loaded with [load_all()].
#'
#' When `system.file` is called from the R console (the global
#' envrironment), this function detects if the target package was loaded with
#' [load_all()], and if so, it uses a customized method of searching
#' for the file. This is necessary because the directory structure of a source
#' package is different from the directory structure of an installed package.
#'
#' When a package is loaded with `load_all`, this function is also inserted
#' into the package's imports environment, so that calls to `system.file`
#' from within the package namespace will use this modified version. If this
#' function were not inserted into the imports environment, then the package
#' would end up calling `base::system.file` instead.
#' @inheritParams base::system.file
#'
#' @usage # system.file(..., package = "base", lib.loc = NULL, mustWork = FALSE)
#' @rdname system.file
#' @name system.file
#' @usage system.file(..., package = "base", lib.loc = NULL, mustWork = FALSE)
shim_system.file <- function(..., package = "base", lib.loc = NULL,
                             mustWork = FALSE) {

  # If package wasn't loaded with devtools, pass through to base::system.file.
  # If package was loaded with devtools (the package loaded with load_all)
  # search for files a bit differently.
  if (!(package %in% dev_packages())) {
    base::system.file(..., package = package, lib.loc = lib.loc,
      mustWork = mustWork)

  } else {
    pkg_path <- find.package(package)

    # First look in inst/
    files_inst <- file.path(pkg_path, "inst", ...)
    present_inst <- file.exists(files_inst)

    # For any files that weren't present in inst/, look in the base path
    files_top <- file.path(pkg_path, ...)
    present_top <- file.exists(files_top)

    # Merge them together. Here are the different possible conditions, and the
    # desired result. NULL means to drop that element from the result.
    #
    # files_inst:   /inst/A  /inst/B  /inst/C  /inst/D
    # present_inst:    T        T        F        F
    # files_top:      /A       /B       /C       /D
    # present_top:     T        F        T        F
    # result:       /inst/A  /inst/B    /C       NULL
    #
    files <- files_top
    files[present_inst] <- files_inst[present_inst]
    # Drop cases where not present in either location
    files <- files[present_inst | present_top]
    if (length(files) > 0) {
      # Make sure backslahses are replaced with slashes on Windows
      normalizePath(files, winslash = "/")
    } else {
      if (mustWork) {
        stop("No file found", call. = FALSE)
      } else {
        ""
      }
    }
    # Note that the behavior isn't exactly the same as base::system.file with an
    # installed package; in that case, C and D would not be installed and so
    # would not be found. Some other files (like DESCRIPTION, data/, etc) would
    # be installed. To fully duplicate R's package-building and installation
    # behavior would be complicated, so we'll just use this simple method.
  }
}

shim_library.dynam.unload <- function(chname, libpath,
                                      verbose = getOption("verbose"),
                                      file.ext = .Platform$dynlib.ext) {

  # If package was loaded by devtools, we need to unload the dll ourselves
  # because libpath works differently from installed packages.
  if (!is.null(dev_meta(chname))) {
    try({
      unload_dll(pkg_name(libpath))
    })
    return()
  }

  # Should only reach this in the rare case that the devtools-loaded package is
  # trying to unload a different package's DLL.
  base::library.dynam.unload(chname, libpath, verbose, file.ext)
}

shim_library.dynam <- function(chname, package, lib.loc,
                               verbose = getOption("verbose"),
                               file.ext = .Platform$dynlib.ext, ...){
  # This shim version of library.dynam addresses the issue raised in:
  #   https://github.com/r-lib/pkgload/issues/48
  # Specifically that load_all() fails to find libraries placed within inst/libs/
  # This CRAN incompatible practice allows for including a dll compiled
  #  elsewhere in an R package.  See also: https://stackoverflow.com/questions/8977346

  # This shim function first attempts using base::libary.dynam() if that fails
  # it uses  .inst_libary.dynam()  which is very similar but insearts "inst/"
  # in the dll/so  path.  Finally, if both of those fail it goes back
  # to  the base::library.dynam() so that the error state is based on that
  # function.

  a <- tryCatch(base::library.dynam(chname, package, lib.loc,
                                    verbose = getOption("verbose"),
                                    file.ext = .Platform$dynlib.ext, ...),
                error = function(e) e)
  if(inherits(a, "error")){
    # Call version of library.dynam that adds the inst subdirectory to
    # the dll paths
    b <- inst_library.dynam(chname, package, lib.loc,
                             verbose = getOption("verbose"),
                             file.ext = .Platform$dynlib.ext, ...)

    # If both attempts fail go back to base::library:dynam so error
    # state and debugging are based on that function
    if(inherits(b, "error"))
      base::library.dynam(chname, package, lib.loc,
                          verbose = getOption("verbose"),
                          file.ext = .Platform$dynlib.ext, ...)

  }
}



inst_library.dynam <- function (chname, package, lib.loc,
                                 verbose = getOption("verbose"),
                                 file.ext = .Platform$dynlib.ext, ...){
  # Version of libary.dynam that looks for libraries or objects to load
  # Within the "/inst" package subdirectory. This is for the rare case that
  # a user has placed a linked library or shared object compiled elsewhere
  # within the inst/.
  # Note this was copied from base::library.dynam v3.4.4
  # Two lines were edited by replacing "libs" with "inst/libs"
  # Otherwise it is unchanged.

  dll_list <- .dynLibs()
  if (missing(chname) || !nzchar(chname))
    return(dll_list)
  package
  lib.loc
  r_arch <- .Platform$r_arch
  chname1 <- paste0(chname, file.ext)
  for (pkg in find.package(package, lib.loc, verbose = verbose)) {
    DLLpath <- if (nzchar(r_arch))
      file.path(pkg, "inst/libs", r_arch)  # This line differs from base::dynam.load
    else file.path(pkg, "inst/libs")       # This line differs from base::dynam.load
    file <- file.path(DLLpath, chname1)
    if (file.exists(file))
      break
    else file <- ""
  }
  if (file == "")
    if (.Platform$OS.type == "windows")
      stop(gettextf("DLL %s not found: maybe not installed for this architecture?",
                    sQuote(chname)), domain = NA)
  else stop(gettextf("shared object %s not found", sQuote(chname1)),
            domain = NA)
  file <- file.path(normalizePath(DLLpath, "/", TRUE), chname1)
  ind <- vapply(dll_list, function(x) x[["path"]] == file,
                NA)
  if (length(ind) && any(ind)) {
    if (verbose)
      if (.Platform$OS.type == "windows")
        message(gettextf("DLL %s already loaded", sQuote(chname1)),
                domain = NA)
    else message(gettextf("shared object '%s' already loaded",
                          sQuote(chname1)), domain = NA)
    return(invisible(dll_list[[seq_along(dll_list)[ind]]]))
  }
  if (.Platform$OS.type == "windows") {
    PATH <- Sys.getenv("PATH")
    Sys.setenv(PATH = paste(gsub("/", "\\\\", DLLpath),
                            PATH, sep = ";"))
    on.exit(Sys.setenv(PATH = PATH))
  }
  if (verbose)
    message(gettextf("now dyn.load(\"%s\") ...", file),
            domain = NA)
  dll <- if ("DLLpath" %in% names(list(...)))
    dyn.load(file, ...)
  else dyn.load(file, DLLpath = DLLpath, ...)
  .dynLibs(c(dll_list, list(dll)))
  invisible(dll)
}

