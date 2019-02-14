context("shim")

# Utility functions -----------------------------
# Take file paths and split them into pieces
expand_path <- function(path) {
  strsplit(path, .Platform$file.sep)
}

# Return the last n elements of vector x
last_n <- function(x, n = 1) {
  len <- length(x)
  x[(len-n+1):len]
}


# Tests -----------------------------------------

test_that("system.file returns correct values when used with load_all", {
  load_all("testShim")
  shim_ns <- ns_env("testShim")

  # The devtools::system.file function should return modified values.
  files <- shim_system.file(c("A.txt", "B.txt", "C.txt", "D.txt"),
                            package = "testShim")
  files <- expand_path(files)

  expect_true(all(last_n(files[[1]], 3) == c("testShim", "inst", "A.txt")))
  expect_true(all(last_n(files[[2]], 3) == c("testShim", "inst", "B.txt")))
  # Note that C.txt wouldn't be returned by base::system.file (see comments
  # in shim_system.file for explanation)
  expect_true(all(last_n(files[[3]], 2) == c("testShim", "C.txt")))
  # D.txt should be dropped
  expect_equal(length(files), 3)

  # If all files are not present, return ""
  files <- shim_system.file("nonexistent", package = "testShim")
  expect_equal(files, "")

  # Test packages loaded the usual way - should just pass through to
  # base::system.file
  expect_identical(base::system.file("Meta", "Rd.rds", package = "stats"),
    shim_system.file("Meta", "Rd.rds", package = "stats"))
  expect_identical(base::system.file("INDEX", package = "stats"),
    shim_system.file("INDEX", package = "stats"))
  expect_identical(base::system.file("nonexistent", package = "stats"),
    shim_system.file("nonexistent", package = "stats"))

  unload("testShim")
})

test_that("shimmed system.file respects mustWork", {
  load_all("testShim")
  find_missing <- function(mustWork) {
    shim_system.file("missing.txt", package = "testShim", mustWork = mustWork)
  }

  expect_equal(find_missing(FALSE), "")
  expect_error(find_missing(TRUE), "No file found")
})


test_that("Shimmed system.file returns correct values when used with load_all", {
  load_all("testShim")
  shim_ns <- ns_env("testShim")

  # Make sure the version of system.file inserted into the namespace's imports
  # is the same as devtools::system.file
  expect_identical(get("system.file", envir = shim_ns), shim_system.file)

  # Another check
  expect_identical(get_system.file(), shim_system.file)

  unload("testShim")
})

test_that("Replacement system.file returns correct values when installed", {
  # This set of tests is mostly a sanity check - it doesn't use the special
  # version of system.file, but it's useful to make sure we know what to look
  # for in the other tests.

  # Make a temp lib directory to install test package into
  old_libpaths <- .libPaths()
  tmp_libpath = file.path(tempdir(), "devtools_test")
  if (!dir.exists(tmp_libpath)) dir.create(tmp_libpath)
  .libPaths(c(tmp_libpath, .libPaths()))


  install.packages(test_path("testShim"), repos = NULL,
    type = "source", quiet = TRUE)
  expect_true(require(testShim))

  # The special version of system.file shouldn't exist - this get() will fall
  # through to the base namespace
  expect_identical(get("system.file", pos = asNamespace("testShim")),
    base::system.file)

  # Test within package testShim
  files <- get_system.file()(c("A.txt", "B.txt", "C.txt", "D.txt"),
    package = "testShim")
  files <- expand_path(files)
  expect_true(all(last_n(files[[1]], 2) == c("testShim", "A.txt")))
  expect_true(all(last_n(files[[2]], 2) == c("testShim", "B.txt")))
  expect_equal(length(files), 2)  # Third and fourth should be dropped

  # If all files are not present, return ""
  files <- get_system.file()("nonexistent", package = "testShim")
  expect_equal(files, "")

  detach("package:testShim", unload = TRUE)

  # Reset the libpath
  .libPaths(old_libpaths)
})



test_that("shim_library.dynam loads compiled dll/so from inst/src/", {

  # Most of the code below is overhead to create a package that contains
  # a compiled .dll (or .so) within  inst/libs/
  # The process:
  # 1. Install the testDllLoad test package (we are going to use its .dll)
  # 2. Copy the testLibDynam package to a temporary directory
  # 3. Copy the contents of the libs/ directory from the installed testDllLoad
  #   package into the temporary testLibDynam/inst/libs
  # 4. test that we can use load_all on the package and that the dll can be used

  # Make a temp lib directory to install test package into
  old_libpaths <- .libPaths()
  tmp_libpath = file.path(tempdir(), "library-dynam-test")
  if (!dir.exists(tmp_libpath)) dir.create(tmp_libpath)
  .libPaths(c(tmp_libpath, .libPaths()))

  # Reset the libpath on exit
  on.exit(.libPaths(old_libpaths), add = TRUE)

  # Create temp directory for assembling testLibDynam with dll or so in inst/libs/
  temp_dir <-tempdir()
  file.copy(test_path("testLibDynam"), temp_dir, recursive = TRUE)
  pkg_dir <- file.path(temp_dir, "testLibDynam")
  expect_true(file.exists(pkg_dir))

  # Install testDllLoad package
  install.packages(test_path("testDllLoad"), repos = NULL, type = "source",
                   INSTALL_opts = "--no-multiarch", quiet = TRUE)
  expect_true(require(testDllLoad))
  unload("testDllLoad")

  # Copy  libs/ from installed testDllLoad packageinto  testDynLib/inst/libs/
  inst_dir <-file.path(pkg_dir, "inst")
  compiled_libs <- file.path(tmp_libpath, "testDllLoad", "libs")
  dir.create(file.path(inst_dir, "libs"), recursive = TRUE, showWarnings = FALSE)
  file.copy(compiled_libs, inst_dir, recursive = TRUE, overwrite = TRUE)

  load_all(pkg_dir)
  expect_true(require(testLibDynam))

  # Check that it's loaded properly, by running a function from the package.
  # nulltest3() calls a C function which returns null.
  expect_true(is.null(nulltest3()))

  # Clean out compiled objects
  pkgbuild::clean_dll("testLibDynam")

  unload("testLibDynam")

  # Unlink temporary package dir
  unlink(pkg_dir, recursive = TRUE)


})

