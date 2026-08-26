# Ensure the user package library and local .Renviron are available.
user_lib <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6")
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}
