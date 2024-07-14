check_packages = function(package_names) {
  for (pkg in package_names) {
    if (!pkg %in% c(names(sessionInfo()$otherPkgs), names(sessionInfo()$basePkgs))) {
      stop(paste("The", pkg, "package is missing and might not be installed or loaded."), call. = FALSE)
    }
  }
}
