# to avoid variability on terminals with different capabilities
# plus generally random options being set

if(getRversion() < "3.2.0") {
  warning("Cannot run tests with R version less than 3.2.0.")
} else if(suppressWarnings(require('unitizer'))) {
  old.opt <- options(
    # warnPartialMatchArgs = TRUE,
    # warnPartialMatchAttr = TRUE,
    # warnPartialMatchDollar = TRUE
  )
  on.exit(old.opt)
  unitize_dir('unitizer', state='recommended')
} else {
  warning("Cannot run tests without package `unitizer`")
}
