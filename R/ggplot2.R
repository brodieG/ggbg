## This file contains code taken directly from `ggplot2`.
##
## These are typically unexported functions that are used within ggplot that we
## end up using as well to match the ggplot2 patterns.
##
## `ggplot2` did not contain an explicit copyright notice as of 4/20/18 so we
## cannot reproduce it here.  It is licensed under GPL-2
## <https://www.r-project.org/Licenses/GPL-2>.
##
## You can find the original source at <https://github.com/tidyverse/ggplot2/>.

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

