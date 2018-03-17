# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

PAV_pipeline = function(fGetData, lfProcess, fVisualise) {
  require(purrr)
  fGetData() %>%
    possibly(lfProcess, .)() %>%
    possibly(fVisualise, NULL)()
}
# domyslnie ma dzialac tak:
# PAV_pipeline(getDataFunctions(PAV_testVisualise),
#              getProcessFunctions(PAV_testVisualise),
#              PAV_testVisualise)
PAV_treePipeline = function(fGetData, lfProcess, lfVisualise) {
  require(purrr)
  # on all simultanously
}

PAV_lfProcess = function(fVisualise, ...) {
  function(fVisualise, ...) {
    when(fVisualise == fVisualise ~ list(...), NULL)
  }
}
