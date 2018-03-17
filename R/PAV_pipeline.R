
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#' @importFrom purrr possibly 
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
#' @importFrom purrr when
PAV_lfProcess = function(fVisualise, ...) {
  function(fVisualise, ...) {
    when(fVisualise == fVisualise ~ list(...), NULL)
  }
}
