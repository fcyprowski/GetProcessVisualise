#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise_if
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#' @importFrom dplyr ungroup

PAV_testData = function() {
  iris
}

PAV_lfTestOne = function(dane) {
  require(dplyr)
  dane %>%
    group_by(Species) %>%
    summarise_if(is.numeric, sum) %>%
    ungroup()
}

PAV_lfTestTwo = function(dane) {
  require(dplyr)
  dane %>%
    mutate(lol = 1000)
}

PAV_testVisualise = function(dane) {
  require(ggplot2)
  ggplot(data = dane) +
    geom_bar(aes(x = Species, y = Sepal.Length), stat = 'identity') +
    geom_text(aes(x = Species, y = Sepal.Length, label = lol))
}



# functions for fast testing ----------------------------------------------



