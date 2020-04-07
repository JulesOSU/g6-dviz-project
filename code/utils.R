#common functions, tools, utilities that are usefully shared

#load R object from file at <path>
loadData <- function (path) {
  readRDS(path)
}
