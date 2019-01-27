#' Create a package
#'
#' @param pkg_name the name of the package which is to be created
#' @param packages packages wrapped in this group
#' @param author author of the new package
#' @param email email of the author
#' @param auto logical. whether to build and install the new package automatically
#' @param overwrite logical. whether to overwrite the package with the same name if it already installed
#'
#' @return a folder with a package skeleton
#' @importFrom utils installed.packages
#' @export
#'
#' @examples
#' \dontrun{
#' packr('zhaor', c('mindr','pinyin', 'beginr', 'bookdownplus', 'steemr', 'rmd'), 'Your Name')
#' }
packr <- function(pkg_name, packages, author = NULL, email = NULL, auto = FALSE, overwrite = FALSE){
  oldwd <- getwd()
  on.exit(setwd(oldwd))

  if(dir.exists(pkg_name)) return(message('The folder ', pkg_name, ' exists. Please use another name.'))
  packages_installed <- system.file(package = pkg_name) != ''
  if(packages_installed & !overwrite) return(message('There is a "', pkg_name, '" package already installed. Please use another package name, or use "overwrite = TRUE" to overwrite it.'))
  if(packages_installed) message('There is a "', pkg_name, '" package already installed. It will be overwritten by your new package.')

  # copy from the template
  dir.create(pkg_name)
  setwd(pkg_name)
  packr_path <- paste0(system.file(package = 'beginr'), '/skeleton/')
  template <- dir(packr_path, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  file.copy(template, getwd(), recursive = TRUE)

  # .Rproj
  file.rename('-pkgr', paste0('-', pkg_name, '.Rproj'))

  # DESCRIPTION
  DESCRIPTION <- readLines('DESCRIPTION', encoding = 'UTF-8')
  DESCRIPTION[3] <- paste('Date:', Sys.Date())
  if(!is.null(author)) for(i in 5:6) DESCRIPTION[i] <- gsub('author', author, DESCRIPTION[i])
  if(!is.null(email)) DESCRIPTION[6] <- gsub('packr@pzhao.net', email, DESCRIPTION[6])
  for(i in c(1, 4, 10)) DESCRIPTION[i] <- gsub('rmd', pkg_name, DESCRIPTION[i])
  DESCRIPTION[8] <- paste(DESCRIPTION[8], paste(packages, collapse = ', '), sep = ', ')
  writeLines(DESCRIPTION, 'DESCRIPTION', useBytes = TRUE)

  # R/*.R
  rfiles <- dir('R', full.names = TRUE)
  lapply(c(rfiles, 'NAMESPACE'), rplc, oldchar = 'rmd', newchar = pkg_name)

  # attach.R
  attachr <- readLines('R/attach.R', encoding = 'UTF-8')
  attachr[1] <- paste0('core <- c("', paste(packages, collapse = '", "'), '")')
  writeLines(attachr, 'R/attach.R', useBytes = TRUE)

  setwd(oldwd)
  if(auto) {
    system(paste0('R CMD build ', pkg_name))
    system(paste0('R CMD INSTALL ', pkg_name))
    require(pkg_name, character.only = TRUE)
  } else {
    message('The package folder ', pkg_name, ' has been created. Now you can open ', pkg_name, '/-', pkg_name, '.Rproj with RStudio and press ctrl + shift + b, or use codes ("R CMD build ', pkg_name, '" and "R CMD INSTALL ', pkg_name, '") to build and install the ', pkg_name, ' package. Have fun.')
  }
}

#' Replace strings in a file
#'
#' @param oldchar old string
#' @param newchar new string
#' @param filename file name
#'
#' @return modified files
rplc <- function(oldchar, newchar, filename){
  oldtxt <- readLines(filename, encoding = 'UTF-8')
  newtxt <- gsub(oldchar, newchar, oldtxt)
  writeLines(newtxt, filename, useBytes = TRUE)
}
