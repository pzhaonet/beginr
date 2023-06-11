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
packr <-
  function(pkg_name,
           packages,
           author = NULL,
           email = NULL,
           auto = FALSE,
           overwrite = FALSE) {
    oldwd <- getwd()
    on.exit(setwd(oldwd))

    if (dir.exists(pkg_name))
      return(message('The folder ', pkg_name, ' exists. Please use another name.'))
    # 检查目标文件夹是否已存在，如果存在则返回错误信息
    # Check if the target folder already exists, and return an error message if it does

    packages_installed <- system.file(package = pkg_name) != ''
    if (packages_installed &
        !overwrite)
      return(
        message(
          'There is a "',
          pkg_name,
          '" package already installed. Please use another package name, or use "overwrite = TRUE" to overwrite it.'
        )
      )
    # 检查是否已安装了同名的包，并根据overwrite参数决定是否覆盖安装
    # Check if a package with the same name is already installed, and decide whether to overwrite it based on the overwrite parameter

    if (packages_installed)
      message(
        'There is a "',
        pkg_name,
        '" package already installed. It will be overwritten by your new package.'
      )
    # 提示已经安装了同名的包，并将其覆盖安装

    # copy from the template
    dir.create(pkg_name)
    setwd(pkg_name)
    # 创建新的包文件夹，并切换到该文件夹下
    # Create a new package folder and switch to that directory

    packr_path <-
      paste0(system.file(package = 'beginr'), '/skeleton/')
    template <-
      dir(
        packr_path,
        all.files = TRUE,
        no.. = TRUE,
        full.names = TRUE
      )
    file.copy(template, getwd(), recursive = TRUE)
    # 从模板文件夹中复制文件到新的包文件夹中
    # Copy files from the template folder to the new package folder

    # .Rproj
    file.rename('-pkgr', paste0('-', pkg_name, '.Rproj'))
    # 重命名.Rproj文件，将包名加入文件名中
    # Rename the .Rproj file and include the package name in the filename

    # DESCRIPTION
    DESCRIPTION <- readLines('DESCRIPTION', encoding = 'UTF-8')
    DESCRIPTION[3] <- paste('Date:', Sys.Date())
    # 更新DESCRIPTION文件中的日期为当前日期
    # Update the date in the DESCRIPTION file to the current date

    if (!is.null(author))
      for (i in 5:6)
        DESCRIPTION[i] <- gsub('author', author, DESCRIPTION[i])
    # 如果提供了作者信息，则将DESCRIPTION文件中的author字段替换为提供的作者信息
    # If author information is provided, replace the 'author' field in the DESCRIPTION file with the provided author information

    if (!is.null(email))
      DESCRIPTION[6] <- gsub('packr@pzhao.net', email, DESCRIPTION[6])
    # 如果提供了电子邮件信息，则将DESCRIPTION文件中的packr@pzhao.net替换为提供的电子邮件信息
    # If email information is provided, replace 'packr@pzhao.net' in the DESCRIPTION file with the provided email information

    for (i in c(1, 4, 10))
      DESCRIPTION[i] <- gsub('rmd', pkg_name, DESCRIPTION[i])
    # 替换DESCRIPTION文件中的'rmd'为提供的包名
    # Replace 'rmd' in the DESCRIPTION file with the provided package name

    DESCRIPTION[8] <-
      paste(DESCRIPTION[8], paste(packages, collapse = ', '), sep = ', ')
    # 在DESCRIPTION文件的Depends字段中添加要依赖的包名
    # Add the package names to the 'Depends' field in the DESCRIPTION file

    writeLines(DESCRIPTION, 'DESCRIPTION', useBytes = TRUE)
    # 将更新后的DESCRIPTION文件写入到文件中
    # Write the updated DESCRIPTION file to disk

    # R/*.R
    rfiles <- dir('R', full.names = TRUE)
    lapply(c(rfiles, 'NAMESPACE'),
           rplc,
           oldchar = 'rmd',
           newchar = pkg_name)
    # 遍历R文件夹中的所有.R文件和NAMESPACE文件，并将其中的'rmd'替换为提供的包名
    # Iterate through all .R files and the NAMESPACE file in the 'R' folder and replace 'rmd' with the provided package name

    # attach.R
    attachr <- readLines('R/attach.R', encoding = 'UTF-8')
    attachr[1] <-
      paste0('core <- c("', paste(packages, collapse = '", "'), '")')
    # 将attach.R文件中的core赋值语句更新为要依赖的包名
    # Update the assignment statement of 'core' in the attach.R file with the package names

    writeLines(attachr, 'R/attach.R', useBytes = TRUE)
    # 将更新后的attach.R文件写入到文件中
    # Write the updated attach.R file to disk

    setwd(oldwd)
    # 切换回原始工作目录
    # Switch back to the original working directory

    if (auto) {
      system(paste0('R CMD build ', pkg_name))
      system(paste0('R CMD INSTALL ', pkg_name))
      require(pkg_name, character.only = TRUE)
    } else {
      message(
        'The package folder ',
        pkg_name,
        ' has been created. Now you can open ',
        pkg_name,
        '/-',
        pkg_name,
        '.Rproj with RStudio and press ctrl + shift + b, or use codes ("R CMD build ',
        pkg_name,
        '" and "R CMD INSTALL ',
        pkg_name,
        '") to build and install the ',
        pkg_name,
        ' package. Have fun.'
      )
    }
    # 如果auto为TRUE，则自动构建和安装包，并加载包；否则输出提示信息，指导用户如何手动构建和安装包
    # If auto is TRUE, build and install the package automatically, and load the package.
    # Otherwise, output a message guiding the user on how to manually build and install the package.
  }

#' Replace strings in a file
#'
#' @param oldchar old string
#' @param newchar new string
#' @param filename file name
#'
#' @return modified files
rplc <- function(oldchar, newchar, filename) {
  oldtxt <- readLines(filename, encoding = 'UTF-8')
  newtxt <- gsub(oldchar, newchar, oldtxt)
  writeLines(newtxt, filename, useBytes = TRUE)
}
