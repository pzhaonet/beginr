#' Create the bibtex entry
#'
#' Output the bibtex entry of installed packages. If \code{bibfile} is not "",
#' bibentry will be saved to bibfile.
#'
#' @param pkgs character vector of package names to cite
#' @param bibfile character string, file path to save the bib entries.  If ""
#' (the default), it prints to the standard output connection, or save the bibtex
#' entry to \code{bibfile}.
#' @return None (invisible NULL)
#' @export
#' @examples
#' bib()
#'
#' \dontrun{
#' bib(pkg = c('mindr', 'bookdownplus', 'pinyin'))
#' }
#' @importFrom grDevices col2rgb colors rainbow rgb rgb2hsv gray hcl
#' @importFrom graphics abline arrows axis box hist legend lines mtext pairs panel.smooth par plot points polygon rect rug strwidth text barplot curve
#' @importFrom stats IQR cor cor.test density dnorm fivenum lm rnorm sd
#' @importFrom utils citation read.table toBibtex write.csv unzip
bib <- function(pkgs = 'base', bibfile = '') {
  # 停止执行并报错，如果 pkgs 参数不是字符向量或包含缺失值
  # Stop and throw an error if pkgs parameter is not a character vector or contains missing values
  stopifnot(is.character(pkgs),!anyNA(pkgs))

  pkgs <- unique(pkgs)
  uninstalled_pkgs <- pkgs[!pkgs %in% installed.packages()]
  if (length(uninstalled_pkgs))
    # 如果存在未安装的包，则停止执行并报错
    # Stop and throw an error if there are any uninstalled packages
    stop(paste(uninstalled_pkgs, collapse = ", "),
         " has/have not been installed",
         call. = FALSE)

  for (pkg in pkgs) {
    # 将包的引用信息转换为 BibTeX 格式
    # Convert the citation information of the package to BibTeX format
    cti <- toBibtex(citation(pkg))

    # 找到 BibTeX 中以 @ 开头的行的位置
    # Find the positions of lines starting with @ in the BibTeX output
    entryloc <- grep(pattern = '^@', cti)

    # 将这些行中的逗号替换为 'R-包名,' 的形式
    # Replace commas in these lines with 'R-package,' format
    cti[entryloc] <-
      gsub(',', paste('R-', pkg, ',', sep = ''), cti[entryloc])

    # 找到含有 & 的行的位置
    # Find the positions of lines containing &
    symbol6loc <- grep('&', cti)

    for (i in symbol6loc) {
      # 将 & 替换为 \&，以便在生成的 BibTeX 中正确显示符号 &
      # Replace & with \& to correctly display the symbol & in the generated BibTeX
      cti[i] <- gsub(pattern = ' &', replacement = ' \\\\&', cti[i])
    }

    if (length(entryloc) > 1)
      # 如果找到的引用行的数量大于 1，则将这些行末尾的逗号替换为递增的数字
      # If there are more than one reference lines found, replace the commas at the end of these lines with incremental numbers
      cti[entryloc] <- paste(substr(cti[entryloc], 1, nchar(cti[entryloc]) - 1),
                             1:length(entryloc), ',', sep = '')

    # 将转换后的 BibTeX 内容写入文件中
    # Write the converted BibTeX content to the file
    cat(cti,
        sep = '\n',
        file = bibfile,
        append = TRUE)
  }

  # 返回 NULL，以隐藏函数的输出
  # Return NULL to hide the output of the function
  invisible(NULL)
}

#' Plot a dataframe, multiple ys against one x
#'
#' @param x a vector
#' @param y a vector or a dataframe with the same length as x
#' @param add logical, whether to add this plot to the previous one
#' @param xlab character
#' @param ylab character
#' @param myaxes logical, whether to display axes automatically
#' @param xlim numeric
#' @param ylim numeric
#' @param mycol colours
#' @param mytype character
#' @param mypch numeric or character
#' @param mycex numeric
#' @param mylty numeric
#' @param lwd numeric
#' @param xerror errorbar, same dimension of x
#' @param yerror same dimension of y
#' @param mycolerrorbar error bar colours
#' @param mylegend character
#' @param mylegendcol colors
#' @param mylegendcex numeric
#' @param legendpos character
#'
#' @return a figure
#' @export
#' @examples
#' x <- seq(0, 2 * pi, length.out = 100)
#' y <- data.frame(sin(x), cos(x))
#' yerror <- data.frame(abs(rnorm(100, sd = 0.3)),
#'                      abs(rnorm(100, sd = 0.1)))
#' dfplot(x, y, yerror = yerror)
#'
dfplot <- function(x,
                   y,
                   add = FALSE,
                   xlab = '',
                   ylab = '',
                   myaxes = FALSE,
                   xlim = NULL,
                   ylim = NULL,
                   mycol = NULL,
                   mytype = 'l',
                   mypch = 20,
                   mycex = 1,
                   mylty = NULL,
                   lwd = 1,
                   xerror = NULL,
                   yerror = NULL,
                   mycolerrorbar = NULL,
                   mylegend = NULL,
                   mylegendcol = mycol,
                   mylegendcex = 1,
                   legendpos = 'top') {
  y <- as.data.frame(y)
  if (is.null(ylim))
    ylim <- range(y, na.rm = TRUE)
  if (length(add) == 1) {
    if (add == FALSE) {
      # 创建一个空的绘图区域，用于后续的绘图
      # Create an empty plotting area for subsequent plotting
      plot(
        x,
        y[, 1],
        xlab = xlab,
        ylab = ylab,
        axes = myaxes,
        xlim = xlim,
        ylim = ylim,
        type = 'n',
        cex = mycex,
        lty = ifelse(is.null(mylty), 1, mylty[1])
      )
    }
  } else {
    message('"add = " must have a length = 1.')
  }
  # ny <- ifelse(is.data.frame(y), dim(y)[2], 1)
  ny <- ncol(y)
  if (is.null(mycol)) {
    # 如果未提供颜色向量，则使用彩虹色作为默认颜色
    # If no color vector is provided, use rainbow colors as default
    mycol <- rainbow(ny)
  }
  if (is.null(mylty)) {
    # 如果未提供线型向量，则使用实线作为默认线型
    # If no line type vector is provided, use solid lines as default
    mylty <- rep(1, ny)
  }

  if (is.null(mycolerrorbar)) {
    # 如果未提供误差线的颜色向量，则使用彩虹色的半透明色作为默认颜色
    # If no color vector for error bars is provided, use rainbow semi-transparent colors as default
    mycolerrorbar <- rainbow(ny, alpha = 0.5)
  }
  for (i in 1:ny) {
    if (!is.null(xerror)) {
      # 绘制 x 方向的误差线所围成的多边形
      # Plot the polygon formed by x-direction error bars
      polygon(
        x = c(x + xerror, rev(x - xerror)),
        y = c(y[, i], rev(y[, i])),
        col = mycolerrorbar[i],
        border = NA
      )
    }
    if (!is.null(yerror)) {
      yerror <- as.data.frame(yerror)
      yerror[is.na(yerror[, i]), i] <- 0
      # 绘制 y 方向的误差线所围成的多边形
      # Plot the polygon formed by y-direction error bars
      polygon(c(x, rev(x)),
              c(y[, i] + yerror[, i], rev(y[, i] - yerror[, i])),
              col = mycolerrorbar[i],
              border = NA)
    }
    # 绘制散点图或线图
    # Plot the scatter plot or line plot
    points(
      x,
      y[, i],
      type = mytype,
      col = mycol[i],
      lwd = lwd,
      lty = mylty[i],
      cex = mycex,
      pch = mypch
    )
  }
  if (!is.null(mylegend)) {
    # 添加图例
    # Add legend
    legend(
      legendpos,
      legend = mylegend,
      col = mylegendcol,
      bty = 'n',
      lty = mylty,
      lwd = lwd,
      cex = mylegendcex
    )
  }
  box()
}


#' Plot a dataframe, one y against multiple xs
#'
#' @param x a vector  or a dataframe with the same length as x
#' @param y a vector
#' @param xlab character
#' @param ylab character
#' @param xlim numeric
#' @param ylim numeric
#' @param mycol colours
#' @param xerror errorbar, same dimension of x
#' @param yerror same dimension of y
#' @param mycolerrorbar error bar colours
#' @param mylty numeric
#' @param mylegend character
#'
#' @return a figure
#' @export
#' @examples
#' x <- seq(0, 2 * pi, length.out = 100)
#' y <- data.frame(sin(x), cos(x))
#' yerror <- data.frame(abs(rnorm(100, sd = 0.3)),
#'                      abs(rnorm(100, sd = 0.1)))
#' dfplot2(y, x, xerror = yerror)
#'
dfplot2 <- function(x,
                    y,
                    xlab = 'x',
                    ylab = 'y',
                    xlim = NULL,
                    ylim = NULL,
                    mycol = NULL,
                    mylty = NULL,
                    xerror = NULL,
                    yerror = NULL,
                    mycolerrorbar = NULL,
                    mylegend = NULL) {
  oldpar <- par(las = 1)
  on.exit(par(oldpar))
  x <- as.data.frame(x)
  # 创建绘图区域并设置绘图参数
  # Create the plotting area and set the plotting parameters
  plot(
    x[, 1],
    y,
    xlab = xlab,
    ylab = ylab,
    axes = FALSE,
    xlim = xlim,
    ylim = ylim,
    type = 'n'
  )
  # nx <- ifelse(is.data.frame(x), dim(x)[2], 1)
  nx <- dim(x)[2]
  if (is.null(mycol)) {
    # 如果未提供颜色向量，则使用彩虹色作为默认颜色
    # If no color vector is provided, use rainbow colors as default
    mycol <- rainbow(nx)
  }
  if (is.null(mylty)) {
    # 如果未提供线型向量，则使用实线作为默认线型
    # If no line type vector is provided, use solid lines as default
    mylty <- rep(1, nx)
  }
  if (is.null(mycolerrorbar)) {
    # 如果未提供误差线的颜色向量，则使用彩虹色的半透明色作为默认颜色
    # If no color vector for error bars is provided, use rainbow semi-transparent colors as default
    mycolerrorbar <- rainbow(nx, alpha = 0.5)
  }
  for (i in 1:nx) {
    if (!is.null(yerror)) {
      # 绘制 y 方向的误差线所围成的多边形
      # Plot the polygon formed by y-direction error bars
      polygon(
        x = c(x[, i] + xerror[, i], rev(x[, i] - xerror[, i])),
        y = c(y, rev(y)),
        col = mycolerrorbar[i],
        border = NA
      )
    }
    if (!is.null(xerror)) {
      xerror <- as.data.frame(xerror)
      # 绘制 x 方向的误差线所围成的多边形
      # Plot the polygon formed by x-direction error bars
      polygon(
        y = c(y, rev(y)),
        x = c(x[, i] + xerror[, i], rev(x[, i] - xerror[, i])),
        col = mycolerrorbar[i],
        border = NA
      )
    }
    # 绘制线图
    # Plot the line plot
    points(
      x[, i],
      y,
      type = 'l',
      col = mycol[i],
      lwd = 2,
      lty = mylty[i]
    )
  }
  if (!is.null(mylegend)) {
    # 添加图例
    # Add legend
    legend(
      'top',
      legend = mylegend,
      col = mycol,
      bty = 'n',
      lty = mylty
    )
  }
  box()
}

#' Display errorbars
#'
#' Create a errorbar, or add errorbars to an existing plot.
#'
#' @param x,y numeric vectors, the coordinates of points at which the error bar
#' are to be plotted.
#' @param xupper,xlower numeric vectors, error value of X axis, the X axis of
#' error bars are from \eqn{x - xlower} to \eqn{x + xupper}.
#' @param yupper,ylower numeric vectors, error value of Y axis, the Y axis of
#' error bars are from \eqn{y - ylower} to \eqn{y + yupper}.
#' @param col,lty graphical parameters, color and line type for the lines, more
#' details see \code{\link[graphics]{par}}.
#'
#' @export
#' @examples
#' x <- seq(0, 2 * pi, length.out = 100)
#' y <- sin(x)
#' plot(x, y, type = 'l')
#' errorbar(x, y, yupper = 0.1, ylower = 0.1)
#'
errorbar <-
  function(x,
           y,
           xupper = NULL,
           xlower = NULL,
           yupper = NULL,
           ylower = NULL,
           col = 'black',
           lty = 1)
  {
    if (!is.null(yupper)) {
      # 绘制 y 方向的上界误差线
      # Plot the upper error bars in the y-direction
      arrows(
        x,
        y,
        x,
        y + yupper,
        angle = 90,
        length = 0.03,
        col = col,
        lty = lty
      )
    }
    if (!is.null(ylower)) {
      # 绘制 y 方向的下界误差线
      # Plot the lower error bars in the y-direction
      arrows(
        x,
        y,
        x,
        y - ylower,
        angle = 90,
        length = 0.03,
        col = col,
        lty = lty
      )
    }
    if (!is.null(xupper)) {
      # 绘制 x 方向的上界误差线
      # Plot the upper error bars in the x-direction
      arrows(
        x,
        y,
        x + xupper,
        y,
        angle = 90,
        length = 0.03,
        col = col,
        lty = lty
      )
    }
    if (!is.null(xlower)) {
      # 绘制 x 方向的下界误差线
      # Plot the lower error bars in the x-direction
      arrows(
        x,
        y,
        x - xlower,
        y,
        angle = 90,
        length = 0.03,
        col = col,
        lty = lty
      )
    }
  }

#' Calculate the skewness of a distribution
#'
#' @param x the data to check
#'
#' @return the skewness of the distribution of x
#' @export
#'
#' @examples mf_skewness(rnorm(100))
mf_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  # 去除缺失值后的样本大小
  # Sample size after removing missing values
  skewness <- n / (n - 1) / (n - 2) * sum((x - mean(x)) ^ 3) / sd(x) ^
    3
  # 偏度计算公式
  # Formula for calculating skewness
  se_skewness <- sqrt(6 / length(x))
  # 偏度的标准误
  # Standard error of skewness
  return(skewness / se_skewness)
}


#' Plot a user-customized hist
#' @param data a numeric vector
#' @param mybreaks character
#' @param myxlim numeric
#' @param myylim numeric
#' @param show_normline logical
#' @param eightlines logical
#' @param eightdigit numeric
#' @param eightcex numeric
#' @param eightcolors colors
#' @param mylegend character
#' @param myxlab character
#' @param return_df logic
#' @param show_n logical
#' @param show_skewness logical
#' @param show_density logical
#' @return a hist plot
#' @export
#' @examples
#' plothist(rnorm(10000))
plothist <-
  function(data = rnorm(1000),
           mybreaks = "Sturges",
           myxlim = NULL,
           myylim = NULL,
           eightlines = TRUE,
           eightdigit = 0,
           eightcex = 0.8,
           eightcolors = c('red', 'darkgreen', 'blue', 'black', 'purple', 'gold')[c(1, 2, 3, 2, 1, 6, 6, 5, 4, 5)],
           mylegend = '',
           myxlab = '',
           return_df = FALSE,
           show_n = TRUE,
           show_skewness = TRUE,
           show_density = FALSE,
           show_normline = FALSE) {
    # 设置默认的 y 轴范围
    if (is.null(myylim))
      myylim <-
        c(0, max(hist(
          data, breaks = mybreaks, plot = FALSE
        )$density) * 1.1)

    # 根据是否设置 x 轴范围来绘制直方图
    if (is.null(myxlim)) {
      hist(
        data,
        col = 'grey',
        border = NA,
        main = '',
        freq = FALSE,
        breaks = mybreaks,
        xlab = myxlab,
        ylim = myylim
      )
    } else {
      hist(
        data,
        col = 'grey',
        border = NA,
        main = '',
        freq = FALSE,
        breaks = mybreaks,
        xlab = myxlab,
        xlim = myxlim,
        ylim = myylim
      )
    }

    # 绘制密度曲线
    if (length(show_density) == 1) {
      if (show_density)
        lines(density(data[!is.na(data)], bw = "SJ"))
    } else {
      message('"show_density" must have a length = 1.')
    }

    # 绘制正态分布曲线
    if (length(show_normline) == 1) {
      if (show_normline)
        curve(dnorm(x, mean = mean(data, na.rm = TRUE), sd(data, na.rm = TRUE)),
              add = TRUE,
              col = 'purple')
    } else {
      message('one of the parameters must have a length = 1.')
    }

    # 绘制数据点的刻度线
    rug(data, col = 'darkgrey')

    # 添加图例
    legend('topleft', bty = 'n', legend = mylegend)

    # 计算偏度并添加图例
    myskew <- mf_skewness(data)
    legend('topright',
           bty = 'n',
           legend = paste(
             ifelse(show_n, paste0('n = ', sum(!is.na(
               data
             )), '\n'), ''),
             ifelse(show_skewness, paste0(
               'skewness = ',
               round(myskew, 2),
               ifelse(myskew > 1.96 | myskew < -1.96, '', '(*)')
             ), ''),
             sep = ''
           ))

    # 绘制八分位数和异常值阈值线
    if (length(eightlines) == 1) {
      if (eightlines) {
        myfive <- fivenum(data)
        threshold <- IQR(data, na.rm = TRUE) * 1.5
        abline(v = c(myfive, myfive[2] - threshold, myfive[4] + threshold),
               col = eightcolors[1:7])
        mtext(
          text = round(myfive, eightdigit),
          side = 3,
          line = c(0, 1, 0, 1, 0),
          at = myfive,
          col = eightcolors[1:5],
          cex = eightcex
        )
        mymean <- mean(data, na.rm = TRUE)
        mysd <- sd(data, na.rm = TRUE)
        abline(
          v = seq(
            from = mymean - mysd,
            by = mysd,
            length.out = 3
          ),
          col = eightcolors[8:10],
          lty = 2
        )
      }
    } else {
      message('one of the parameters must have a length = 1.')
    }

    box()

    # 返回数据框
    if (length(return_df) == 1) {
      if (return_df)
        return(data.frame(
          para = c(
            'min',
            '1q',
            'median',
            '3q',
            'max',
            'lower',
            'upper',
            'mean',
            'sd'
          ),
          value = c(myfive, myfive[2] - threshold, myfive[4] + threshold, mymean, mysd)
        ))
    } else {
      message('one of the parameters must have a length = 1.')
    }
  }


#' Save a list into an ASCII file. in: a list. out: a file.
#' @param x a list
#' @param file character. file name
#' @return a file
#' @export
#' @examples
#' alist <- list(a = 1:10, b = letters)
#' list2ascii(alist)
#'
list2ascii <-
  function(x, file = paste(deparse(substitute(x)), ".txt", sep = ""))
  {
    # 将R列表写入ASCII文件的函数
    # 这可以用于为那些希望在电子表格或其他程序中使用数据的人创建文件。
    # This can be used to create files for those who want to use
    # a spreadsheet or other program on the data.
    #
    # 保存当前宽度
    # save current width
    tmp.wid = getOption("width")
    # 增加输出宽度
    # increase output width
    options(width = 10000)
    # 重定向输出到文件
    # redirect output to file
    sink(file)
    # 打印对象
    # print the object
    print(x)
    # 取消重定向
    # cancel redirection
    sink()
    # 恢复行宽
    # restore linewidth
    options(width = tmp.wid)
    # 从函数中返回（无返回值）
    # return (nothing) from function
    return(invisible(NULL))
  }

#' plot a linear regression figure and return a list of parameters.
#' @param x numeric
#' @param y numeric
#' @param xlim numeric
#' @param ylim numeric
#' @param plot.title character
#' @param xlab character
#' @param ylab character
#' @param refline logical. if a reference line is plotted
#' @param slope slope of refline
#' @param intercept intercept of refline
#' @param showr2 logical
#' @param showleg logical
#' @return a figure
#' @export
#' @examples plotlm(1:10, 1:10 + rnorm(10))
#'
plotlm <- function(x,
                   y,
                   xlim = range(as.numeric(x), na.rm = TRUE),
                   ylim = range(as.numeric(y), na.rm = TRUE),
                   plot.title = "linear regression",
                   xlab = 'x',
                   ylab = 'y',
                   refline = FALSE,
                   slope = 1,
                   intercept = 0,
                   showr2 = TRUE,
                   showleg = TRUE) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  # 绘制散点图
  plot(
    x,
    y,
    xlim = xlim,
    ylim = ylim,
    xlab = xlab,
    ylab = ylab,
    col = "grey",
    pch = 19,
    main = plot.title
  )
  # 线性回归拟合
  lm.my <- lm(y ~ x)
  lm.sum <- summary(lm.my)
  lm.rs <- round(lm.sum$r.squared, digits = 3)
  b <- signif(lm.my$coefficients[1], 3)
  a <- signif(lm.my$coefficients[2], 3)

  # 绘制回归直线
  abline(lm.my, col = "black", lwd = 2)
  # 如果 refline 参数为 TRUE，则绘制参考直线
  # If refline parameter is TRUE, plot the reference line
  if (length(refline) == 1) {
    if (refline)
      abline(
        a = intercept,
        b = slope,
        col = 'blue',
        lty = 2
      )
  } else {
    message('one of the parameters must have a length = 1.')
  }

  # 显示回归方程
  # Display the regression equation
  text(
    xlim[1],
    ylim[2] - diff(ylim) * 0.1,
    substitute(paste(italic(y), ' = ', a, italic(x), c, b), list(
      a = a,
      b = b,
      c = ifelse(b < 0, '', ' + ')
    )),
    cex = 1.2,
    pos = 4
  )

  # 显示样本数量和 R 平方值
  # Display the sample size and R-squared value
  text(
    xlim[1],
    ylim[2] - diff(ylim) * 0.2,
    as.expression(substitute(italic(n) == r, list(r = length(
      x
    )))),
    cex = 1.2,
    pos = 4
  )

  # 如果 showr2 参数为 TRUE，则显示 R 平方值
  # If showr2 parameter is TRUE, display the R-squared value
  if (length(showr2) == 1) {
    if (showr2)
      text(
        xlim[1],
        ylim[2] - diff(ylim) * 0.3,
        as.expression(substitute(italic(R) ^ 2 == r, list(r = lm.rs))),
        cex = 1.2,
        pos = 4
      )
  } else {
    message('one of the parameters must have a length = 1.')
  }

  # 如果 showleg 参数为 TRUE，则显示图例
  # If showleg parameter is TRUE, display the legend
  if (length(showleg) == 1) {
    if (showleg)
      legend(
        'bottomright',
        legend = c('data', 'linear', if(refline) '1:1'),
        col = c('darkgrey', 'black', 'blue'),
        pch = c(19,-1,-1),
        lty = c(0, 1, 2),
        bty = 'n'
      )
  } else {
    message('one of the parameters must have a length = 1.')
  }

  return(list(lm.sum$coefficients, lm.sum$r.squared))
}

#' calculate linear regression between every two columns in a data frame. in: a dataframes. out: a dataframe showing the linear regression.
#'
#' @param simply logical
#' @param intercept logical
#' @param data a dataframe
#'
#' @return another dataframe
#' @export
#' @examples
#' df <- data.frame(a = 1:10, b = 1:10 + rnorm(10), c = 1:10 + rnorm(10))
#' lmdf(df)
#'
lmdf <- function(data,
                 simply = FALSE,
                 intercept = TRUE) {
  ncol <-
    ncol(data)  # 获取数据框的列数 (Get the number of columns in the data frame)
  output <-
    data.frame()  # 创建一个空的数据框用于存储结果 (Create an empty data frame to store the results)
  k <- 1
  for (i in 1:(ifelse(simply, ncol - 1, ncol))) {
    x <- data[, i]
    for (j in (ifelse(simply, i + 1, 1)):ncol) {
      if (j != i) {
        y <- data[, j]

        # 根据是否包含截距项选择线性回归模型
        if (length(intercept) == 1) {
          if (intercept) {
            lm.my <- lm(y ~ x)
            outputcol <-
              c(
                'x',
                'y',
                'r.squared',
                'adj.r.squared',
                'intercept',
                'slope',
                'Std.Error.intercept',
                'Std.Error.slope',
                't.intercept',
                't.slope',
                'Pr.intercept',
                'Pr.slope'
              )
          } else {
            lm.my <- lm(y ~ x + 0)
            outputcol <-
              c(
                'x',
                'y',
                'r.squared',
                'adj.r.squared',
                'slope',
                'Std.Error.slope',
                't.slope',
                'Pr.slope'
              )
          }
        } else {
          message('one of the parameters must have a length = 1.')  # 报错，如果参数的长度不为1 (Throw an error if the length of the parameters is not 1)
        }

        # 提取线性回归模型的统计摘要信息 (Extract the statistical summary of the linear regression model)
        lm.sum <- summary(lm.my)
        output <-
          rbind(output, rep(NA, length(outputcol)))  # 在结果数据框中添加一行空白行 (Add a blank row to the result data frame)
        output[k, 1:2] <-
          names(data)[c(i, j)]  # 存储自变量和因变量的列名 (Store the column names of the independent and dependent variables)
        output[k, 3:length(outputcol)] <-
          c(lm.sum$r.squared,
            lm.sum$adj.r.squared,
            c(lm.sum$coefficients))  # 存储相关统计信息 (Store the relevant statistical information)
        k <- k + 1
      }
    }
  }
  names(output) <-
    outputcol  # 设置结果数据框的列名 (Set the column names of the result data frame)
  return(output)  # 返回结果数据框 (Return the result data frame)
}

#' Enhancement of names()
#'
#' @param data a dataframe
#'
#' @return a list
#' @export
#' @examples
#' df <- data.frame(a = NA, b = NA, c = NA)
#' name(df)
#'
name <- function(data) {
  # 提取数据框的列名并创建一个单行数据框
  # Extract the column names from the data frame and create a single-row data frame
  y <- as.data.frame(matrix(names(data), nrow = 1))
  names(y) <- 1:length(names(data))

  # 返回结果列表，包括列名的向量、列名的字符串和单行数据框
  # Return a list of results, including a vector of column names, a string of column names, and a single-row data frame
  return(list(names(data),
              paste(
                "'", paste(names(data), collapse = "','"), "'", sep = ''
              ),
              y))
}

#' plot pair-wise correlations. in: a dataframe. out: a figure.
#'
#' @param data a dataframe
#' @param lower.panel can be panel.lm or panel.smooth
#' @param upper.panel panel.cor
#' @param diag.panel panel.diag
#' @param lwd numeric
#' @param col colors
#' @param labels character
#' @param cex.labels character
#'
#' @return a pair plot
#' @export
#' @examples
#' df <- data.frame(a = 1:10, b = 1:10 + rnorm(10), c = 1:10 + rnorm(10))
#' plotpairs(df)
#'
plotpairs <-
  function(data,
           lower.panel = c(panel.lm, panel.smooth)[[1]],
           upper.panel = panel.cor,
           diag.panel  =  panel.diag,
           lwd = 2,
           col = "grey",
           labels = names(data),
           cex.labels = 4) {
    # Function to create scatterplot matrix with additional panels

    # remove character columns and NA values
    # 去除字符列和缺失值
    data <- data[, lapply(data, class) != 'character']
    datana <- is.na(data)
    data <- data[(rowSums(datana) == 0),]

    panel.hist <- function(x, ...) {
      # Function to create histogram panel
      # 生成直方图的函数
      usr <- par("usr")
      on.exit(par(usr))
      par(usr = c(usr[1:2], 0, 1.5))
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks
      nB <- length(breaks)
      y <- h$counts
      y <- y / max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
    }

    panel.cor <- function(x,
                          y,
                          digits = 2,
                          prefix = "",
                          cex.cor,
                          ...) {
      # Function to create correlation panel
      # 绘制相关系数矩阵的函数
      usr <- par("usr")
      on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- cor(x, y)

      test <- cor.test(x, y)
      Signif <- ifelse(
        test$p.value < 0.01,
        "p < 0.01",
        ifelse(
          0.01 <= test$p.value & test$p.value < 0.05,
          "p < 0.05",
          paste("p = ", round(test$p.value, 3), sep =
                  "")
        )
      )

      txt <- format(round(r, 2), digits = digits)[1]
      txt <- paste(prefix, txt, sep = "")
      if (missing(cex.cor))
        cex.cor <- 0.8 / strwidth(txt)

      text(
        0.5,
        0.5,
        paste('R =', txt),
        cex = 2 * abs(r),
        col = c(rgb(
          1, seq(0, 0.5, length.out = 10), seq(0, 0.5, length.out = 10)
        ), rgb(0.5, 0.5, 0.5), rgb(
          seq(0.5, 0, length.out = 10), seq(0.5, 0, length.out = 10), 1
        ))[round(r * 10, 0) + 11]
      ) # size and gradient color
      text(
        0.5,
        0.2,
        Signif,
        col = ifelse(round(test$p.value, 3) < 0.05, "red", "black"),
        font = ifelse(round(test$p.value, 3) < 0.01, 2, 1),
        cex = 1
      )
    }

    panel.diag = function (x, ...) {
      # Function to create diagonal panel
      # 对角线绘制直方图和密度曲线的函数
      par(new = TRUE)
      hist(
        x,
        col = "grey",
        probability = TRUE,
        axes = FALSE,
        main = ""
      )
      lines(density(x),
            col = "blue",
            lwd = 3)
      rug(x)
    }

    panel.lm <-
      function (x,
                y,
                col = par("col"),
                bg = NA,
                pch = par("pch"),
                cex = 1,
                col.regres = "red",
                ...) {
        # Function to create scatter plot with regression line
        # 绘制带回归线的散点图的函数
        points(
          x,
          y,
          pch = pch,
          col = col,
          bg = bg,
          cex = cex
        )
        ok <- is.finite(x) & is.finite(y)
        if (any(ok))
          abline(stats::lm(y[ok] ~ x[ok]), col = col.regres, ...)
      }

    pairs(
      data,
      lower.panel = lower.panel,
      upper.panel = upper.panel,
      diag.panel  =  diag.panel,
      lwd = 2,
      col = "darkgrey",
      labels = labels,
      cex.labels = 2,
      pch  = 16
    )
  }


#' plot pair-wise correlations  with p value. in: a dataframe. out: a figure.
#'
#' @param data a dataframe
#' @param lower.panel can be panel.lm or panel.smooth
#' @param upper.panel panel.cor
#' @param diag.panel panel.diag
#' @param lwd numeric
#' @param col colors
#' @param labels character
#' @param cex.labels character
#'
#' @return a pair plot
#' @export
#' @examples
#' df <- data.frame(a = 1:10, b = 1:10 + rnorm(10), c = 1:10 + rnorm(10))
#' plotpairs2(df)

plotpairs2 <-
  function(data,
           lower.panel = panel.smooth,
           upper.panel = panel.cor,
           diag.panel  =  panel.diag,
           lwd = 2,
           col = "grey",
           labels = '',
           cex.labels = 4) {
    # Function to create scatterplot matrix with additional panels
    # 创建带有额外面板的散点图矩阵的函数

    panel.cor <- function(x,
                          y,
                          digits = 2,
                          prefix = "",
                          cex.cor,
                          ...) {
      # Function to create correlation panel
      # 创建相关系数面板的函数
      usr <- par("usr")
      on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- cor(x, y)
      txt <- format(c(r, 0.123456789), digits = digits)[1]
      txt <- paste(prefix, txt, sep = "")
      if (missing(cex.cor))
        cex.cor <- 0.8 / strwidth(txt)
      test <- cor.test(x, y)
      Signif <- if (round(test$p.value, 3) < 0.01)
      {
        print("p<0.01")
      }
      else
      {
        if (0.01 <= round(test$p.value, 3) & round(test$p.value, 3) < 0.05)
        {
          print("p<0.05")
        }
        else
        {
          paste("p=", round(test$p.value, 3), sep = "")
        }
      }
      text(
        0.5,
        0.35,
        Signif,
        col = ifelse(round(test$p.value, 3) < 0.05, "red", "black"),
        font = ifelse(round(test$p.value, 3) < 0.01, 2, 1),
        cex = 1
      )
      text(0.5,
           0.65,
           txt,
           col = ifelse(r < 0, "red", "blue"),
           cex = 1.3)
    }

    panel.smooth <-
      function (x,
                y,
                col = "grey",
                bg = NA,
                pch = 18,
                cex = 0.8,
                col.smooth = "red",
                span = 2 / 3,
                iter = 3,
                ...) {
        # Function to create smooth panel
        # 创建平滑面板的函数
        points(
          x,
          y,
          pch = pch,
          col = col,
          bg = bg,
          cex = cex
        )
        ok <- is.finite(x) & is.finite(y)
        if (any(ok))
          lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
                col = col.smooth, ...)
      }

    panel.diag = function (x, ...) {
      # Function to create diagonal panel
      # 创建对角线面板的函数
      par(new = TRUE)
      hist(
        x,
        col = "grey",
        probability = TRUE,
        axes = FALSE,
        main = ""
      )
      lines(density(x),
            col = "blue",
            lwd = 2)
      dnormseq <- round(min(x), digits = 0):round(max(x), digits = 0)
      lines(dnormseq,
            dnorm(dnormseq, mean(x), sd(x)),
            col = "red",
            lwd = 2)
    }

    pairs(
      data,
      lower.panel = lower.panel,
      upper.panel = upper.panel,
      diag.panel  =  diag.panel,
      lwd = 2,
      col = "grey",
      labels = labels,
      cex.labels = 4
    )
    # Create the scatterplot matrix using the specified panels and settings
    # 使用指定的面板和设置创建散点图矩阵
  }


#' plot daily download counts of packages
#'
#' @param mypkg character vector of package names.
#' @param from character in 'Y-m-d'
#' @param type the same as that in 'plot()'
#' @param pch the same as that in 'plot()'
#' @param col the same as that in 'plot()'
#' @param cex the same as that in 'plot()'
#' @param textcex cex of the package name
#' @param to character in 'Y-m-d'
#'
#' @return a figure
#' @export
#'
#' @examples plotpkg(mypkg = 'rmarkdown')
plotpkg <- function(mypkg = 'bookdownplus',
                    from = Sys.Date() - 30,
                    to = Sys.Date(),
                    type = 'o',
                    pch = 19,
                    col = 'blue',
                    cex = 1,
                    textcex = 5) {
  # Function to plot package downloads from CRAN
  # 绘制CRAN软件包下载量的函数

  from <- as.Date(from)
  to <- as.Date(to)
  if (class(try(cranlogs::cran_downloads(
    packages = mypkg,
    from = from,
    to = to
  ))
  ) != 'try-error') {
    # Check if package downloads data is available
    # 检查软件包下载数据是否可用

    nr_down <-
      cranlogs::cran_downloads(packages = mypkg,
                               from = from,
                               to = to)
    # Get the number of downloads
    # 获取下载次数

    nr_down$sum <- cumsum(nr_down$count)
    # Calculate the cumulative sum of downloads
    # 计算下载次数的累积和

    oldpar <- par(mar = c(2, 6, 0.5, 0), las = 1)
    on.exit(par(oldpar))
    # Set plot parameters
    # 设置绘图参数

    plot(
      nr_down$date,
      nr_down$sum,
      xlab = '',
      ylab = 'Downloads',
      type = type,
      pch = pch,
      col = col,
      cex = cex
    )
    # Plot the downloads
    # 绘制下载量曲线

    legend(
      'topright',
      legend = paste0('Total: ', sum(nr_down$count, na.rm = TRUE)),
      bty = 'n',
      cex = cex
    )
    # Add legend showing the total number of downloads
    # 添加图例显示总下载量

    par(new = TRUE)
    plot(
      0:1,
      0:1,
      xlab = '',
      ylab = '',
      axes = FALSE,
      type = 'n'
    )
    # Create an empty plot for displaying the package name
    # 创建一个空图用于显示软件包名称

    text(0.5, 0.5, mypkg, cex = textcex, col = 'grey')
    # Add the package name as text
    # 添加软件包名称作为文本
  } else {
    message('The server is unavailable. Please try later.')
    # Display a message if the server is unavailable
    # 如果服务器不可用，则显示消息
  }
}


#' plot a blank figure
#' @return a blank figure
#' @export
#' @examples
#' plotblank()
#'
plotblank <- function() {
  plot(
    1,
    type = 'n',
    axes = FALSE,
    xlab = '',
    ylab = ''
  )
}

#' A reminder for colors
#' @return  a figure
#' @export
#' @examples
#' plotcolors()
#'
plotcolors <- function() {
  SetTextContrastColor <- function(color)
  {
    ifelse(mean(col2rgb(color)) > 127, "black", "white")  # 判断文本对比色，如果RGB均值大于127，则返回黑色，否则返回白色
  }
  # Define this array of text contrast colors that correponds to each
  # member of the colors() array.
  TextContrastColor <-
    unlist(lapply(colors(), SetTextContrastColor))  # 根据colors()数组中的颜色定义文本对比色数组

  oldpar <-
    par(mfrow = c(2, 1), mar = c(0, 0, 0, 0))  # 设置绘图参数，创建一个2行1列的画布，设置边距
  # 1a. Plot matrix of R colors, in index order, 25 per row.
  # This example plots each row of rectangles one at a time.
  colCount <- 25 # number per row  # 每行显示的颜色数量
  rowCount <- 27  # 总行数
  plot(
    c(1, colCount),
    c(0, rowCount),
    type = "n",
    ylab = "",
    xlab = "",
    axes = FALSE,
    ylim = c(rowCount, 0)
  )  # 创建绘图区域，设置坐标轴和边界

  for (j in 0:(rowCount - 1))
  {
    base <- j * colCount  # 当前行的起始索引
    remaining <- length(colors()) - base  # 剩余可用颜色数
    RowSize <-
      ifelse(remaining < colCount, remaining, colCount)  # 当前行的颜色数，如果剩余颜色不足，则使用剩余颜色数
    rect((1:RowSize) - 0.5,
         j - 0.5,
         (1:RowSize) + 0.5,
         j + 0.5,
         border = "black",
         col = colors()[base + (1:RowSize)]
    )  # 绘制矩形表示颜色，并设置边界颜色和填充颜色
    text((1:RowSize),
         j,
         paste(base + (1:RowSize)),
         cex = 0.7,
         col = TextContrastColor[base + (1:RowSize)])  # 在矩形下方显示颜色索引，并设置文本对比色
  }

  # 1b. Plot matrix of R colors, in "hue" order, 25 per row.
  # This example plots each rectangle one at a time.
  RGBColors <-
    col2rgb(colors()[1:length(colors())])  # 提取colors()中颜色的RGB值
  HSVColors <- rgb2hsv(RGBColors[1, ], RGBColors[2, ], RGBColors[3, ],
                       maxColorValue = 255)  # 将RGB值转换为HSV值
  HueOrder <-
    order(HSVColors[1, ], HSVColors[2, ], HSVColors[3, ])  # 根据Hue、Saturation、Value进行排序
  plot(
    0,
    type = "n",
    ylab = "",
    xlab = "",
    axes = FALSE,
    ylim = c(rowCount, 0),
    xlim = c(1, colCount)
  )  # 创建绘图区域，设置坐标轴和边界

  for (j in 0:(rowCount - 1))
  {
    for (i in 1:colCount)
    {
      k <- j * colCount + i
      if (k <= length(colors()))
      {
        rect(i - 0.5,
             j - 0.5,
             i + 0.5,
             j + 0.5,
             border = "black",
             col = colors()[HueOrder[k]])  # 绘制矩形表示颜色，并设置边界颜色和填充颜色
        text(i,
             j,
             paste(HueOrder[k]),
             cex = 0.7,
             col = TextContrastColor[HueOrder[k]])  # 在矩形下方显示颜色索引，并设置文本对比色
      }
    }
  }
  par(oldpar)  # 恢复原始绘图参数
}


#' A reminder for color bars. More palettes can be found in 'colormap', 'RColorBrewer', and 'dichromat' packages.
#'
#' @return a figure
#' @export
#'
#' @examples plotcolorbar()
plotcolorbar <- function() {
  mycex <- 3
  oldpar <-
    par(mfrow = c(7, 1), mar = c(2, 0, 0, 0))
  on.exit(par(oldpar))
  # 绘制颜色条形图函数
  # Function for plotting color bar charts

  colorbar <- function(myfunction) {
    barplot(
      rep(1, 100),
      col = get(myfunction)(100)[1:100],
      yaxt = 'n',
      border = NA,
      space = 0
    )
    # 绘制条形图，使用指定的颜色函数生成颜色
    # Plot bar chart using the specified color function to generate colors

    legend(
      'center',
      legend = paste0(myfunction, '(n)'),
      bty = 'n',
      cex = mycex
    )
    # 在图中心显示图例，图例内容为颜色函数的名称加上'(n)'
    # Display legend in the center of the plot with the name of the color function followed by '(n)'

    axis(
      1,
      at = seq(0, 100, by = 10),
      tck = 0.2,
      col.ticks = 'white',
      col = 'white'
    )
    # 绘制x轴刻度线
    # Draw x-axis tick lines

    axis(
      3,
      at = seq(0, 100, by = 10),
      labels = NA,
      tck = 0.2,
      col = 'white'
    )
    # 绘制顶部的x轴刻度线
    # Draw top x-axis tick lines
  }

  lapply(
    X = c(
      'rainbow',
      'heat.colors',
      'terrain.colors',
      'topo.colors',
      'cm.colors'
    ),
    FUN = colorbar
  )
  # 调用colorbar函数绘制五种不同颜色函数生成的颜色条形图
  # Call colorbar function to plot color bar charts generated by five different color functions

  barplot(
    rep(1, 100),
    col = gray(1:100 / 100),
    yaxt = 'n',
    border = NA,
    space = 0
  )
  # 绘制灰度颜色条形图
  # Plot grayscale color bar chart

  legend('center',
         legend = 'gray(x)',
         bty = 'n',
         cex = mycex)
  # 在图中心显示图例，图例内容为'gray(x)'
  # Display legend in the center of the plot with the name 'gray(x)'

  axis(
    1,
    at = seq(0, 100, by = 10),
    labels = seq(0, 100, by = 10) / 100,
    tck = 0.2,
    col.ticks = 'white',
    col = 'white'
  )
  # 绘制x轴刻度线
  # Draw x-axis tick lines

  barplot(
    rep(1, 360),
    col = hcl(1:360),
    yaxt = 'n',
    border = NA,
    space = 0
  )
  # 绘制hcl颜色条形图
  # Plot hcl color bar chart

  legend('center',
         legend = 'hcl(x)',
         bty = 'n',
         cex = mycex)
  # 在图中心显示图例，图例内容为'hcl(x)'
  # Display legend in the center of the plot with the name 'hcl(x)'

  axis(
    1,
    at = seq(0, 360, by = 30),
    labels = seq(0, 360, by = 30),
    tck = 0.2,
    col.ticks = 'white',
    col = 'white'
  )
  # 绘制x轴刻度线
  # Draw x-axis tick lines
}


#' A reminder for lty
#'
#' @param mylwd numeric. line width
#'
#' @return a figure reminding you lty
#' @export
#' @examples
#' plotlty()
#'
plotlty <- function(mylwd = 2) {
  ltynr <- 6
  # 定义线型数量
  # Define the number of line types

  plot(
    0:ltynr + 1,
    0:ltynr + 1,
    type = 'n',
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  # 创建空白绘图区域
  # Create an empty plot region

  axis(2,
       las = 1,
       lwd = 0,
       at = seq(1, ltynr))
  # 绘制y轴刻度线
  # Draw y-axis tick lines

  abline(h = seq(1, ltynr),
         lty = 1:ltynr,
         lwd = mylwd)
  # 绘制水平线，使用不同的线型
  # Draw horizontal lines with different line types
}



#' A reminder for pch
#'
#' @param mycex cex
#'
#' @return a figure reminding you pch
#' @export
#' @examples
#' plotpch()
#'
plotpch <- function(mycex = 2) {
  n_row <- 2
  n_col <- 13
  mypch <- 0:(n_row * n_col - 1)
  x <- rep(1:n_col, n_row)
  y <- rep(seq(1, 2, length.out = n_row), each = n_col)
  # 创建数据点的横坐标和纵坐标
  # Create the x and y coordinates for the data points
  plot(
    x,
    y,
    pch = mypch,
    ylim = c(0.5, 2.5),
    cex = mycex,
    xlab = '',
    ylab = '',
    axes = FALSE
  )
  # 绘制散点图
  # Draw a scatter plot

  text(
    x,
    y,
    labels = mypch,
    pos = 1,
    offset = 1,
    cex = mycex
  )
  # 在每个数据点上绘制标签
  # Add labels to each data point
}


#' A reminder for type
#' @return a figure reminding you type
#' @export
#' @examples
#' plottype()
#'
plottype <- function() {
  oldpar <-
    par(mfrow = c(3, 3),
        cex = 1.2,
        mar = c(0, 0, 0, 0))
  on.exit(par(oldpar))
  # 设置绘图参数，将图形排列为3行3列，设置文字大小为1.2，设置边距为0
  # Set plotting parameters: arrange the plots in a 3x3 grid, set the text size to 1.2, and set the margin to 0

  y <- rnorm(n = 6)
  # 生成随机正态分布数据作为y坐标
  # Generate random normal distribution data for the y-coordinates

  for (i in c("p", 'l', "b", "c", "o", "h", "s", "S", "n")) {
    # 遍历不同的绘图类型

    plot(
      x = 1:6,
      y,
      type = i,
      axes = FALSE,
      cex = 1.5
    )
    # 绘制散点图、线图、带线框的散点图、阶梯图等不同类型的图形
    # Draw scatter plots, line plots, scatter plots with a connecting line, step plots, etc.

    box()
    # 添加边框

    legend(
      'bottomright',
      legend = paste('type = "', i, '"', sep = ''),
      bty = "n",
      text.col = 'blue'
    )
    # 添加图例，显示当前绘图类型
    # Add a legend displaying the current plot type
  }
}


#' Read multiple tables into a list.
#'
#' @param sep the field separator character.
#' @param mydir the folder path
#' @param output the type of the output. 'list' or 'data.frame'.
#' @param header logical. Indicating whether the file contains the names of the variables as its first line.
#' @param skip the number of lines of the data file to skip before beginning to read data.
#'
#' @return a list or a data frame
#' @export
#'
readdir <-
  function(mydir = getwd(),
           sep = c(","),
           output = c('list', 'data.frame'),
           header = TRUE,
           skip = 0)
  {
    x <- dir(mydir, full.names = TRUE)
    # 获取指定目录下的文件名（包括路径）
    # Get the file names (including paths) in the specified directory

    x_name <- dir(mydir)
    # 获取指定目录下的文件名（不包括路径）
    # Get the file names (excluding paths) in the specified directory

    sep <- rep_len(sep, length(x))
    # 将分隔符重复为与文件数量相同的长度
    # Repeat the separator to match the length of the number of files

    output <- match.arg(output)
    # 确保output参数的值为'list'或'data.frame'
    # Ensure that the value of the 'output' parameter is either 'list' or 'data.frame'

    if (output == 'list') {
      y <- list()
      # 创建一个空列表，用于存储读取的文件数据
      # Create an empty list to store the read file data

      for (i in 1:length(x)) {
        y[[x_name[i]]] <-
          read.table(
            x[i],
            header = header,
            sep = sep[i],
            fill = TRUE,
            na.strings = c("Inf", "-Inf", "NA", "NaN"),
            stringsAsFactors = FALSE,
            skip = skip
          )
        # 读取每个文件的数据，并将其存储在列表中，以文件名为索引
        # Read the data from each file and store it in the list with the file name as the index
      }
    }
    if (output == 'data.frame') {
      y <- data.frame()
      # 创建一个空数据框，用于存储读取的文件数据
      # Create an empty data frame to store the read file data

      for (i in 1:length(x)) {
        newy <- read.table(
          x[i],
          header = header,
          sep = sep[i],
          fill = TRUE,
          na.strings = c("Inf", "-Inf", "NA", "NaN"),
          stringsAsFactors = FALSE,
          skip = skip
        )
        newy$filename <- x_name[i]
        # 读取每个文件的数据，并将文件名作为一个列添加到数据框中
        # Read the data from each file and add the file name as a column to the data frame

        y <- rbind(y, newy)
        # 将读取的数据框与之前的数据框合并
        # Combine the read data frame with the previous data frame
      }
    }
    return(y)
    # 返回读取的文件数据，类型根据output参数确定（列表或数据框）
    # Return the read file data, with the type determined by the output parameter (list or data frame)
  }


#' Create a new R package demo folder
#' @return a folder with an R package skeleton
#' @export
#' @examples
#' rpkg()
#'
rpkg <- function() {
  mypath <- paste0(.libPaths(), '/beginr/zip/')
  unzip(paste0(mypath[dir.exists(mypath)][1], 'rpkg.zip'))
}

#' standard error
#'
#' @param x numeric
#' @param na.rm logical
#'
#' @return se
#' @export
#' @examples
#' se(1:10)
#'
se <- function(x, na.rm = TRUE) {
  sd(x, na.rm = na.rm) / sqrt(sum(!is.na(x)))
}


#' a friendly version of tapply for dataframes
#'
#' @param data dataframe
#' @param select character, column names to calc
#' @param myfactor a colname as factor
#' @param ... function to apply to data
#' @param na.rm logical
#'
#' @return a dataframe
#' @export
#'
# tapplydf <- function(data, select = names(data), myfactor, ..., na.rm = c(TRUE, FALSE, NULL)[1])
# {
#   if (is.null(na.rm)) {
#     y <- data.frame(tapply(data[, select[1]], data[, myfactor],...))
#   } else {
#     y <- data.frame(tapply(data[, select[1]], data[, myfactor],..., na.rm = na.rm))
#   }
#   names(y) <- select[1]
#   y[, myfactor] <- rownames(y)
#   y <- y[, c(2,1)]
#   if (length(select) > 1){
#     for (i in select[-1]){
#       if (is.null(na.rm)) {
#         yi <- data.frame(tapply(data[, i], data[, myfactor],...))
#       } else {
#         yi <- data.frame(tapply(data[, i], data[, myfactor],..., na.rm = na.rm))
#       }
#       names(yi) <- i
#       yi[, myfactor] <- rownames(yi)
#       y <- merge(y, yi, by = myfactor)
#
#     }
#   }
#   return(y)
# }
tapplydf <-
  function(data,
           select = names(data),
           myfactor,
           ...,
           na.rm = c(TRUE, FALSE, NULL)[1])
  {
    y <- tapply2(data, select[1], myfactor, ..., na.rm = na.rm)
    # 使用tapply2函数将数据按照第一个选择变量和分组因子进行聚合
    # Aggregate the data using the first selected variable and the grouping factor using the tapply2 function

    if (length(select) > 1) {
      for (i in select[-1]) {
        yi <- tapply2(data, i, myfactor, ..., na.rm = na.rm)
        # 对于每个剩余的选择变量，使用tapply2函数进行聚合
        # For each remaining selected variable, aggregate using the tapply2 function

        y <- merge(y, yi, by = myfactor[1])
        # 将聚合结果与之前的结果合并，按照第一个分组因子进行合并
        # Merge the aggregated results with the previous results, merging by the first grouping factor
      }
    }
    return(y)
    # 返回聚合结果
    # Return the aggregated results
  }



#' a friendly version of tapply for a column in a dataframe
#'
#' @param data dataframe
#' @param select character, column names to calc
#' @param myfactor a colname as factor
#' @param ... function to apply to data
#' @param na.rm logical
#'
#' @return a dataframe
#' @export
tapply2 <-
  function(data,
           select = names(data)[1],
           myfactor,
           ...,
           na.rm = c(TRUE, FALSE, NULL)[1])
  {
    if (is.null(na.rm)) {
      y <- data.frame(tapply(data[, select], data[, myfactor], ...))
    } else {
      y <-
        data.frame(tapply(data[, select], data[, myfactor], ..., na.rm = na.rm))
    }
    # 使用tapply函数对数据进行分组聚合，并将结果存储在y中
    # Use the tapply function to aggregate data based on grouping factor, and store the result in y

    if (length(myfactor) == 1)
      names(y) <- paste0(select)
    else
      names(y) <- paste0(select, '_', names(y))
    # 根据聚合结果的维度设置列名，如果只有一个分组因子，则列名为选择变量的名称；
    # 否则，列名为选择变量名称加上对应的分组因子名称
    # Set column names based on the dimensions of the aggregation result, if there is only one grouping factor, column names are set to the name of the selected variable;
    # otherwise, column names are set to the concatenation of the selected variable name and the corresponding factor names

    y[, myfactor[1]] <- rownames(y)
    # 将第一个分组因子的值赋给结果的第一列
    # Assign the values of the first grouping factor to the first column of the result

    y <- y[, c(ncol(y), 1:(ncol(y) - 1))]
    # 调整列的顺序，将第一列移动到最后
    # Rearrange the columns by moving the first column to the last position

    return(y)
    # 返回聚合结果
    # Return the aggregated result
  }


#' a friendly version of tapply
#'
#' @param colname character
#' @param x a datafrom
#' @param ... the function to apply to data
#' @param factor factor for tapply
#'
#' @return a dataframe
#' @export
#'
tapplydfv <- function(colname = "tapply", x, factor, ...)
{
  y <- data.frame(tapply(x, factor, ..., na.rm = TRUE))
  # 使用tapply函数对向量x按照因子factor进行聚合，并将结果存储在y中
  # Use the tapply function to aggregate vector x based on factor, and store the result in y

  names(y) <- colname
  # 设置结果的列名为colname
  # Set the column names of the result to colname

  y$rownames <- rownames(y)
  # 将结果的行名赋给y$rownames列
  # Assign the row names of the result to the y$rownames column

  y <- y[, c(2, 1)]
  # 调整列的顺序，将y$rownames列移动到第一列
  # Rearrange the columns by moving the y$rownames column to the first position

  return(y)
  # 返回聚合结果
  # Return the aggregated result
}

#' save csv file with asking if the file already exists.
#'
#' @param data a data frame
#' @param writefile destination file
#' @param row.names logical
#'
#' @return write a file
#' @export
#'
writefile <- function(data, writefile, row.names = FALSE) {
  if (file.exists(writefile)) {
    warning('File exists! New file is not saved!')
  } else {
    write.csv(data, file = writefile, row.names = row.names)
  }
}
