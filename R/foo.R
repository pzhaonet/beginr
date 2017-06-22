#' Create a bib file for R packages, including the citations of user-defined packages.
#' @param pkg packages
#' @param bibfile file path and name to save the bib entries.
#' @return a bib file
#' @export
#' @examples
#' mf_bib()
#' mf_bib(pkg = c('base', 'knitr'))
mf_bib <- function(pkg = c('base'), bibfile = 'packages.bib'){
  pkg <- unique(pkg[order(pkg)])
  for (i in pkg){
    # if (class(try(citation(i))) == 'try-error') install.packages(i)
    cti <- toBibtex(citation(i))
    entryloc <- grep(pattern = '^@', cti)
    cti[entryloc] <- gsub(',', paste('R-',i, ',', sep =''), cti[entryloc])
    symbol6loc <- grep('&', cti)
    for (j in symbol6loc) {
      cti[j] <- gsub(pattern = ' &', replacement = ' \\\\&', cti[j])
    }
    if (length(entryloc) > 1)  cti[entryloc] <- paste(substr(cti[entryloc], 1, nchar(cti[entryloc])-1), 1:length(entryloc), ',', sep ='')
    cat(cti, sep = '\n', file = bibfile, append = TRUE)
  }
}

#' Plot a dataframe, multiple ys against one x
#' @param x a vector
#' @param y a vector or a dataframe with the same length as x
#' @param add logical, whether to add this plot to the previous one
#' @param xlab character
#' @param ylab character
#' @param myaxes logical, whether to display axes automatically
#' @param xlim
#' @param ylim
#' @param mycol colours
#' @param mytype
#' @param mypch
#' @param mycex
#' @param mylty
#' @param lwd
#' @param xerror errorbar, same dimension of x
#' @param yerror same dimension of y
#' @param mycolerrorbar error bar colours
#' @param mylegend
#' @param mylegendcol
#' @param myledendcex numeric
#' @param legendpos character
#' @return a figure
#' @export
#' @examples
#'
mf_dfplot <- function(x, y,
                      add = FALSE,
                      xlab = '', ylab = '',
                      myaxes = FALSE,
                      xlim = NULL, ylim = NULL,
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
  if (is.null(ylim)) ylim <- range(y, na.rm = TRUE)
  if (add == FALSE) {
    plot(x, y[, 1],
         xlab = xlab, ylab = ylab,
         axes = myaxes, xlim = xlim, ylim = ylim, type = 'n', cex = mycex, lty = ifelse(is.null(mylty), 1, mylty[1]))
  }
  # ny <- ifelse(is.data.frame(y), dim(y)[2], 1)
  ny <- ncol(y)
  if (is.null(mycol)) {
    mycol <- rainbow(ny)
  }
  if (is.null(mylty)) {
    mylty <- rep(1, ny)
  }
  if (is.null(mycolerrorbar)) {
    mycolerrorbar <- rainbow(ny, alpha =  0.5)
  }
  for (i in 1:ny){
    if (!is.null(xerror)){
      polygon(x = c(x + xerror, rev(x - xerror)),
              y = c(y[, i], rev(y[, i])),
              col = mycolerrorbar[i], border = NA)
    }
    if (!is.null(yerror)){
      yerror <- as.data.frame(yerror)
      yerror[is.na(yerror[, i]), i] <- 0
      polygon(c(x, rev(x)),
              c(y[, i] + yerror[, i], rev(y[, i] - yerror[, i])),
              col = mycolerrorbar[i], border = NA)
    }
    points(x, y[, i], type = mytype, col = mycol[i], lwd = lwd, lty = mylty[i], cex = mycex, pch = mypch)
  }
  if (!is.null(mylegend)){
    legend(legendpos, legend = mylegend, col = mylegendcol, bty = 'n', lty = mylty, lwd = lwd, cex = mylegendcex)
  }
  box()
}

#' Plot a dataframe, one y against multiple xs
#' @param x a vector  or a dataframe with the same length as x
#' @param y a vector
#' @param xlab character
#' @param ylab character
#' @param xlim
#' @param ylim
#' @param mycol colours
#' @param xerror errorbar, same dimension of x
#' @param yerror same dimension of y
#' @param mycolerrorbar error bar colours
#' @param mylegend
#' @return a figure
#' @export
#' @examples
#'
mf_dfplot2 <- function(x, y,
                       xlab = 'x', ylab = 'y',
                       xlim = NULL, ylim = NULL,
                       mycol = NULL,
                       mylty = NULL,
                       xerror = NULL,
                       yerror = NULL,
                       mycolerrorbar = NULL,
                       mylegend = NULL) {
  par(las=1)
  x <- as.data.frame(x)
  plot(x[, 1], y,
       xlab = xlab, ylab = ylab,
       axes = FALSE, xlim = xlim, ylim = ylim, type = 'n')
  # ny <- ifelse(is.data.frame(y), dim(y)[2], 1)
  nx <- dim(x)[2]
  if (is.null(mycol)) {
    mycol <- rainbow(nx)
  }
  if (is.null(mylty)) {
    mylty <- rep(1, nx)
  }
  if (is.null(mycolerrorbar)) {
    mycolerrorbar <- rainbow(nx, alpha =  0.5)
  }
  for (i in 1:nx){
    if (!is.null(yerror)){
      polygon(x = c(x[, i] + xerror[, i], rev(x[, i] - xerror[, i])),
              y = c(y, rev(y)),
              col = mycolerrorbar[i], border = NA)
    }
    if (!is.null(xerror)){
      xerror <- as.data.frame(xerror)
      polygon(y = c(y, rev(y)),
              x = c(x[, i] + xerror[, i], rev(x[, i] - xerror[, i])),
              col = mycolerrorbar[i], border = NA)
    }
    points(x[, i], y, type = 'l', col = mycol[i], lwd = 2, lty = mylty[i])
  }
  if (!is.null(mylegend)){
    legend('top', legend = mylegend, col = mycol, bty = 'n', lty = mylty)
  }
  box()
}

#' add error bars to a scatterplot.
#' @param x
#' @param y
#' @param xupper
#' @param xlower
#' @param yupper
#' @param ylower
#' @param col
#' @param lty
#' @return errorbars in a plot
#' @export
#' @examples
#'
mf_errorbar <- function(x, y, xupper = NULL, xlower = NULL, yupper = NULL, ylower = NULL, col = 'black', lty = 1)
{
  if (!is.null(yupper)){
    arrows(x, y, x, y + yupper, angle = 90, length = 0.03, col = col, lty = lty)
  }
  if (!is.null(ylower)){
    arrows(x, y, x, y - ylower, angle = 90, length = 0.03, col = col, lty = lty)
  }
  if (!is.null(xupper)){
    arrows(x, y, x+xupper, y, angle = 90, length = 0.03, col = col, lty = lty)
  }
  if (!is.null(xlower)){
    arrows(x, y, x-xlower, y, angle = 90, length = 0.03, col = col, lty = lty)
  }
}


#' Plot a user-customized hist
#' @param data
#' @param mybreaks
#' @param myxlim
#' @param myylim
#' @param eightlines
#' @param eightdigit
#' @param eightcex
#' @param eightcolors
#' @param mylegend
#' @param myxlab
#' @param show_n
#' @param show_skewness
#' @param show_density
#' @return a hist plot
#' @export
#' @examples
#'
mf_hist <- function(data, mybreaks = "Sturges", myxlim = NULL, myylim = NULL,
                    eightlines = TRUE, eightdigit = 0, eightcex = 0.8, eightcolors = c('red','darkgreen','blue', 'black', 'purple', 'gold')[c(1,2,3,2,1,6,6,5,4,5)],
                    mylegend = '', myxlab = '',
                    show_n = TRUE, show_skewness = TRUE, show_density = FALSE) {
  if (is.null(myylim)) myylim <- c(0, max(hist(data, breaks = mybreaks, plot = FALSE)$density) * 1.1)
  if (is.null(myxlim)) {
    hist(data, col = 'grey', border = NA, main = '', freq = FALSE, breaks = mybreaks, xlab = myxlab, ylim = myylim)#, axes = FALSE, breaks = mybreaks)
  } else {
    hist(data, col = 'grey', border = NA, main = '', freq = FALSE, breaks = mybreaks, xlab = myxlab, xlim = myxlim, ylim = myylim)#, axes = FALSE, breaks = mybreaks)
  }
  if (show_density) lines(density(data[!is.na(data)], bw = "SJ"))
  rug(data, col = 'darkgrey')
  legend('topleft', bty = 'n', legend = mylegend)
  legend(
    'topright', bty = 'n',
    legend = paste(ifelse(show_n, paste('n = ', sum(!is.na(data)), '\n', sep = ''), ''),
                   ifelse(show_skewness, paste('skewness = ', round(mf_skewness(data), 2), sep = ''), ''),
                   sep = '')
  )
  if (eightlines) {
    myfive <- fivenum(data)
    threshold <- IQR(data, na.rm = TRUE) * 1.5
    abline(v = c(myfive, myfive[2] - threshold, myfive[4] + threshold), col = eightcolors[1:7])
    mtext(text = round(myfive, eightdigit), side = 3, line = c(0, 1, 0, 1, 0), at = myfive, col = eightcolors[1:5], cex = eightcex)
    # mtext(text = round(myfive[c(1, 3, 5)], eightdigit), side = 3, line = 0, at = myfive[c(1, 3, 5)], col = eightcolors[c(1, 3, 5)], cex = eightcex)
    # mtext(text = round(myfive[c(2, 4)], eightdigit), side = 3, line = 1, at = myfive[c(2, 4)], col = eightcolors[c(3, 5)], cex = eightcex)
    mymean <- mean(data, na.rm = TRUE)
    mysd <- sd(data, na.rm = TRUE)
    abline(v = seq(from = mymean - mysd, by = mysd, length.out = 3), col = eightcolors[8:10], lty = 2)
  }
  box()
  # return(c(myfive, threshold, mymean, mysd))
  return(data.frame(para = c('min', '1q', 'median', '3q', 'max', 'lower', 'upper', 'mean', 'sd'),
                    value =c(myfive, myfive[2] - threshold, myfive[4] + threshold, mymean, mysd)))
}

#' Save a list into an ASCII file. in: a list. out: a file.
#' @param x a list
#' @param file
#' @return a file
#' @export
#' @examples
#'
mf_list2ascii <- function(x, file = paste(deparse(substitute(x)), ".txt", sep = ""))
{
  # MHP July 7, 2004
  # R or S function to write an R list to an ASCII file.
  # This can be used to create files for those who want to use
  # a spreadsheet or other program on the data.
  #
  tmp.wid = getOption("width")  # save current width
  options(width=10000)          # increase output width
  sink(file)                    # redirect output to file
  print(x)                      # print the object
  sink()                        # cancel redirection
  options(width=tmp.wid)        # restore linewidth
  return(invisible(NULL))       # return (nothing) from function
}

#' plot a linear regression figure and return a list of parameters. in: two vectors. out: a figure and a list.
#' @param x
#' @param y
#' @param xlim
#' @param ylim
#' @param plot.title
#' @param xlab
#' @param ylab
#' @param refline logical, reference line
#' @param slope slope of refline
#' @param intercept of refline
#' @param showr2 logical
#' @param showleg logical
#' @return a figure
#' @export
#' @examples
#'
mf_lm <- function(x, y,
                  xlim = range(as.numeric(x), na.rm = TRUE), ylim = range(as.numeric(y), na.rm = TRUE),
                  plot.title="linear regression", xlab = 'x', ylab = 'y',
                  refline = FALSE, slope = 1, intercept = 0,
                  showr2 = TRUE,
                  showleg = TRUE){
  x <- as.numeric(x)
  y <- as.numeric(y)
  plot(x, y, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, col = "grey", pch = 19)
  lm.my <- lm(y ~ x)
  lm.sum <- summary(lm.my)
  lm.rs <- round(lm.sum$r.squared, digits = 3)
  b <- signif(lm.my$coefficients[1], 3)
  a <- signif(lm.my$coefficients[2], 3)

  abline(lm.my, col = "black", lwd = 2)
  if (refline) abline(a = intercept, b = slope, col = 'blue', lty = 2)
  text(xlim[1],
       ylim[2] - diff(ylim) * 0.1,
       # substitute(paste(italic(y), ' = ', a, italic(x), ' + ', b), list(a = a, b = b)),
       substitute(paste(italic(y), ' = ', a, italic(x), c, b), list(a = a, b = b, c = ifelse(b < 0, '', ' + '))),
       cex = 1.2, pos = 4)
  text(xlim[1],
       ylim[2]- diff(ylim) * 0.2,
       # expression(italic(R)^2==r, list(r = lm.rs)),
       as.expression(substitute(italic(n)==r, list(r = length(x)))),
       cex = 1.2, pos = 4)
  if (showr2)  text(xlim[1],
                    ylim[2]- diff(ylim)* 0.3,
                    # expression(italic(R)^2==r, list(r = lm.rs)),
                    as.expression(substitute(italic(R)^2==r, list(r = lm.rs))),
                    cex = 1.2, pos = 4)
  if (showleg) legend('bottomright', legend = c('data', 'linear', '1:1'), col = c('darkgrey', 'black', 'blue'), pch = c(19, -1, -1), lty = c(0, 1, 2), bty = 'n')
  return(list(lm.sum$coefficients, lm.sum$r.squared))
}

#' calculate linear regression between every two columns in a data frame. in: a dataframes. out: a dataframe showing the linear regression.
#' @param data a datafrram
#' @return another dataframe
#' @export
#' @examples
#'
mf_lmdf <- function(data, simply = FALSE, intercept = TRUE){
  ncol <- ncol(data)
  output <- data.frame()
  k <- 1
  for (i in 1:(ifelse(simply, ncol - 1, ncol))){
    x <- data[, i]
    for (j in (ifelse(simply, i + 1, 1)): ncol){
      if (j!=i) {
        y <- data[, j]
        if (intercept) {
          lm.my <- lm(y ~ x)
          outputcol <- c('x', 'y', 'r.squared', 'adj.r.squared', 'intercept', 'slope', 'Std.Error.intercept', 'Std.Error.slope', 't.intercept', 't.slope', 'Pr.intercept', 'Pr.slope')
        } else {
          lm.my <- lm(y ~ x + 0)
          outputcol <- c('x', 'y', 'r.squared', 'adj.r.squared', 'slope', 'Std.Error.slope', 't.slope', 'Pr.slope')
        }
        lm.sum <- summary(lm.my)
        output <- rbind(output, rep(NA, length(outputcol)))
        output[k, 1:2] <- names(data)[c(i,j)]
        output[k, 3:length(outputcol)] <- c(lm.sum$r.squared, lm.sum$adj.r.squared, c(lm.sum$coefficients))
        # output <- rbind(output, c(names(data)[c(i,j)], lm.sum$r.squared, lm.sum$adj.r.squared, c(lm.sum$coefficients)))
        k <- k + 1
      }
    }
  }
  names(output) <- outputcol
  return(output)
}

#' Enhancement of names()
#' @param data a dataframe
#' @return a list
#' @export
#' @examples
#'
mf_names <- function(data) {
  #   print(paste(names(data), collapse = "','"))
  #   print(matrix(names(data), nrow = 1))
  y <- as.data.frame(matrix(names(data), nrow = 1))
  names(y) <- 1:length(names(data))
  list(names(data),
       paste("'", paste(names(data), collapse = "','"), "'", sep = ''),
       y)
}



#' plot pair-wise correlations. in: a dataframe. out: a figure.
#' @param data a dataframe
#' @param lower.panel
#' @param upper.panel
#' @param diag.panel
#' @param lwd
#' @param col
#' @param labels
#' @param cex.labels
#' @return a pair plot
#' @export
#' @examples
#'
mf_pairs <- function(data, lower.panel = c(panel.lm, panel.smooth)[[1]], upper.panel=panel.cor, diag.panel  =  panel.diag, lwd = 2, col = "grey", labels=names(data), cex.labels=4){

  # remove character columns and NA values
  data <- data[, lapply(data, class) != 'character']
  datana <- is.na(data)
  data <- data[(rowSums(datana) == 0), ]

  panel.hist <- function(x, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
  }
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y)
    #   txt <- format(c(r, 0.123456789), digits=digits)[1]

    test <- cor.test(x,y)
    Signif <- ifelse(test$p.value < 0.01, "p < 0.01",
                     ifelse(0.01 <= test$p.value & test$p.value < 0.05,
                            "p < 0.05",
                            paste("p = ",round(test$p.value,3), sep="")))


    txt <- format(round(r, 2), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)

    #   text(0.5, 0.5, txt, cex = 2 * r + 4, col=c(rgb(1,seq(0,0.1,length.out=10),seq(0,0.9,length.out=10)),rgb(0.5,0.5,0.5), rgb(seq(0.1,0,length.out=10),seq(1,0,length.out=10),1))[round(r*10, 0)+11]) # size and gradient color
    text(0.5, 0.5, paste('R =', txt), cex = 2 * abs(r), col=c(rgb(1,seq(0,0.5,length.out=10),seq(0,0.5,length.out=10)),rgb(0.5,0.5,0.5), rgb(seq(0.5,0,length.out=10),seq(0.5,0,length.out=10),1))[round(r*10, 0)+11]) # size and gradient color

    text(0.5, 0.2, Signif, col=ifelse(round(test$p.value,3)<0.05, "red", "black"), font=ifelse(round(test$p.value,3)<0.01, 2, 1), cex=1)
    #  text(0.5, 0.5, txt, cex = cex.cor * r) # size
    #  text(0.5, 0.5, txt, col=rainbow(21)[round(r*10, 0)+11]) #rainbow color
  }
  panel.diag = function (x, ...) {
    par(new = TRUE)
    hist(x,
         #       col = "light blue",
         col = "grey",
         probability = TRUE,
         axes = FALSE,
         main = "")
    lines(density(x),
          #        col = "red",
          col = "blue",
          lwd = 3)
    rug(x)
  }

  panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                        cex = 1, col.regres = "red", ...)
  {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
      abline(stats::lm(y[ok] ~ x[ok]), col = col.regres, ...)
  }

  pairs(data, lower.panel=lower.panel, upper.panel=upper.panel, diag.panel  =  diag.panel, lwd = 2, col = "darkgrey", labels=labels, cex.labels=2, pch  = 16)
}

#' plot pair-wise correlations  with p value. in: a dataframe. out: a figure.
#' @param data a dataframe
#' @param lower.panel
#' @param upper.panel
#' @param diag.panel
#' @param lwd
#' @param col
#' @param labels
#' @param cex.labels
#' @return a pair plot
#' @export
#' @examples
#'
################ mf_pairs2 ########################################
mf_pairs2 <- function(data, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel  =  panel.diag, lwd = 2, col = "grey", labels='', cex.labels=4){
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y)
    txt <- format(c(r, 0.123456789), digits=digits)[1]#防止末位为0
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    test <- cor.test(x,y)
    Signif <- if(round(test$p.value,3)<0.01)
    {print("p<0.01")}
    else
    {
      if (0.01<=round(test$p.value,3) & round(test$p.value,3)<0.05)
      {print("p<0.05")}
      else
      {paste("p=",round(test$p.value,3), sep="")}
    }
    text(0.5, 0.35, Signif, col=ifelse(round(test$p.value,3)<0.05, "red", "black"), font=ifelse(round(test$p.value,3)<0.01, 2, 1), cex=1)
    text(0.5, 0.65, txt, col=ifelse(r<0, "red", "blue"), cex=1.3)
  }

  panel.smooth<-function (x, y, col = "grey", bg = NA, pch = 18, cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...)
  {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = col.smooth, ...)
  }

  panel.diag = function (x, ...) {
    par(new = TRUE)
    hist(x,
         #col = "light blue",
         col = "grey",
         probability = TRUE,
         axes = FALSE,
         main = "")
    lines(density(x),
          #col = "red",
          col = "blue",
          lwd = 2)
    dnormseq <-round(min(x),digits=0):round(max(x),digits=0)
    lines(dnormseq, dnorm(dnormseq, mean(x), sd(x)), col="red", lwd=2)
  }
  pairs(data, lower.panel=lower.panel, upper.panel=upper.panel, diag.panel  =  diag.panel, lwd = 2, col = "grey", labels=labels, cex.labels=4)
}

#' plot a blank figure
#' @return a blank figure
#' @export
#' @examples
#'
mf_plotblank <- function() plot(1, type = 'n', axes = FALSE, xlab = '', ylab = '')

#' A reminder for colors
#' @return  a figure
#' @export
#' @examples
#'
mf_plotcolors <- function(){
  SetTextContrastColor <- function(color)
  {
    ifelse( mean(col2rgb(color)) > 127, "black", "white")
  }
  # Define this array of text contrast colors that correponds to each
  # member of the colors() array.
  TextContrastColor <- unlist( lapply(colors(), SetTextContrastColor) )

  oldpar <- par(mfrow = c(2, 1), mar = c(0, 0, 0, 0))
  # 1a. Plot matrix of R colors, in index order, 25 per row.
  # This example plots each row of rectangles one at a time.
  colCount <- 25 # number per row
  rowCount <- 27
  plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
        axes=FALSE, ylim=c(rowCount,0))
  # title("R colors")

  for (j in 0:(rowCount-1))
  {
    base <- j*colCount
    remaining <- length(colors()) - base
    RowSize <- ifelse(remaining < colCount, remaining, colCount)
    rect((1:RowSize)-0.5,j-0.5, (1:RowSize)+0.5,j+0.5,
         border="black",
         col=colors()[base + (1:RowSize)])
    text((1:RowSize), j, paste(base + (1:RowSize)), cex=0.7,
         col=TextContrastColor[base + (1:RowSize)])
  }

  # 1b. Plot matrix of R colors, in "hue" order, 25 per row.
  # This example plots each rectangle one at a time.
  RGBColors <- col2rgb(colors()[1:length(colors())])
  HSVColors <- rgb2hsv( RGBColors[1,], RGBColors[2,], RGBColors[3,],
                        maxColorValue=255)
  HueOrder <- order( HSVColors[1,], HSVColors[2,], HSVColors[3,] )
  plot(0, type="n", ylab="", xlab="",
       axes=FALSE, ylim=c(rowCount,0), xlim=c(1,colCount))

  # title("R colors -- Sorted by Hue, Saturation, Value")

  for (j in 0:(rowCount-1))
  {
    for (i in 1:colCount)
    {
      k <- j*colCount + i
      if (k <= length(colors()))
      {
        rect(i-0.5,j-0.5, i+0.5,j+0.5, border="black", col=colors()[ HueOrder[k] ])
        text(i,j, paste(HueOrder[k]), cex=0.7, col=TextContrastColor[ HueOrder[k] ])
      }
    }
  }
  par(oldpar)
}

#' lty
#' @return a figure reminding you lty
#' @export
#' @examples
#'
mf_plotlty <- function(){
  ltynr <- 6
  plot(0:ltynr + 1, 0:ltynr + 1, type = 'n', axes = FALSE, xlab = "", ylab = "")
  axis(2, las = 1, lwd = 0, at = seq(1, ltynr))
  abline(h = seq(1, ltynr), lty = 1:ltynr )
}


#' pch numbers
#' @return a figure reminding you pch
#' @export
#' @examples
#'
mf_plotpch <- function(){
  mypch <- 0:25
  x <- rep(1:13, 2)
  y <- rep(c(1, 1.8), each = 13)
  plot(x, y, pch = mypch, ylim = c(0.5, 2.5), cex = 5, xlab = '', ylab = '', axes = FALSE)
  text(x, y, labels = mypch, pos = 1, offset = 2, cex = 2)
}

#' type
#' @return a figure reminding you type
#' @export
#' @examples
#'
mf_plottype <- function(){
  par(mfrow = c(3,3), cex = 1.2, mar = c(0, 0, 0, 0))
  y <- rnorm(n = 6)
  for (i in c("p", 'l', "b", "c", "o", "h", "s", "S", "n")) {
    plot(x= 1:6, y,  type = i, axes = FALSE, cex = 1.5)
    box()
    legend('bottomright', legend = paste('type = "', i, '"', sep = ''), bty = "n", text.col = 'blue')
  }
}


#' batch read several files. in: a path. out: a list containing all data frames
#' @param wd working dir
#' @param sep sep of each file
#' @return a list
#' @export
#' @examples
#'
mf_readdir <- function(wd = ".", sep = c(","))
{
  x <- dir("./")
  y <- list()
  for (i in 1:length(input.ls))
  {
    input.p <- paste(wd, x[i], sep = "\\")
    y[[as.character(i)]] <- read.table(input.p,
                                       header = TRUE,
                                       sep = sep[i],
                                       fill = TRUE,
                                       na.strings = c("Inf","-Inf","NA", "NaN"),
                                       stringsAsFactors = FALSE)
  }
  return(y)
}

#' standard error
#' @param x
#' @param na.rm logical
#' @return se
#' @export
#' @examples
#'
mf_se <- function(x, na.rm = TRUE) {sd(x, na.rm = na.rm)/sqrt(sum(!is.na(x)))}


#' a friendly version of tapply for dataframes. in and out: same as tapply(). the built-in function tapply returns a matrix with unfriendly row name and colname. mf-tapply returns a friendly dataframe
#' @param data dataframe
#' @param select character, column names to calc
#' @param myfactor a colname as factor
#' @param na.rm
#' @return a dataframe
#' @export
#' @examples
#'
mf_tapply <- function(data, select = names(data), myfactor, ..., na.rm = c(TRUE, FALSE, NULL)[1])
{
  if (is.null(na.rm)) {
    y <- data.frame(tapply(data[, select[1]], data[, myfactor],...))
  } else {
    y <- data.frame(tapply(data[, select[1]], data[, myfactor],..., na.rm = na.rm))
  }
  names(y) <- select[1]
  y[, myfactor] <- rownames(y)
  y <- y[, c(2,1)]
  if (length(select) > 1){
    for (i in select[-1]){
      if (is.null(na.rm)) {
        yi <- data.frame(tapply(data[, i], data[, myfactor],...))
      } else {
        yi <- data.frame(tapply(data[, i], data[, myfactor],..., na.rm = na.rm))
      }
      names(yi) <- i
      yi[, myfactor] <- rownames(yi)
      y <- merge(y, yi, by = myfactor)

    }
  }
  return(y)
}


#' a friendly version of tapply
#' @param colname
#' @param x
#' @param factor
#' @return a dataframe
#' @export
#' @examples
#'
mf_vtapply <- function(colname = "tapply", x, factor, ...) # x must be a vector
{
  y <- data.frame(tapply(x, factor, ..., na.rm = TRUE))
  names(y) <- colname
  y$rownames <- rownames(y)
  y <- y[, c(2,1)]
  return(y)
}

#' save csv file with asking if the file already exists.
#' @param data
#' @param writefile destination file
#' @param row.names logical
#' @return write a file
#' @export
#' @examples
#'
mf_write <- function(data, writefile, row.names = FALSE)
  if (file.exists(writefile)){
    warning('File exists! New file is not saved!')
  } else {
    write.csv(data, file = writefile, row.names = row.names)
  }
