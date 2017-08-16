
<div class="figure">
<img src="http://cranlogs.r-pkg.org/badges/grand-total/beginr" />

</div>
<div id="introduction" class="section level1">
<h1>Introduction</h1>
<p>‘beginr’ is a collection of useful functions for R beginners, including hints for the arguments of the ‘plot()’ function, self-defined functions for error bars, user-customized pair plots and hist plots, enhanced linear regression figures, etc.. This package could be helpful to R experts as well.</p>
</div>
<div id="installation" class="section level1">
<h1>Installation</h1>
<pre class="r"><code># stable version on CRAN
install.package(&quot;beginr&quot;)
# or development version on GitHub
devtools::install_github(&quot;pzhaonet/beginr&quot;)</code></pre>
</div>
<div id="quick-start" class="section level1">
<h1>Quick Start</h1>
<div id="functions-as-memos" class="section level2">
<h2>Functions as Memos</h2>
<p>If you often forget the options for <code>pch</code>, <code>lty</code>, <code>type</code>, and <code>col</code> when you <code>plot()</code>, you can run <code>plotpch()</code>, <code>plotlty()</code>, <code>plottype()</code>, <code>plotcolors()</code>, and <code>plotcolorbar()</code>. No need to search the internet any more.</p>
<pre class="r"><code>beginr::plotpch()</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-2-1.png" width="672" /></p>
<pre class="r"><code>beginr::plotlty()</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-2-2.png" width="672" /></p>
<pre class="r"><code>beginr::plottype()</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-2-3.png" width="672" /></p>
<pre class="r"><code>beginr::plotcolors()</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-2-4.png" width="672" /></p>
<pre class="r"><code>beginr::plotcolorbar()</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-2-5.png" width="672" /></p>
</div>
<div id="functions-for-easy-plotting" class="section level2">
<h2>Functions for Easy Plotting</h2>
<p>Linear regression is often used in scientific work, but it is annoying to display the results. In R you have to run <code>lm()</code>, <code>plot()</code>, <code>abline()</code>, <code>text()</code> and so on and so forth. Now with ‘beginr’ you only have to run <code>plotlm()</code>.</p>
<pre class="r"><code>x &lt;- 1:10
y &lt;- 1:10 + rnorm(10)
beginr::plotlm(x, y)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-3-1.png" width="672" /></p>
<pre><code>## [[1]]
##               Estimate Std. Error   t value     Pr(&gt;|t|)
## (Intercept) -0.8826021 0.59514844 -1.482995 1.763634e-01
## x            1.1693736 0.09591686 12.191533 1.899987e-06
## 
## [[2]]
## [1] 0.9489254</code></pre>
<p>If you want to display the distribution of a data-set, use <code>plothist()</code>, which gives you a histogram with markers of the median, mean, quantiles, standard deviation, sample number and the skewness.</p>
<pre class="r"><code>x &lt;- rnorm(10000)
beginr::plothist(x)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-4-1.png" width="672" /></p>
<pre><code>##     para         value
## 1    min -3.9594329262
## 2     1q -0.6743155802
## 3 median  0.0151220580
## 4     3q  0.6806285656
## 5    max  4.1576382225
## 6  lower -2.7065331755
## 7  upper  2.7128461609
## 8   mean -0.0005589431
## 9     sd  1.0064399683</code></pre>
<p>I like <code>pairs()</code>, and ‘beginr’ gives more powerful features to <code>plotpairs()</code> and <code>plotpairs2()</code>.</p>
<pre class="r"><code>df &lt;- data.frame(a = 1:10, b = 1:10 + rnorm(10), c = 1:10 + rnorm(10))
beginr::plotpairs(df)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-5-1.png" width="672" /></p>
<pre class="r"><code>beginr::plotpairs2(df)</code></pre>
<pre><code>## [1] &quot;p&lt;0.01&quot;</code></pre>
<pre><code>## [1] &quot;p&lt;0.01&quot;</code></pre>
<pre><code>## [1] &quot;p&lt;0.01&quot;</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-5-2.png" width="672" /></p>
<p>I often have to plot one independent variable (x) and multiple dependent variables (y<sub>1</sub>, y<sub>2</sub>, …, y<sub>n</sub>) in one 2-D coordinate system, or one dependent variable (y) against multiple independent variables (x<sub>1</sub>, x<sub>2</sub>, …, x<sub>n</sub>) with their error bars. Use <code>dfplot()</code> or <code>dfplot2()</code> in ‘beginr’.</p>
<pre class="r"><code>x &lt;- seq(0, 2 * pi, length.out = 100)
y &lt;- data.frame(sin(x), cos(x))
yerror &lt;- data.frame(abs(rnorm(100, sd = 0.3)), abs(rnorm(100, sd = 0.1)))
beginr::dfplot(x, y, yerror = yerror)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-6-1.png" width="672" /></p>
<pre class="r"><code>beginr::dfplot2(y, x, xerror = yerror)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-6-2.png" width="672" /></p>
<p>If you would like to add errorbars only, then use <code>errorbar()</code>.</p>
<pre class="r"><code>x &lt;- seq(0, 2 * pi, length.out = 100)
y &lt;- sin(x)
plot(x, y, type = &quot;l&quot;)
beginr::errorbar(x, y, yupper = 0.1, ylower = 0.1)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-7-1.png" width="672" /></p>
</div>
<div id="functions-for-data-frames" class="section level2">
<h2>Functions for Data Frames</h2>
<p><code>lmdf()</code> performs the linear regression between every two columns in a data frame.</p>
<pre class="r"><code>df &lt;- data.frame(a = 1:10, b = 1:10 + rnorm(10), c = 1:10 + rnorm(10))
beginr::lmdf(df)</code></pre>
<pre><code>##   x y r.squared adj.r.squared  intercept     slope Std.Error.intercept
## 1 a b 0.9630442     0.9584247 -0.7901086 1.1271749           0.4843895
## 2 a c 0.8568806     0.8389907 -0.3427519 1.0425054           0.9346580
## 3 b a 0.9630442     0.9584247  0.8783157 0.8543876           0.3749251
## 4 b c 0.8157952     0.7927696  0.6004724 0.8856059           0.9426967
## 5 c a 0.8568806     0.8389907  1.0688792 0.8219436           0.7466774
## 6 c b 0.8157952     0.7927696  0.4432909 0.9211717           0.9729768
##   Std.Error.slope t.intercept   t.slope Pr.intercept     Pr.slope
## 1      0.07806644  -1.6311430 14.438661   0.14150586 5.177385e-07
## 2      0.15063377  -0.3667137  6.920795   0.72334232 1.219459e-04
## 3      0.05917360   2.3426432 14.438661   0.04722045 5.177385e-07
## 4      0.14878374   0.6369730  5.952303   0.54193651 3.410966e-04
## 5      0.11876434   1.4315142  6.920795   0.19016727 1.219459e-04
## 6      0.15475888   0.4556027  5.952303   0.66078732 3.410966e-04</code></pre>
<p>There are two functions as improvements of <code>tapply()</code> for factor calculation.</p>
<pre class="r"><code>beginr::tapplydf()
beginr::tapplydfv()</code></pre>
</div>
<div id="functions-for-reading-and-writing-files" class="section level2">
<h2>Functions for Reading and Writing files</h2>
<p><code>readdir()</code> reads multiple files into a list.</p>
<pre class="r"><code>beginr::readdir()</code></pre>
<p><code>writefile()</code> avoids overwriting the files which already exist.</p>
<pre class="r"><code>beginr::writefile()</code></pre>
<p><code>list2ascii()</code> saves a list as a text file.</p>
<pre class="r"><code>alist &lt;- list(a = 1:10, b = letters)
beginr::list2ascii(alist)</code></pre>
</div>
<div id="functions-for-packages" class="section level2">
<h2>Functions for Packages</h2>
<p><code>bib()</code> creates bibliographic entries as texts or a file (‘.bib’).</p>
<pre class="r"><code>beginr::bib(pkg = c(&quot;mindr&quot;, &quot;bookdownplus&quot;, &quot;pinyin&quot;, &quot;beginr&quot;))</code></pre>
<pre><code>## @Manual{R-beginr,
##   title = {beginr: Functions for R Beginners},
##   author = {Peng Zhao},
##   year = {2017},
##   note = {R package version 0.1.0},
##   url = {https://github.com/pzhaonet/beginr},
## }
## @Manual{R-bookdownplus,
##   title = {bookdownplus: Generate Varied Books and Documents with R &#39;bookdown&#39; Package},
##   author = {Peng Zhao},
##   year = {2017},
##   note = {R package version 1.2.2},
##   url = {https://github.com/pzhaonet/bookdownplus},
## }
## @Manual{R-mindr,
##   title = {mindr: Convert Files Between Markdown or Rmarkdown Files and Mindmaps},
##   author = {Peng Zhao},
##   year = {2017},
##   note = {R package version 1.1.0},
##   url = {https://github.com/pzhaonet/mindr},
## }
## @Manual{R-pinyin,
##   title = {pinyin: Convert Chinese Characters into Pinyin},
##   author = {Peng Zhao},
##   year = {2017},
##   note = {R package version 1.1.0},
##   url = {https://github.com/pzhaonet/pinyin},
## }</code></pre>
<p><code>plotpkg()</code> displays a figure showing the downloads of packages.</p>
<pre class="r"><code>beginr::plotpkg(&quot;beginr&quot;, from = &quot;2017-06-23&quot;)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released.en_files/figure-html/unnamed-chunk-14-1.png" width="672" /></p>
<p><code>rpkg()</code> creates a new R package in an easy way.</p>
<pre class="r"><code>beginr::rpkg()</code></pre>
</div>
</div>

## Updates

- 2017-08-16. **v0.1.1**. Bugs fixed. `plothist()` improved.
- 2017-07-19. **v0.1.0**. Updated on CRAN. See the [release note](https://github.com/pzhaonet/beginr/releases/tag/v0.1).
- 2017-07-04. **v0.0.3**. Two more functions: `plotcolorbar()` and `plotpkg()`.
- 2017-06-30. **v0.0.2**. One more function: `rpkg()` to create a skeleton for creating a new R package.
- 2017-06-23. **v0.0.1**. Released on [CRAN](https://cran.r-project.org/web/packages/beginr/).
- 2017-06-22. **v0.0.0**. Preliminary.

<div id="references" class="section level1 unnumbered">
<h1>References</h1>
<div id="refs" class="references">
<div id="ref-R-beginr">
<p>Zhao, Peng. 2017. <em>Beginr: Functions for R Beginners</em>. <a href="https://github.com/pzhaonet/beginr" class="uri">https://github.com/pzhaonet/beginr</a>.</p>
</div>
</div>
</div>
