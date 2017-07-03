# beginr: an R package for beginners

 ![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/beginr)

## Introduction

Useful functions for R beginners, including hints for the arguments of the 'plot()' function, self-defined functions for error bars, user-customized pair plots and hist plots, enhanced linear regression figures, etc.. This package could be helpful to R experts as well.

The instruction is available only in Chinese at the moment. A detailed description in English is coming soon.

## Updates

- 2017-06-30. **v0.0.2**. One more function: `rpkg()` to create a skeleton for creating a new R package.
- 2017-06-23. **v0.0.1**. Released on [CRAN](https://cran.r-project.org/web/packages/beginr/).
- 2017-06-22. **v0.0.0**. Preliminary.

<h1>简介</h1>
<p>又一次高兴地宣布, 我的 R 语言扩展包 ‘beginr’ <span class="citation">(Zhao 2017a)</span> 在 CRAN <a href="https://cran.r-project.org/web/packages/beginr/">正式发布</a>了.</p>
<p><code>beginr</code>是我为 R 语言初学者和自己写的工具包, 可能对中级用户也有帮助, 是我这些年来收集和自己编写的一些简化操作的函数和备忘录. 其实这个包的文档我写得不够详细, 有些函数还没来及整理进去, 之所以匆匆发布, 是想先把 ‘beginr’ 这个名字给占住. 毕竟, 我中意的名字如 ‘learnr’, ’startr’都被别人用过了.</p>
<p>安装方法：</p>
<pre class="r"><code># 稳定版：
install.packages(&#39;beginr&#39;)
# 开发版：
devtools::install_github(&quot;pzhaonet/beginr&quot;)</code></pre>
<p>下面分类介绍一下其中的函数.</p>
</div>
<div class="section level1">
<h1>备忘函数</h1>
<p>我初学 R 的时候用 <code>plot()</code> 作图时, 常常忘记不同形状数据点(<code>pch</code>)对应的编号, 实线虚线(<code>lty</code>)的编号, 散点图类型(<code>type</code>)的代码, 以及最难选择的颜色代码. 每次忘了都要上网搜一下. 现在, beginr 里提供了 <code>plotpch()</code>, <code>plotlty()</code>, <code>plottype()</code>, <code>plotcolors()</code>等函数, 想不起来的时候运行一下就行了.</p>
<pre class="r"><code>beginr::plotpch()</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released_files/figure-html/unnamed-chunk-1-1.png" width="672" /></p>
<pre class="r"><code>beginr::plotlty()</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released_files/figure-html/unnamed-chunk-1-2.png" width="672" /></p>
<pre class="r"><code>beginr::plottype()</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released_files/figure-html/unnamed-chunk-1-3.png" width="672" /></p>
<pre class="r"><code>beginr::plotcolors()</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released_files/figure-html/unnamed-chunk-1-4.png" width="672" /></p>
</div>
<div class="section level1">
<h1>快速作图函数</h1>
<p>我常常需要做线性拟合, 每次又是作图又是添加拟合方程, 繁琐. 现在, beginr 里的一个 <code>plotlm()</code>函数就搞定.</p>
<pre class="r"><code>x &lt;- 1:10
y &lt;- 1:10 + rnorm(10)
beginr::plotlm(x, y)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released_files/figure-html/unnamed-chunk-2-1.png" width="672" /></p>
<pre><code>## [[1]]
##              Estimate Std. Error   t value     Pr(&gt;|t|)
## (Intercept) 0.4159901  0.6289426 0.6614119 0.5269346876
## x           0.9033416  0.1013633 8.9119225 0.0000199195
## 
## [[2]]
## [1] 0.9084902</code></pre>
<p>经常需要了解一组数据的分布, 看看是不是正态分布. beginr 里的 <code>plothist()</code> 函数不仅做出直方图和平滑曲线, 并且标出中值, 中位数, 四分位数, 标准偏差, 样本数, 以及用来判断是否正态分布的 skewness 值.</p>
<pre class="r"><code>x &lt;- rnorm(10000)
beginr::plothist(x)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released_files/figure-html/unnamed-chunk-3-1.png" width="672" /></p>
<pre><code>##     para       value
## 1    min -3.76996026
## 2     1q -0.69121344
## 3 median -0.01363909
## 4     3q  0.65530341
## 5    max  3.77244746
## 6  lower -2.71056150
## 7  upper  2.67465147
## 8   mean -0.01694626
## 9     sd  1.00395114</code></pre>
<p>我很喜欢系统自带的成对儿散点图函数<code>pairs()</code>, 只是功能再强大一点就好了. beginr 里的 <code>plotpairs()</code> 和 <code>plotpairs2()</code> 可以实现.</p>
<pre class="r"><code>df &lt;- data.frame(a = 1:10, b = 1:10 + rnorm(10), c = 1:10 + rnorm(10))
beginr::plotpairs(df)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released_files/figure-html/unnamed-chunk-4-1.png" width="672" /></p>
<pre class="r"><code>beginr::plotpairs2(df)</code></pre>
<pre><code>## [1] &quot;p&lt;0.01&quot;</code></pre>
<pre><code>## [1] &quot;p&lt;0.01&quot;</code></pre>
<pre><code>## [1] &quot;p&lt;0.01&quot;</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released_files/figure-html/unnamed-chunk-4-2.png" width="672" /></p>
<p>我经常需要将一组自变量 (x) 和多组因变量 (y<sub>1</sub>, y<sub>2</sub>, …, y<sub>n</sub>) 作在同一个坐标系, 或者一组因变量 (y) 对多组自变量 ((x<sub>1</sub>, x<sub>2</sub>, …, x<sub>n</sub>)), 并且画上各自的误差线. beginr 里一条 <code>dfplot()</code> 或 <code>dfplot2()</code> 函数就能完成.</p>
<pre class="r"><code>x &lt;- seq(0, 2 * pi, length.out = 100)
y &lt;- data.frame(sin(x), cos(x))
yerror &lt;- data.frame(abs(rnorm(100, sd = 0.3)), abs(rnorm(100, sd = 0.1)))
beginr::dfplot(x, y, yerror = yerror)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released_files/figure-html/unnamed-chunk-5-1.png" width="672" /></p>
<pre class="r"><code>beginr::dfplot2(y, x, xerror = yerror)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released_files/figure-html/unnamed-chunk-5-2.png" width="672" /></p>
<p>如果只是想简单添加误差线, 那就用 <code>errorbar()</code> 函数.</p>
<pre class="r"><code>x &lt;- seq(0, 2 * pi, length.out = 100)
y &lt;- sin(x)
plot(x, y, type = &quot;l&quot;)
beginr::errorbar(x, y, yupper = 0.1, ylower = 0.1)</code></pre>
<p><img src="http://www.pzhao.org/post/2017-06-23-beginr_released_files/figure-html/unnamed-chunk-6-1.png" width="672" /></p>
</div>
<div class="section level1">
<h1>数据框操作</h1>
<p>beginr 提供了一些对数据框计算的函数, 例如计算多列数据两两的相关性和线性拟合结果, 可以用 <code>lmdf()</code> 函数.</p>
<pre class="r"><code>df &lt;- data.frame(a = 1:10, b = 1:10 + rnorm(10), c = 1:10 + rnorm(10))
beginr::lmdf(df)</code></pre>
<pre><code>##   x y r.squared adj.r.squared  intercept     slope Std.Error.intercept
## 1 a b 0.9133257     0.9024914 -0.8552049 1.1068170           0.7479861
## 2 a c 0.9564644     0.9510225  0.4109244 0.9939429           0.4651945
## 3 b a 0.9133257     0.9024914  1.1824085 0.8251822           0.5572414
## 4 b c 0.8405828     0.8206557  1.6679602 0.8045523           0.7680559
## 5 c a 0.9564644     0.9510225 -0.1559840 0.9622931           0.4763496
## 6 c b 0.8405828     0.8206557 -0.9085403 1.0447833           1.0556844
##   Std.Error.slope t.intercept   t.slope Pr.intercept     Pr.slope
## 1      0.12054888  -1.1433432  9.181479   0.28595427 1.599783e-05
## 2      0.07497289   0.8833389 13.257364   0.40281690 9.998673e-07
## 3      0.08987464   2.1218961  9.181479   0.06662826 1.599783e-05
## 4      0.12387583   2.1716653  6.494829   0.06165829 1.891323e-04
## 5      0.07258556  -0.3274570 13.257364   0.75172684 9.998673e-07
## 6      0.16086388  -0.8606174  6.494829   0.41449570 1.891323e-04</code></pre>
<p>还有两个函数对<code>tapply()</code>进行了改善, 用来进行因子计算.</p>
<pre class="r"><code>beginr::tapplydf()
beginr::tapplydfv()</code></pre>
</div>
<div class="section level1">
<h1>文件读写函数</h1>
<p>经常需要同时对多个数据文件进行处理, 逐个读入 R 里太麻烦. beginr 提供了个 <code>readdir()</code> 函数, 可以一次性把指定文件夹里所有数据读入, 保存在一个列表 (list) 里.</p>
<pre class="r"><code>beginr::readdir()</code></pre>
<p>R 自带的 write 系列文件保存函数, 一不小心就把原有同名文件给覆盖了. beginr 提供了个安全的函数 <code>writefile()</code>, 避免一失足成千古恨.</p>
<pre class="r"><code>beginr::writefile()</code></pre>
<p><code>list2ascii()</code> 函数可以把一个列表原样保存成文本文件.</p>
<pre class="r"><code>alist &lt;- list(a = 1:10, b = letters)
beginr::list2ascii(alist)</code></pre>
<p>很多人引用了别人的工作却往往不列出参考文献. 不仅初学者常常如此, 很多学术论文里有些插图一眼就能看出是 R 的某个扩展包作出来的, 但参考文献里却没有列出来, 这往小了说是不尊重别人的工作,往大了说是缺乏学术道德. 对开源社区的生态圈是不利的. beginr 为初学者提供了 <code>bib()</code> 函数, 可以为指定的 R 扩展包生成文献引用信息, 既可以打印出结果, 也可以直接保存为 ‘.bib’文件方便为’<a href="http://www.pzhao.org/zhhttp://www.pzhao.org/post/bookdownplus-released/">bookdownplus</a>‘<span class="citation">(Zhao 2017b)</span> 或’<a href="http://www.pzhao.org/zhhttp://www.pzhao.org/post/r-blogdown/">blogdown</a>’ <span class="citation">(Xie 2017)</span> 调用.</p>
<pre class="r"><code>beginr::bib(pkg = c(&quot;mindr&quot;, &quot;bookdownplus&quot;, &quot;pinyin&quot;))</code></pre>
<pre><code>## @Manual{R-bookdownplus,
##   title = {bookdownplus: Generate Varied Books and Documents with R &#39;bookdown&#39; Package},
##   author = {Peng Zhao},
##   year = {2017},
##   note = {R package version 1.0.3},
##   url = {https://github.com/pzhaonet/bookdownplus},
## }
## @Manual{R-mindr,
##   title = {mindr: Convert Files Between Markdown or Rmarkdown Files and Mindmaps},
##   author = {Peng Zhao},
##   year = {2017},
##   note = {R package version 1.0.4},
##   url = {https://CRAN.R-project.org/package=mindr},
## }
## @Manual{R-pinyin,
##   title = {pinyin: Convert Chinese Characters into Pinyin},
##   author = {Peng Zhao},
##   year = {2017},
##   note = {R package version 1.0.3},
##   url = {https://github.com/pzhaonet/pinyin},
## }</code></pre>
<p>好啦, 以后我还会把更多有用的工具添加到 beginr 包里. 欢迎关注 beginr的<a href="https://github.com/pzhaonet/beginr">项目主页</a>. 另外, 使用beginr的地方请记得列出参考文献哦.</p>
<pre class="r"><code>beginr::bib(pkg = c(&quot;beginr&quot;))</code></pre>
<pre><code>## @Manual{R-beginr,
##   title = {beginr: Functions for R Beginners},
##   author = {Peng Zhao},
##   year = {2017},
##   note = {R package version 0.0.1},
##   url = {https://github.com/pzhaonet/beginr},
## }</code></pre>
<div class="section level2 unnumbered">
<h2>参考文献</h2>
<div id="refs" class="references">
<div id="ref-R-blogdown">
<p>Xie, Yihui. 2017. <em>Blogdown: Create Blogs and Websites with R Markdown</em>. <a href="https://github.com/rstudio/blogdown" class="uri">https://github.com/rstudio/blogdown</a>.</p>
</div>
<div id="ref-R-beginr">
<p>Zhao, Peng. 2017a. <em>Beginr: Functions for R Beginners</em>. <a href="https://github.com/pzhaonet/beginr" class="uri">https://github.com/pzhaonet/beginr</a>.</p>
</div>
<div id="ref-R-bookdownplus">
<p>———. 2017b. <em>Bookdownplus: Generate Varied Books and Documents with R ’Bookdown’ Package</em>. <a href="https://CRAN.R-project.org/package=bookdownplus" class="uri">https://CRAN.R-project.org/package=bookdownplus</a>.</p>
</div>
</div>
</div>
</div>

    </div>
  </div>

</article>