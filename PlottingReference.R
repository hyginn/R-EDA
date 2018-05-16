# ================================================== #
# Boris Steipe <boris.steipe@utoronto.ca>            #
#                                                    #
#                                                    #
# Plotting reference                                 #
#                                                    #
# ================================================== #

# ==================================================
# Contents
# ==================================================

# 1 - Types of plots
# 2 - Colors
# 3 - Lines
# 4 - Coordinates
# 5 - Title, legends etc.
# 6 - Plot symbols and text
# 7 - Drawing on plots
# 8 - Plots of X-Y-Z coordinates
# 9 - The plot window
# 10 - Special packages


# ==================================================
# The Normal distribution
# ==================================================

# We often use the Normal distribution to generate sample data, or controls
# for statistical experiments.

?dnorm

x <- seq(-4, 4, 0.1)
f <- dnorm(x, mean=0, sd=1)
plot(x, f, xlab="x", ylab="density", lwd=5, type="l")



# ==================================================
# 1 - Types of plots
# ==================================================

# This lists the generic plots only. Many more
# specialized plot-types are available.

# plot()
# pie()
# hist()
# stripchart()
# stem()
# barplot()
# boxplot()

# Examples:
# generate some data to plot
x <- rnorm(200)
y <- x^3 * 0.25 + rnorm(200, 0, 0.75)

x <- runif(200)
y <- x^3 * 0.25 + runif(200)


# plot() ==========================================
?plot
# generate some data to plot
x <- rnorm(200)
y <- x^3 * 0.25 + rnorm(200, 0, 0.75)

?plot
# standard scatterplot
plot(x,y)

# Add a rug representation:
rug(x)
rug(y, side=2, col="red")



# barplot() =======================================
?barplot
barplot(table(round(y)))

# hist() ==========================================
?hist
set.seed(12357)
x <- rnorm(50)
hist(x, breaks=5)
# add a stripchart() of the actual values
stripchart(x, pch="|", add=TRUE, col="red3", xlim=c(-3, 3), at=-0.5)

# Note: a similar plot for bivarite data is achieved
# with the rug() function.

# assigning the output of hist() makes the values
# used in constructing the histogram accessible:

info <- hist(x, breaks=5)
info

# we can explicitly set breakpoints in a vector:
# here we set them at 0.5 sigma intervals from
# -3 to 3

s <- 1.0
hist(x, breaks=seq(-3*s, 3*s, by=0.5*s))

# we can color the bars individually...
hcol <- c("#4F47FD", "#6982FC", "#8AA6EF", "#AFBBDB", "#BEBEBE", "#A9A9A9",
          "#A9A9A9", "#BEBEBE", "#DBBBAF", "#EFA68A", "#FC8269", "#FD474F")

# Most parameters of a generic plot apply.
h <- hist(x, breaks=seq(-3*s, 3*s, by=0.5*s),
          col=hcol,
          main="",
          xlab=expression(sigma),
          ylab="Counts")

# ... and we can add the individual counts to the plot.
text(h$mids, h$counts, h$counts, adj = c(0.5, -0.5), col = hcol)

# stem() ==========================================
# A stem-and-leaf plot is like a histogram, but output on the console.
# Useful e.g. when you need to send a distribution in the text
# of an e-mail
?stem
stem(x)

# pie() ===========================================
?pie
x <- rnorm(200)
pie(table(abs(round(x*2))))

# boxplot() =======================================
?boxplot
x <- rnorm(200)
boxplot(x)

m <- x
m <- cbind(m, x^2)
m <- cbind(m, x^3)
m <- cbind(m, x^4)
m <- cbind(m, x^5)
boxplot(m)
boxplot(log(abs(m)))



# ==================================================
# 2 - Colors
# ==================================================

# Colors can be specified by number, by name, as hex-triplets
# as rgb or hsv values, and through color palettes.

# Colors by number =================================
# The col=... parameter for plots is 1 by default and you can
# set it to the range 0:8.
# 0: white
# 1: black (the default)
# 2: red
# 3: green
# 4: blue
# 5: cyan
# 6: magenta
# 7: yellow
# 8: grey
barplot(rep(1,9), col=0:8, axes=FALSE, names.arg=c(0:8))

# As you can see, these primary colors are decidedly garish
# and offend even the most rudimentary sense of aesthetics. Fortunately
# there are much more sophisticated ways to define colors in R.

# Colors by name ===================================
# You may have noticed that "red", "green", and "blue"
# work for the col=... parameter, but you probably would
# not have imagined that "peachpuff", "firebrick" and
# "goldenrod" are valid as well. In fact, there are
# 657 named colors in R. Access them all by typing:
colors()

pie(c(1, 1, 2, 3, 5, 8, 13),
    col=c(
    "firebrick2",
    "tomato",
    "goldenrod1",
    "peachpuff",
    "papayawhip",
    "seashell",
    "whitesmoke"
    )
    )

# Show all colors in stacks in plot area,
# with labels to identify them

range <- 51                       # number of elements in stack
nColours <- length(colors())
nCol <- ceiling(nColours / range) # number of stacks

# empty frame to acommodate all stacks
plot(1, xlim=c(0, nCol), ylim=c(0, range), axes=FALSE, type="n")

for (i in 1:nColours) {              # for all elements in color vector
    xL <- floor((i-1) / range)       # calculate corner coordinates for
    xR <- xL + 1                     # rectangles
    yB <- range - ((i-1) %% range)
    yT <- yB + 1
    thisCol <- colors()[i]           # get colour name
    rect(xL, yB, xR, yT, col=thisCol)
	if (sum(col2rgb(thisCol)) < (255 * 3 * 0.5)) {  # too dark?
		tCol = "#FFFFFF"                            # white text
	} else {
		tCol = "#000000"                            # else black text
	}
#   text(xL+0.5, yB+0.5, label=i,       col=tCol, cex=0.7)   # index
    text(xL+0.5, yB+0.5, label=thisCol, col=tCol, cex=0.7)   # name
}


# Read more about named colors (and related topics) at
# http://research.stowers-institute.org/efg/R/Color/Chart/

# Colors as hex-triplets ===========================
# Hex triplets in R work exactly as in HTML: a triplet of
# RGB values in two-digit hexadecimal representation. The
# first two digits specify the red value, the second two
# are for green, then blue. R accepts a fourth pair of
# digits to optionally specify the transparency, the
# semantics of the code is thus "#RRGGBB" or "#RRGGBBAA".
# Read more e.g. at http://en.wikipedia.org/wiki/Web_colors

# The function col2rgb() converts color names to rgb values ...
col2rgb("violetred")

# ... and rgb() converts rgb values to hex-code:
rgb(1, 0.5, 0.23)

# Unfortunately the output of col2rgb does not quite match
# rgb(). col2rgb creates rows with values between 0 and 255,
# and rgb by default expects columns with intensities from
# 0 to 1, you have to transpose and divide.
rgb(t(col2rgb("red"))/255)        # "#FF0000"
rgb(t(col2rgb("peachpuff"))/255)  # "#FFDAB9"

# There are many tools on the Web that help to generate
# pleasing palettes.

# Here is an example -"Creative Cloud"- taken from
#    https://kuler.adobe.com/

CC <- c("#011640", "#024059", "#F2F0D0", "#BE6C5C", "#8C3037" )
hist(rnorm(1000), breaks=20 , col=CC)

# R colors are actually specified as quartets: the fourth value
# the "Alpha channel" defines the transparency. Setting this to
# values other than "FF" (the default) can be useful for very
# crowded plots, or for creating overlays.

x <- rnorm(2000)
y <- x^3 * 0.25 + rnorm(2000, 0, 0.75)
# compare:
plot(x,y, pch=19, col="#EE3A8C")
plot(x,y, pch=19, col="#EE3A8C12") # Alpha at ~ 10%

# or with multiple overlays of varying size ...
plot(x,y, pch=16, col="#AA330009")
points(x,y, pch=19, cex=2, col="#44558803")
points(x,y, pch=20, cex=0.5, col="#EE3A8C08")

# Color palettes ===================================
# R has several inbuilt color palettes, or you can build your own.

# Inbuilt palettes =================================
?rainbow
# view the palettes
opar <- par(mfrow=c(3,2))
n <- 20
sq <- rep(1, n)
barplot(sq, col=rainbow(n),        axes=F, main="rainbow(n)")
barplot(sq, col=cm.colors(n),      axes=F, main="cm.colors(n)")
barplot(sq, col=topo.colors(n),    axes=F, main="topo.colors(n)")
barplot(sq, col=terrain.colors(n), axes=F, main="terrain.colors(n)")
barplot(sq, col=heat.colors(n),    axes=F, main="heat.colors(n)")
par <- opar

# Example: We generate random points and calculate a "density"
# at each point. The we plot each point and color it according
# to its density. See the section on X-Y-Z plotting re. the
# inbuilt functions supplied by R for this purpose.

n <- 1000
x <- rnorm(n)
y <- x^3 * 0.25 + rnorm(n, sd=0.75)
z <- rep(0, n) # initialize z
for (i in 1:n) { # calculate a density from the proximity of other points
	dx <- x-x[i]; dx <- dx * dx # square of the distance in x
	dy <- y-y[i]; dy <- dy * dy # square of the distance in y
	d <- dx + dy                # square of the sum
	d <- d[-i]                  # remove the self-element
	z[i] <- 1/sum(d)            # let density decay with 1/r^2
}
z <- z/max(z) - 0.00001 # normalize, but keep the max below 1.0
# now map each of the values of z into an interval of the palette
n <- 20                   # number of intervals
z <- floor(z * n) + 1     # convert to integers
pal <- rainbow(n)         # get a vector of colors from the palette
cz <- pal[z]              # apply the color for each density value
plot(x,y, col=cz, pch=16) # plot

# use a different palette
pal <- heat.colors(n); cz <- pal[z]
plot(x,y, col=cz, pch=16)

# Custom palettes =================================
# "Cold" values should really be black, not red. Lets define a
# custom palette: colorRampPalette() is a function that returns
# a function. The returned function can be used to calculate
# a palette, a trajectory along a number of waypoints in
# colorspace.

# And let's use the actual values of glowing steel - as taken
# from here for example:
#     http://i.imgur.com/qEk0QJf.jpg
# and pick colors with this tool:
#     http://imagecolorpicker.com/en

fc <- colorRampPalette(c("#301C25",
                         "#F53A43",
                         "#FFC754",
                         "#FEFDDE"),
                         bias=0.5) # assigns a function to fc
fc # look at the function
fc(n) # use the function to get n values
pal <- fc(n); cz <- pal[z] # assign the colors according to z
plot(x,y, col=cz, pch=16)

# Useful palettes have also been described specifically
# for cartography. http://colorbrewer2.org/ has palettes for seqential
# and qualitative diferences, and options for colorblind-safe and
# photocopy friendly palettes. You can use them via an R package:

if (!require(RColorBrewer, quietly=TRUE)) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
display.brewer.all()

# Here, we apply a Brewer palette to a Voronoi tesselation of a
# point set.

if (!require(deldir, quietly=TRUE)) {
  install.packages("deldir")
  library(deldir)
}

# Create a point set along a logarithmic spiral, with a bit
# of added noise.
li <- 0.1
n <- 45
dl <- 1.06
ncirc <- 13
da <- (2*pi)/ncirc
fnoise <-0.13

# create a matrix of points
x <- numeric(n)
x <- cbind(x, numeric(n))

set.seed(16180)
for (i in 1:n) {
	l <- li * (dl^(i-1))
	x[i,1] <- (l+(rnorm(1)*fnoise*l)) * cos((i-1)*da)
	x[i,2] <- (l+(rnorm(1)*fnoise*l)) * sin((i-1)*da)
}
plot(x[,1], x[,2])
ts <- deldir(x[,1], x[,2])       # calculate the tesselation
tl <- tile.list(ts)      # calculate the list of tiles
plot.tile.list(tl)       # plot it

# Let's color the cells by distance from a defined point
# using a Brewer palette
points(x[25,1], x[25,2], pch=20, col="red")   # pick a point

vec <- c(x[25,1], x[25,2]) # define a point

# define a function for Euclidian distance
vDist <- function(x,v) { sqrt(sum((x-v)^2)) }  # calculates Euclidian distance
d <- apply(x,1,vDist, v=vec)                   # apply this to the point set

dCol <- floor(((d-min(d))/(max(d)-min(d)) * 10)) + 1 # map d into 10 intervals
dCol[which(dCol>10)] <- 10                           # demote the largest one

pal <- brewer.pal(10, "RdGy")   # create the palette

# plot the tesselation, color by palette
plot.tile.list(tl, fillcol = pal[dCol], cex=0.8, pch=20, col.pts="slategrey")

# =================================================
# Finally, you can extract color palettes from images like here:
#    http://www.pictaculous.com/

# Or you can use the "eye-dropper" tool of standard image editing
# software to access to color value of a particular pixel, or online here ...
#    http://html-color-codes.info/colors-from-image/
#    http://imagecolorpicker.com/



# ==================================================
# 3 - Lines
# ==================================================

plot(c(0,10), c(0,10), type = "n", axes = FALSE, xlab = "", ylab = "")

# Line type
for (i in 1:8) {
	y <- 10.5-(i/2)
	segments(1,y,5,y, lty=i)
	text(6, y, paste("lty = ", i), col="grey60", adj=0, cex=0.75)
}

# Line width
for (i in 1:10) {
	y <- 5.5-(i/2)
	segments(1,y,5,y, lwd=(0.3*i)^2)
	text(6, y, paste("lwd = ", (0.3*i)^2), col="grey60", adj=0, cex=0.75)
}



# ==================================================
# 4 - Coordinates
# ==================================================
# For Details, see:
?plot.default

n <- 1000
x <- rnorm(n)
y <- x^3 * 0.25 + rnorm(n, sd=0.75)

plot(x,y)  # Default

# Axes
plot(x,y, xlim=c(-4, 4)) # fixed limits
plot(x,y, xlim=c(-4, 4), ylim=c(10, -10)) # reverse is possible
plot(x,y, log="xy")

# The axis parameters in the default plot are limited.
# If you want more control, suppress the printing of an axis
# in the plot and use the axis() function instead.
?axis

plot(x,y, axes=FALSE)
axis(1)
axis(2)

# Specifying
# - the length of the tickmarks (tcl) as a fraction
#   of the height of a line of text: positive values:
#   inside, negative values: outside the plot area;
# - the positions of the tickmarks (at). The axis is
# only drawn to the end of the plot area, even if a
# tickmark position is defined beyond it. With this,
# you can make x and y axes intersect.
plot(x,y, axes=FALSE)
axis(1, at=-3:3, tcl=0.3)
axis(2, at=seq(-9, 6, 3), tcl=-0.3)

# Specifying
# - labels: the "labels" vector and the "at" vector must have
#   the same length, even if some tickmarks do not fall
#   inside the plot area and are not shown.
# - gridlines: use tck instead of tcl: (ticks as fraction of plot area)
plot(x,y, axes=FALSE, xlab="", ylab="")
axis(1, at=c(-10, -2, 0, 2, 10),
        labels=c("", "minus two", "zero", "two", ""),
        tcl=0.3,
        cex.axis=0.75,
        col.axis="steelblue")
axis(2, at=seq(-6, 6, 3),
        labels=rep("", 5), # supress labels with empty string
        lwd=0,             # supress axis line
        tck=1.0,           # draw tickmarks full length of plot area
        lwd.ticks=0.5,     # line width for ticks
        lty=2,             # line type for axis and ticks
        col.ticks="red4"   # color for tickmarks
        )


# Axis-labels and title are straightforward parameters of plot
plot(x,y, xlab="rnorm(n)",
          ylab="x^3 * 0.25 + rnorm(n, sd=0.75)",
          cex.main=1.3,
          main="Sample\nPlot",
          cex.sub=0.75,
          col.sub="grey",
          sub="Scatterplot of noisy 3d-degree polynomial"
          )

# Add gridlines
?grid
grid()



# ==================================================
# 5 - Title, legends etc.
# ==================================================



# ==================================================
# Titles and margin text
# ==================================================

# mtext()
# title()


# ==================================================
# Legends
# ==================================================

# legend()
# See also: box()

# ==================================================
# Layout
# ==================================================

# par, lattice, constant aspect ratio


# Most parameters of the plot window can be set via
# the functions plot(), hist() etc., but some need to
# be set via the par() function. Calling par() without
# arguments lists the current state of the plotting
# parameters. Calling it with arguments, returns the
# old parameters and sets new parameters. Thus setting
# new parameters and saving the old ones can be done
# in one step. The parameters that have to be set via
# par include:

# -  multiple plots in one window (mfrow, mfcol, mfg)
# - margin layout (mai, mar mex, oma, omd, omi)
# - controlling position and size of a plot in the figure (fig, plt, ps, pty)
# - see ?par for details.

n <- 1000
x <- rnorm(n)
y <- x^3 * 0.25 + rnorm(n, sd=0.75)

# set window background and plotting axes via par
opar <- par(bg="steelblue", fg="lightyellow")
# set axis lables and titles via plot parameters
plot(x,y, col.axis="lightyellow", col.lab="lightyellow")
par(opar)

plot(x,y) # confirm reset

# See the reference for hypervariate X-Y-Z plots for
# an example of multiple plots in one window.

# See the reference for interactive plots for an example
# of opening and plotting into a second window.


# Trellis plots (automatic from scatterplot) ... lattice package



# ==================================================
# 6 - Plot symbols and text
# ==================================================

# pch
# id 1:20 are regular symbols
# create coordinates
plot(c(0,10), c(0,10), type = "n", axes = FALSE, xlab = "", ylab = "")

# coordinates for first 25 symbols
x1 <- rep(0.5:9.5, 2)[1:20]
y1 <- sort(rep(9.5:8.5, 10), dec=TRUE)[1:20]
points(x1, y1, pch=1:20)

# id 21:25 can have different border and fill colors
x2 <- 0.5:4.5
y2 <- rep(7.5,5)
 points(x2, y2, pch=21:25, col="slategrey", bg=rainbow(5))
#points(x2, y2, pch=21:25, col="slategrey", bg=c("red", "green", "blue"))

# ten extra symbols are defined as characters
x3 <- 0.5:9.5
y3 <- rep(6.5,10)
extra = c(".", "o", "O", "0","a","A", "*", "+","-","|")
points(x3, y3, pch=extra) # note: ext is a character vector

# The ASCII codes for characters 32 to 126 can also be used as plotting characters
x4 <- rep(seq(0.5,9.5,0.5), 5)[1:96]
y4 <- sort(rep(5.5:0.5, 19), dec=TRUE)[1:96]
points(x4, y4, pch=32:126, col="navyblue")


# Plotting arbitrary text
# use the text() function to plot characters and strings to coordinates
?text

# Example: add labels to the symbols
# first set: plain symbols (1 to 20)
text(x1-0.4, y1, paste(1:20), cex=0.75)
# symbols with separate background (21 to 25)
text(x2-0.4, y2, paste(21:25), cex=0.75)
# third set: special characters
text(x3-0.4, y3, extra, col="slateblue", cex=0.75, vfont=c("serif", "plain")) # change font for clarity

# a large set of Hershey vector fonts is available which gives access to many
# more plotting and labeling options via text()
demo(Hershey)

# Plotting other symbols:
# In the most general way, Unicode characters can be plotted as text.
# The code is passed in hexadecimal, long integer, with a negative sign.
# Here is a quarter note (Unicode: 266a) using plot()
plot(0.5,0.5, pch=-0x266aL, cex=5, xlab="", ylab="")
# However, rendering varies across platforms since it depends on
# unicode support. It is safer to use the inbuilt Hershey vector fonts.



# ==================================================
# 7 - Drawing on plots
# ==================================================

# abline()
# segments()
# lines()
# arrows() ... but to get a filled arrow use polygon()


# Example: dividing a plot into 60° regions, centred on a point.
# A general approach to "lines" on a plot is provided by segments().
# However in this special case one can use abline().
# We have to take care though that the aspect ratio for the
# plot is exactly 1 - otherwise our angles are not right.
# Therefore we need to set the asp parameter for plots.

# For a general sketch
#  - we plot the frame a bit larger, don't draw axes
#  - draw the ablines
#  - draw two arrows to symbolize the coordinate axes

p <- c(4, 2)
plot(p[1], p[2],
     xlim=c(-0.5,10.5),
     ylim=c(-0.5,10.5),
     xlab="", ylab="",
     axes=FALSE,
     asp=1.0)
abline(h=p[2], lty=2)  # horizontal
abline(p[2] - (p[1]*tan(pi/3)),  tan(pi/3), lty=2)  # intercept, slope
abline(p[2] + (p[1]*tan(pi/3)), -tan(pi/3), lty=2)  # intercept, slope
arrows(0, 0, 10, 0, length=0.1)   # length of arrow
arrows(0, 0, 0, 10, length=0.1)


# curves()
# rect()
# polygon()
# More: see the Index of functions for the graphics package


# ==================================================
# 8 - Plots of X-Y-Z coordinates
# ==================================================

?contour
# Example (mostly taken from the help page)
x <- -6:16
op <- par(mfrow = c(2, 2))
contour(outer(x, x), method = "edge", vfont = c("sans serif", "plain"))
z <- outer(x, sqrt(abs(x)), FUN = "/")
image(x, x, z, col=colorRampPalette(c("#301C25", "#F53A43", "#FFC754", "#FEFDDE"))(60))
contour(x, x, z, col = "pink", add = TRUE, method = "edge",
        vfont = c("sans serif", "plain"))
contour(x, x, z, ylim = c(1, 6), method = "simple", labcex = 1)
contour(x, x, z, ylim = c(-6, 6), nlev = 20, lty = 2, method = "simple")
par(op)

#see also persp()
?persp
demo(persp)



# ==================================================
# 9 - The plot window
# ==================================================
# Most parameters of the plot window can be set via
# the functions plot(), hist() etc., but some need to
# be set via the par() function. Calling par() without
# arguments lists the current sate of the plotting
# parameters. Calling it with arguments, returns the
# old parameters and sets new parameters. Thus setting
# new parameters and saving the old ones can be done
# in one step. The parameters that have to be set via
# par include:

# - multiple plots in one window (mfrow, mfcol, mfg)
# - margin layout (mai, mar mex, oma, omd, omi)
# - controlling position and size of a plot in the
#   figure (fig, plt, ps, pty)
# - see ?par for details.
# See the section on X-Y-Z plots for an example of
# multiple plots in one window.

n <- 1000
x <- rnorm(n)
y <- x^3 * 0.25 + rnorm(n, sd=0.75)

# set window background and plotting axes via par
opar <- par(bg="steelblue", fg="lightyellow")
# set axis lables and titles via plot parameters
plot(x,y, col.axis="lightyellow", col.lab="lightyellow")
par(opar)

plot(x,y) # confirm reset



# ==================================================
# 10 - Special packages
# ==================================================

# Packages in the standard distribution ...
# ... use with library("package")
#   graphics

#   grid

#   lattice

# Packages that can be downloaded from CRAN
# ... use with install.packages("package"), then
#              library("package")

#   hexbin
#   ggplot2

# Packages that can be downloaded  from BioConductor
#   prada:
if (!require(prada, quietly=TRUE)) {
    source("http://www.bioconductor.org/biocLite.R")
    biocLite("prada")
}

#      Try:
n <- 1000
x <- rnorm(n)
y <- x^3 * 0.25 + rnorm(n, sd=0.75)
# smoothed scatterplot with outliers
smoothScatter(x,y, nrpoints=200, pch=20, cex=0.5, col="#6633BB55")

# density adapted colors
plot (x, y, col=densCols(x,y), pch=19, cex=1.5)

# [End]

