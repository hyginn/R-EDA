# R-EDA-HypothesisTesting.R
#
# Purpose:  Explorations of Significance, and Hypothesis Testing
#
# Version: 2
#
# Date:    2018  05
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#
# V 2      2018 restructuring; merge "significnce" and "hypothesis testing"
# V 1.1    2017 updates
# V 1.0    First code 2016
#
# TODO:
#
#
# == HOW TO WORK WITH THIS FILE ================================================
#
#  Go through this script line by line to read and understand the
#  code. Execute code by typing <cmd><enter>. When nothing is
#  selected, that will execute the current line and move the cursor to
#  the next line. You can also select more than one line, e.g. to
#  execute a block of code, or less than one line, e.g. to execute
#  only the core of a nested expression.
#
#  DO NOT save edits to this file! Otherwise this will create a conflict
#  when we update it. If you do edit it, save it under a new file name like
#  myR-EDA-Introduction.R  Your "local" files will not be overwritten by
#  updates.
#
#  If there are portions you don't understand, use R's help system,
#  Google for an answer, or ask me. Don't continue if you don't
#  understand what's going on. That's not how it works ...
#
#  Once you have typed and executed the function init(), you will find a file
#  called myEDANotes.R in the project directory.
#
#  Open it, you can place all of your code-experiments and notes into that
#  file. This will be your "Lab Journal" for this session. Copy code to
#  myEDANotes.R to experiment with options, or just play.
#  Especially play.
#
# ==============================================================================
#
# Module 5: Significance, and Hypothesis Testing
#
# ==============================================================================


#TOC> ==========================================================================
#TOC> 
#TOC>   Section  Title                                              Line
#TOC> ------------------------------------------------------------------
#TOC>   1        PART ONE: SIGNIFICANCE                               73
#TOC>   2        Significance and p-value                             76
#TOC>   2.1        Significance levels                                87
#TOC>   2.2        probability and p-value                           104
#TOC>   2.2.1          p-value illustrated                           134
#TOC>   2.3        One- or two-sided                                 187
#TOC>   2.4        Significance by integration                       227
#TOC>   2.5        Significance by simulation or permutation         233
#TOC>   2.6        Final tasks                                       336
#TOC>   3        PART TWO: HYPOTHESIS TESTING                        346
#TOC>   3.1        Basic statistical tests                           362
#TOC>   3.2        Wilcoxon test                                     461
#TOC>   3.3        The multiple testing problem                      511
#TOC>   3.3.1          Bonferroni correction                         560
#TOC>   3.3.2          FDR                                           585
#TOC> 
#TOC> ==========================================================================


# =    1  PART ONE: SIGNIFICANCE  ==============================================


# =    2  Significance and p-value  ============================================

# The idea of the probability of an event has a precise mathematical
# interpretation, but how is it useful to know the probability? Usually we are
# interested in whether we should accept or reject a hypothesis based on the
# observations we have. A rational way to do this is to say: if the probability
# of observing the data is very small under the null-hypothesis, then we will
# assume the observation is due to something other than the null-hypothesis. But
# what do we mean by the "probability of our observation"? And what is "very
# small"?

# ==   2.1  Significance levels  ===============================================

# A "very small" probability is purely a matter of convention - a cultural
# convention. In the biomedical field we usually call probabilities of less then
# 0.05 (5%) small enough to reject the null-hypothesis. Thus we call
# observations with a probability of less than 0.05 "significant" and if we want
# to highlight this in text or in a graph, we often mark them with an asterisk
# (*). Also we often call observations with a probability of less than 0.01
# "highly significant" and mark them with two asterisks (**). But there is no
# special significance in these numbers, the cutoff point for significance could
# also be 0.0498631, or 0.03, or 1/(pi^3). 0.05 is just the value that the
# British statistician Ronald Fisher happened to propose for this purpose in
# 1925. Incidentally, Fisher later recommended to use different cutoffs for
# different purposes (cf.
# https://en.wikipedia.org/wiki/Statistical_significance).


# ==   2.2  probability and p-value  ===========================================

# But what do we even mean by the probability of an observation?
# Assume I am drawing samples from a normal distribution with a mean of 0 and a
# standard deviation of 1. The sample I get is ...

set.seed(sqrt(5))
x <- rnorm(1)
print(x, digits = 22)
# [1] -0.8969145466249813791748

# So what's the probability of that number? Obviously, the probability of
# getting exactly this number is very, very, very small. But also obviously,
# this does not mean that observing this number is in any way significant - we
# always observe some number. That's not what we mean in this case. There are
# several implicit assumptions when we speak of the probability of an
# observation:

# 1: the observation can be compared to a probability distribution;
# 2: that distribution can be integrated between any specific value
#      and its upper and lower bounds (or +- infinity).

# Then what we really mean by the probability of an observation in the context
# of that distribution is: the probability of observing that value, or a value
# more extreme than the one we have. We call this the p-value. Note that we are
# not talking about an individual number anymore, we are talking about the area
# under the curve between our observation and the upper (or lower) bound of the
# curve, as a fraction of the whole.


# ===   2.2.1  p-value illustrated                      

# Let's illustrate. First we draw a million random values from our
# standard, normal distribution:

set.seed(112358)
r <- rnorm(1000000)

# Let's see what the distribution looks like:

(h <- hist(r))

# The histogram details are now available in the list h -  e.g. h$counts

# Where is the value we have drawn previously?
abline(v = x, col = "#EE0000")

# How many values are smaller?
sum(r < x)

# Let's color the bars:
#    first, make a vector of red and green colors for the bars with breaks
#    smaller and larger then x, white for the bar that contains x ...
hCol <- rep("#EE000044", sum(h$breaks < x) - 1)
hCol <- c(hCol, "#FFFFFFFF")
hCol <- c(hCol, rep("#00EE0044", sum(h$breaks > x) - 1))
# ... then plot the histogram, with colored bars ...
hist(r, col = hCol)
# ... add two colored rectangles into the white bar ...
idx <- sum(h$breaks < x)
xMin <- h$breaks[idx]
xMax <- h$breaks[idx + 1]
y <- h$counts[idx]
rect(xMin, 0, x, y, col = "#EE000044", border = TRUE)
rect(x, 0, xMax, y, col = "#00EE0044", border = TRUE)
# ... and a red line for our observation.
abline(v = x, col = "#EE0000", lwd = 2)

# The p-value of our observation is the area colored red as a fraction of the
# whole histogram (red + green).


# Task:
#    Explain how the expression sum(r < x) works to give us a count of values
#    with the property we are looking for.

# Task:
#    Write an expression to estimate the probability that a value
#    drawn from the vector r is less-or-equal to x. The result you get
#    will depend on the exact values that went into the vector r but it should
#    be close to 0.185  That expression is the p-value associated with x.


# ==   2.3  One- or two-sided  =================================================

# The shape of our histogram confirms that the rnorm() function has returned
# values that appear distributed according to a normal distribution. In a normal
# distribution, readily available tables tell us that 5% of the values (i.e. our
# significance level) lie 1.96 (or approximately 2) standard deviations away
# from the mean. Is this the case here? How many values in our vector r are
# larger than 1.96?

sum(r > 1.96)
# [1] 24589

# Wait - that's about 2.5% , not 5% as expected. Why?

# The answer is: we have to be careful with two-sided distributions. 2 standard
# deviations away from the mean means either larger or smaller than 1.96 . This
# can give rise to errors. If we are simply are interested in outliers, no
# matter larger or smaller, then the 1.96 SD cutoff for significance is correct.
# But if we are specifically interested in, say, larger values, because a
# smaller value is not meaningful, then the significance cutoff, expressed as
# standard deviations, is relaxed. We can use the quantile function to see what
# the cutoff values are:

quantile(r)
quantile(r, probs = c(0.025, 0.975)) # for the symmetric 2.5% boundaries
# close to ± 1.96, as expected
quantile(r, probs = 0.95) # for the single 5% boundary
# close to 1.64 . Check counts to confirm:
sum(r > quantile(r, probs = 0.95))
# [1] 50000
# which is 5%, as expected.


# To summarize: when we evaluate the significance of an event, we divide a
# probability distribution into two parts at the point where the event was
# observed. We then ask whether the integral over the more extreme part is less
# or more than 5% of the whole. If it is less, we deem the event to be
# significant.
#

# ==   2.4  Significance by integration  =======================================

# If the underlying probability distribution can be analytically or numerically
# integrated, the siginificance of an observation can be directly computed.


# ==   2.5  Significance by simulation or permutation  =========================

# But whether the integration is correct, or relies on assumptions that may not
# be warranted for biological data, can be a highly technical question.
# Fortunately, we can often simply run a simulation, a random resampling, or a
# permutation and then count the number of outcomes, just as we did with our
# rnorm() samples. We call this an empirical p-value. (Actually, the "empirical
# p-value" is defined as (Nobs + 1) / (N + 1).  )

# Here is an example. Assume you have a protein sequence and
# you speculate that positively charged residues are close to negatively charged
# residues to balance charge locally. A statistic that would capture this is the
# mean minimum distance between all D,E residues and the closest R,K,H
# residue. Let's compute this for the sequence of yeast Mbp1.

MBP1 <- paste0("MSNQIYSARYSGVDVYEFIHSTGSIMKRKKDDWVNATHILKAANFAKAKRTRILEKEVLK",
               "ETHEKVQGGFGKYQGTWVPLNIAKQLAEKFSVYDQLKPLFDFTQTDGSASPPPAPKHHHA",
               "SKVDRKKAIRSASTSAIMETKRNNKKAEENQFQSSKILGNPTAAPRKRGRPVGSTRGSRR",
               "KLGVNLQRSQSDMGFPRPAIPNSSISTTQLPSIRSTMGPQSPTLGILEEERHDSRQQQPQ",
               "QNNSAQFKEIDLEDGLSSDVEPSQQLQQVFNQNTGFVPQQQSSLIQTQQTESMATSVSSS",
               "PSLPTSPGDFADSNPFEERFPGGGTSPIISMIPRYPVTSRPQTSDINDKVNKYLSKLVDY",
               "FISNEMKSNKSLPQVLLHPPPHSAPYIDAPIDPELHTAFHWACSMGNLPIAEALYEAGTS",
               "IRSTNSQGQTPLMRSSLFHNSYTRRTFPRIFQLLHETVFDIDSQSQTVIHHIVKRKSTTP",
               "SAVYYLDVVLSKIKDFSPQYRIELLLNTQDKNGDTALHIASKNGDVVFFNTLVKMGALTT",
               "ISNKEGLTANEIMNQQYEQMMIQNGTNQHVNSSNTDLNIHVNTNNIETKNDVNSMVIMSP",
               "VSPSDYITYPSQIATNISRNIPNVVNSMKQMASIYNDLHEQHDNEIKSLQKTLKSISKTK",
               "IQVSLKTLEVLKESSKDENGEAQTNDDFEILSRLQEQNTKKLRKRLIRYKRLIKQKLEYR",
               "QTVLLNKLIEDETQATTNNTVEKDNNTLERLELAQELTMLQLQRKNKLSSLVKKFEDNAK",
               "IHKYRRIIREGTEMNIEEVDSSLDVILQTLIANNNKNKGAEQIITISNANSHA")

# first we split this string into individual characters:
v <- unlist(strsplit(MBP1, ""))

# and find the positions of our charged residues

ED  <- grep("[ED]", v)
RKH <- grep("[RKH]", v)

sep <- numeric(length(ED)) # this vector will hold the distances
for (i in seq_along(ED)) {
  sep[i] <- min(abs(RKH - ED[i]))
}

# TASK: read and explain this bit of code

# Now that sep is computed, what does it look like?

table(sep)  # these are the minimum distances
# 24 of D,E residues are adjacent to R,K,H;
# the longest separation is 28 residues.

# What is the mean separation?
mean(sep)

# The value is 4.1 . Is this significant? Honestly, I would be hard pressed
# to solve this analytically. But by permutation it's soooo easy.

# First, we combine what we have done above into a function:

chSep <- function(v) {
  # computes the mean minimum separation of oppositely charged residues
  # Parameter: v (char) a vector of amino acids in the one-letter code
  # Value: msep (numeric) mean minimum separation

  ED  <- grep("[EDed]", v)
  RKH <- grep("[RKHrkh]", v)

  sep <- numeric(length(ED))
  for (i in seq_along(ED)) {
    sep[i] <- min(abs(RKH - ED[i]))
  }
  return(mean(sep))
}

# Execute the function to define it.

# Confirm that the function gives the same result as the number we
# calculated above:
chSep(v)

# Now we can produce a random permutation of v, and recalculate
set.seed(pi)
w <- sample(v, length(v)) # This shuffles the vector v. Memorize this
# code paradigm. It is very useful.
chSep(w)
# 3.273 ... that's actually less than what we had before.

# Let's do this 10000 times and record the results (takes a few seconds):

N <- 10000
chs <- numeric(N)
for (i in 1:N) {
  chs[i] <- chSep(sample(v, length(v))) # charge
}

hist(chs)
abline(v = chSep(v), col = "#EE0000")

# Contrary to our expectations, the actual observed mean minimum charge
# separation seems to be larger than what we observe in randomly permuted
# sequences. But is this significant? Your task to find out.


# ==   2.6  Final tasks  =======================================================

# TASK: From chs, compute the empirical p-value of a mean minimum charge
#       separation to be larger or equal to the value observed for the
#       yeast MBP1 sequence. Is the result significant?





# =    3  PART TWO: HYPOTHESIS TESTING  ========================================

# Let's consider again the genes that were deemed by our previous
# analysis (R-EDA-Clustering.R) to be "significantly differentially"
# expressed. The dataset is from  "Cell cycle expression profiles in HeLa cells"
# (GSE26922) see: http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE26922
# and contains triplicate measurements for t0 (blocked) and
#  t= 2,4,6,8 and 12h post block-release.

# R-EDA-Clustering.R explains how to load the data - here we simply
# re-load the expression set from from the locally saved file:
load("./data/GSE26922.RData")




# ==   3.1  Basic statistical tests  ===========================================

# Two sample t-test:
# In the two-sample t-test, we take multiple
# observations of our samples and ask: is the
# mean of the observations of the samples different
# from each other.

# t-test a single gene of the GSE26922 dataset for
# differential expression. Remember that these are
# triplicates. Therefore our first sample is characterized
# by three replicates at T0 (columns 1:3), and the
# second sample comprises all other columns lumped
# together. Remember: under the null hypothesis
# they should all be the same, therefore we do not
# need to consider the different time points
# separately.

# Let's look at the top and the bottom differentially
# expressed genes in tT as determined by the
# eBayes() function of the limma package?

# No. 1 in tT
tT$ID[1]
ntT1 <- which(rownames(ex) == tT$ID[1])

# No. 250 in tT
tT$ID[nrow(tT)]
ntT250 <- which(rownames(ex) == tT$ID[nrow(tT)])

# retrieve the actual log(expression) values
# from ex and plot them
gtT1 <- ex[ntT1, ]
gtT250 <- ex[ntT250, ]
plot(gtT1,    ylim=c(8,14), type="b", col="darkseagreen")
lines(gtT250,               type="b", col="limegreen")

# perform a sample-against-sample t-test for the cycle
# values against the T0 values
g1 <- t.test(ex[ntT1, 1:3], ex[ntT1, 4:18])
g1

g250 <- t.test(gtT250[1:3], gtT250[4:18])
g250

# What is in the results object?
typeInfo(g1)
g1$p.value       # this is the p-value of H0
# if it's less than 0.5, we
# generally reject H0

# Let's compute p-values for
# all the 32,321 rows of ex
tAll <- numeric(nrow(ex))
for (i in 1:nrow(ex)) {
  tAll[i] <- t.test(ex[i, 1:3], ex[i, 4:18])$p.value
}

hist(tAll, breaks = 40)
abline(v=0.05, col="firebrick2", lwd=4)
range(tAll)

# How many rows have a p less than 0.05?
sum(tAll < 0.05)   # Crafty code! Why does this work?
# Hint try: as.numeric(TRUE)
#           as.numeric(FALSE)

gpMin <- which(tAll == min(tAll))
gpMax <- which(tAll == max(tAll))

plot(ex[gpMin, ],   type="b", col="black", ylim=c(8,14))
lines(ex[gpMax, ],  type="b", col="red")
lines(ex[ntT1, ],   type="b", col="darkseagreen")
lines(ex[ntT250, ], type="b", col="limegreen")

# what are the t-values for these four genes?
tVal    <- t.test(ex[ntT1,   1:3], ex[ntT1,   4:18])$statistic
tVal[2] <- t.test(ex[ntT250, 1:3], ex[ntT250, 4:18])$statistic
tVal[3] <- t.test(ex[gpMin,  1:3], ex[gpMin,  4:18])$statistic
tVal[4] <- t.test(ex[gpMax,  1:3], ex[gpMax,  4:18])$statistic
names(tVal) <- c("ntT1", "ntT250", "gpMin", "gpMax")
tVal


# How do the observed p-values relate to the
# t-statistic?
x <- seq(min(tVal)-2, max(tVal)+2, 0.1)
f <- dt(x, df=16)
plot(x, f, xlab="x", ylab="density", type="l", lwd=2)
abline(h = 0, lwd = 0.3)
segments(tVal, -0.01, tVal, 0.05, col="red", lwd=1)
text(tVal, 0.052, labels=names(tVal), adj=c(0, 0.5), srt=90, col="red")

# Note that the underlying assumptions for t-tests
# are oK for microarrays, but less suitable for
# NGS data. See here:
# http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0123658
# for alternatives.

# ==   3.2  Wilcoxon test  =====================================================

# Non-parametric test principle illustrated.

# generate two random data samples with slightly
# different means
set.seed(53)
n <- 25
M <- matrix(nrow = n+n, ncol=2)
for (i in 1:n) {
  M[i,1]   <- rnorm(1, 10, 1)
  M[i,2]   <- 1
  M[i+n,1] <- rnorm(1, 11, 1)
  M[i+n,2] <- 2
}
boxplot(M[1:25,1], M[26:50,1]) # this is how different the two groups are
plot(M[,1], col=M[,2]) # we can see the trend in the values, plotted by index
plot(sample(1:(2*n)), M[,1], col=M[,2]) # however, with random indices it is
# quite hard to tell that these are
# two different distributions ...

# The Wilcoxon test counts for each observation of one
# group how many observations from the other group are
# ranked below it.

# To illustrate:
o <- order(M[,1])
plot(M[o,1], col=M[o,2])

wilcox.test(M[1:n,1], M[(1:n)+n,1])

# TASK:
#   repeat this test for a single random population with
#   mean 10.5.
#   1 - plot with black and red circles as before, study: does it look differnet
#   2 - are the group differences significant
#   3 - repeat this 1000 times and plot the distribution of p-values
#   4 - add the value we just got as an abline()



# Exercise - try this on expression profiles ...
#   - what profiles would you like to check for similarity?
#   - pull the data out of "ex"
#   - should you use all 18 datapoints, or compare the
#     six means of three replicates?
# What is H0 (your null hypothesis) in this case?



# ==   3.3  The multiple testing problem  ======================================

# When we repeat a test that can have a false
# positive outcome, there is a small chance
# that it will indeed turn out falsely positive. When
# we repeat the test many times, it is virtually
# gueranteed that we will have a number - albeit
# small - of false positives. When we test for
# significant differential expression across the
# genome, we are repeating our test 32,000 times.


set.seed(11358)
n <- 32321

synthVals <- as.matrix(rep(0, n*18))
dim(synthVals) <- c(n, 18)
for (i in 1:n) {
  synthVals[i,1:18] <- sample(ex[i, ])
}

# To do: spike in some "real data"!
# To discuss: is sample a good approach?

myt.test <- function(n){
  p <- t.test(synthVals[n,1:3], synthVals[n,4:18])$p.value
  return(p)
}

pVals <- rep(0, n)
for (i in 1:n) {
  pVals[i] <- myt.test(i)
}

hist(pVals, breaks=50)
abline(v=0.05, col="red")


as.numeric(TRUE)  # 1 ... TRUE is coerced into 1
sum(as.numeric(pVals < 0.05)) # ... so summing
# over all TRUE
# values counts
# them.
min(pVals)
rowMin <- which(pVals == min(pVals))
plot(synthVals[rowMin, ],
     type="b",
     col="darkseagreen")

# ===   3.3.1  Bonferroni correction                    
#
# The solution is to rescale the p-value cutoff
# for significance, by dividing it by the number
# of observations. Our cutoff of p = 0.05
# becomes ...
0.05 / n  # 1.546982e-06
# ... i.e. a very small number instead.

# What we have done is: replaced false positive
# problems with false negatives.

# But let's consider what that means, applied to
# our real data, and our synthetic data:

# our real data ...
sum(as.numeric(tAll < 0.05))       # as is
sum(as.numeric(tAll < 0.05 / n))   # Bonferroni corrected

# the synthetic data ...
sum(as.numeric(pVals < 0.05))      # as is
sum(as.numeric(pVals < 0.05 / n))  # Bonferroni corrected



# ===   3.3.2  FDR                                      

# A more modern concept to control p-Values, more suitable to situations with
# very high numbers of multiple tests is to use FDR (False Discovery Rate).


# Let's illustrate this with sample data
set.seed(100)
N <- 10000
alpha <- 0.05
y1 <- matrix(rnorm(9000*4, 0, 1), 9000, 4)
y2 <- matrix(rnorm(1000*4, 5, 1), 1000, 4)
y <- rbind(y1, y2)

myt.test <- function(y){
  t.test(y, alternative="two.sided")$p.value
}
P <- apply(y, 1, myt.test)
sum(P<alpha)

# ...

Psorted <- sort(P)
plot(Psorted, xlim=c(0, 1000), ylim=c(0, .01))
abline(b=alpha/N, a=0, col=2)

p <- p.adjust(P, method="bonferroni")
sum(p<0.05)
p <- p.adjust(P, method="fdr")
sum(p<0.05)

# Calculate the true FDR
sum(p[1:9000]<0.05)/sum(p<0.05)





# That's all.

# [End]
