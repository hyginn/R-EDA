# R-EDA.R
#
# Purpose: Exploratory Data Analysis with R workshop
#          - workshop setup
#          - contents and reference to workshop modules
#          - contents of supporting modules and assets
#
# Version: 3.0
#
# Date:    2019  05  14
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#
# V 3.0    Reconceived 2019
# V 2.0    2018 updates
# V 1.1    2017 updates
# V 1.0    First code 2016
#
# TODO:
#
# ==============================================================================


#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                          Line
#TOC> ----------------------------------------------
#TOC>   1        INTRODUCTION                     40
#TOC>   2        GETTING STARTED                 109
#TOC>   2.1        Keep a Journal ...            112
#TOC>   2.2        What's in the box ...         146
#TOC>   3        INTRODUCTION                    204
#TOC>   4        REGRESSION                      210
#TOC>   5        DIMENSION REDUCTION             216
#TOC>   6        CLUSTERING                      222
#TOC>   7        NOTES                           229
#TOC>
#TOC> ==========================================================================


# =    1  INTRODUCTION  ========================================================

# You have worked through the pre-class introductory tutorial linked from
# http://steipe.biochemistry.utoronto.ca/abc/index.php/R_tutorial (and if you
# haven't - let us know!). This is a classical way of introducing material in a
# structured, piecewise, linear fashion, concept by concept. In our class-time we
# will do something different: we'll work on four mini-projects that integrate
# the pre-work concepts. In this way, we'll cover all the basics, introduce some
# new ideas, but most importantly, practice using R in real-world scenarios.

# It's easy to hack together a few lines of code - that's something you can
# always do. But we'll take the opportunity in this workshop to practice some
# sound principles of development: specifying tasks, breaking workflow up
# into functions, organizing a project directory, writing tests, working
# with scripts to keep things reproducible, and more.

# We will proceed by defining tasks, then it is up to you to break down the
# task into small, computable steps, and to implement them. As we go along,
# we'll also learn to validate and debug code.

# Usually the tasks will involve steps that you don't know how to do. That's
# exactly what will happen in real life: in a research context we don't stop
# after we solve a problem. Any solution is only a starting point for the next
# question to which you don't know the answer. Being in a state of "knowing
# nothing" is not an exception, it's our daily life. Therefore the focus of the
# workshop is not (only) to provide answers, because "answers" I can give you
# will be (mostly) irrelevant for the tasks you work on at home. Rather we will
# focus on problem-solving strategies: precisely defining questions and
# constructing solutions, systematically, step by step. "Community intelligence"
# will be our key to progress, and - as experience shows - the solutions we come
# up with as a team are going to be much smarter than what I could come up with
# by myself. But note: what you take home from here is not going to be in a
# folder of handouts that collect dust in your office, it's going to be in the
# notes you take, the scripts you write, and the attitudes and habits you adopt
# (1).

# Our first four workshop modules focus on:
#   - an introduction to EDA
#   - regression
#   - dimension reduction methods
#   - clustering
#   - a fifth unit on machine laerning is contributed by Lauren Erdman


# The tasks are contained in R scripts. Basically all learning in this workshop
# involves working from a script, executing code and experimenting with it.
# That makes it simple for you to save your notes and comments together
# with your work, where they belong.

# But there's a catch: sometimes we update scripts during the course or
# workshop. All of the material is in an R project that is loaded from
# GitHub, so updating files with our edits is easy - just select "Pull branches"
# from the version control menu. However, that would overwrite your local
# changes - your notes and comments. So we made "personal" copies of all the
# scripts and other assets. When you excuted the "init()" function, this created
# copies of the scripts, that you can work with and edit safely.

# Source script:          |  Copy that you open and edit:
# ------------------------+-------------------------------
# journal.md              |  myJournal.md
# EDAintroduction.R       |  myEDAintroduction.R
# regression.R            |  myRegression.R
# dimensionReduction.R    |  myDimensionReduction.R
# clustering.R            |  myClustering.R

# Once again: edit, annotate, extend and work with my<file>.R files. Save these
# files and refer to them.


# =    2  GETTING STARTED  =====================================================

# TASK: Confirm that everything is set up correctly:
getwd()          # Confirm the correct directory

#        Check your "Environment pane": The functions:
#           biCode()
#           init()
#           objectInfo()
#           readFASTA
#           ... should be present. These have been loaded on startup,
#           thus confirming that the startup script has worked.

# ==   2.1  Keep a Journal ...  ================================================

# To profit the most from this workshop, it is imperative that you take notes,
# lots of notes. Code examples and task annotations go directly into the
# my<file>.R files but concepts are much better paraphrased and hand-written in
# your journal: you'll find that this dramatically improves your focus and
# understanding.

# There are many ways to keep a journal:
#  - handwritten notes are excellent because manual note-taking enhances
#    engagement and memory.
#  - Text-files can be edited within RStudio; but in a text file you won't get
#    syntax highlighting, and you wont get clickable links.
#  - R scripts are great for code, but not so great for documentation. Also
#    you won't get hyperlinks.
#  - A great compromise is a markdown file. That's what we'll use in this
#    workshop. More details below.
#  - In my University courses, my students use MediaWiki. I find this to be the
#    tool with the widest range of advantages: text, formatting, images and
#    tables are trivial, hyperlinking to local and non-local documents too,
#    document management works, collaborative editing is easy, search functions
#    find things within and accross documents, and you get a full page-history
#    and attribution. The only downside is that you have to maintain your
#    own Wiki installation.

# TASK:
#   Open the file myJournal.md for editing. Study the code example inside out
#   by selecting and executing code.

# ==   2.2  What's in the box ...  =============================================

# TASK:
#   Copy and paste the directory tree into your journal. Add comments what
#   these files are.

# --R-EDA/
#   |__.gitignore
#   |__.init.R
#   |__.Rprofile
#   |__assets/
#      |__EDA_Datasets.ppt
#      |__FND-CSC-Data_models.pdf
#      |__Jaitin-2014_SingleCellRNAseq.zip
#      |__Pramila-2006_YeastCellCycle.zip
#      |__Weissgerber_2015_BeyondBarcharts.zip
#   |__data/
#      |__0TST.pdb
#      |__CCgenes.RData
#      |__Fig_3-CharacteristicGenes.txt
#      |__GSE26922.RData
#      |__LPSdat.RData
#      |__nlsParams.RData
#      |__S288C_YDL056W_MBP1_coding.fsa
#      |__table_S3.csv
#      |__table_S3.xls
#      |__ygData.RData
#      |__ygProfiles.RData
#   |__dev/
#      |__addTOC.R
#      |__grepSrc.R
#      |__insertSnip.R
#      |__rptTwee.R
#      |__updateTOC.R
#   |__EDAintroduction.R
#   |__journal.md
#   |__R-EDA.R
#   |__R-EDA.Rproj
#   |__R/
#      |__biCode.R
#      |__crabsPlot.R
#      |__functionTemplate
#      |__objectInfo.R
#      |__pBar.R
#      |__readFASTA.R
#   |__README.md
#   |__Regression.R
#   |__sampleSolutions/
#      |__[...]
#   |__scripts/
#      |__PlottingReference.R
#      |__RPR-RegEx.R
#      |__scriptTemplate.R
#      |__unitTesting.R
#   |__tests/
#      |__test_biCode.R



# =    3  INTRODUCTION  ========================================================

# In the file pane of RStudio, click on "myEDAintroduction.R" to work with it.


# =    4  REGRESSION  ==========================================================

# In the file pane of RStudio, click on "myRegression.R" to work with it.



# =    5  DIMENSION REDUCTION  =================================================

# The dimension reduction unit is in the "myDimensionReduction.R" file.



# =    6  CLUSTERING  ==========================================================

# The clustering unit is in the script "myClustering.R".




# =    7  NOTES  ===============================================================

# (1) I am reminded of a student complaint: "I didn't like this lecturer. She
#     made us learn everything by ourselves."

# [END]
