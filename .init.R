# .init.R
# Functions to initialize this session
# Boris Steipe
# ==============================================================================

cat("Initializing ...\n")

# Source local functions
cat("    sourcing local functions from \"./R\" directory ...\n")
for (script in list.files(path = "./R",
                          pattern = "\\.R$",
                          full.names = TRUE)) {
    source(script)
    cat(sprintf("        ... %s\n", script))
}

cat("\n")

# Functions for making local, editable copies of scripts
checkFileExists <- function(FN) {
    if (! file.exists(FN)) {
        stop(sprintf("PANIC: expected file \"%s\" not found. \ %s ",
                     FN,
                     "Aborting initialization. Contact your instructor.\n\n"))
    }
}

writeMyCopy <- function(FN, prefix = "my", outFile) {
    # Create a local copy of file FN if the copy doesn't exist yet.
    # Side effect: write it to <prefix><FN>, or to outFile, if outFile
    #   is specified.

    if (missing(outFile)) {
        outFile <- sprintf("%s%s", prefix, FN)
        i <- nchar(prefix) + 1
        substr(outFile, i, i) <- toupper(substr(outFile, i, i))
    }

    checkFileExists(FN)

    if (! file.exists(outFile)) {
        cat(sprintf("    creating local script file: \"%s\" ... ", outFile))
        txt <- readLines(FN)
        txt[1] <- sprintf("# %s", outFile)
        txt[5] <- sprintf("#   |  %s%s     |",
                          "Edit this file with your notes,",
                          " experiments, and comments.")
        writeLines(txt, outFile)
        cat(" oK.\n")
    }
}

# Create a local copy of all core .R modules if those copies don't exist yet.
writeMyCopy("tmp.R", outFile = "myEDANotes.R")


# Clean up
rm(checkFileExists)
rm(writeMyCopy)

cat("... done.\n")

# Open main document in the script pane
file.edit("R-EDA.R")


# [End]
