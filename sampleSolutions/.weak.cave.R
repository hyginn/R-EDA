## Sample solution >>>                                               (weak.cave)

# TASK: Apply PCA to the crabs dataset to distinguish species and sex
#       from morphometric measurements.
head(crabs)
pcaCrabs <- prcomp(crabs[, 4:8])

plot(pcaCrabs)
summary(pcaCrabs)
str(pcaCrabs)

# Plot projections along the components into a scatterplot.
# Axes for points are scaled as values, for vectors as variance
# Default for biplot() is the first and second component.

biplot(pcaCrabs, xlabs=as.numeric(fac))
legend(-120, -63,
       c("1: B.F", "2: B.M", "3: O.F", "4: O.M"),
       box.col=1, bg="lightgrey")

# Plot the first against the third principal component
biplot(pcaCrabs, xlabs=as.numeric(fac), choices = c(1, 3))
legend(-120, -63,
       c("1: B.F", "2: B.M", "3: O.F", "4: O.M"),
       box.col=1,
       bg="lightgrey")

# Plot the second against the third principal component
biplot(pcaCrabs, xlabs=as.numeric(fac), choices = c(2, 3))
legend(-15, -8,
       c("1: B.F", "2: B.M", "3: O.F", "4: O.M"),
       box.col=1,
       bg="lightgrey")


#       Plot the results of important
#       PCs as a scatterplot in which blue males are shown as blue
#       triangles, orange males as orange triangles, blue females as
#       blue circles and orange females as orange circles.

# Somewhat pedestrian
plot(pcaCrabs$x[,2], pcaCrabs$x[,2], type ="n")
points(pcaCrabs$x[  1: 50, 2], pcaCrabs$x[  1: 50, 3], pch=17, col="blue")
points(pcaCrabs$x[ 51:100, 2], pcaCrabs$x[ 51:100, 3], pch=19, col="blue")
points(pcaCrabs$x[101:150, 2], pcaCrabs$x[101:150, 3], pch=17, col="orange")
points(pcaCrabs$x[151:200, 2], pcaCrabs$x[151:200, 3], pch=19, col="orange")

# A more advaced function is in ./R/crabsPlot.R
crabsPlot(pcaCrabs$x[,2], pcaCrabs$x[,3],
          crabs[ ,1], crabs[ ,2], pcaCrabs$x[ ,1],
          main = "Principal components 2 and 3 distinguish crabs",
          xlab = "PC2",
          ylab = "PC3")


## <<< Sample solution
# [END]
