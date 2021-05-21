# This addresses question 2: Do woodpeckers usually peck above the leaf height?
# Kathryn Busby
# mkbusby@email.arizona.edu
# May 20, 2021

# Bring in the file.

peckdist <- read.csv(file="Data/peckdist.csv")

#===============================================================================
# (2) Where do woodpeckers usually peck?
#===============================================================================

# First, how many pecks are within 10 cm of the leaf height?

pecks10cmleaf <- length(which(peckdist$peckleaf < 10 & peckdist$peckleaf > -10))
allpecks <- length(peckdist$peckleaf)

ten.cm.percent.pecks <- (pecks10cmleaf/allpecks)*100

# Another Chi-squared. This one is basically the same thing, but asks if pecks
# are more often above or below the leaf height.
# In our data, peck heights in the column peck.leaf are negative if they are
# below the leaf height.

# This time if the peck height is negative, it gets assigned "C", and if
# positive it's "D". "C" means below. "D" means above.

C.count <- nrow(peckdist[which(peckdist$peckleaf < 0),])
D.count <- nrow(peckdist[which(peckdist$peckleaf > 0),])
CD.count <- as.matrix(c(C.count,D.count))

chisq.test(CD.count, y=NULL, p=(rep(1/nrow(CD.count), nrow(CD.count))))

Percent.C <- (C.count/(C.count + D.count))*100
Percent.D <- (D.count/(C.count + D.count))*100

# Let's make a function that will automatically figure out the expected
# categories for a Chi-square.

ChiSq.Expected <- function(data, numcat) {
  sum <- sum(data)
  expected.vector <- sum/numcat
  return(expected.vector)
}

# Do most pecks occur above or below leaf height?
# To do a Chi-square comparing the overall heights of pecks to an equal above and below distribution,
# need to use the count of all pecks, and the counts of pecks above and below respectively.
# Need heights of each individual peck, but need to be able to index to whether they are above or
# below leaf height.
# Is this identical to what I did above? Yes it is. Also matches manual calculation. Good sanity
# check I guess.

countCD <- C.count + D.count

exp <- ChiSq.Expected(countCD, 2)

# Do that test:

peck.ht.chisq <- chisq.test(CD.count, y=NULL, p=(rep(1/2, 2)))

# Output file:

sink(file = paste0("Output/",Sys.Date(),"_BirdPecksQ2.txt"))
cat("Q2: Where do birds peck relative to the leaves?\n")
cat("\nNumber of nests with measured bird peck marks:\n")
print(length(unique(peckdist$nestcode)))
cat("\nPercent of pecks above leaf height:")
print(Percent.D)
cat("\nPercent of pecks below leaf height:")
print(Percent.C)
cat("\nPercent of pecks within 10cm of leaf height:")
print(ten.cm.percent.pecks)
cat("\nRange of peck heights off the ground (cm):")
print(range(peckdist$abspeckht))
cat("\nMedian peck height:")
print(median(peckdist$abspeckht))
cat("\nMedian number of pecks per nest excluding non-pecked nests:")
print(median(peckdist$peck.count))
cat("\nChi-squared test to see if most pecks are above or below the leaf height:\n")
print(peck.ht.chisq)

sink()
