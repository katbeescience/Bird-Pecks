# This addresses question 2: Do woodpeckers usually peck above the leaf height?
# Kathryn Busby
# mkbusby@email.arizona.edu
# May 20, 2021

# Bring in the file.

peckdist <- read.csv(file="Data/peckdist.csv")

#===============================================================================
# (2) Do woodpeckers peck either above or below leaf height most frequently?
#===============================================================================

# How many pecks are within 10 cm of the leaf height?

pecks10cmleaf <- length(which(peckdist$peckleaf < 10 & peckdist$peckleaf > -10))
allpecks <- length(peckdist$peckleaf)

ten.cm.percent.pecks <- (pecks10cmleaf/allpecks)*100

# Chi-squared goodness-of-fit.
# In our data, peck heights in the column peck.leaf are negative if they are
# below the leaf height.

# If the peck height is negative (above leaf height), it is assigned "C".
# If positive (below leaf height), it is assigned "D".

C.count <- nrow(peckdist[which(peckdist$peckleaf < 0),])
D.count <- nrow(peckdist[which(peckdist$peckleaf > 0),])
CD.count <- as.matrix(c(C.count,D.count))

chisq.test(CD.count, y=NULL, p=(rep(1/nrow(CD.count), nrow(CD.count))))

# Calculate percentage of Cs and Ds.

Percent.C <- (C.count/(C.count + D.count))*100
Percent.D <- (D.count/(C.count + D.count))*100

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
