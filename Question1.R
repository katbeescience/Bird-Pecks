# This addresses question 1: Where are entrance holes relative to leaf height?
# Kathryn Busby
# mkbusby@email.arizona.edu
# May 20, 2021

# Bring in file

peckcountsonly <- read.csv(file="Data/entleafonly.csv")

#===============================================================================

# (1) Where do carpenter bees place their nests relative to sotol leaf height?

#===============================================================================

# Is the entrance height relative to the leaf height more likely than not to be
# negative?
# Chi-squared test for goodness-of-fit.
# All entrances above leaves will be labeled "A"s, and all those below will be
# "B"s.
# Chi-square null hypothesis (expected values) is that there will be an equal
# number of As and Bs.

A.count <- length(entleafonly$AB[which(entleafonly$AB==0)])
B.count <- length(entleafonly$AB[which(entleafonly$AB==1)])
AB.count <- length(peckcountsonly$AB)

value.vector <- c(A.count,B.count)

ent.ht.chisq <- chisq.test(value.vector, y=NULL, p=(rep(1/length(value.vector),
                                                        length(value.vector))))

# Save an output file of results.

sink(file = paste0("Output/",Sys.Date(),"_BirdPecksQ1.txt"))

cat("(Q1) Where do carpenter bees place nest entrances relative to leaf height?\n")
cat("\nTotal number of stalks with entrance and leaf height available:")
print(nrow(entleafonly))
cat("\nMedian entrance height (cm):")
print(median(entleafonly$entcm))
cat("\nMedian leaf height (cm):")
print(median(entleafonly$leafcm))
cat("\nPercent of entrances above leaf height:")
print((A.count/AB.count)*100)
cat("\nPercent of entrances below leaf height:")
print((B.count/AB.count)*100)


cat("\nChi-square test to see if there's a difference between the counts of entrances below versus above leaf height:\n")
print(ent.ht.chisq)

sink()

