# This script makes basic visualizations of variable distributions and
# tests for normality two different ways (qqplot and Shapiro-Wilk).
# Kathryn Busby
# 17-April-2021

# Load packages

# install.packages("ggpubr")
library(ggpubr)

# Bring in cleaned up .csv from other script

entleafonly <- read.csv(file="Data/entleafonly.csv")
peckdist <- read.csv(file="Data/peckdist.csv")

# #=============================================================================

# See distributions and test for normality

# #=============================================================================

# The first plot is a histogram of the variable indicated.
# When plotted in qqplot, do points fall along straight line? If normal, yes.
# After assessing visually, then apply a Shapiro-Wilk test to get a numeric
# answer. If p < .05, it's NOT normal.

# For entrance height distribution:

ggdensity(entleafonly$entcm, xlab ="Entrance Height")
ggqqplot(entleafonly$entcm)
shapiro.test(entleafonly$entcm)

# For leaf height distribution:

ggdensity(entleafonly$leafcm, xlab="Leaf Height")
ggqqplot(entleafonly$leafcm)
shapiro.test(entleafonly$leafcm)

# For peck height relative to entrance:

ggdensity(peckdist$peckent, xlab="Pecks Relative to Entrance")
ggqqplot(peckdist$peckent)
shapiro.test(peckdist$peckent)

# For pecks relative to leaf height:

ggdensity(peckdist$peckleaf, xlab="Pecks Relative to Leaf Height")
ggqqplot(peckdist$peckleaf)
shapiro.test(peckdist$peckleaf)

# For absolute peck height:

ggdensity(peckdist$abspeckht, xlab="Absolute Peck Height")
ggqqplot(peckdist$abspeckht)
shapiro.test(peckdist$abspeckht)
