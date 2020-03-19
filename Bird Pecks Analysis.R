# Do woodpeckers affect where carpenter bees place their nests within the stalks of sotol plants?
# Do sotol's barbed leaves provide bees protection from woodpeckers?
# Script by Kathryn Busby
# Friday, November 15, 2019

#=========================================================================================================

# Load packages.

# install.packages("ggpubr")
# install.packages("coin")
# install.packages("olsrr")

# Load libraries.

library(tidyverse)
library(ggpubr)
library(coin)
library(ggplot2)
library(olsrr)

# Bring in the file.

all.data <- read.csv(file= "../Data/20191017_Formatted CBP.csv")

# Clean it up.

all.have.lfht <- tidy.df <- all.data %>%
  filter(!all.data$Leaf.Height..cm. == "NA") %>%
  rename(peck.ent = Distance.of.Pecks.from.Entrance..cm., peck.leaf = Peck.Height.minus.Leaf.Height..cm.,
         ent.leaf = Position.of.Entrance.Relative.to.Leaf, AorB = Entrance.Above.or.Below)

tidy.df <- all.data %>%
  filter(!all.data$Leaf.Height..cm. == "NA" | !all.data$Entrance.Height..cm. == "NA") %>%
  rename(peck.ent = Distance.of.Pecks.from.Entrance..cm., peck.leaf = Peck.Height.minus.Leaf.Height..cm.,
         ent.leaf = Position.of.Entrance.Relative.to.Leaf, AorB = Entrance.Above.or.Below)

tidy.df <- tidy.df %>% select(Site, peck.ent, peck.leaf, ent.leaf, AorB, peck.ht, peck.count)

#===========================================================================================================

# Test for normality. When plotted in qqplot, do points fall along straight line?
# After assessing visually, then apply a Shapiro-Wilk test to get a number answer.
# Do this test to different combos of variables.

ggdensity(all.data$Entrance.Height..cm., xlab ="Entrance Height")
ggqqplot(all.data$Entrance.Height..cm.)
shapiro.test(all.data$Entrance.Height..cm.)

ggdensity(all.data$Leaf.Height..cm., xlab="Leaf Height")
ggqqplot(all.data$Leaf.Height..cm.)
shapiro.test(all.data$Leaf.Height..cm.)

ggdensity(tidy.df$peck.ent, xlab="Pecks Relative to Entrance")
ggqqplot(tidy.df$peck.ent)
shapiro.test(tidy.df$peck.ent)

ggdensity(tidy.df$peck.leaf, xlab="Pecks Relative to Leaf Height")
ggqqplot(tidy.df$peck.leaf)
shapiro.test(tidy.df$peck.leaf)

ggdensity(tidy.df$peck.ht, xlab="Absolute Peck Height")
ggqqplot(tidy.df$peck.ht)
shapiro.test(tidy.df$peck.ht)

# The warnings are due to the plot ignoring values of NA, which is fine.
# Nothing here looks normally distributed except peck height relative to entrance,
# which is very near to being significantly different from normal.
# It will be interesting to see how that variable acts in our upcoming tests.

#===========================================================================================================

# All tests are non-parametric, except the linear regression. In that case, we check heteroskedasticity.
# The following analyses are three-part to answer these three questions:
# (1) Do carpenter bees place nest entrances low rather than high relative to height of sotol leaves?
# (2) Do woodpeckers usually peck higher than the height of leaves, rather than lower?
# (3) Do carpenter bees that place their entrances lower relative to leaf heights avoid predation more?

#===========================================================================================================

# (1) Do carpenter bees place nest entrances low rather than high relative to height of sotol leaves?
# Is the entrance height relative to the leaf height more likely than not to be negative?
# For this, we can use the variable ent.leaf and do a Chi-squared test for goodness-of-fit.
# All entrances above leaves will be labeled "A"s, and all those below will be "B"s.
# Chi-square null hypothesis (expected values) is that there will be an equal number of As and Bs.

# Okay, first, this could be done in tidyverse.
A.count <- length(tidy.df$AorB[which(tidy.df$AorB=="2")])
A.count
B.count <- length(tidy.df$AorB[which(tidy.df$AorB=="1")])
B.count

AB.count <- length(tidy.df$AorB)
AB.count

value.vector <- c(A.count,B.count)

ent.ht.chisq <- chisq.test(value.vector, y=NULL, p=(rep(1/length(value.vector), length(value.vector))))
ent.ht.chisq

# Chi-square test says the probability that we can reject our null is highly significant.
# Looks like bees do choose to dig an entrance below the height of the leaves.
# This is an especially strong pattern since we believe from casual observation that there is usually
# more stalk height above than below the leaf tips. So our null hypothesis that there would be an
# equal chance of the hole above versus below leaf height was actually a conservative one (good).

# Here's a nice figure for this result:

ent.lf.boxplot <- ggplot(tidy.df, aes(y=ent.leaf)) +
  geom_boxplot(width=.2) +
  scale_x_discrete(labs("")) +
  scale_y_continuous(labs(y="Entrance Height Relative to Leaf Height (cm)"), limits = c(-100, 60)) +
  geom_hline(yintercept = 0, linetype="dashed", color="black") +
  theme_classic()
ent.lf.boxplot

#===========================================================================================================

# (2) Do woodpeckers usually peck higher than the height of leaves, rather than lower?

# Another Chi-squared. This one is basically the same thing, but asks if pecks are more often above
# or below the leaf height.
# In our data, peck heights in the column peck.leaf are negative if they are below the leaf height.
# Need to set up another null hypothesis of expected values to compare like above.
# This time if the peck height is negative, it gets assigned "C", and if positive it's "D"
# "C" means below. "D" means above.

C.count <- nrow(tidy.df[which(tidy.df$peck.leaf < 0),])
D.count <- nrow(tidy.df[which(tidy.df$peck.leaf > 0),])
CD.count <- as.matrix(c(C.count,D.count))

chisq.test(CD.count, y=NULL, p=(rep(1/nrow(CD.count), nrow(CD.count))))

Percent.C <- (C.count/(C.count + D.count))*100
Percent.D <- (D.count/(C.count + D.count))*100

# Let's make a function that will automatically figure out the expected categories for a Chi-square.

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


# Add fact that they were within 10 cm of leaf height.

# Looks like there's a really significant chance of rejecting our null, which was that there were the same
# number of pecks above leaves as there were below leaves.
# However, what's interesting is the direction of the pattern. There are way more pecks below the leaf
# height than above. This could make sense, because that's where the nests are.

# There needs to be a better way of looking at this. This could be where we do the KS test to see if the
# distribution of pecks increases in frequency as peck.leaf becomes more positive.

# Let's try to do a k-s test to see if the actual above and below
# distributions are significantly different from a uniform distribution.
# First create indeces to the data.

above.ht.dist <- na.omit(tidy.df[which(tidy.df$AorB=="2"),])
head(above.ht.dist)
nrow(above.ht.dist)
min.above <- min(above.ht.dist$peck.leaf)
max.above <- max(above.ht.dist$peck.leaf)
 
below.ht.dist <- na.omit(tidy.df[which(tidy.df$AorB=="1"),])
head(below.ht.dist)
nrow(below.ht.dist)
min.below <- min(below.ht.dist$peck.leaf)
max.below <- max(below.ht.dist$peck.leaf)

compare_dist_below = runif(nrow(below.ht.dist), min = min.below, max = max.below)
compare_dist_above = runif(nrow(above.ht.dist), min = min.above, max = max.above)

ks.test(x=below.ht.dist$peck.leaf, y = compare_dist_below)
ks.test(x=above.ht.dist$peck.leaf, y = compare_dist_above)

# Okay we're testing against a randomly selected uniform distribution in the lines above, which is good
# because it introduces uncertainty.
# The way to figure out signficance then is to bootstrap it, running this test 10,000 times in a for-loop.
# Pseudocode: For i in 10,000, run the ks.test above. Then build a dataframe of each p-value. Once it is
# built, do stats on the stats to get a more accurate estimate of the p-value.

ks.below <- ks.test(x=below.ht.dist$peck.leaf, y = compare_dist_below)
ks.above <- ks.test(x=above.ht.dist$peck.leaf, y = compare_dist_above)
p.ks.below <- ks.below$p.value
p.ks.above <- ks.above$p.value
D.ks.below <- ks.below$statistic
D.ks.above <- ks.above$statistic

for (i in 1:10000) {
  compare_dist_below = runif(nrow(below.ht.dist), min = min.below, max = max.below)
  compare_dist_above = runif(nrow(above.ht.dist), min = min.above, max = max.above)
  ks.below <- ks.test(x=below.ht.dist$peck.leaf, y = compare_dist_below)
  p.ks.below <- c(p.ks.below, ks.below$p.value)
  D.ks.below <- c(D.ks.below, ks.below$statistic)
  ks.above <- ks.test(x=above.ht.dist$peck.leaf, y = compare_dist_above)
  p.ks.above <- c(p.ks.above, ks.above$p.value)
  D.ks.above <- c(D.ks.above, ks.above$statistic)
  i=i+1
}

p.ks.below
p.ks.above
D.ks.below
D.ks.above

mean.p.ks.below <- mean(p.ks.below)
mean.p.ks.below
mean.D.ks.below <- mean(D.ks.below)
mean.D.ks.below
mean.p.ks.above <- mean(p.ks.above)
mean.p.ks.above
mean.D.ks.above <- mean (D.ks.above)
mean.D.ks.above

# Here are some nice figures for this result:
# Boxplot of peck heights relative to leaf heigth, similar to how it was depicted in the previous figure for Q1.

overall.peck.lf.boxplot <- ggplot(tidy.df, aes(y=tidy.df$peck.leaf)) +
  geom_boxplot(width=.2) +
  scale_x_discrete(labs("")) +
  scale_y_continuous(labs(y="Peck Height Relative to Leaf Height (cm)")) +
  geom_hline(yintercept = 0, linetype="dashed", color="black") +
  theme_classic()
overall.peck.lf.boxplot

# Vertical scatterplot showing distributions of pecks binned above and below leaf height as points.

above.dist.peck.lf <- ggplot(above.ht.dist, aes(y=above.ht.dist$peck.leaf, x=2)) +
  geom_point() +
  scale_x_discrete(labs("")) +
  scale_y_continuous(labs(y="Peck Height Relative to Leaf Height for Stalks Above Leaf Height (cm)")) +
  geom_hline(yintercept = 0, linetype="dashed", color="red") +
  theme_classic()
above.dist.peck.lf

below.dist.peck.lf <- ggplot(below.ht.dist, aes(y=below.ht.dist$peck.leaf, x=2)) +
  geom_point() +
  scale_x_discrete(labs("")) +
  scale_y_continuous(labs(y="Peck Height Relative to Leaf Height for Stalks Below Leaf Height (cm)")) +
  geom_hline(yintercept = 0, linetype="dashed", color="red") +
  theme_classic()
below.dist.peck.lf

# The following plot depicts the distribution of bird pecks as apostrophes. This will be used to superimpose
# the real distribution over an illustration of a stalk.

overall.peck.lf.dist <- ggplot(tidy.df, aes(y=tidy.df$peck.leaf, x=2)) +
  geom_jitter(width=.025, height=0, cex=7, shape="'") +
  scale_x_discrete(labs("")) +
  scale_y_continuous(labs(y="Peck Height Relative to Leaf Height (cm)")) +
  #geom_hline(yintercept = 0, linetype="dashed", color="red") +
  theme_classic()
overall.peck.lf.dist

#===========================================================================================================

# (3) Do carpenter bees that place their entrances lower relative to leaf heights avoid predation more?
# Test to see if nests with no predation had a lower median height than those with predation.
# The non-parametric test that accomplishes this is a Wilcoxon-Mann-Whitney test.
# Null predicts that the entrance heights relative to leaf heights for the two groups are the same.
# HA predicts that entrance heights are different between the w.peck and no.peck group.

# First of all, we only need to look at each nest ID once. So make a dataframe with only unique IDs.

unique.ID <- all.data[which(duplicated(all.data$ID)==F),]
head(unique.ID)

w.peck.ent.leaf <- as.double(unique.ID$Position.of.Entrance.Relative.to.Leaf[which(!unique.ID$peck.count == "NA")])
no.peck.ent.leaf <- as.double(unique.ID$Position.of.Entrance.Relative.to.Leaf[which(is.na(unique.ID$peck.count) == TRUE)])
wilcox.ent.ht <- wilcox.test(x=w.peck.ent.leaf, y=no.peck.ent.leaf, paired=FALSE)
wilcox.ent.ht

# The WMW test does not show predated vs non-predated nests to have different mean heights.

# Here we use a correlation method to look for the strength of the relationship between height of
# the entrance and the frequency of pecks.

cor(unique.ID$Position.of.Entrance.Relative.to.Leaf,
    unique.ID$peck.count, method = c("pearson"))

# Here's a logistic regression, which uses 0, 1 response varibles.
# We need to get the peck count to be interpreted as a yes/no binary variable.
# Now build a logisitic regression that looks at whether nests with no pecks had lower entrances
# than nests with no pecks.

unique.ID$peck.binary <- ifelse(is.na(unique.ID$peck.count), 0, 1)
log.reg.ent.ht <- glm(unique.ID$peck.binary~unique.ID$Position.of.Entrance.Relative.to.Leaf, family=binomial(link='logit'))
summary(log.reg.ent.ht)
ggplot(unique.ID, aes(x=factor(peck.binary), y=Position.of.Entrance.Relative.to.Leaf)) + geom_boxplot()

# There's no mean difference between pecked and unpecked entrance heights relative to leaf. BUT...
# Looking at this another way, does entrance height relative to leaf height predict number of pecks?

unique.ID$peck.count[which(is.na(unique.ID$peck.count))] <- 0

non.zero.unique <- unique.ID[which(unique.ID$peck.count > 0),]
zero.unique <- unique.ID[which(is.na(unique.ID$peck.count)),]

lm <- lm(non.zero.unique$peck.count~non.zero.unique$Position.of.Entrance.Relative.to.Leaf)
summary(lm)
lm.plot <- plot(tidy.df$peck.count~tidy.df$ent.leaf)
abline(lm)
# It looks like ent.leaf does not predict peck.count. p=.4457

# Is it appropriate to be using a linear regression here? The following line tests for heteroskedasticity,
# against the null hypothesis that our variance is homogenous. So we don't want to reject the null.
hetero <- ols_test_score(lm)
hetero

# Here's a nice figure for this result:

lm.peck.ent <- ggplot(non.zero.unique, aes(y=peck.count, x=Position.of.Entrance.Relative.to.Leaf)) +
  geom_point() +
  stat_smooth(se=FALSE, method="lm", col="black") + 
  scale_y_continuous("Number of Pecks", limits=c(0,12), breaks=seq(0,12,2)) +
  scale_x_continuous("Entrance Height Relative to Leaf Height (cm)") +
  theme_classic()
lm.peck.ent

# Compare mean entrance heights, with/without pecks using WMW.

mean(zero.unique$Position.of.Entrance.Relative.to.Leaf)
mean(non.zero.unique$Position.of.Entrance.Relative.to.Leaf)
wilcox.test(zero.unique$Position.of.Entrance.Relative.to.Leaf, non.zero.unique$Position.of.Entrance.Relative.to.Leaf)

# Compare peck height relative to leaf distribution against entrance height relative to leaf distribution.
# It's not appropriate to use a KS test for this, because of the presence of ties.
# We also cannot use a Chi-square test because the sample sizes are different.
# Here is a Mann-Whitney U Test for independent things.
# The two distributions are indexed through tidy.df$ent.leaf and tidy.df$peck.leaf

peck.leaf <- tidy.df$peck.leaf[!is.na(tidy.df$peck.leaf)]
ent.leaf <- tidy.df$ent.leaf

wilcox.test(peck.leaf,ent.leaf)
ks.test(peck.leaf, ent.leaf)

# Okay let's try this... since pecks and entrances are paired in a way, what if I use the average
# peck height of each unique stalk, and then do a paired test to compare the entrance height 
# distributions to the peck height distribution?

# For each unique bee, average the peck heights. Then add that to a data frame containing the
# unique IDs and the entrance heights of each nest. Could just use a for-loop to get each mean,
# or could possibly do this in tidyverse.

tidy.unique <- unique.ID %>%
  select(ID, ent.leaf=Position.of.Entrance.Relative.to.Leaf)

# Mean peck height for each entrance height equals all the peck heights relative to leaf height
# for that ID, summed up and then divided by the peck count for that ID.

# for (i in 1:length(unique.ID$ID)) {
#   stlk.pk.ht <- sum(unique.ID$peck.ht[which(unique.ID$ID==)])
# }

# What if we used the absolute peck height and absolute entrance heights, then kept them paired?

plot(all.data$Entrance.Height..cm.~all.data$peck.ht)
# wilcox.test(all.data$Entrance.Height..cm.~all.data$peck.ht)

# Or maybe the appropriate thing to do is to compare the entrance hole distribution to the 
# distribution of the lowest single peck on each nest.Then they will be truly paired.
# Then we might expect a negative correlation.
# Best analysis pending...

#===========================================================================================================

# To finish everything up nicely, let's print off the results in a tidy, descriptive output file.

# dir.create("../Output")
sink(file = "../Output/CBP_Test_Results.txt")

cat("The following analyses are three-part to answer these three questions:\n
(1) Do carpenter bees place nest entrances low rather than high relative to height of sotol leaves?\n
    Chi-square test to see if there's a difference between the counts of entrances below versus above leaf height:\n")
print(ent.ht.chisq)

cat("(2) Do woodpeckers usually peck higher than the height of leaves, rather than lower?\n
      KS test to look at whether distribution of pecks goes up with height relative to leaves:\n
      The numbers below are mean p-values determining whether we can reject the null that the peck distribution\n
      above and below leaf heights is equal to randomly generated uniform distributions bootstrapped 10,000 times.\n")
print(c("Mean p-value above leaf height = ", mean.p.ks.above))
print(c("Mean p-value below leaf height = ", mean.p.ks.below))

cat("\n   Another way to answer this question would be to compare the distribution of peck heights
        to an equal distribution above and below leaf height, using a Chi-square. Here it is:\n")
print(peck.ht.chisq)

cat("\n   And to make sure we're looking in the right direction, see histogram of the peck distribution relative to leaf height.\n")

cat("\n(3) Do carpenter bees that place their entrances lower relative to leaf heights avoid predation more?\n
      Linear regression predicts relationship between entrance height and number of pecks.\n
      The higher the entrance, the more pecks there are.")
print(summary(lm))

cat(" Heteroskedasticity test to justify using the linear model. We need to NOT reject this null:")
print(hetero)

sink()
