# This addresses question 3: Are nests that avoid predation lower?
# Kathryn Busby
# mkbusby@email.arizona.edu
# May 20, 2021

# Load libraries.

#install.packages("olsrr")
library(olsrr)

# Bring in file:

peckcountsonly <- read.csv(file="Data/peckcountsonly.csv")

#===============================================================================
# (3) Do carpenter bees that place their entrances lower relative to leaf
# heights avoid predation more?
#===============================================================================

# Test to see if nests with no predation had a lower median height than those
# with predation.
# The non-parametric test that accomplishes this is a Wilcoxon-Mann-Whitney test.
# Null predicts that the entrance heights relative to leaf heights for the two
# groups are the same.
# HA predicts that entrance heights are different between the w.peck and no.peck
# group.

# Entleaf where peck count is 0 versus not 0.

w.peck.ent.leaf <- peckcountsonly$entleaf[which(!peckcountsonly$peck.count == 0)]
no.peck.ent.leaf <- peckcountsonly$entleaf[which(peckcountsonly$peck.count == 0)]

# Wilcox Test:

wilcox.ent.ht <- wilcox.test(x=w.peck.ent.leaf, y=no.peck.ent.leaf, paired=FALSE)
wilcox.ent.ht

# Here we use a correlation method to look for the strength of the relationship
# between height of the entrance and the frequency of pecks.

cor(peckcountsonly$entleaf,
    peckcountsonly$peck.count, method = c("pearson"))

# Here's a logistic regression, which uses 0, 1 response varibles.
# We need to get the peck count to be interpreted as a yes/no binary variable.
# Now build a logisitic regression that looks at whether nests with no pecks had lower entrances
# than nests with no pecks.

peckcountsonly$peck.binary <- ifelse((peckcountsonly$peck.count==0), 0, 1)
log.reg.ent.ht <- glm(peckcountsonly$peck.binary~peckcountsonly$entleaf,
                      family=binomial(link='logit'))
summary(log.reg.ent.ht)

# Preliminary boxplot of entrance heights with and without pecks.

ggplot(peckcountsonly, aes(x=factor(peck.binary),
                           y=entleaf)) +
  geom_boxplot()

# Looking at this another way, does entrance height relative to leaf height
# predict number of pecks?

non.zero.unique <- peckcountsonly[which(peckcountsonly$peck.binary == 1),]
zero.unique <- peckcountsonly[which(peckcountsonly$peck.binary == 0),]

lm <- lm(non.zero.unique$peck.count~non.zero.unique$entleaf)
summary(lm)
lm.plot <- plot(peckcountsonly$peck.count~peckcountsonly$entleaf)
abline(lm)

# The following line tests for heteroskedasticity, against the null hypothesis
# that our variance is homogenous.

hetero <- ols_test_score(lm)
hetero

# Compare mean entrance heights, with/without pecks using WMW.

median(zero.unique$entleaf)
median(non.zero.unique$entleaf)
wilcox.test(zero.unique$entleaf, non.zero.unique$entleaf)

# Compare peck height relative to leaf height distribution against entrance
# height relative to leaf distribution.
# It's not appropriate to use a KS test for this,
# because of the presence of ties.
# We also cannot use a Chi-square test because the sample sizes are different.
# Here is a Mann-Whitney U Test for independent things.
# The two distributions are indexed through peckcountsonly$entleaf and
# peckcountsonly$peckleaf

peck.leaf <- peckdist$peckleaf
ent.leaf <- peckdist$entleaf

wilcox.test(peck.leaf,ent.leaf)
ks.test(peck.leaf, ent.leaf)

# Make an output file:

sink(file=paste0("Output/",Sys.Date(),"_BirdPecksQ3.txt"))
cat("Q3: When bees build lower, do they get pecked less?\n")
cat("\nNumber of stalks with peck counts available, 0 or otherwise:")
print(nrow(peckcountsonly))
cat("\nNumber of stalks that got pecked:")
print(length(w.peck.ent.leaf))
cat("\nPercent pecked stalks:")
print((length(w.peck.ent.leaf)/nrow(peckcountsonly)) * 100)
cat("\nNumber of stalks that did not get pecked:")
print(length(no.peck.ent.leaf))
cat("\nPercent of stalks that did not get pecked:")
print((length(no.peck.ent.leaf)/nrow(peckcountsonly)) * 100)
cat("\nWilcox rank sum test to see if entrance heights are different between pecked and unpecked nests:")
print(wilcox.ent.ht)
cat("\nMedian entrance height relative to leaves of nests that got pecked:")
print(median(w.peck.ent.leaf))
cat("\nMedian entrance height relative to leaves of nests that did not get pecked:")
print(median(no.peck.ent.leaf))
sink()


