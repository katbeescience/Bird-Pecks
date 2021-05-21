# This script cleans up the data for use in later steps.
# Kathryn Busby
# mkbusby@email.arizona.edu
# May 20, 2021

# Load packages

library(tidyverse)

# Bring in data file

bp <- read.csv(file="Data/20210418_BirdPecks.csv")
head(bp)

# Clean up data

tidybp <- bp %>%
  rename("nestcode"=StalkFieldDataTable_NestCode,
         "site"=WhichSite.,
         "leafmm"=LeafHeight.mm.,
         "entmm"=EntranceHeight.mm.,
         "peck.count"=TotalNumberOfBirdPecks)
head(tidybp)

# If any of these are missing nest entrance height or leaf height, we can't
# really use them. So we'll go ahead and remove those rows with NAs in either
# of those two columns.

tidybp <- tidybp %>%
  drop_na(leafmm, entmm)
head(tidybp)

# Transform entrance and leaf mm measurements to cm.

tidybp$leafcm <- tidybp$leafmm / 10
tidybp$entcm <- tidybp$entmm / 10

# We're going to want a column looking at entrance height relative to leaf.

tidybp$entleaf <- tidybp$entcm - tidybp$leafcm

# And a column to designate above or below.

i <- 1
for (i in 1:nrow(tidybp)){
if (tidybp$entleaf[i] > 0) {
  tidybp$AB[i] <- 0
  i <- i + 1
} else {
  tidybp$AB[i] <- 1
  i <- i + 1
}
}

# There are still some NAs for pecks. So let's save this tibbl as an object
# from which entleaf stats can be gotten. Let's get rid of the peck stuff so we
# won't accidentally try to use it for stats later.

entleafonly <- tidybp %>%
  select(nestcode, site, leafcm, entcm, AB)
head(entleafonly)

# Now let's take tidybp and make a similar object where we can use peck counts,
# including entrance height and leaf height.

peckcountsonly <- tidybp %>%
  drop_na(peck.count) %>%
  select(nestcode, site, leafcm, entcm, entleaf, peck.count, AB)
head(peckcountsonly)

# We'll need to put the peck measurements into long format, but that will
# generate a bunch of NAs. Start with no NAs in entmm, leafmm, or peck.count.
# We don't need to stare at the notes either, so we'll drop that column for now.

peckdist <- tidybp %>%
  drop_na(peck.count) %>%
  #select(-notes) %>%
  pivot_longer(cols=c(BirdPredUp.mm.1,
               BirdPredUp.mm.2,
               BirdPredUp.mm.3,
               BirdPredUp.mm.4,
               BirdPredUp.mm.5,
               BirdPredUp.mm.6,
               BirdPredUp.mm.7,
               BirdPredUp.mm.8,
               BirdPredUp.mm.9,
               BirdPredUp.mm.10,
               BirdPredDn.mm.1,
               BirdPredDn.mm.2,
               BirdPredDn.mm.3,
               BirdPredDn.mm.4,
               BirdPredDn.mm.5,
               BirdPredDn.mm.6,
               BirdPredDn.mm.7,
               BirdPredDn.mm.8,
               BirdPredDn.mm.9,
               BirdPredDn.mm.10),
               values_to="peckentmm",
               names_to="peck.ent")

# Now we want to be able to compare peck locations relative to leaf height.
# First, we have peck locations labeled "Up" and others labeled "Dn".
# To use math with them, we'll need all the "Dn" ones to be negative numbers.

downindices <- grep("BirdPredDn.mm.", peckdist$peck.ent)
peckdist$peckentmm[downindices] <- peckdist$peckentmm[downindices]*(-1)

# Transform mm peck distances to cm.

peckdist$peckentcm <- peckdist$peckentmm / 10

# For peck distance analyses, I don't think we need to keep rows where there
# were no pecks. We also don't need the up/down labels anymore. Let's get rid.
# Finally, there will be leftover NAs in the peck locations left over from when
# we pivoted longer. So we'll get rid of those too.

peckdist <- peckdist %>%
  select(-c(peck.ent, leafmm, entmm, peckentmm)) %>%
  filter(peck.count != 0) %>%
  drop_na(peckentcm)

# This object does need another column to show where the peck is relative to the
# leaf height. This is calculated peck height minus leaf height.

peckdist$abspeckht <- peckdist$peckentcm + peckdist$entcm

peckdist$peckleaf <-  peckdist$abspeckht - peckdist$leafcm

# Let's make .csvs out of all these different objects.
# We can have them pull into other scripts, and then we only need this script
# if we want to pull in a new query from the database flexibly.

write.csv(x=entleafonly, file="Data/entleafonly.csv")
write.csv(x=peckcountsonly, file="Data/peckcountsonly.csv")
write.csv(x=peckdist, file="Data/peckdist.csv")


