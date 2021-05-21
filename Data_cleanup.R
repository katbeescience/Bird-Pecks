# This script cleans up the data for use in later steps.
# Kathryn Busby
# mkbusby@email.arizona.edu
# May 20, 2021

# Load packages

library(tidyverse)

# Bring in data file

bp <- read.csv(file="Data/Entrances.csv")
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
# use them. Remove those rows with NAs in either of those two columns.

tidybp <- tidybp %>%
  drop_na(leafmm, entmm)
head(tidybp)

# Transform entrance and leaf mm measurements to cm.

tidybp$leafcm <- tidybp$leafmm / 10
tidybp$entcm <- tidybp$entmm / 10

# Make a column for entrance height relative to leaf.

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

# Make an object that removes peck data for clarity.

entleafonly <- tidybp %>%
  select(nestcode, site, leafcm, entcm, AB)
head(entleafonly)

# Create a similar object where we can use peck counts,
# including entrance height and leaf height.

peckcountsonly <- tidybp %>%
  drop_na(peck.count) %>%
  select(nestcode, site, leafcm, entcm, entleaf, peck.count, AB)
head(peckcountsonly)

# Put the peck measurements into long format, but that will
# generate NAs. Start with no NAs in entmm, leafmm, or peck.count.
# Drop notes column.

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

# To compare peck locations relative to leaf height:
# First, we have peck locations labeled "Up" and others labeled "Dn".
# To use math with them, we'll need all the "Dn" ones to be negative numbers.

downindices <- grep("BirdPredDn.mm.", peckdist$peck.ent)
peckdist$peckentmm[downindices] <- peckdist$peckentmm[downindices]*(-1)

# Transform mm peck distances to cm.

peckdist$peckentcm <- peckdist$peckentmm / 10

# For peck distance analyses, don't keep rows with no pecks.
# We also don't need the up/down labels anymore.
# There will be NAs in the peck locations left over from when
# we pivoted longer. Get rid of those too.

peckdist <- peckdist %>%
  select(-c(peck.ent, leafmm, entmm, peckentmm)) %>%
  filter(peck.count != 0) %>%
  drop_na(peckentcm)

# This object needs another column to show where the peck is relative to the
# leaf height. This is calculated peck height minus leaf height.

peckdist$abspeckht <- peckdist$peckentcm + peckdist$entcm

peckdist$peckleaf <-  peckdist$abspeckht - peckdist$leafcm

# Let's make .csvs out of all these different objects.
# They will be pulled into other scripts.

write.csv(x=entleafonly, file="Data/entleafonly.csv")
write.csv(x=peckcountsonly, file="Data/peckcountsonly.csv")
write.csv(x=peckdist, file="Data/peckdist.csv")


