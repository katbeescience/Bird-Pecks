# Figure 6: Peck distribution boxplot.
# Kathryn Busby
# mkbusby@email.arizona.edu
# May 20, 2021

# Load libraries:

library(ggplot2)

# Bring in file:

peckdist <- read.csv("Data/peckdist.csv")

# Vertical scatterplot showing distributions of pecks binned above and below
# leaf height as points.

overall.peck.lf.dist <- ggplot(peckdist, aes(y=peckleaf, x=2)) +
  geom_jitter(width=.025, height=0, cex=35, shape="'") +
  scale_x_discrete(labs("")) +
  scale_y_continuous(labs(y="Peck Height Relative to Leaf Height (cm)"),
                     limits = c(-100, 100)) +
  #geom_hline(yintercept = 0, linetype="dashed", color="red") +
  theme_classic(base_size=50)
overall.peck.lf.dist

#===========================================================================================================


# Boxplot of peck heights relative to leaf heigth, similar to how it was depicted in the previous figure for Q1.

overall.peck.lf.boxplot <- ggplot(peckdist, aes(y=peckleaf, size=5)) +
  geom_boxplot(width=.2, outlier.size=10, fill="white") +
  guides(fill=FALSE, size=FALSE) +
  scale_x_discrete(labs("")) +
  scale_y_continuous(labs(y="Peck Height Relative to Leaf Height (cm)"),
                     limits = c(-100, 100)) +
  geom_hline(yintercept = 0, linetype="dashed", color="black", size=5) +
  theme_classic(base_size=50)
overall.peck.lf.boxplot

# Save the plots.

ggsave(plot=overall.peck.lf.boxplot,
       filename=paste0("Output/",Sys.Date(),"_peckleafboxplot.png"),
       height=25,
       width=20,
       units="in",
       dpi=300)

ggsave(plot=overall.peck.lf.dist,
       filename=paste0("Output/",Sys.Date(),"_peckleafdist.png"),
       height=25,
       width=20,
       units="in",
       dpi=300)
