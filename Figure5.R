# Figure 5: Entrance height boxplot.
# Kathryn Busby
# mkbusby@email.arizona.edu
# May 20, 2021

# Load libraries

library(ggplot2)

# Bring in file

peckcountsonly <- read.csv(file="Data/peckcountsonly.csv")

# A figure for this result

ent.lf.boxplot <- ggplot(peckcountsonly, aes(y=entleaf, size=5)) +
  geom_boxplot(width=.2, outlier.size=10, fill="white") +
  guides(size=FALSE, fill=FALSE) +
  scale_x_discrete(labs("")) +
  scale_y_continuous(labs(y="Entrance Height Relative to Leaf Height (cm)"),
                     limits = c(-100, 60)) +
  geom_hline(yintercept = 0, linetype="dashed", color="black", size=5) +
  theme_classic(base_size = 50)

ent.lf.boxplot

# Save the figure to a file.

ggsave(plot=ent.lf.boxplot,
       filename=paste0("Output/",Sys.Date(),"_entleafboxplot.png"),
       height=25,
       width=20,
       units="in",
       dpi=300)


