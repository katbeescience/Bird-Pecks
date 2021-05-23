# Figure 7: Pecked versus unpecked entrance heights.
# Kathryn Busby
# mkbusby@email.arizona.edu
# May 20, 2021

# Load libraries

library(ggplot2)

# Bring in the file

peckcountsonly <- read.csv("Data/peckcountsonly.csv")

# First we need separate objects for the pecked and unpecked stalk info.

peckcountsonly$peck.binary <- ifelse((peckcountsonly$peck.count==0),
                                     "Pecked Stalks",
                                     "Unpecked Stalks")

# Make a boxplot comparing the median entrance heights of stalks with and
# without pecks

pecknopeckboxplot <- ggplot(peckcountsonly, aes(x=peck.binary, y=entleaf,
                                                group=peck.binary,
                                                fill=peck.binary)) +
  geom_boxplot(outlier.size=10) +
  guides(fill=FALSE, size=FALSE) +
  scale_y_continuous(labs(y="Entrance height (cm)"),
                          limits=c(-100, 50)) +
  scale_fill_manual(values=c("goldenrod2","forestgreen")) +
  annotate("text",
           x=2.4,y=40,
           label = paste0("n=",nrow(peckcountsonly)),
           cex=20) +
  xlab("") +
  theme_classic(base_size = 50)

pecknopeckboxplot

# Save the plot

ggsave(plot=pecknopeckboxplot,
       filename = paste0("Output/",Sys.Date(),"_peckentboxplot.png"),
       height=30,
       width=20,
       units="in",
       dpi=300)


