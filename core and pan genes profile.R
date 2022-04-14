library(dplyr)
library(ggplot2)


#import datasets
df_core <- read.csv("../Number_of_Conserved_Genes.csv", header = TRUE)
df_pan <- read.csv("../Number_of_Genes_in_Pan_Geneome.csv", header = TRUE)
df_core_pan <- read.csv("../Number_of_Genes_in_Core and Pan_Geneome.csv", header = TRUE)

df <- melt(df_core_pan, id.vars = 'genomes', variable.name = 'series')


#calculate mean, SD, SE and 95% CI per timepoint for each group
options(dplyr.summarise.inform = FALSE)
stats_core_pan <- df %>%
  group_by(genomes, series) %>%
  summarise(
    count = n(),
    mean_genome = mean(value,na.rm=TRUE),
    sd_genome = sd(value, na.rm=TRUE),
    se_genome = sd_genome/sqrt(count),
    ci95lower = mean_genome - se_genome*1.96,
    ci95upper = mean_genome + se_genome*1.96
    
)

#mean profiles in the same graphs
mp <- ggplot(stats_core_pan, aes(x=genomes, y=mean_genome, color=series, shape=series)) + 
  geom_line() +
  geom_point(size = 2.5) + 
  labs(color="genomes", x="Genome Number", y = "Gene Cluster Number") +
  scale_x_continuous(breaks=seq(1,10,1), limits = c(1,5)) +
  scale_shape_manual(values=c(15,16)) +
  guides(shape='none') + 
  theme_bw()

mp

#Adding errorbars (95% confidence interval) to the mean profile
mp + geom_errorbar(aes(ymin = ci95lower, ymax = ci95upper), width = 0.5)
