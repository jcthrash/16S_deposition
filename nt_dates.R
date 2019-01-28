# R script to grab deposition dates of 16S sequences for cultivated prokaryotes in the NCBI nucleotide
# database and convert them into a plot.

# See https://ropensci.org/tutorials/rentrez_tutorial/

# Clear existing settings and data
rm(list=ls()) 

# Load required packages
library(rentrez)
library(ggplot2)
library(grid)

# Testing search terms
entrez_search(db="nucleotide", term="(16S rRNA[TITL] OR 16S SSU[TITL] OR 16S ribosomal RNA[TITL]) NOT (uncultured bacterium[ORGN] OR environmental samples[ORGN] OR uncultured archaeon[ORGN] OR uncultured[TITL] OR Eukaryota[ORGN] OR clone[TITL] OR similar[TITL] OR phytoplasma[TITL])")

# Search for seqeunces deposited in the nucleotide database by year
search_year <- function(year, term){
  query <- paste(term, "AND (", year, "[PDAT])")
  entrez_search(db="nucleotide", term=query, retmax=0)$count
}
year <- 1982:2018
seqs <- sapply(year, search_year, term="(16S rRNA[TITL] OR 16S SSU[TITL] OR 16S ribosomal RNA[TITL]) NOT (uncultured bacterium[ORGN] OR environmental samples[ORGN] OR uncultured archaeon[ORGN] OR uncultured[TITL] OR Eukaryota[ORGN] OR clone[TITL] OR similar[TITL] OR phytoplasma[TITL])", USE.NAMES=FALSE)

# Combine these into a dataframe for plotting in ggplot2
df <- do.call(rbind, Map(data.frame, year=year, seqs=seqs))

# Compute the cumulative sequencews in a third column
df$sums <- cumsum(df$seqs)

# Create the first plot of deposited sequences by year
p1 <- ggplot() + 
  geom_point(data=df, aes(x=year, y=seqs), size=3, color="black", alpha=0.7) +
  geom_smooth(data=df, aes(x=year, y=seqs), formula = y ~ x, color="light blue", alpha=0.2) +
  labs(x= "Year", y = "Sequences deposited") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_y_continuous(labels=scales::comma) +
  #theme_bw() + 
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90)) 
  #geom_smooth(method = "lm", formula = y ~ splines::bs(x, 5), se = TRUE, color="light blue",
  #            aes(x=year, y=seqs), alpha = 0.2)
  
# Create a secondary plot with cumulative sequences by year
p2 <- ggplot() +
  geom_line(data=df, aes(x=year, y=sums)) +
  #theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels=scales::comma) +
  labs(y = "Cumulative") +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(angle = 90))

# Generate an inset location for the second graph
vp <- viewport(width = 0.4, height = 0.4, x = 0.2, y = 0.95, just=c("left","top"))

# Plot both graphs, assigning the second one to the inset location
print(p1)
print(p2, vp=vp)

