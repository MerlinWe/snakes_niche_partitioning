################################################################
########## Niche partitioning Naja naja & Ptyas mucosa #########
################################################################

# ----- Preparation -----
library(tidyverse) # data manipulation
library(data.table) # data manipulation
library(Hmisc) # data manipulation
library(reshape2) # data manipulation
library(spaa) # metrics 
library(sp) # mapping
library(raster) # mapping
library(GISTools) # mapping
library(maptools) # mapping
library(sf) # mapping
library(rgdal) # mapping
library(rgeos) # mapping
library(dismo) # mapping
library(geosphere) # mapping
library(ggmap) # mapping
library(rnaturalearth) # mapping
library(rnaturalearthdata) # mapping
library(ggspatial) # mapping
library(RColorBrewer) # aesthetics
library(scales) # aesthetics
library(ggpubr) # plot arrangement
library(grid) # plot arrangement
library(gridExtra) # plot arrangement
library(extrafont)  # fonts

options(scipen = 999) # disable scientific notation 
setwd() # set working directory

dat.naja <- read.csv("Naja.csv", na.strings = c("-", ""), header = TRUE) # Import Naja naja data
dat.ptyas <- read.csv("Ptyas.csv", na.strings = c("-", ""), header = TRUE)  # Import Ptyas mucosa data 

dat.naja$ID <- "Naja" # add snake ID
dat.ptyas$ID <- "Ptyas" # add snake ID

names <- c("Prey", "Group", "Class", "Order", "Family", "Genus", "Species", "Success", "Size", "DOI", "Observer",
           "Location", "State", "Biozone", "Lat", "Long", "Source", "Notes", "Link", "ID") 

colnames(dat.naja) <- names
colnames(dat.ptyas) <- names 

dat.all <- rbind(dat.naja, dat.ptyas) # Generate combined database 

rm(names)

########## Descriptive statistics ##########

# >>>>>>>>>> Source descriptives <<<<<<<<<

dat.source <- dat.all[, c("ID", "Source")] # Group source classes 
dat.source <- mutate(dat.source, 
                     Source = ifelse(grepl("Facebook|Instagram|YouTube|Youtube|Flickr", Source), "Social Media", 
                                     ifelse(grepl("Google", Source), "Search Engines",
                                            ifelse(grepl("This study|pers. comm.|Whatsapp|Pers. Observation|Pers. Obs.", Source), "Pers. Obs.",
                                                   ifelse(grepl("India Nature Watch|iNaturalist|Reptiles of India", Source), "Citizen Science",
                                                          "Literature")))))
dat.all <- mutate(dat.all, Source = ifelse(grepl("Facebook|Instagram|YouTube|Youtube|Flickr", Source), "Social Media", 
                                             ifelse(grepl("Google", Source), "Search Engines",
                                                    ifelse(grepl("This study|pers. comm.|Whatsapp|Pers. Observation|Pers. Obs.", Source), "Pers. Obs.",
                                                           ifelse(grepl("India Nature Watch|iNaturalist|Reptiles of India", Source), "Citizen Science",
                                                                  "Literature")))))

dat.source <- dat.source %>% # Count records per Snake species per source class
  dplyr::group_by(Source, ID) %>%
  dplyr::summarise(Count = n()) %>%
  arrange(ID)

naja_perc <- subset(dat.source, ID == "Naja")$Count / ## Calculate percentages
  sum(subset(dat.source, ID == "Naja")$Count) * 100 
naja_perc <- round(naja_perc, digits = 1)
naja_perc <- paste(naja_perc,"%")

ptyas_perc <- subset(dat.source, ID == "Ptyas")$Count / ## Calculate percentages
  sum(subset(dat.source, ID == "Ptyas")$Count) * 100 
ptyas_perc <- round(ptyas_perc, digits = 1)
ptyas_perc <- paste(ptyas_perc,"%")

dat.source$Perc <- c(naja_perc, ptyas_perc) # save in df 
rec_num <- dat.source$Perc # save numbers for labeling

n_naja  <- nrow(dat.naja)  # get sample size as value 
n_ptyas <- nrow(dat.ptyas) # get sample size as value 

source_plot <- ggplot(dat.source, aes(x=Source, y=Count, fill = ID))+
  geom_bar(position ="dodge", stat="identity", colour = "black") +
  theme_bw()+
  xlab("")+
  ylab("Number of records")+
  theme(text = element_text(size=11),
        legend.key = element_rect(fill = NA , color = "transparent"), legend.position = c(0.15,0.963),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill=alpha('transparent', 0.0)))+
  scale_fill_grey(labels = c(paste0("Naja naja (n =",n_naja,")"), paste0("Ptyas mucosa (n =", n_ptyas, ")")), start = 0.5, end = 1)+
  geom_text(aes(label = rec_num), 
            position = position_dodge(1), 
            color="black",vjust = -0.6,
            size = 3.5)+
  labs(fill = "") 

ggsave("source_plot.jpeg", plot = source_plot, path = "/Users/serpent/OneDrive - Van Hall Larenstein/Niche Partioning/Plots",
       width = 7, height = 6, dpi = 700, device = "jpeg")

sources <- dat.source
rm(source_plot, naja_perc, ptyas_perc, rec_num) # clean gl. env. 

# >>>>>>>>>> Clean & Prepare data for quantitative analysis <<<<<<<<<<<

## Clean out duplicated records due to observes sharing 
## the same record on multiple platforms 

dat.all$Notes[is.na(dat.all$Notes)] <- "None" # define empty notes
dat.distinct <- subset(dat.all, Notes != "Squamatabase" & Observer != "Yatin Kalki") # exclude exceptions
add <- subset(dat.all, Notes == "Squamatabase" | Observer == "Yatin Kalki") # save exceptions
add <- dplyr::select(add, -c(Source, Notes, Link)) # define columns
dat.distinct <- dplyr::select(dat.distinct, -c(Source, Notes, Link)) # define columns
dat.distinct <- distinct(dat.distinct) # exclude duplicated records
dat.distinct <- rbind(dat.distinct, add) # add exceptions

dat.distinct <- subset(dat.distinct, Group != "Testidines") # remove the soft shell turtle 

dat.naja <- subset(dat.distinct, ID == "Naja") # make sure to continue with distinct records
dat.ptyas <- subset(dat.distinct, ID == "Ptyas") # make sure to continue with distinct records

n_naja  <- nrow(dat.naja)  # get sample size as value 
n_ptyas <- nrow(dat.ptyas) # get sample size as value 

# ~~~~~ Count prey classes per species ~~~~~ 

dat.prey <- dat.distinct %>%        # use distinct records
  dplyr::group_by(Group, ID) %>%    # group by ID and prey group 
  dplyr::summarise(Count = n()) %>% # Summarize respective counts 
  dplyr::arrange(ID)                # Arrange by ID 

# Calculate Naja percentages
naja_perc <- subset(dat.prey, ID == "Naja")$Count / 
  sum(subset(dat.prey, ID == "Naja")$Count) * 100 
naja_perc <- round(naja_perc, digits = 1)
naja_perc <- paste0(naja_perc,"%")

## Calculate Ptyas percentages
ptyas_perc <- subset(dat.prey, ID == "Ptyas")$Count /
  sum(subset(dat.prey, ID == "Ptyas")$Count) * 100 
ptyas_perc <- round(ptyas_perc, digits = 1)
ptyas_perc <- paste0(ptyas_perc,"%")

# Save values 
dat.prey$Perc <- c(naja_perc, ptyas_perc) # save in df 
prey_num <- dat.prey$Perc # save numbers for labeling

# Build global prey plot
prey_plot <- ggplot(dat.prey, aes(x=Group, y=Count, fill = ID))+
  geom_bar(position ="dodge", stat="identity", colour = "black", alpha = .8) +
  theme_bw()+
  xlab("")+
  ylab("Number of records")+
  theme(text = element_text(size=11),
        legend.key = element_rect(fill = NA , color = "transparent"), legend.position = c(0.5,0.963),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill=alpha('transparent', 0.0)))+
  scale_fill_grey(labels = c(paste0("Naja naja (n =",n_naja,")"), paste0("Ptyas mucosa (n =", n_ptyas, ")")), start = 0.5, end = 1)+
  geom_text(aes(label = prey_num), 
            position = position_dodge(1), 
            color="black",vjust = -0.6,
            size = 3.5)+
  labs(fill = "") 

# Export plot
#ggsave("prey_plot.jpeg", plot = prey_plot, path = "/Users/serpent/OneDrive - Van Hall Larenstein/Niche Partioning/Plots",
#       width = 7, height = 6, dpi = 700, device = "jpeg")

class.prey <- dat.prey # rename prey df on class level 
rm(prey_num, naja_perc, ptyas_perc, dat.source, prey_plot, dat.prey, add)

############### Calculating niche metrics ###############

## ~~~ Calculate niche metrics on four taxonomic levels ~~~

# Levels of prey identification (LPI): 
# (1) Ordinal level (Order)
# (2) Family level (Family)
# (3) Generic level (Genus)
# (4) Species level (Species)

## ~~~ Niche width of Naja naja ~~~ 

setDT(dat.naja) # set naja data as data table 

## Prey items with an Order LPI  - Naja naja
naja_ord <- na.omit(dat.naja, cols = "Order") # remove prey items NOT identified on Order Level 
ord_perc <- nrow(naja_ord)/nrow(dat.naja)*100 # check percentage of records on order LPI 
ord_samp <- nrow(naja_ord) # save sample size on ordinal level
# Calculate prey proportions & counts on order level 
naja_ord <- naja_ord %>%            # use ordinal records 
  dplyr::group_by(Order) %>%        # group by order
  dplyr::summarise(Ord_Count = n()) # Count prey items 
naja_ord$Ord_Proportion <- naja_ord$Ord_Count / # Calculate proportion
  sum(naja_ord$Ord_Count)
colnames(naja_ord) <- c("Prey","Ord_Count","Ord_Proportion") # set column names 


## Prey items with a Family LPI  - Naja naja
naja_fam <- na.omit(dat.naja, cols = "Family") # remove prey items NOT identified on Family Level 
fam_perc <- nrow(naja_fam)/nrow(dat.naja)*100 # check percentage of records on order LPI 
fam_samp <- nrow(naja_fam) # save sample size on family level
# Calculate prey proportions & counts on family level 
naja_fam <- naja_fam %>%             # use family records 
  dplyr::group_by(Family) %>%        # group by family
  dplyr::summarise(Fam_Count = n())  # Count prey items 
naja_fam$Fam_Proportion <- naja_fam$Fam_Count / # Calculate proportion
  sum(naja_fam$Fam_Count)
colnames(naja_fam) <- c("Prey","Fam_Count","Fam_Proportion") # set column names 


## Prey items with a Genus LPI  - Naja naja
naja_gen <- na.omit(dat.naja, cols = "Genus") # remove prey items NOT identified on Genus Level 
gen_perc <- nrow(naja_gen)/nrow(dat.naja)*100 # check percentage of records on order LPI 
gen_samp <- nrow(naja_gen) # save sample size on generic level
# Calculate prey proportions & counts on genus level 
naja_gen <- naja_gen %>%             # use genus records 
  dplyr::group_by(Genus) %>%         # group by genus
  dplyr::summarise(Gen_Count = n())  # Count prey items 
naja_gen$Gen_Proportion <- naja_gen$Gen_Count / # Calculate proportion
  sum(naja_gen$Gen_Count)
colnames(naja_gen) <- c("Prey","Gen_Count","Gen_Proportion") # set column names 


## Prey items with a Species LPI  - Naja naja
naja_spe <- na.omit(dat.naja, cols = "Species") # remove prey items NOT identified on Species Level 
spe_perc <- nrow(naja_spe)/nrow(dat.naja)*100 # check percentage of records on order LPI 
spe_samp <- nrow(naja_spe) # save sample size on species level
# Calculate prey proportions & counts on species level 
naja_spe <- naja_spe %>%             # use species records 
  dplyr::group_by(Species) %>%       # group by genus
  dplyr::summarise(Spe_Count = n())  # Count prey items 
naja_spe$Spe_Proportion <- naja_spe$Spe_Count / # Calculate proportion
  sum(naja_spe$Spe_Count)
colnames(naja_spe) <- c("Prey","Spe_Count","Spe_Proportion") # set column names 

# ~~~ Calculate Levines index for niche width ~~~ 

# Prepare input data 
naja_width <- Merge(naja_ord, naja_fam, naja_gen, naja_spe,
               all = TRUE, id = ~ Prey) # merge data frames of different LPI 
naja_width[is.na(naja_width)] <- 0 # replace missing values with 0 
naja_width <- naja_width[, c("Ord_Proportion", # keep relevant columns 
                             "Fam_Proportion", "Gen_Proportion", "Spe_Proportion")] 
# Calculate index values
naja_width <- niche.width(naja_width, method = "levins")

# Standardize index values
ord_width <- (naja_width$Ord_Proportion-1)/(nrow(naja_ord)-1) # order LPI
fam_width <- (naja_width$Fam_Proportion-1)/(nrow(naja_fam)-1) # Family LPI
gen_width <- (naja_width$Gen_Proportion-1)/(nrow(naja_gen)-1) # Genus LPI
spe_width <- (naja_width$Spe_Proportion-1)/(nrow(naja_spe)-1) # Species LPI

# Merge data 
naja_width  <- round(naja_width, digits = 2)           # round values 
B_a   <- c(ord_width, fam_width, gen_width, spe_width) # standardized values
B_a   <- round(B_a, digits = 2)                        # round values
percs <- c(ord_perc, fam_perc, gen_perc, spe_perc)     # percentage of records 
percs <- round(percs, digits = 2)                      # round percentages
n <- c(ord_samp, fam_samp, gen_samp, spe_samp)         # sample sizes

# Build dataset 
levin_naja <- rbind(naja_width, B_a, percs, n)
rownames(levin_naja) <- c("Naja.B", "Naja.B_a", "Naja.Percentage", "Naja.Samplesize")
colnames(levin_naja) <- c("Order", "Family", "Genus", "Species")

# Standard deviation of std. Levins index 
naja_sd <- sd(levin_naja["Naja.B_a", ])

# Clean Gl. Env.
rm(naja_ord, naja_fam, naja_gen, naja_spe, 
   naja_width, B_a, percs, n,
   ord_perc, fam_perc, gen_perc, spe_perc,
   ord_samp, fam_samp, gen_samp, spe_samp,
   ord_width, fam_width, gen_width, spe_width)


## ~~~ Niche width of Ptyas mucosa ~~~ 

setDT(dat.ptyas) # set ptyas data as data table 

## Prey items with an Order LPI  - Ptyas mucosa
ptyas_ord <- na.omit(dat.ptyas, cols = "Order") # remove prey items NOT identified on Order Level 
ord_perc <- nrow(ptyas_ord)/nrow(dat.ptyas)*100 # check percentage of records on order LPI 
ord_samp <- nrow(ptyas_ord) # save sample size on ordinal level
# Calculate prey proportions & counts on order level 
ptyas_ord <- ptyas_ord %>%            # use ordinal records 
  dplyr::group_by(Order) %>%          # group by order
  dplyr::summarise(Ord_Count = n())   # Count prey items 
ptyas_ord$Ord_Proportion <- ptyas_ord$Ord_Count / # Calculate proportion
  sum(ptyas_ord$Ord_Count)
colnames(ptyas_ord) <- c("Prey","Ord_Count","Ord_Proportion") # set column names 


## Prey items with a Family LPI  - Ptyas mucosa
ptyas_fam <- na.omit(dat.ptyas, cols = "Family") # remove prey items NOT identified on Family Level 
fam_perc <- nrow(ptyas_fam)/nrow(dat.ptyas)*100 # check percentage of records on order LPI 
fam_samp <- nrow(ptyas_fam) # save sample size on family level
# Calculate prey proportions & counts on family level 
ptyas_fam <- ptyas_fam %>%             # use family records 
  dplyr::group_by(Family) %>%          # group by family
  dplyr::summarise(Fam_Count = n())    # Count prey items 
ptyas_fam$Fam_Proportion <- ptyas_fam$Fam_Count / # Calculate proportion
  sum(ptyas_fam$Fam_Count)
colnames(ptyas_fam) <- c("Prey","Fam_Count","Fam_Proportion") # set column names 


## Prey items with a Genus LPI  - Ptyas mucosa
ptyas_gen <- na.omit(dat.ptyas, cols = "Genus") # remove prey items NOT identified on Genus Level 
gen_perc <- nrow(ptyas_gen)/nrow(dat.ptyas)*100 # check percentage of records on order LPI 
gen_samp <- nrow(ptyas_gen) # save sample size on generic level
# Calculate prey proportions & counts on genus level 
ptyas_gen <- ptyas_gen %>%             # use genus records 
  dplyr::group_by(Genus) %>%           # group by genus
  dplyr::summarise(Gen_Count = n())    # Count prey items 
ptyas_gen$Gen_Proportion <- ptyas_gen$Gen_Count / # Calculate proportion
  sum(ptyas_gen$Gen_Count)
colnames(ptyas_gen) <- c("Prey","Gen_Count","Gen_Proportion") # set column names 


## Prey items with a Species LPI  - Ptyas mucosa
ptyas_spe <- na.omit(dat.ptyas, cols = "Species") # remove prey items NOT identified on Species Level 
spe_perc <- nrow(ptyas_spe)/nrow(dat.ptyas)*100 # check percentage of records on order LPI 
spe_samp <- nrow(ptyas_spe) # save sample size on species level
# Calculate prey proportions & counts on species level 
ptyas_spe <- ptyas_spe %>%             # use species records 
  dplyr::group_by(Species) %>%         # group by genus
  dplyr::summarise(Spe_Count = n())    # Count prey items 
ptyas_spe$Spe_Proportion <- ptyas_spe$Spe_Count / # Calculate proportion
  sum(ptyas_spe$Spe_Count)
colnames(ptyas_spe) <- c("Prey","Spe_Count","Spe_Proportion") # set column names 

# ~~~ Calculate Levines index for niche width ~~~ 

# Prepare input data 
ptyas_width <- Merge(ptyas_ord, ptyas_fam, ptyas_gen, ptyas_spe,
                    all = TRUE, id = ~ Prey) # merge data frames of different LPI 
ptyas_width[is.na(ptyas_width)] <- 0 # replace missing values with 0 
ptyas_width <- ptyas_width[, c("Ord_Proportion", # keep relevant columns 
                             "Fam_Proportion", "Gen_Proportion", "Spe_Proportion")] 
# Calculate index values
ptyas_width <- niche.width(ptyas_width, method = "levins")

# Standardize index values
ord_width <- (ptyas_width$Ord_Proportion-1)/(nrow(ptyas_ord)-1) # order LPI
fam_width <- (ptyas_width$Fam_Proportion-1)/(nrow(ptyas_fam)-1) # Family LPI
gen_width <- (ptyas_width$Gen_Proportion-1)/(nrow(ptyas_gen)-1) # Genus LPI
spe_width <- (ptyas_width$Spe_Proportion-1)/(nrow(ptyas_spe)-1) # Species LPI

# Merge data 
ptyas_width  <- round(ptyas_width, digits = 2)         # round values 
B_a   <- c(ord_width, fam_width, gen_width, spe_width) # standardized values
B_a   <- round(B_a, digits = 2)                        # round values
percs <- c(ord_perc, fam_perc, gen_perc, spe_perc)     # percentage of records 
percs <- round(percs, digits = 2)                      # round percentages
n <- c(ord_samp, fam_samp, gen_samp, spe_samp)         # sample sizes

# Build dataset 
levin_ptyas <- rbind(ptyas_width, B_a, percs, n)
rownames(levin_ptyas) <- c("Ptyas.B", "Ptyas.B_a", "Ptyas.Percentage", "Ptyas.Samplesize")
colnames(levin_ptyas) <- c("Order", "Family", "Genus", "Species")

# Standard deviation of std. Levins index
ptyas_sd <- sd(levin_ptyas["Ptyas.B_a", ])

# Clean Gl. Env.
rm(ptyas_ord, ptyas_fam, ptyas_gen, ptyas_spe, 
   ptyas_width, B_a, percs, n,
   ord_samp, fam_samp, gen_samp, spe_samp,
   ord_perc, fam_perc, gen_perc, spe_perc, 
   ord_width, fam_width, gen_width, spe_width)

# ~~~~~~~~~~ Piankas index for niche overlap ~~~~~~~~~~

# Define Pianka`s Measure 
pianka <- function(m=matrix(rpois(80,1),nrow=10)) {
  m <- m/rowSums(m)
  pairwise <- cbind(t(combn(nrow(m),2)),0)	# set up pairwise species list
  for (i in 1:nrow(pairwise)) 
    pairwise[i,3] <- sum(m[pairwise[i,1],]*m[pairwise[i,2],])/
    sqrt(sum(m[pairwise[i,1],]^2)*sum(m[pairwise[i,2],]^2))
  return(mean(pairwise[,3]))
}

# Count overlapping predations per taxonomic level
ord_overlap <- data.frame(unclass(table(dat.distinct$ID, dat.distinct$Order)))   # Order
fam_overlap <- data.frame(unclass(table(dat.distinct$ID, dat.distinct$Family)))  # Family
gen_overlap <- data.frame(unclass(table(dat.distinct$ID, dat.distinct$Genus)))   # Genus
spe_overlap <- data.frame(unclass(table(dat.distinct$ID, dat.distinct$Species))) # Species

# Calculate Piankas index per taxonomic level 
ord_pianka <- pianka(ord_overlap) 
fam_pianka <- pianka(fam_overlap) 
gen_pianka <- pianka(gen_overlap) 
spe_pianka <- pianka(spe_overlap) 

pianka <- as.data.frame( # merge Pianaka values 
  cbind(ord_pianka, fam_pianka, gen_pianka, spe_pianka))
colnames(pianka) <- c("Order", "Family", "Genus", "Species")
rownames(pianka) <- "Overlap"

# Standard deviation Piankas index 
pianka_sd <- sd(pianka["Overlap", ])

# Conclude metrics
metrics <- rbind(levin_naja, levin_ptyas, pianka)
metrics <- round(metrics, digits = 2)
global_metrics <- metrics # re-store item 

rm(ord_overlap, fam_overlap, gen_overlap, spe_overlap,
   ord_pianka, fam_pianka, gen_pianka, spe_pianka,
   levin_naja, levin_ptyas, metrics) # clean Gl. Env.

# >>>>> Dietary descriptives & Metrics of Naja naja at different sizes <<<<<

# Get sized subsets 
naja_large  <- subset(dat.naja, Size == "L")
naja_medium <- subset(dat.naja, Size == "M")
naja_small  <- subset(dat.naja, Size == "S")

# ~~~~~ Large Naja naja ~~~~~

setDT(naja_large) # set large naja data as data table 

large_ord <- na.omit(naja_large, cols = "Order") # Order
ord_perc <- nrow(large_ord)/nrow(naja_large)*100 # Percentage
ord_samp <- nrow(large_ord) # sample size 
large_ord <- large_ord %>%      
  dplyr::group_by(Order) %>%        
  dplyr::summarise(Ord_Count = n()) 
large_ord$Ord_Proportion <- large_ord$Ord_Count / 
  sum(large_ord$Ord_Count)
colnames(large_ord) <- c("Prey","Ord_Count","Ord_Proportion")

large_fam <- na.omit(naja_large, cols = "Family") # Family
fam_perc <- nrow(large_fam)/nrow(naja_large)*100 # Percentage
fam_samp <- nrow(large_fam) # sample size 
large_fam <- large_fam %>%      
  dplyr::group_by(Family) %>%        
  dplyr::summarise(Fam_Count = n()) 
large_fam$Fam_Proportion <- large_fam$Fam_Count / 
  sum(large_fam$Fam_Count)
colnames(large_fam) <- c("Prey","Fam_Count","Fam_Proportion")

large_gen <- na.omit(naja_large, cols = "Genus") # Genus
gen_perc <- nrow(large_gen)/nrow(naja_large)*100 # Percentage
gen_samp <- nrow(large_gen) # sample size
large_gen <- large_gen %>%      
  dplyr::group_by(Genus) %>%        
  dplyr::summarise(Gen_Count = n()) 
large_gen$Gen_Proportion <- large_gen$Gen_Count / 
  sum(large_gen$Gen_Count)
colnames(large_gen) <- c("Prey","Gen_Count","Gen_Proportion")

large_spe <- na.omit(naja_large, cols = "Species") # Species
spe_perc <- nrow(large_spe)/nrow(naja_large)*100 # Percentage
spe_samp <- nrow(large_spe)
large_spe <- large_spe %>%      
  dplyr::group_by(Species) %>%        
  dplyr::summarise(Spe_Count = n()) 
large_spe$Spe_Proportion <- large_spe$Spe_Count / 
  sum(large_spe$Spe_Count)
colnames(large_spe) <- c("Prey","Spe_Count","Spe_Proportion")

large_width <- Merge(large_ord, large_fam, large_gen, large_spe,all = TRUE, id = ~ Prey) 
large_width[is.na(large_width)] <- 0 
large_width <- large_width[, c("Ord_Proportion", "Fam_Proportion", "Gen_Proportion", "Spe_Proportion")] 
large_width <- niche.width(large_width, method = "levins")

ord_width <- (large_width$Ord_Proportion-1)/(nrow(large_ord)-1) # Order LPI
fam_width <- (large_width$Fam_Proportion-1)/(nrow(large_fam)-1) # Family LPI
gen_width <- (large_width$Gen_Proportion-1)/(nrow(large_gen)-1) # Genus LPI
spe_width <- (large_width$Spe_Proportion-1)/(nrow(large_spe)-1) # Species LPI

large_width  <- round(large_width, digits = 2)         # round values 
B_a   <- c(ord_width, fam_width, gen_width, spe_width) # standardized values
B_a   <- round(B_a, digits = 2)                        # round values
percs <- c(ord_perc, fam_perc, gen_perc, spe_perc)     # percentage of records 
percs <- round(percs, digits = 2)                      # round percentages
n <- c(ord_samp, fam_samp, gen_samp, spe_samp)         # sample sizes 

levin_large <- rbind(large_width, B_a, percs, n)
rownames(levin_large) <- c("Large.B", "Large.B_a", "Large.Percentage", "Large.Samplesize")
colnames(levin_large) <- c("Order", "Family", "Genus", "Species")

naja.large_sd <- sd(levin_large["Large.B_a", ]) # SD

# ~~~~~ Medium Naja naja ~~~~~

setDT(naja_medium) # set medium naja data as data table 

medium_ord <- na.omit(naja_medium, cols = "Order") # Order
ord_perc <- nrow(medium_ord)/nrow(naja_medium)*100 # Percentage
ord_samp <- nrow(medium_ord) # sample size 
medium_ord <- medium_ord %>%      
  dplyr::group_by(Order) %>%        
  dplyr::summarise(Ord_Count = n()) 
medium_ord$Ord_Proportion <- medium_ord$Ord_Count / 
  sum(medium_ord$Ord_Count)
colnames(medium_ord) <- c("Prey","Ord_Count","Ord_Proportion")

medium_fam <- na.omit(naja_medium, cols = "Family") # Family
fam_perc <- nrow(medium_fam)/nrow(naja_medium)*100 # Percentage
fam_samp <- nrow(medium_fam) # sample size 
medium_fam <- medium_fam %>%      
  dplyr::group_by(Family) %>%        
  dplyr::summarise(Fam_Count = n()) 
medium_fam$Fam_Proportion <- medium_fam$Fam_Count / 
  sum(medium_fam$Fam_Count)
colnames(medium_fam) <- c("Prey","Fam_Count","Fam_Proportion")

medium_gen <- na.omit(naja_medium, cols = "Genus") # Genus
gen_perc <- nrow(medium_gen)/nrow(naja_medium)*100 # Percentage
gen_samp <- nrow(medium_gen) # sample size 
medium_gen <- medium_gen %>%      
  dplyr::group_by(Genus) %>%        
  dplyr::summarise(Gen_Count = n()) 
medium_gen$Gen_Proportion <- medium_gen$Gen_Count / 
  sum(medium_gen$Gen_Count)
colnames(medium_gen) <- c("Prey","Gen_Count","Gen_Proportion")

medium_spe <- na.omit(naja_medium, cols = "Species") # Species
spe_perc <- nrow(medium_spe)/nrow(naja_medium)*100 # Percentage
spe_samp <- nrow(medium_spe) # sample size 
medium_spe <- medium_spe %>%      
  dplyr::group_by(Species) %>%        
  dplyr::summarise(Spe_Count = n()) 
medium_spe$Spe_Proportion <- medium_spe$Spe_Count / 
  sum(medium_spe$Spe_Count)
colnames(medium_spe) <- c("Prey","Spe_Count","Spe_Proportion")

medium_width <- Merge(medium_ord, medium_fam, medium_gen, medium_spe,all = TRUE, id = ~ Prey) 
medium_width[is.na(medium_width)] <- 0 
medium_width <- medium_width[, c("Ord_Proportion", "Fam_Proportion", "Gen_Proportion", "Spe_Proportion")] 
medium_width <- niche.width(medium_width, method = "levins")

ord_width <- (medium_width$Ord_Proportion-1)/(nrow(medium_ord)-1) # Order LPI
fam_width <- (medium_width$Fam_Proportion-1)/(nrow(medium_fam)-1) # Family LPI
gen_width <- (medium_width$Gen_Proportion-1)/(nrow(medium_gen)-1) # Genus LPI
spe_width <- (medium_width$Spe_Proportion-1)/(nrow(medium_spe)-1) # Species LPI

medium_width  <- round(medium_width, digits = 2)       # round values 
B_a   <- c(ord_width, fam_width, gen_width, spe_width) # standardized values
B_a   <- round(B_a, digits = 2)                        # round values
percs <- c(ord_perc, fam_perc, gen_perc, spe_perc)     # percentage of records 
percs <- round(percs, digits = 2)                      # round percentages
n <- c(ord_samp, fam_samp, gen_samp, spe_samp)         # sample sizes

levin_medium <- rbind(medium_width, B_a, percs, n)
rownames(levin_medium) <- c("Medium.B", "Medium.B_a", "Medium.Percentage", "Medium.Samplesize")
colnames(levin_medium) <- c("Order", "Family", "Genus", "Species")

naja.medium_sd <- sd(levin_medium["Medium.B_a", ]) # SD

# ~~~~~ Small Naja najas ~~~~~

setDT(naja_small) # set small naja data as data table 

small_ord <- na.omit(naja_small, cols = "Order") # Order
ord_perc <- nrow(small_ord)/nrow(naja_small)*100 # Percentage
ord_samp <- nrow(small_ord) # sample size 
small_ord <- small_ord %>%      
  dplyr::group_by(Order) %>%        
  dplyr::summarise(Ord_Count = n()) 
small_ord$Ord_Proportion <- small_ord$Ord_Count / 
  sum(small_ord$Ord_Count)
colnames(small_ord) <- c("Prey","Ord_Count","Ord_Proportion")

small_fam <- na.omit(naja_small, cols = "Family") # Family
fam_perc <- nrow(small_fam)/nrow(naja_small)*100 # Percentage
fam_samp <- nrow(small_fam) # sample size 
small_fam <- small_fam %>%      
  dplyr::group_by(Family) %>%        
  dplyr::summarise(Fam_Count = n()) 
small_fam$Fam_Proportion <- small_fam$Fam_Count / 
  sum(small_fam$Fam_Count)
colnames(small_fam) <- c("Prey","Fam_Count","Fam_Proportion")

small_gen <- na.omit(naja_small, cols = "Genus") # Genus
gen_perc <- nrow(small_gen)/nrow(naja_small)*100 # Percentage
gen_samp <- nrow(small_gen) # sample size 
small_gen <- small_gen %>%      
  dplyr::group_by(Genus) %>%        
  dplyr::summarise(Gen_Count = n()) 
small_gen$Gen_Proportion <- small_gen$Gen_Count / 
  sum(small_gen$Gen_Count)
colnames(small_gen) <- c("Prey","Gen_Count","Gen_Proportion")

small_spe <- na.omit(naja_small, cols = "Species") # Species
spe_perc <- nrow(small_spe)/nrow(naja_small)*100 # Percentage
spe_samp <- nrow(small_spe) # sample size 
small_spe <- small_spe %>%      
  dplyr::group_by(Species) %>%        
  dplyr::summarise(Spe_Count = n()) 
small_spe$Spe_Proportion <- small_spe$Spe_Count / 
  sum(small_spe$Spe_Count)
colnames(small_spe) <- c("Prey","Spe_Count","Spe_Proportion")

small_width <- Merge(small_ord, small_fam, small_gen, small_spe,all = TRUE, id = ~ Prey) 
small_width[is.na(small_width)] <- 0 
small_width <- small_width[, c("Ord_Proportion", "Fam_Proportion", "Gen_Proportion", "Spe_Proportion")] 
small_width <- niche.width(small_width, method = "levins")

ord_width <- (small_width$Ord_Proportion-1)/(nrow(small_ord)-1) # Order LPI
fam_width <- (small_width$Fam_Proportion-1)/(nrow(small_fam)-1) # Family LPI
gen_width <- (small_width$Gen_Proportion-1)/(nrow(small_gen)-1) # Genus LPI
spe_width <- (small_width$Spe_Proportion-1)/(nrow(small_spe)-1) # Species LPI

small_width  <- round(small_width, digits = 2)         # round values 
B_a   <- c(ord_width, fam_width, gen_width, spe_width) # standardized values
B_a   <- round(B_a, digits = 2)                        # round values
percs <- c(ord_perc, fam_perc, gen_perc, spe_perc)     # percentage of records 
percs <- round(percs, digits = 2)                      # round percentages
n <- c(ord_samp, fam_samp, gen_samp, spe_samp)         # sample sizes

levin_small <- rbind(small_width, B_a, percs, n)
rownames(levin_small) <- c("Small.B", "Small.B_a", "Small.Percentage", "Small-Samplesize")
colnames(levin_small) <- c("Order", "Family", "Genus", "Species")

naja.small_sd <- sd(levin_small["Small.B_a", ]) # SD

## Merge size subset naja metrics 
naja_metrics <- rbind(levin_large, levin_medium, levin_small) 
naja_metrics <- round(naja_metrics, digits = 2)

# >>>>> Dietary descriptives & Metrics of Ptyas mucosa at different sizes <<<<<

# Get sized subsets 
ptyas_large  <- subset(dat.ptyas, Size == "L")
ptyas_medium <- subset(dat.ptyas, Size == "M")
ptyas_small  <- subset(dat.ptyas, Size == "S")

# ~~~~~ Large Ptyas mucosa ~~~~~

setDT(ptyas_large) # set large ptyas data as data table 

large_ord <- na.omit(ptyas_large, cols = "Order") # Order
ord_perc <- nrow(large_ord)/nrow(ptyas_large)*100 # Percentage
ord_samp <- nrow(large_ord) # sample size 
large_ord <- large_ord %>%      
  dplyr::group_by(Order) %>%        
  dplyr::summarise(Ord_Count = n()) 
large_ord$Ord_Proportion <- large_ord$Ord_Count / 
  sum(large_ord$Ord_Count)
colnames(large_ord) <- c("Prey","Ord_Count","Ord_Proportion")

large_fam <- na.omit(ptyas_large, cols = "Family") # Family
fam_perc <- nrow(large_fam)/nrow(ptyas_large)*100 # Percentage
fam_samp <- nrow(large_fam) # sample size 
large_fam <- large_fam %>%      
  dplyr::group_by(Family) %>%        
  dplyr::summarise(Fam_Count = n()) 
large_fam$Fam_Proportion <- large_fam$Fam_Count / 
  sum(large_fam$Fam_Count)
colnames(large_fam) <- c("Prey","Fam_Count","Fam_Proportion")

large_gen <- na.omit(ptyas_large, cols = "Genus") # Genus
gen_perc <- nrow(large_gen)/nrow(ptyas_large)*100 # Percentage
gen_samp <- nrow(large_gen) # sample size
large_gen <- large_gen %>%      
  dplyr::group_by(Genus) %>%        
  dplyr::summarise(Gen_Count = n()) 
large_gen$Gen_Proportion <- large_gen$Gen_Count / 
  sum(large_gen$Gen_Count)
colnames(large_gen) <- c("Prey","Gen_Count","Gen_Proportion")

large_spe <- na.omit(ptyas_large, cols = "Species") # Species
spe_perc <- nrow(large_spe)/nrow(ptyas_large)*100 # Percentage
spe_samp <- nrow(large_spe)
large_spe <- large_spe %>%      
  dplyr::group_by(Species) %>%        
  dplyr::summarise(Spe_Count = n()) 
large_spe$Spe_Proportion <- large_spe$Spe_Count / 
  sum(large_spe$Spe_Count)
colnames(large_spe) <- c("Prey","Spe_Count","Spe_Proportion")

large_width <- Merge(large_ord, large_fam, large_gen, large_spe,all = TRUE, id = ~ Prey) 
large_width[is.na(large_width)] <- 0 
large_width <- large_width[, c("Ord_Proportion", "Fam_Proportion", "Gen_Proportion", "Spe_Proportion")] 
large_width <- niche.width(large_width, method = "levins")

ord_width <- (large_width$Ord_Proportion-1)/(nrow(large_ord)-1) # Order LPI
fam_width <- (large_width$Fam_Proportion-1)/(nrow(large_fam)-1) # Family LPI
gen_width <- (large_width$Gen_Proportion-1)/(nrow(large_gen)-1) # Genus LPI
spe_width <- (large_width$Spe_Proportion-1)/(nrow(large_spe)-1) # Species LPI

large_width  <- round(large_width, digits = 2)         # round values 
B_a   <- c(ord_width, fam_width, gen_width, spe_width) # standardized values
B_a   <- round(B_a, digits = 2)                        # round values
percs <- c(ord_perc, fam_perc, gen_perc, spe_perc)     # percentage of records 
percs <- round(percs, digits = 2)                      # round percentages
n <- c(ord_samp, fam_samp, gen_samp, spe_samp)         # sample sizes 

levin_large <- rbind(large_width, B_a, percs, n)
rownames(levin_large) <- c("Large.B", "Large.B_a", "Large.Percentage", "Large.Samplesize")
colnames(levin_large) <- c("Order", "Family", "Genus", "Species")

ptyas.large_sd <- sd(levin_large["Large.B_a", ]) # SD

# ~~~~~ Medium Ptyas mucosa ~~~~~

setDT(ptyas_medium) # set medium ptyas data as data table 

medium_ord <- na.omit(ptyas_medium, cols = "Order") # Order
ord_perc <- nrow(medium_ord)/nrow(ptyas_medium)*100 # Percentage
ord_samp <- nrow(medium_ord) # sample size 
medium_ord <- medium_ord %>%      
  dplyr::group_by(Order) %>%        
  dplyr::summarise(Ord_Count = n()) 
medium_ord$Ord_Proportion <- medium_ord$Ord_Count / 
  sum(medium_ord$Ord_Count)
colnames(medium_ord) <- c("Prey","Ord_Count","Ord_Proportion")

medium_fam <- na.omit(ptyas_medium, cols = "Family") # Family
fam_perc <- nrow(medium_fam)/nrow(ptyas_medium)*100 # Percentage
fam_samp <- nrow(medium_fam) # sample size 
medium_fam <- medium_fam %>%      
  dplyr::group_by(Family) %>%        
  dplyr::summarise(Fam_Count = n()) 
medium_fam$Fam_Proportion <- medium_fam$Fam_Count / 
  sum(medium_fam$Fam_Count)
colnames(medium_fam) <- c("Prey","Fam_Count","Fam_Proportion")

medium_gen <- na.omit(ptyas_medium, cols = "Genus") # Genus
gen_perc <- nrow(medium_gen)/nrow(ptyas_medium)*100 # Percentage
gen_samp <- nrow(medium_gen) # sample size 
medium_gen <- medium_gen %>%      
  dplyr::group_by(Genus) %>%        
  dplyr::summarise(Gen_Count = n()) 
medium_gen$Gen_Proportion <- medium_gen$Gen_Count / 
  sum(medium_gen$Gen_Count)
colnames(medium_gen) <- c("Prey","Gen_Count","Gen_Proportion")

medium_spe <- na.omit(ptyas_medium, cols = "Species") # Species
spe_perc <- nrow(medium_spe)/nrow(ptyas_medium)*100 # Percentage
spe_samp <- nrow(medium_spe) # sample size 
medium_spe <- medium_spe %>%      
  dplyr::group_by(Species) %>%        
  dplyr::summarise(Spe_Count = n()) 
medium_spe$Spe_Proportion <- medium_spe$Spe_Count / 
  sum(medium_spe$Spe_Count)
colnames(medium_spe) <- c("Prey","Spe_Count","Spe_Proportion")

medium_width <- Merge(medium_ord, medium_fam, medium_gen, medium_spe,all = TRUE, id = ~ Prey) 
medium_width[is.na(medium_width)] <- 0 
medium_width <- medium_width[, c("Ord_Proportion", "Fam_Proportion", "Gen_Proportion", "Spe_Proportion")] 
medium_width <- niche.width(medium_width, method = "levins")

ord_width <- (medium_width$Ord_Proportion-1)/(nrow(medium_ord)-1) # Order LPI
fam_width <- (medium_width$Fam_Proportion-1)/(nrow(medium_fam)-1) # Family LPI
gen_width <- (medium_width$Gen_Proportion-1)/(nrow(medium_gen)-1) # Genus LPI
spe_width <- (medium_width$Spe_Proportion-1)/(nrow(medium_spe)-1) # Species LPI

medium_width  <- round(medium_width, digits = 2)       # round values 
B_a   <- c(ord_width, fam_width, gen_width, spe_width) # standardized values
B_a   <- round(B_a, digits = 2)                        # round values
percs <- c(ord_perc, fam_perc, gen_perc, spe_perc)     # percentage of records 
percs <- round(percs, digits = 2)                      # round percentages
n <- c(ord_samp, fam_samp, gen_samp, spe_samp)         # sample sizes

levin_medium <- rbind(medium_width, B_a, percs, n)
rownames(levin_medium) <- c("Medium.B", "Medium.B_a", "Medium.Percentage", "Medium.Samplesize")
colnames(levin_medium) <- c("Order", "Family", "Genus", "Species")

ptyas.medium_sd <- sd(levin_medium["Medium.B_a", ]) # SD

## Merge size subset ptyas metrics 
ptyas_metrics <- rbind(levin_large, levin_medium) 
ptyas_metrics <- round(ptyas_metrics, digits = 2)

# ~~~~~~~~~ Calculate niche overlap for large and medium sized Naja & Ptyas ~~~~~~~~~~

# Define Pianka`s Measure 
pianka <- function(m=matrix(rpois(80,1),nrow=10)) {
  m <- m/rowSums(m)
  pairwise <- cbind(t(combn(nrow(m),2)),0)	# set up pairwise species list
  for (i in 1:nrow(pairwise)) 
    pairwise[i,3] <- sum(m[pairwise[i,1],]*m[pairwise[i,2],])/
    sqrt(sum(m[pairwise[i,1],]^2)*sum(m[pairwise[i,2],]^2))
  return(mean(pairwise[,3]))
}

## >>> Large individuals <<<

# Count overlapping predations per taxonomic level
ord_overlap <- data.frame(unclass(table( # Order
  subset(dat.distinct, Size == "L")$ID, subset(dat.distinct, Size == "L")$Order)))   
fam_overlap <- data.frame(unclass(table( # Family
  subset(dat.distinct, Size == "L")$ID, subset(dat.distinct, Size == "L")$Family)))
gen_overlap <- data.frame(unclass(table( # Genus
  subset(dat.distinct, Size == "L")$ID, subset(dat.distinct, Size == "L")$Genus)))  
spe_overlap <- data.frame(unclass(table( # Species
  subset(dat.distinct, Size == "L")$ID, subset(dat.distinct, Size == "L")$Species))) 

# Calculate Piankas index per taxonomic level 
ord_pianka <- pianka(ord_overlap) 
fam_pianka <- pianka(fam_overlap) 
gen_pianka <- pianka(gen_overlap) 
spe_pianka <- pianka(spe_overlap) 

pianka_large <- as.data.frame( # merge Pianaka values 
  cbind(ord_pianka, fam_pianka, gen_pianka, spe_pianka))
colnames(pianka_large) <- c("Order", "Family", "Genus", "Species")
rownames(pianka_large) <- "Overlap (Large)"
# Standard deviation Piankas index of large individuals
large_pianka_sd <- sd(pianka_large["Overlap", ])

## >>> Medium individuals <<<

# Count overlapping predations per taxonomic level
ord_overlap <- data.frame(unclass(table( # Order
  subset(dat.distinct, Size == "M")$ID, subset(dat.distinct, Size == "M")$Order)))   
fam_overlap <- data.frame(unclass(table( # Family
  subset(dat.distinct, Size == "M")$ID, subset(dat.distinct, Size == "M")$Family)))
gen_overlap <- data.frame(unclass(table( # Genus
  subset(dat.distinct, Size == "M")$ID, subset(dat.distinct, Size == "M")$Genus)))  
spe_overlap <- data.frame(unclass(table( # Species
  subset(dat.distinct, Size == "M")$ID, subset(dat.distinct, Size == "M")$Species))) 

# Calculate Piankas index per taxonomic level 
ord_pianka <- pianka(ord_overlap) 
fam_pianka <- pianka(fam_overlap) 
gen_pianka <- pianka(gen_overlap) 
spe_pianka <- pianka(spe_overlap) 

pianka_medium <- as.data.frame( # merge Pianaka values 
  cbind(ord_pianka, fam_pianka, gen_pianka, spe_pianka))
colnames(pianka_medium) <- c("Order", "Family", "Genus", "Species")
rownames(pianka_medium) <- "Overlap (Medium)"
# Standard deviation Piankas index of large individuals
medium_pianka_sd <- sd(pianka_medium["Overlap", ])

# Conclude Pinaka size subsets
pianka_sizes <- rbind(pianka_large, pianka_medium)

# Clean Gl. Env.
rm(large_ord, large_fam, large_gen, large_spe, large_width,
   medium_ord, medium_fam, medium_gen, medium_spe, medium_width,
   small_ord, small_fam, small_gen, small_spe, small_width,
   B_a, percs, levin_large, levin_medium, levin_small,
   naja_large, naja_small, naja_medium, n, 
   ptyas_large, ptyas_medium, ptyas_small,
   ord_samp, fam_samp, gen_samp, spe_samp,
   ord_perc, fam_perc, gen_perc, spe_perc, 
   ord_width, fam_width, gen_width, spe_width,
   ord_overlap, fam_overlap, gen_overlap, spe_overlap,
   ord_pianka, fam_pianka, gen_pianka, spe_pianka, 
   pianka_large, pianka_medium, pianka)

########## Relevant Plots & Maps ##########

# ~~~~~~~~~~ Comparative plot for all prey items ~~~~~~~~~~~

# Prepare input data
setDT(dat.distinct) # set distinct data as DT 
overlap.in <- na.omit(dat.distinct, cols = c("Prey")) # check for no missing data
overlap <- t(unclass(table(overlap.in$ID, overlap.in$Prey))) # get overlap
prey <- rownames(overlap)  # add prey items
rownames(overlap) <- NULL 
overlap <- as.data.frame(overlap)  
overlap$prey <- prey

# Prepare for matching prey classes ... 
overlap.in <- distinct(overlap.in, Prey, .keep_all = TRUE) # unique only 

overlap <- arrange(overlap, prey)
overlap.in <- arrange(overlap.in, Prey)
identical(overlap$prey, overlap.in$Prey) # check if labels match (must be TRUE)
overlap$Group <- overlap.in$Group # match prey groups 
colnames(overlap) <- c("Naja", "Ptyas", "Prey", "Group")

# Label shared prey items
overlap$Prey <- ifelse(overlap$Naja >=1 & overlap$Ptyas >=1,
                       paste(overlap$Prey, "(s)"),paste(overlap$Prey))

rownames(overlap) <- NULL
overlap <- overlap[-52, ] # fix misspelling
rownames(overlap) <- NULL
overlap[54,2] <- 2 # fix misspelling
overlap[52,3] <- "Unidentified rhacophoridae" # fix misspelling

overlap$italicize <- str_detect(overlap$Prey, "Unidentified")

# Define factor order
overlap$Prey <- factor(overlap$Prey, levels=(rev(overlap$Prey[order(overlap$Group)])), 
                       ordered=TRUE)

n_naja  <- sum(overlap$Naja)  # get current sample sizes 
n_ptyas <- sum(overlap$Ptyas) # get current sample sizes 

# Middle plot (Names)
g.mid <- ggplot(overlap, aes(x=1,y=Prey, fill = Group)) +
  geom_text(aes(label=Prey), family = "Times New Roman", size = 9, color = "black",
            fontface = ifelse(overlap$italicize, "plain", "italic")) +
  scale_color_viridis_d(begin = 1, end = 0)+
  geom_segment(aes(x=0.92,xend=0.96,yend=Prey, color = Group), color = "transparent")+
  geom_segment(aes(x=1.04,xend=1.07,yend=Prey, color = Group), color = "transparent")+
  ggtitle("Prey item")+
  ylab(NULL)+
  theme_void()+
  scale_x_continuous(expand=c(0,0),limits=c(0.92,1.07))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(family="Times New Roman", size = 30, colour = "transparent",
                                    margin = margin(t = 20, r = 0, b = 20, l = 0)),
        panel.background=element_blank(),
        axis.text.x=element_text(color = NA, size = 30),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,-1,1,-1), "mm"),
        legend.position = "none",
        plot.title = element_text(family="Times New Roman",
                                  hjust = 0.5, vjust = .5, size = 36, colour = "transparent",
                                  margin = margin(t = 20, r = 0, b = 20, l = 0)))
# Naja (left) plot
naja <- ggplot(data = overlap, aes(x = Prey, y = Naja, fill = Group)) +
  scale_fill_viridis_d(begin = 1, end = 0)+
  geom_bar(stat = "identity", color = "black", alpha = .8) +
  ggtitle(paste0("Naja naja (*n* = ", n_naja, ")")) +
  theme_void()+
  ylab("Relative frequency")+
  theme(axis.title.x = element_text(family="Times New Roman", size = 30,
                                    margin = margin(t = 20, r = 0, b = 20, l = 0)), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(family="Times New Roman", size = 30),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,0,1,-1), "mm"),
        plot.title = ggtext::element_markdown(family="Times New Roman",
                                  hjust = 0.5, vjust = .5, size = 36,
                                  margin = margin(t = 20, r = 0, b = 20, l = 0)),
        legend.direction = "vertical",
        legend.position = c(.1, .55),
        legend.title = element_blank(),
        legend.text = element_text(family = "Times New Roman", size = 26),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1.5, 'cm'), 
        legend.key.width = unit(1, 'cm'),
        legend.spacing.y = unit(3, 'cm'),
        legend.background = element_rect(color = NA)) +
  guides(fill = guide_legend(byrow = TRUE))+
  scale_y_reverse() +
  coord_flip()
# Ptyas (right) plot
ptyas <- ggplot(data = overlap, aes(x = Prey, y = Ptyas, fill = Group)) +
  scale_fill_viridis_d(begin = 1, end = 0)+
  xlab(NULL) +
  geom_bar(stat = "identity", color = "black", alpha = .8) +
  ggtitle(paste0("Ptyas mucosa (*n* = ", n_ptyas, ")")) +
  theme_void() +
  ylab("Relative frequency")+
  theme(axis.title.x = element_text(family="Times New Roman", size = 30,
                                    margin = margin(t = 20, r = 0, b = 20, l = 0)), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(family="Times New Roman", size = 30),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,0,1,-1), "mm"),
        plot.title = ggtext::element_markdown(family="Times New Roman",
                                  hjust = 0.5, vjust = .5, size = 36,
                                  margin = margin(t = 20, r = 0, b = 20, l = 0)),
        legend.position = "none") +
  coord_flip()
# Build plot grid
gg1 <- ggplot_gtable(ggplot_build(naja))
gg2 <- ggplot_gtable(ggplot_build(ptyas))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))
comp_plot <- grid.arrange(gg1, gg.mid, gg2, ncol=3, widths=c(4/9,1.5/9,4/9))

ggsave("comp_plot.jpeg",
       plot = comp_plot, 
       path = "/Users/serpent/OneDrive - Van Hall Larenstein/Niche Partioning/Plots",
       width = 30, 
       height = 30, 
       dpi = 700, 
       device = "jpeg",
       bg = "white")

rm(gg1, gg2, g.mid, gg.mid, overlap, overlap.in, naja, ptyas, comp_plot, prey)

# >>>>>>>>>> Mapping <<<<<<<<<<<

India <- ne_countries(scale = "medium", returnclass = "sf", country = c("India")) # get India as a transparent overlay
rest <- ne_countries(scale = "medium", returnclass = "sf") # get remaining overlay base-map 
rest <- subset(rest, name != "India") # exclude India

naja.cords <- na.omit(dat.naja[ ,c("Lat", "Long")]) # clean Naja coordinates
ptyas.cords <- na.omit(dat.ptyas[, c("Lat", "Long")]) # clean Ptyas coordinates

n_naja  <- nrow(naja.cords)  # get current sample sizes
n_ptyas <- nrow(ptyas.cords) # get current sample sizes 

naja_plot <- ggplot() + # heat map with Naja naja records 
  geom_sf(data = rest, fill= "antiquewhite", colour = "grey50") +
  geom_sf(data = India, fill= "antiquewhite", colour = "black") +
  annotation_scale(location = "bl", width_hint = 0.5, text_family = "Times New Roman") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.35, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(65, 95), ylim = c(7, 35))+
  
  stat_density_2d(data = naja.cords, aes(x=Long, y=Lat, fill = ..level..), 
                  alpha=0.38, geom="polygon", contour_var = "ndensity")+
  
  geom_point(data = naja.cords, aes(x=Long, y=Lat), colour="black", shape = 20)+
  
  scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral")))+
  ggtitle(paste0("(A) Feeding events of *Naja naja* (*n* = ", n_naja, ")"))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "aliceblue"),
        plot.title = ggtext::element_markdown(family="Times New Roman"),
        legend.title = element_text(size = 14,hjust = 0, family="Times New Roman"),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.spacing.y = unit(.3, "cm"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.size = unit(.5, "cm"),
        legend.key.width = unit(4,"cm"),
        legend.text = element_text(family="Times New Roman"),
        axis.title.x = element_text(family="Times New Roman"),
        axis.title.y = element_text(family="Times New Roman"),
        axis.text.x=element_text(family="Times New Roman"),
        axis.text.y=element_text(family="Times New Roman")
  )+
  labs(fill = "Density estimate

")+
  xlab("")+
  ylab("")+
  xlim(60, 100)+
  ylim(5, 45)

ptyas_plot <- ggplot() + # heat map with Ptyas mucosa records 
  geom_sf(data = rest, fill= "antiquewhite", colour = "grey50") +
  geom_sf(data = India, fill= "antiquewhite", colour = "black") +
  annotation_scale(location = "bl", width_hint = 0.5, text_family = "Times New Roman") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.35, "in"),
                         style = north_arrow_fancy_orienteering) +
  stat_density_2d(data = ptyas.cords, aes(x=Long, y=Lat, fill = ..level..), 
                  alpha=0.38, geom="polygon", contour_var = "ndensity")+
  geom_point(data = ptyas.cords, aes(x=Long, y=Lat), colour="black", shape = 20)+
  scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral")))+
  coord_sf(xlim = c(65, 95), ylim = c(7, 35))+
  ggtitle(paste0("(B) Feeding events of *Ptyas mucosa* (*n* = ", n_ptyas, ")"))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "aliceblue"),
        plot.title = ggtext::element_markdown(family="Times New Roman"),
        legend.title = element_text(size = 14,hjust = 0, family="Times New Roman"),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.spacing.y = unit(.3, "cm"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.size = unit(.5, "cm"),
        legend.key.width = unit(4,"cm"),
        legend.text = element_text(family="Times New Roman"),
        axis.title.x = element_text(family="Times New Roman"),
        axis.title.y = element_text(family="Times New Roman"),
        axis.text.x=element_text(family="Times New Roman"),
        axis.text.y=element_text(family="Times New Roman")
        )+
  labs(fill = "Density estimate

")+
  xlab("")+
  ylab("")+
  xlim(60, 100)+
  ylim(5, 45)

final_map <- ggarrange(naja_plot, ptyas_plot, common.legend = TRUE, legend="bottom")

ggsave("niche_map.jpeg", plot = final_map, path = "/Users/serpent/OneDrive - Van Hall Larenstein/Niche Partioning/Plots",
       width = 10, height = 6, dpi = 700, device = "jpeg")

# ---------- Spatial overlap & Over- / Underrepresentation ----------

# Calculate the common Lat and Long range for Naja naja and Ptyas mucosa
xrng <- range(c(naja.cords$Long, ptyas.cords$Long))
yrng <- range(c(naja.cords$Lat, ptyas.cords$Lat))

x_1 <- range(c(65, 97))
y_1 <- range(c(7, 37))

# Calculate the 2d density estimate over the common range
d1 <- kde2d(naja.cords$Long, naja.cords$Lat, lims=c(x_1, y_1), n=200)
d2 <- kde2d(ptyas.cords$Long, ptyas.cords$Lat, lims=c(x_1, y_1), n=200)

# Confirm that the grid points for each density estimate are identical
identical(d1$x, d2$x) # TRUE
identical(d1$y, d2$y) # TRUE

# Calculate the difference between the 2d density estimates
diff12 <- d1 
diff12$z <- d2$z - d1$z

# Melt data into long format; add row and column names (x and y grid values) to the z-value matrix
rownames(diff12$z) <- diff12$x
colnames(diff12$z) <- diff12$y
diff12.m <- melt(diff12$z, id.var=rownames(diff12)) 
names(diff12.m) <- c("Long","Lat","z")

# Rasterize & Crop to India 
India <- raster::getData('GADM', country='IND', level=0) 
coordinates(diff12.m) <- ~ Long + Lat
gridded(diff12.m) <- TRUE
rasterDF <- raster(diff12.m)
rasterDF_crop <- crop(rasterDF, extent(India))
rasterDF_masked <- mask(rasterDF_crop, India)
df_masked <- na.omit(raster::as.data.frame(rasterDF_masked,xy=TRUE))

India <- ne_countries(scale = "medium", returnclass = "sf", country = c("India")) # get India as a transparent overlay
rest <- ne_countries(scale = "medium", returnclass = "sf") # get remaining overlay basemap 
rest <- subset(rest, name != "India") # exclude India

overlap_map <- ggplot() + 
  geom_tile(data = df_masked, mapping = aes(x, y, fill = z), colour = "transparent")+
  stat_contour(data = df_masked, mapping = aes(x, y, z=z, colour=..level..), 
               binwidth=0.001, colour = "black", alpha = .1) +
  
  scale_fill_gradient2(low="red1",mid="antiquewhite", high="midnightblue", midpoint=0) +
  scale_colour_gradient2(low="red1", mid="antiquewhite", high="midnightblue", midpoint=0) +
  
  geom_sf(data = rest, fill= "antiquewhite", colour = "grey50") +
  geom_sf(data = India, fill= "transparent", colour = "black") +
  annotation_scale(location = "bl", width_hint = 0.5, text_family = "Times New Roman") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.35, "in"),
                         style = north_arrow_fancy_orienteering, text_family = "Times New Roman") +
  coord_sf(xlim = c(65, 95), ylim = c(7, 35))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "aliceblue"),
        legend.title = element_text(size = 12, hjust = 0, family="Times New Roman"),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.spacing.y = unit(.3, "cm"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.size = unit(.5, "cm"), 
        legend.key.width = unit(1.3,"cm"),
        legend.text = element_text(family="Times New Roman"),
        axis.title.x = element_text(family="Times New Roman"),
        axis.title.y = element_text(family="Times New Roman"),
        axis.text.x=element_text(family="Times New Roman"),
        axis.text.y=element_text(family="Times New Roman"))+
  labs(fill = "Difference estimate

")+
  xlab("")+
  ylab("")+
  xlim(60, 100)+
  ylim(5, 45)

ggsave("overlap_map.jpeg", plot = overlap_map, path = "/Users/serpent/OneDrive - Van Hall Larenstein/Niche Partioning/Plots",
       width = 6, height = 6, dpi = 700, device = "jpeg")

# In blueish areas: More Ptyas records
# In reddish areas: More Naja records
# In normally colored areas: Equal density 

rm(d1, d2, df_masked, diff12, diff12.m, India, naja.cords, ptyas.cords,rasterDF, rasterDF_crop,
   rasterDF_masked, rest, naja, ptyas, x_1, y_1, xrng, yrng, naja_plot, ptyas_plot, overlap_map, final_map) # Clean Gl. Env.

# >>>>>>>>>> Descriptive tables <<<<<<<<<<

# Amphibians 
amphibians <- subset(dat.distinct, Class == "Amphibia")
amph_naja <- amphibians %>% subset( ID == "Naja") %>%
  dplyr::group_by(Prey) %>%
  dplyr::summarise(Naja = n())
amph_ptyas <- amphibians %>% subset( ID == "Ptyas") %>%
  dplyr::group_by(Prey) %>%
  dplyr::summarise(Ptyas = n())
amphibians <- Merge(amph_naja, amph_ptyas, id = ~ Prey)
amphibians[is.na(amphibians)] <- 0
rm(amph_naja, amph_ptyas)

# Birds 
aves <- subset(dat.distinct, Class == "Aves")
aves_naja <- aves %>% subset( ID == "Naja") %>%
  dplyr::group_by(Prey) %>%
  dplyr::summarise(Naja = n())
aves_ptyas <- aves %>% subset( ID == "Ptyas") %>%
  dplyr::group_by(Prey) %>%
  dplyr::summarise(Ptyas = n())
aves <- Merge(aves_naja, aves_ptyas, id = ~ Prey)
aves[is.na(aves)] <- 0
rm(aves_naja, aves_ptyas)

# Lizards
sauria <- subset(dat.distinct, Group == "Sauria")
sauria_naja <- sauria %>% subset( ID == "Naja") %>%
  dplyr::group_by(Prey) %>%
  dplyr::summarise(Naja = n())
sauria_ptyas <- sauria %>% subset( ID == "Ptyas") %>%
  dplyr::group_by(Prey) %>%
  dplyr::summarise(Ptyas = n())
sauria <- Merge(sauria_naja, sauria_ptyas, id = ~ Prey)
sauria[is.na(sauria)] <- 0
rm(sauria_naja, sauria_ptyas)

# Mammals
mammalia <- subset(dat.distinct, Group == "Mammalia")
mammalia_naja <- mammalia %>% subset( ID == "Naja") %>%
  dplyr::group_by(Prey) %>%
  dplyr::summarise(Naja = n())
mammalia_ptyas <- mammalia %>% subset( ID == "Ptyas") %>%
  dplyr::group_by(Prey) %>%
  dplyr::summarise(Ptyas = n())
mammalia <- Merge(mammalia_naja, mammalia_ptyas, id = ~ Prey)
mammalia[is.na(mammalia)] <- 0
rm(mammalia_naja, mammalia_ptyas)

# Snakes
serpentes <- subset(dat.distinct, Group == "Serpentes")
serpentes_naja <- serpentes %>% subset( ID == "Naja") %>%
  dplyr::group_by(Prey) %>%
  dplyr::summarise(Naja = n())
serpentes_ptyas <- serpentes %>% subset( ID == "Ptyas") %>%
  dplyr::group_by(Prey) %>%
  dplyr::summarise(Ptyas = n())
serpentes <- Merge(serpentes_naja, serpentes_ptyas, id = ~ Prey)
serpentes[is.na(serpentes)] <- 0
rm(serpentes_naja, serpentes_ptyas)

# Identify shared prey species 
comb <- rbind(amphibians, aves, mammalia, sauria, serpentes)
comb$over <- comb$Naja + comb$Ptyas
comb <- subset(comb, Naja > 0 & Ptyas > 0)
rownames(comb) <- NULL

# ~~~~~ Write necessary tables ~~~~~ 
write.table(global_metrics, file = "Niche_metrics.csv", sep = ",", row.names = TRUE)     # niche metrics
write.table(naja_metrics, file = "Naja_metrics.csv", sep = ",", row.names = TRUE) # size metrics 
write.table(ptyas_metrics, file = "Ptyas_metrics.csv", sep = ",", row.names = TRUE) # size metrics 

setwd("/Users/serpent/OneDrive - Van Hall Larenstein/Niche Partioning/Stats/Prey")
write.table(amphibians, file = "Amphibians.csv", sep=",", row.names = FALSE, col.names = TRUE) # Amphibians
write.table(aves, file = "Aves.csv", sep=",", row.names = FALSE, col.names = TRUE) # Birds
write.table(mammalia, file = "Mammalia.csv", sep=",", row.names = FALSE, col.names = TRUE) # Mammals
write.table(sauria, file = "Sauria.csv", sep=",", row.names = FALSE, col.names = TRUE) # Lizards
write.table(serpentes, file = "Serpentes.csv", sep=",", row.names = FALSE, col.names = TRUE) # Snakes

rm(amphibians, aves, mammalia, sauria, serpentes, n_naja, n_ptyas) # Clean Gl. Env. 
dev.off() # clean plots 

# ~~~~~~~~~ Descriptive values used in manuscript ~~~~~~~~~~ 

# --- Abstract ---

# Row 42 (prev. unpublished trophic interactions)
nrow(as.data.frame(unique(subset(dat.all, Source != "Literature" & ID == "Ptyas")$Species))) +
  nrow(as.data.frame(unique(subset(dat.all, Source != "Literature" & ID == "Naja")$Species)))

# --- Results ---

# Row 196 - 197
nrow(subset(dat.all, ID == "Naja")) # total naja records 
nrow(dat.naja) # distinct naja records
nrow(dat.naja) - nrow(subset(dat.naja, is.na(dat.naja$Species))) # species level 
nrow(subset(dat.naja, is.na(dat.naja$Species)))-nrow(subset(dat.naja, is.na(dat.naja$Genus))) # genus level 
nrow(subset(dat.naja, is.na(dat.naja$Genus)))-nrow(subset(dat.naja, is.na(dat.naja$Family))) # family level
nrow(subset(dat.naja, is.na(dat.naja$Family)))-nrow(subset(dat.naja, is.na(dat.naja$Order))) # order level 
nrow(subset(dat.naja, is.na(dat.naja$Order)))-nrow(subset(dat.naja, is.na(dat.naja$Class))) # class level 

# Row 198 - 200 (Naja class level preferences)
subset(class.prey, ID == "Naja")

# Row 200 (minimum Naja LPI %)
min(global_metrics["Naja.Percentage", ])

# Row 203 (maximum Naja B_a)
max(global_metrics["Naja.B_a", ])

# Row 204 (SD of Naja B_a)
round(naja_sd, digits = 2)

# Row 206 - 207 (ophiophagic events)
nrow(subset(dat.naja, Group == "Serpentes")) # from total ophiophagic events...
nrow(subset(dat.naja, Size == "L")) # large animals ...
nrow(subset(dat.naja, Size == "L")) / nrow(dat.naja) * 100 # ... percentage ...
round(nrow(subset(dat.naja, Group == "Serpentes" & Size =="L"))/
  nrow(subset(dat.naja, Group == "Serpentes"))*100, digits = 2) # percentage 
round(nrow(subset(dat.naja, Size == "L" & Group == "Serpentes"))/
      nrow(subset(dat.naja, Size == "L"))*100, digits = 2) # percentage of their diet ...
round(nrow(subset(dat.naja, Group == "Serpentes" & Size =="L")), digits = 2) # ... n =  ...  

# Row 211 SD B_a large Naja 
round(naja.large_sd,digits = 2)

# Row 211 (medium najas)
nrow(subset(dat.naja, Size == "M"))
round(nrow(subset(dat.naja, Size == "M")) / nrow(dat.naja)*100, digits = 2)

# Row 212 (Medium Naja B_a Species LPI)
naja_metrics["Medium.B_a", ]

# Row 2013 (Frog preference)
nrow(subset(dat.naja, Size == "M" & Group == "Anura"))
nrow(subset(dat.naja, Size == "M" & Group == "Anura")) / nrow(subset(dat.naja, Size == "M")) * 100

# Row 216 (Available data below ordinal LPI)
naja_metrics[c("Medium.Percentage", "Medium.Samplesize"), -1]

# Row 217 (SD Medium)
round(naja.medium_sd, digits = 2)

# Row 219 (Small individuals)
nrow(subset(dat.naja, Size == "S"))

# Row 220 (Prey species and unpublished one)
nrow(as.data.frame(unique(dat.naja$Species)))
nrow(as.data.frame(unique(subset(dat.all, Source != "Literature" & ID == "Naja")$Species)))

# Row 223 (Ptyas records)
nrow(subset(dat.all, ID == "Ptyas")) # all records
nrow(dat.ptyas) # distinct records

# Row 223 - 225
nrow(dat.ptyas)-nrow(as.data.frame(dat.ptyas$Species[is.na(dat.ptyas$Species)])) # species LPI
nrow(as.data.frame(dat.ptyas$Species[is.na(dat.ptyas$Species)]))-nrow(as.data.frame(dat.ptyas$Genus[is.na(dat.ptyas$Genus)])) # Genus
nrow(as.data.frame(dat.ptyas$Genus[is.na(dat.ptyas$Genus)]))-nrow(as.data.frame(dat.ptyas$Family[is.na(dat.ptyas$Family)])) # Family
nrow(as.data.frame(dat.ptyas$Family[is.na(dat.ptyas$Family)]))-nrow(as.data.frame(dat.ptyas$Order[is.na(dat.ptyas$Order)])) # Order
nrow(as.data.frame(dat.ptyas$Order[is.na(dat.ptyas$Order)]))-nrow(as.data.frame(dat.ptyas$Class[is.na(dat.ptyas$Class)])) # Class

# Row 225-227
subset(class.prey, ID == "Ptyas") # class level preferences

# Row 229 (Available species LPI)
global_metrics["Ptyas.Percentage", 4]

# Row 230 (SD Ptyas all)
round(ptyas_sd, digits = 2)

# Row 232 (Number of medium Ptyas)
round(nrow(subset(dat.ptyas, Size == "M")) / nrow(dat.ptyas)*100, digits = 2) # Percentage
nrow(subset(dat.ptyas, Size == "M"))

ptyas_metrics["Medium.B_a", 3] # B_a on Species LPI
ptyas_metrics["Medium.Percentage", 4] # Species LPI

# Row 232 (Number of large Ptyas)
round(nrow(subset(dat.ptyas, Size == "L")) / nrow(dat.ptyas)*100, digits = 2) # Percentage
nrow(subset(dat.ptyas, Size == "L")) # n 

ptyas_metrics["Large.B_a", 3] # B_a on Species LPI
ptyas_metrics["Large.Percentage", 4] # Species LPI
ptyas_metrics["Large.Percentage", 3] # Genus LPI

# Row 238-239 (Prey items and unpublished ones)
nrow(as.data.frame(unique(dat.ptyas$Species)))
nrow(as.data.frame(unique(subset(dat.all, Source != "Literature" & ID == "Ptyas")$Species)))

# Row 241 (Overlap)
min(global_metrics["Overlap", ]) # minimum overlap
max(global_metrics["Overlap", ]) # maximum overlap

# Row 244 (SD Overlap)
round(pianka_sd, digits = 2)

# Row 245 (out of n prey items on species level ...)
nrow(as.data.frame(unique(dat.all$Species)))

# Row 246 (... from n prey items)
nrow(as.data.frame(unique(dat.all$Prey)))

# Row 246 (shared prey items)
nrow(comb)

# Row 247-248 (relative importance of shared prey)
round(sum(comb$Naja) / nrow(dat.naja)*100, digits = 2) # Naja naja 
round(sum(comb$Ptyas) / nrow(dat.ptyas)*100, digits = 2) # Ptyas mucosa 

# Row 250 (available data with coordinates)
round((nrow(dat.naja)-nrow(subset(dat.naja, is.na(dat.naja$Lat))))/nrow(dat.naja)*100, digits = 2) # Percentage Naja
(nrow(dat.naja)-nrow(subset(dat.naja, is.na(dat.naja$Lat)))) # n Naja 
round((nrow(dat.ptyas)-nrow(subset(dat.ptyas, is.na(dat.ptyas$Lat))))/nrow(dat.ptyas)*100, digits = 2) # Percentage Ptyas
(nrow(dat.ptyas)-nrow(subset(dat.ptyas, is.na(dat.ptyas$Lat)))) # n Ptyas

# Row 260-267 (source contributions)
sources[5,c(1,3:4)]  # Naja SM
sources[10,c(1,3:4)] # Ptyas Sm
sources[2,c(1,3:4)]  # Naja Lit
sources[7,c(1,3:4)]  # Ptyas Lit
sources[3,c(1,3:4)]  # Naja Pers
sources[8,c(1,3:4)]  # Ptyas Pers
sources[4,c(1,3:4)]  # Naja SE
sources[9,c(1,3:4)]  # Ptyas SE
sources[1,c(1,3:4)]  # Naja CS
sources[6,c(1,3:4)]  # Ptyas CS 

# --- Discussion --- 

# Row 276 (Available Data B_a Naja Species LPI)
global_metrics["Naja.Percentage", 4]

# Row 308-309 (rel. importance of hemerophilic species in naja naja)
round(nrow(subset(dat.naja, Species == "naja"))/nrow(subset(dat.naja, Group == "Serpentes"))*100, digits = 2)
round(nrow(subset(dat.naja, Species == "russelii"))/nrow(subset(dat.naja, Group == "Serpentes"))*100, digits = 2)
round(nrow(subset(dat.naja, Species == "mucosa"))/nrow(subset(dat.naja, Group == "Serpentes"))*100, digits = 2)

# Row 314 (Ophiophagic naja events from social media)
round((nrow(subset(dat.all, ID == "Naja" & Group == "Serpentes" & Source == "Social Media"))/
  nrow(subset(dat.all, ID == "Naja" & Group == "Serpentes")))*100, digits = 2)

nrow(comb)/nrow(distinct(dat.naja, Prey))*100 # direct competitor naja 
nrow(comb)/nrow(distinct(dat.ptyas, Prey))*100 # direct competitor ptyas

sum(comb$Naja)/nrow(dat.naja)*100 # all predations observed
sum(comb$Ptyas)/nrow(dat.ptyas)*100 # all predations observed

#################### END ####################

## Author: Merlin Weiss (2022); Weiss & Kalki (2022) -
## Trophic niche partitioning between sympatric Naja naja and Ptyas mucosa:
## Crowdsourced data in application to community ecology

citation()
citation("tidyverse") 
citation("data.table") 
citation("Hmisc") 
citation("reshape2") 
citation("spaa") 
citation("sp") 
citation("raster") 
citation("GISTools") 
citation("maptools") 
citation("sf") 
citation("rgdal") 
citation("rgeos") 
citation("dismo") 
citation("geosphere") 
citation("ggmap") 
citation("rnaturalearth") 
citation("rnaturalearthdata") 
citation("ggspatial") 
citation("RColorBrewer") 
citation("scales") 
citation("ggpubr") 
citation("grid") 
citation("gridExtra") 
citation("extrafont")  
