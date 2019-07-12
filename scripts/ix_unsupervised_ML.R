# METADATA ====
# Description: unsupervised ML on youth data
# Created: 2019-07-06 (Neil Rankin)
# Updated: 2019-07-06 (Neil Rankin)
# Reviewed: NA

# SUMMARY: Three exercises in this file:
#         - Recap on joins
#         - Cluster using K-means and Hierachical (including elbow plots)
#                 - plus understand characteristics of clusters
#         - Put into a regression model


# turn off scientific notation
options(scipen=999)

library(tidyverse)

# READ IN DATA
# in time this should point to your 'wrangled'/cleaned data file
# commented out currently since we'll only use it later
# df <- read.csv("data/raw/teaching_training_data.csv")

# now read in data for each assessment
df_cft <- read.csv("data/raw/teaching_training_data_cft.csv")
df_com <- read.csv("data/raw/teaching_training_data_com.csv")
df_grit <- read.csv("data/raw/teaching_training_data_grit.csv")
df_num <- read.csv("data/raw/teaching_training_data_num.csv")
df_opt <- read.csv("data/raw/teaching_training_data_opt.csv")

# remember to look at your data
# what do you notice?

# each individual should only have one assessment
# set up the data so this is the case...
# also need to only keep the unid and score

df_cft <- df_cft %>% 
  select(unid, cft_score) %>% 
  distinct(unid, .keep_all = TRUE)

# we want to do this for all 5 asssessments
# but it is a pain to copy and paste and then change inputs
# perfect opportunity for us to write our own function
# See Jenny Bryan's tutorials starting here (http://stat545.com/block011_write-your-own-function-01.html)

helper_function <- function(file_name) {
  file_name %>% 
    select(2:3) %>% 
    distinct(unid, .keep_all = TRUE)
}

df_com <- helper_function(df_com)
df_grit <- helper_function(df_grit)
df_num <- helper_function(df_num)
df_opt <- helper_function(df_opt)

# look at your data to make sure it is doing what you want it to


# JOINS
# we are now going to join these dataframes together to create an assessment df

df_assess <- left_join(df_cft, df_com, by ="unid")

# Could do this in a pipe way
df_assess <- df_assess %>% 
  left_join(df_grit, by ="unid") %>% 
  left_join(df_num, by ="unid") %>% 
  left_join(df_opt, by ="unid")

# notice the different number of observations in each df
# why does df_assess have fewer observations than df_grit?

# change all the `left_joins` to `inner_joins`What happens?

# Clean up so we only have one df in the environment
rm(df_cft, df_com, df_grit, df_num, df_opt)

# CLUSTERING
# going to need to load some packages
library(cluster)

# Start by only using candidates which have observations for 3 variables

df_cluster_n <- df_assess %>% 
  filter(!is.na(cft_score) & !is.na(com_score) & !is.na(opt_score))

df_unid <- df_cluster_n %>% 
  select(unid)

# Now scale
df_cluster <- df_cluster_n %>% 
  select(cft_score, com_score, opt_score) %>% 
  scale()

# what does this data look like?

# KMEANS
k4 <- kmeans(df_cluster, centers = 4, nstart = 25)

table(k4$cluster)

# look at characteristics within each cluster
k4_cluster <- as.data.frame(k4$cluster)

df_cluster_k4 <- bind_cols(as.data.frame(df_cluster), k4_cluster) %>% 
  rename(cluster = `k4$cluster`)

ggplot(data = df_cluster_k4) + 
  geom_jitter(aes(x = cft_score, y = com_score, color = as.factor(cluster)))


ggplot(data = df_cluster_k4) + 
  geom_jitter(aes(x = cft_score, y = opt_score, color = as.factor(cluster)))

df_cluster_k4_sum <- df_cluster_k4 %>% 
  group_by(cluster) %>% 
  summarise(cft_score = mean(cft_score), 
            com_score = mean(com_score), 
            opt_score = mean(opt_score), 
            nobs = n())


# HOW MANY CLUSTERS ARE OPTIMAL?
#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- df_cluster
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Pick optimal number then run through process again

# Hierachical clustering
# First stage is getting a 'Dissimilarity matrix'
d <- dist(df_cluster, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Looks like 3 clusters in dendogram
clusterCut <- cutree(hc1, 3)
table(clusterCut)

df_cluster_h3 <- bind_cols(as.data.frame(df_cluster), as.data.frame(clusterCut))

# could also bind to k-means clusters and look at similiarities

df_cluster_h3_sum <- df_cluster_h3 %>% 
  group_by(clusterCut) %>% 
  summarise(cft_score = mean(cft_score), 
            com_score = mean(com_score), 
            opt_score = mean(opt_score), 
            nobs = n())

# DO THESE CLUSTERS HAVE ANY EXPLANATORY POWER?

h3 <- df_cluster_h3 %>% 
  select(clusterCut) %>% 
  rename(h3 = clusterCut)

k4 <- df_cluster_k4 %>% 
  select(cluster) %>% 
  rename(k4 = cluster)

df_clusters_unid <- bind_cols(df_unid, h3) %>% 
  bind_cols(k4)

# now load the broader df
df <- read.csv("data/raw/teaching_training_data.csv")

df <- left_join(df, df_clusters_unid, by = "unid")


# and we can start running our regression again

reg1 <- lm(working ~ gender + as.factor(k4), data = df)
summary(reg1)

reg2 <- lm(working ~ gender + as.factor(h3), data = df)
summary(reg2)

# put both in
reg3 <- lm(working ~ gender + as.factor(k4) + as.factor(h3), data = df)
summary(reg3)

# allow to vary by gender
reg4 <- lm(working ~ gender * as.factor(k4), data = df)
summary(reg4)

# who are the 'cluster 1s' from the kmeans?
