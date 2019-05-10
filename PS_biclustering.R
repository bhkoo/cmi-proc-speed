### Processing Speed Biclustering
### Bonhwang Koo
### 7/19/2018

library(psych)
library(viridis)
library(fmsb)
library(cluster)
library(ggplot2)
library(reshape)
library(klaR)

dir <- "/Volumes/Data/Research/Healthy Brain Network/AACAP/2017/Proc Speed/Data June 2018"
dir <- setwd("R:/Data June 2018")
setwd(dir)

dimensional <- c("CBCL_Int_T", "CBCL_Ext_T", "MFQ_P_Total_AgeSex_Ctrl", 
                 "MFQ_SR_Total_AgeSex_Ctrl", "SCARED_P_Total_AgeSex_Ctrl", "SCARED_SR_Total_AgeSex_Ctrl", 
                 "ARI_P_Total_Score_AgeSex_Ctrl", "ARI_S_Total_Score_AgeSex_Ctrl", "SWAN_IN_AgeSex_Ctrl", 
                 "SWAN_HY_AgeSex_Ctrl", "ASD_latent_factor", "PC1")
categorical <- c("ADHD_I", "ADHD_H", "ADHD_C", "ASD", "DMDD", 
                 "GAD", "Learning", "Anx_Soc", "Anx_All", "Depression", 
                 "ASD_ADHD_I", "ASD_ADHD_H", "ASD_ADHD_C", "ASD_ADHD_All", "ASD_noADHD")
categorical_short <- c("ADHD_I", "ADHD_H", "ADHD_C", "ASD", "DMDD", 
                 "GAD", "Learning", "Anx_Soc", "Depression")

df_clust <- read.csv("Data Cleaned/Biclustering_Data.csv", stringsAsFactors = FALSE)
df_clust_dim <- df_clust[, c("EID", dimensional)]
df_clust_dim_noPC1 <- df_clust[, c("EID", dimensional[dimensional != "PC1"])]
df_clust_dx <- df_clust[, c("EID", categorical_short)]

dfs <- list(df_clust_dim, df_clust_dim_noPC1)
names(dfs) <- c("all", "asmt")

## znorm
## Apply z-score normalization to a numeric vector
znorm <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

## znorm
## Apply z-score normalization to a numeric vector
ranknorm <- function(x) {
  rank(x)/length(x)
}

##FUNCTION## Apply clustering to data matrix. Output clustering variables and visualization.
TwoWayclust <- function(Data1, group_k = 3, variable_k = 3, method = "euclidean", gower = FALSE) {
  #takes in a csv dataset, cleans it up, standardizes it, and calculates the twowayclust on the data at the threshold defined by the user
  #saves the heatmap, and outputs the mycl for the group differences function
  
  #change so that if variable is already loaded into workspace the can call it
  #if variable is a path then do the following, else just set Clusterset equal to the dataset_path variable
  if (!gower) {
    r_dist <- dist(Data1, method = method)
    c_dist <- dist(t(Data1), method = method)
  } else {
    r_dist <- daisy(Data1, metric = "gower")
    c_dist <- daisy(t(Data1), metric = "gower")
  }
  hr <- hclust(r_dist, method = "ward.D2");
  hc <- hclust(c_dist, method = "ward.D2")
  # 4 Subject Group Assignment
  Sub_Group <- cutree(hr, k = group_k)
  mycolhr <- rainbow(length(unique(Sub_Group)), start = 0.1, end = 0.9); 
  mycolhr <- mycolhr[as.vector(Sub_Group)]
  
  # 5 Variable Group Assignment
  Var_Group <- cutree(hc, k = variable_k)
  mycolhc <- rainbow(length(unique(Var_Group)), start = 0.1, end = 0.9); 
  mycolhc <- mycolhc[as.vector(Var_Group)]
  
  # 6 Visualization
  h<-heatmap(as.matrix(Data1), Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = inferno(256), scale = "none", RowSideColors = mycolhr, ColSideColors = mycolhc)
  h
  #Action- Figure out how to export the visualization to disk-
  #Action- Figure out how to save all relevant variables- Var-Group, Sub_group, mycolhr, hr, hc
}

TwoWayclust_means <- function(Data1, group_k = 3, variable_k = 3, method = "euclidean", gower = FALSE) {
  if (!gower) {
    r_dist <- dist(Data1, method = method)
    c_dist <- dist(t(Data1), method = method)
  } else {
    r_dist <- daisy(Data1, metric = "gower")
    c_dist <- daisy(t(Data1), metric = "gower")
  }
  hr <- hclust(r_dist, method = "ward.D2");
  hc <- hclust(c_dist, method = "ward.D2")
  
  # 4 Subject Group Assignment
  Sub_Group <- cutree(hr, k = group_k)
  mycolhr <- rainbow(length(unique(Sub_Group)), start = 0.1, end = 0.9); 
  mycolhr <- mycolhr[as.vector(Sub_Group)]
  
  # 5 Variable Group Assignment
  Var_Group <- cutree(hc, k = variable_k)
  mycolhc <- rainbow(length(unique(Var_Group)), start = 0.1, end = 0.9); 
  mycolhc <- mycolhc[as.vector(Var_Group)]
  
  # plot as line plot
  df_clust_ordered <- Data1[hr$order, hc$order]
  hr_ordered <- Sub_Group[hr$order]
  hc_ordered <- Var_Group[hc$order]
  df_clust_ordered <- cbind(df_clust_ordered, unname(hr_ordered))
  colnames(df_clust_ordered)[which(colnames(df_clust_ordered) == "")] <- "group"
  
  df_clust_scaled_means <- aggregate(df_clust_ordered, list(group = unname(hr_ordered)), mean)
  df_clust_scaled_means <- melt(df_clust_scaled_means, id.vars = "group")
  names(df_clust_scaled_means) <- c("group", "variable", "mean")
  
  df_clust_scaled_se <- aggregate(df_clust_ordered, list(group = unname(hr_ordered)), function(x) sd(x)/sqrt(length(x)))
  df_clust_scaled_se <- melt(df_clust_scaled_se, id.vars = "group")
  names(df_clust_scaled_se) <- c("group", "variable", "se")
  
  df_clust_scaled_all <- merge(df_clust_scaled_means, df_clust_scaled_se, by = c("group", "variable"))
  
  ggplot(df_clust_scaled_all, aes(x = variable, y = mean, color = as.factor(group), group = as.factor(group))) + 
    geom_point() + 
    geom_line() +
    geom_ribbon(aes(ymin = mean - se,
                    ymax = mean + se,
                    fill = as.factor(group)),
                alpha = 0.2) +
    #geom_vline(xintercept = vlines, colour = "#919d9d") +
    labs(title = "Comparing Means of Clustered Groups", x = "Measure", y = "Mean Scaled Score") +
    scale_color_manual(name = "Cluster", values = rainbow(length(unique(Sub_Group)), start=0.1, end=0.9)) + 
    scale_fill_discrete(guide = FALSE) +
    #scale_x_discrete(labels = c("GFTA Sounds-\nin-Words", "CTOPP Rapid\n Symbolic Naming",
    #                            "CTOPP Elision", "WIAT Pseudo-word\n Decoding",
    #                            "WIAT Spelling", "WIAT Word Reading",
    #                            "CTOPP Non-word\n Repetition", "CTOPP Blending\n Words",
    #                            "Verbal IQ")) +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 60, hjust = 1))
}

### Clustering Diagnostic Data
# Remove EID column and append as row name
EID <- df_clust_dx$EID
df_clust_dx <- df_clust_dx[,-1]
df_clust_dx$ADHD_I_1dx <- as.numeric(rowSums(df_clust_dx) == 1 & df_clust_dx$ADHD_I == 1)
df_clust_dx$ADHD_I[which(df_clust_dx$ADHD_I == 1 & df_clust_dx$ADHD_I_1dx == 1)] <- 0
df_clust_dx$ADHD_H_1dx <- as.numeric(rowSums(df_clust_dx) == 1 & df_clust_dx$ADHD_H == 1)
df_clust_dx$ADHD_H[which(df_clust_dx$ADHD_H == 1 & df_clust_dx$ADHD_H_1dx == 1)] <- 0
df_clust_dx$ADHD_C_1dx <- as.numeric(rowSums(df_clust_dx) == 1 & df_clust_dx$ADHD_C == 1)
df_clust_dx$ADHD_C[which(df_clust_dx$ADHD_C == 1 & df_clust_dx$ADHD_C_1dx == 1)] <- 0
df_clust_dx$ASD_1dx <- as.numeric(rowSums(df_clust_dx) == 1 & df_clust_dx$ASD == 1)
df_clust_dx$ASD[which(df_clust_dx$ASD == 1 & df_clust_dx$ASD_1dx == 1)] <- 0
df_clust_dx$DMDD_1dx <- as.numeric(rowSums(df_clust_dx) == 1 & df_clust_dx$DMDD == 1)
df_clust_dx$DMDD[which(df_clust_dx$DMDD == 1 & df_clust_dx$DMDD_1dx == 1)] <- 0
df_clust_dx$GAD_1dx <- as.numeric(rowSums(df_clust_dx) == 1 & df_clust_dx$GAD == 1)
df_clust_dx$GAD[which(df_clust_dx$GAD == 1 & df_clust_dx$GAD_1dx == 1)] <- 0
df_clust_dx$Learning_1dx <- as.numeric(rowSums(df_clust_dx) == 1 & df_clust_dx$Learning == 1)
df_clust_dx$Learning[which(df_clust_dx$Learning == 1 & df_clust_dx$Learning_1dx == 1)] <- 0
df_clust_dx$Anx_Soc_1dx <- as.numeric(rowSums(df_clust_dx) == 1 & df_clust_dx$Anx_Soc == 1)
df_clust_dx$Anx_Soc[which(df_clust_dx$Anx_Soc == 1 & df_clust_dx$Anx_Soc_1dx == 1)] <- 0
df_clust_dx$Depression_1dx <- as.numeric(rowSums(df_clust_dx) == 1 & df_clust_dx$Depression == 1)
df_clust_dx$Depression[which(df_clust_dx$Depression == 1 & df_clust_dx$Depression_1dx == 1)] <- 0
rownames(df_clust_dx) <- EID
# Cluster rows by 
r_dist <- dist(df_clust_dx, method = "binary")
hr <- hclust(r_dist, method="ward.D2")
# Cluster columns 
c_dist <- dist(t(df_clust_dx), method = "binary")
hc <- hclust(c_dist, method="ward.D2")

png("Plots/Bicluster-dx/Trees/row_clusters_dx.png")
plot(hr, labels = FALSE)
dev.off()

png("Plots/Bicluster-dx/Trees/col_clusters_dx.png")
plot(hc)
dev.off()

## Silhouette Values
silhouette_values <- data.frame(variables = character(), axis = character(), k = numeric(), avg_sil_width = numeric(),
                                med_sil_width = numeric(), min_sil_width = numeric(), max_sil_width = numeric(), 
                                mean_clust_size = numeric(), stringsAsFactors = FALSE)


for (k_row in c(2:10)) {
  
  r_dist <- dist(df_clust_dx, method = "binary")
  hr <- hclust(r_dist, method="ward.D2")
  Sub_Group <- cutree(hr, k = k_row)
  
  png(paste0("Plots/Bicluster-dx/Silhouette/silhouette_row_k", k_row, ".png"))
  sil_r <- silhouette(Sub_Group, r_dist)
  plot(sil_r)
  dev.off()
  
  sil_r_summary <- summary(sil_r)
  
  silhouette_values <- rbind(silhouette_values, 
                             data.frame(variables = "dx", 
                                        axis = "row", 
                                        k = k_row, 
                                        avg_sil_width = mean(sil_r[, "sil_width"]),
                                        med_sil_width = median(sil_r[, "sil_width"]),
                                        min_sil_width = min(sil_r[, "sil_width"]),
                                        max_sil_width = max(sil_r[, "sil_width"]),
                                        mean_clust_size = mean(sil_r_summary$clus.sizes)))
}

for (k_col in c(2:7)) {
  
  c_dist <- dist(t(df_clust_dx), method = "binary")
  hc <- hclust(c_dist, method="ward.D2")
  Var_Group <- cutree(hc, k = k_col)
  
  png(paste0("Plots/Bicluster-dx/Silhouette/silhouette_col_k", k_col, ".png"))
  sil_c <- silhouette(Var_Group, c_dist)
  plot(sil_c)
  dev.off()
  
  sil_c_summary <- summary(sil_c)
  
  silhouette_values <- rbind(silhouette_values, 
                             data.frame(variables = "dx", 
                                        axis = "col", 
                                        k = k_col, 
                                        avg_sil_width = mean(sil_c[, "sil_width"]),
                                        med_sil_width = median(sil_c[, "sil_width"]),
                                        min_sil_width = min(sil_c[, "sil_width"]),
                                        max_sil_width = max(sil_c[, "sil_width"]),
                                        mean_clust_size = mean(sil_c_summary$clus.sizes)))
}
  

silhouette_values <- silhouette_values[with(silhouette_values, order(variables, axis)), ]
ggplot(silhouette_values[,], aes(x = k, y = avg_sil_width)) + geom_line() + geom_point() + facet_grid(axis ~ ., scales = "free_y")
ggsave("Plots/Bicluster-dx/Silhouette/silhouette-widths.png")

# Heatmap: Group k = 2:10, variable k = 3
for (i in c(3:8)) {
  png(paste0("Plots/Bicluster-dx/Heatmap/Heatmap_rowk", i, ".png"))
  par(oma = c(10, 2, 0, 0))
  TwoWayclust(df_clust_dx, group_k = i, variable_k = 6, method = "binary")
  dev.off()
}

# Remove EID column and append as row name
rownames(df_clust_dx) <- df_clust_dx$EID
df_clust_dx <- df_clust_dx[,-1]
df_clust_dx <- df_clust_dx[rowSums(df_clust_dx) >= 2, ] # Look at participants with 2 or more diagnoses
df_clust_dx <- as.matrix(df_clust_dx)
# 3.1 Euclidean + Ward Clustering of Subjects
#r_dist <- dist(df_clust_dx, method = "binary")
r_dist <- daisy(df_clust_dx, metric = "gower")
hr <- hclust(r_dist, method = "ward.D2");
plot(hr)
#r_dist <- dist(Data1, method = "binary")
#hr <- hclust(r_dist, method = "complete");

# 3.2 Spearman + Complete Clustering of Variables
#c_dist <- dist(t(df_clust_dx), method = "binary")
c_dist <- daisy(t(df_clust_dx), metric = "gower")
hc <- hclust(c_dist, method = "ward.D2")
plot(hc)
#c_dist <- dist(t(Data1), method = "binary")
#hc <- hclust(c_dist, method = "complete")

# 4 Subject Group Assignment
Sub_Group <- cutree(hr, k = 4)
mycolhr <- rainbow(length(unique(Sub_Group)), start = 0.1, end = 0.9); 
mycolhr <- mycolhr[as.vector(Sub_Group)]

# 5 Variable Group Assignment
Var_Group <- cutree(hc, k = 3)
mycolhc <- rainbow(length(unique(Var_Group)), start = 0.1, end = 0.9); 
mycolhc <- mycolhc[as.vector(Var_Group)]

# 6 Visualization
h<-heatmap(as.matrix(df_clust_dx), Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = inferno(256), scale = "none", RowSideColors = mycolhr, ColSideColors = mycolhc)
h

# cluster_k_row_values <- list(all = c(3, 4),
#                          asmt = c(3, 5, 8),
#                          dx = c(3, 4))
# 
# cluster_k_col_values <- list(all = c(3, 4, 7),
#                              asmt = c(3, 4, 5),
#                              dx = c(3, 5, 6))

cluster_k_row_values <- list(all = c(3, 4, 5),
                             asmt = c(3, 4, 5))

cluster_k_col_values <- list(all = c(3, 4, 6),
                             asmt = c(3, 4, 5))


# cluster_k_row_values <- list(all = c(3, 5, 7),
#                              asmt = c(3, 5, 6))
# 
# cluster_k_col_values <- list(all = c(3, 4, 7),
#                              asmt = c(3, 4, 5))


#heights <- data.frame(fields = c("all", "asmt", "dx"), rows = c(5, 3, 7), cols = c(7, 3, 3), stringsAsFactors = FALSE)
heights <- data.frame(fields = names(dfs), rows = c(4, 3), cols = c(6, 4), stringsAsFactors = FALSE)

for (i in 1:length(dfs)) {
  df <- dfs[[i]]
  # Remove EID column and append as row name
  EID <- df$EID
  df <- df[,-1]
  # Centers and scales data.
  #df <- scale(df)
  #df <- apply(df, 2, znorm)
  df <- apply(df, 2, ranknorm)
  rownames(df) <- EID
  
  png(paste0("Plots/Bicluster-znorm/Heatmaps/heatmap_", heights[i, "fields"], ".png"))
  TwoWayclust(df, group_k = heights[i, "rows"], variable_k = heights[i, "cols"])
  dev.off()
  
  TwoWayclust_means(df, group_k = heights[i, "rows"], variable_k = heights[i, "cols"])
  ggsave(paste0("Plots/Bicluster-znorm/Means/bicluster_means_", heights[i, "fields"], ".png"))
}

#### K-Mode Clustering
categorical <- c("ADHD_I", "ADHD_H", "ADHD_C", "ASD", "DMDD", 
                 "GAD", "Learning", "Anx_Soc", "Depression")
df_clust <- read.csv("Data Cleaned/Biclustering_Data.csv", stringsAsFactors = FALSE)
df_clust_dx <- df_clust[, c(categorical)]
set.seed(1234)
df_clust_dx <- df_clust_dx[, c(sample(1:9))]

clust_df <- data.frame(nclust = numeric(), weighted = logical(), mean_dist = numeric(), stringsAsFactors = FALSE)
for (nclust in 2:10) {
  for (bool in c(TRUE, FALSE)) {
    clust <- kmodes(df_clust_dx[, 2:ncol(df_clust_dx)], nclust, weighted = bool)
    df_clust_dx_2 <- data.frame(df_clust_dx)
    df_clust_dx_2[, "cluster"] <- clust$cluster
    #df_clust_dx_2 <- df_clust_dx_2[order(df_clust_dx_2$cluster), ]
    df_clust_dx_2 <- df_clust_dx_2[with(df_clust_dx_2, order(cluster, ADHD_C, ADHD_I, ADHD_H, ASD, DMDD, GAD, Learning, Anx_Soc, Depression)), ]
    mycolhr <- rainbow(length(unique(df_clust_dx_2$cluster)), start = 0.1, end = 0.9); 
    mycolhr <- mycolhr[df_clust_dx_2$cluster]
    png(paste0("Plots/Bicluster-dx/Heatmap-kmode/Heatmap-kmode_nclust", nclust, "_", ifelse(bool, "weighted", "unweighted"),".png"))
    heatmap(as.matrix(df_clust_dx_2[, names(df_clust_dx_2) != "cluster"]), Rowv = NA, Colv = NA, col = inferno(256), scale = "none", RowSideColors = mycolhr)
    dev.off()
  }
}

#### Hierarchical Clustering using Gower Distance
# Cluster rows by 
r_dist <- daisy(df_clust_dx, metric = "gower")
hr <- hclust(r_dist, method="ward.D2")
# Cluster columns 
c_dist <- daisy(t(df_clust_dx), metric = "gower")
hc <- hclust(c_dist, method="ward.D2")

png("Plots/Bicluster-dx/Trees-gower/row_clusters_dx.png")
plot(hr, labels = FALSE)
dev.off()

png("Plots/Bicluster-dx/Trees-gower/col_clusters_dx.png")
plot(hc)
dev.off()

## Silhouette Values
silhouette_values_gower <- data.frame(variables = character(), axis = character(), k = numeric(), avg_sil_width = numeric(),
                                med_sil_width = numeric(), min_sil_width = numeric(), max_sil_width = numeric(), 
                                mean_clust_size = numeric(), stringsAsFactors = FALSE)


for (k_row in c(2:10)) {
  
  r_dist <- daisy(df_clust_dx, metric = "gower")
  hr <- hclust(r_dist, method="ward.D2")
  Sub_Group <- cutree(hr, k = k_row)
  
  # png(paste0("Plots/Bicluster-dx/Silhouette/silhouette_row_k", k_row, ".png"))
  sil_r <- silhouette(Sub_Group, r_dist)
  # plot(sil_r)
  # dev.off()
  
  sil_r_summary <- summary(sil_r)
  
  silhouette_values_gower <- rbind(silhouette_values_gower, 
                             data.frame(variables = "dx", 
                                        axis = "row", 
                                        k = k_row, 
                                        avg_sil_width = mean(sil_r[, "sil_width"]),
                                        med_sil_width = median(sil_r[, "sil_width"]),
                                        min_sil_width = min(sil_r[, "sil_width"]),
                                        max_sil_width = max(sil_r[, "sil_width"]),
                                        mean_clust_size = mean(sil_r_summary$clus.sizes)))
}

for (k_col in c(2:7)) {
  
  c_dist <- daisy(t(df_clust_dx), metric = "gower")
  hc <- hclust(c_dist, method="ward.D2")
  Var_Group <- cutree(hc, k = k_col)
  
  # png(paste0("Plots/Bicluster-dx/Silhouette/silhouette_col_k", k_col, ".png"))
  sil_c <- silhouette(Var_Group, c_dist)
  # plot(sil_c)
  # dev.off()
  
  sil_c_summary <- summary(sil_c)
  
  silhouette_values_gower <- rbind(silhouette_values_gower, 
                             data.frame(variables = "dx", 
                                        axis = "col", 
                                        k = k_col, 
                                        avg_sil_width = mean(sil_c[, "sil_width"]),
                                        med_sil_width = median(sil_c[, "sil_width"]),
                                        min_sil_width = min(sil_c[, "sil_width"]),
                                        max_sil_width = max(sil_c[, "sil_width"]),
                                        mean_clust_size = mean(sil_c_summary$clus.sizes)))
}


silhouette_values_gower <- silhouette_values_gower[with(silhouette_values_gower, order(variables, axis)), ]
ggplot(silhouette_values_gower[,], aes(x = k, y = avg_sil_width)) + geom_line() + facet_grid(axis ~ ., scales = "free_y")
ggsave("Plots/Bicluster-dx/Silhouette-gower/silhouette-widths.png")

# Heatmap: Group k = 2:10, variable k = 5
for (i in c(2:10)) {
  png(paste0("Plots/Bicluster-dx/Heatmap-gower/Heatmap_rowk", i, ".png"))
  TwoWayclust(df_clust_dx, group_k = i, variable_k = 5, gower = TRUE)
  dev.off()
}

### Dimensional Data
dimensional <- c("CBCL_Int_T", "CBCL_Ext_T", "MFQ_P_Total_AgeSex_Ctrl", 
                 "MFQ_SR_Total_AgeSex_Ctrl", "SCARED_P_Total_AgeSex_Ctrl", "SCARED_SR_Total_AgeSex_Ctrl", 
                 "ARI_P_Total_Score_AgeSex_Ctrl", "ARI_S_Total_Score_AgeSex_Ctrl", "SWAN_IN_AgeSex_Ctrl", 
                 "SWAN_HY_AgeSex_Ctrl", "ASD_latent_factor", "PC1")

df_clust_dim <- df_clust[, c(dimensional)]
df_clust_dim_noPC1 <- df_clust[, c("EID", dimensional[dimensional != "PC1"])]

clust_df <- data.frame(nclust = numeric(), weighted = logical(), mean_dist = numeric(), stringsAsFactors = FALSE)

df_clust_dim_scaled <- scale(df_clust_dim)
#df_clust_dim_scaled <- apply(df_clust_dim, 2, znorm)
# Cluster rows by 
r_dist <- dist(df_clust_dim_scaled, method = "euclidean")
hr <- hclust(r_dist, method="ward.D2")
# Cluster columns 
c_dist <- dist(t(df_clust_dim_scaled), method = "euclidean")
hc <- hclust(c_dist, method="ward.D2")

png("Plots/Bicluster-dim/Trees/row_clusters_dx.png")
plot(hr, labels = FALSE)
dev.off()

png("Plots/Bicluster-dim/Trees/col_clusters_dx.png")
plot(hc)
dev.off()

## Silhouette Values
silhouette_values <- data.frame(variables = character(), axis = character(), k = numeric(), avg_sil_width = numeric(),
                                med_sil_width = numeric(), min_sil_width = numeric(), max_sil_width = numeric(), 
                                mean_clust_size = numeric(), stringsAsFactors = FALSE)
for (k_row in c(2:10)) {
  
  df_clust_dim_scaled <- scale(df_clust_dim)
  #df_clust_dim_scaled <- apply(df_clust_dim, 2, znorm)
  r_dist <- dist(df_clust_dim_scaled, method = "euclidean")
  hr <- hclust(r_dist, method="ward.D2")
  Sub_Group <- cutree(hr, k = k_row)
  
  # png(paste0("Plots/Bicluster-dim/Silhouette/silhouette_row_k", k_row, ".png"))
  sil_r <- silhouette(Sub_Group, r_dist)
  # plot(sil_r)
  # dev.off()
  
  sil_r_summary <- summary(sil_r)
  
  silhouette_values <- rbind(silhouette_values, 
                             data.frame(variables = "dim", 
                                        axis = "row", 
                                        k = k_row, 
                                        avg_sil_width = mean(sil_r[, "sil_width"]),
                                        med_sil_width = median(sil_r[, "sil_width"]),
                                        min_sil_width = min(sil_r[, "sil_width"]),
                                        max_sil_width = max(sil_r[, "sil_width"]),
                                        mean_clust_size = mean(sil_r_summary$clus.sizes)))
}

for (k_col in c(2:7)) {
  
  df_clust_dim_scaled <- scale(df_clust_dim)  
  #df_clust_dim_scaled <- apply(df_clust_dim, 2, znorm)
  c_dist <- dist(t(df_clust_dim_scaled), method = "euclidean")
  hc <- hclust(c_dist, method="ward.D2")
  Var_Group <- cutree(hc, k = k_col)
  
  # png(paste0("Plots/Bicluster-dx/Silhouette/silhouette_col_k", k_col, ".png"))
  sil_c <- silhouette(Var_Group, c_dist)
  # plot(sil_c)
  # dev.off()
  
  sil_c_summary <- summary(sil_c)
  
  silhouette_values <- rbind(silhouette_values, 
                             data.frame(variables = "dim", 
                                        axis = "col", 
                                        k = k_col, 
                                        avg_sil_width = mean(sil_c[, "sil_width"]),
                                        med_sil_width = median(sil_c[, "sil_width"]),
                                        min_sil_width = min(sil_c[, "sil_width"]),
                                        max_sil_width = max(sil_c[, "sil_width"]),
                                        mean_clust_size = mean(sil_c_summary$clus.sizes)))
}


silhouette_values <- silhouette_values[with(silhouette_values, order(variables, axis)), ]
ggplot(silhouette_values[,], aes(x = k, y = avg_sil_width)) + geom_line() + facet_grid(axis ~ ., scales = "free_y")
ggsave("Plots/Bicluster-dim/Silhouette/silhouette-widths.png")

# Heatmap: Group k = 2:10, variable k = 5
for (i in c(2:10)) {
  png(paste0("Plots/Bicluster-dim/Heatmap/Heatmap_rowk", i, ".png"))
  par(oma = c(10, 2, 0, 0))
  TwoWayclust(df_clust_dim_scaled, group_k = i, variable_k = 3, method = "euclidean")
  dev.off()
}
for (i in c(2:10)) {
  TwoWayclust_means(df_clust_dim_scaled, group_k = i, variable_k = 3, method = "euclidean")
  ggsave(paste0("Plots/Bicluster-dim/Heatmap-means/Heatmap_rowk", i, ".png"))
}

### Cluster dimensional data without PC1
df_clust_dim_noPC1 <- df_clust[, c(dimensional[dimensional != "PC1"])]

clust_df <- data.frame(nclust = numeric(), weighted = logical(), mean_dist = numeric(), stringsAsFactors = FALSE)

df_clust_dim_noPC1_scaled <- scale(df_clust_dim_noPC1)
#df_clust_dim_scaled <- apply(df_clust_dim, 2, znorm)
# Cluster rows by 
r_dist <- dist(df_clust_dim_noPC1_scaled, method = "euclidean")
hr <- hclust(r_dist, method="ward.D2")
# Cluster columns 
c_dist <- dist(t(df_clust_dim_noPC1_scaled), method = "euclidean")
hc <- hclust(c_dist, method="ward.D2")

png("Plots/Bicluster-dim-noPC1/Trees/row_clusters_dx.png")
plot(hr, labels = FALSE)
dev.off()

png("Plots/Bicluster-dim-noPC1/Trees/col_clusters_dx.png")
plot(hc)
dev.off()

## Silhouette Values
silhouette_values <- data.frame(variables = character(), axis = character(), k = numeric(), avg_sil_width = numeric(),
                                med_sil_width = numeric(), min_sil_width = numeric(), max_sil_width = numeric(), 
                                mean_clust_size = numeric(), stringsAsFactors = FALSE)
for (k_row in c(2:10)) {
  
  df_clust_dim_noPC1_scaled <- scale(df_clust_dim_noPC1)
  #df_clust_dim_noPC1_scaled <- apply(df_clust_dim_noPC1, 2, znorm)
  r_dist <- dist(df_clust_dim_noPC1_scaled, method = "euclidean")
  hr <- hclust(r_dist, method="ward.D2")
  Sub_Group <- cutree(hr, k = k_row)
  
  # png(paste0("Plots/Bicluster-dim/Silhouette/silhouette_row_k", k_row, ".png"))
  sil_r <- silhouette(Sub_Group, r_dist)
  # plot(sil_r)
  # dev.off()
  
  sil_r_summary <- summary(sil_r)
  
  silhouette_values <- rbind(silhouette_values, 
                             data.frame(variables = "dim", 
                                        axis = "row", 
                                        k = k_row, 
                                        avg_sil_width = mean(sil_r[, "sil_width"]),
                                        med_sil_width = median(sil_r[, "sil_width"]),
                                        min_sil_width = min(sil_r[, "sil_width"]),
                                        max_sil_width = max(sil_r[, "sil_width"]),
                                        mean_clust_size = mean(sil_r_summary$clus.sizes)))
}

for (k_col in c(2:7)) {
  
  df_clust_dim_noPC1_scaled <- scale(df_clust_dim_noPC1)  
  #df_clust_dim_noPC1_scaled <- apply(df_clust_dim_noPC1, 2, znorm)
  c_dist <- dist(t(df_clust_dim_noPC1_scaled), method = "euclidean")
  hc <- hclust(c_dist, method="ward.D2")
  Var_Group <- cutree(hc, k = k_col)
  
  # png(paste0("Plots/Bicluster-dx/Silhouette/silhouette_col_k", k_col, ".png"))
  sil_c <- silhouette(Var_Group, c_dist)
  # plot(sil_c)
  # dev.off()
  
  sil_c_summary <- summary(sil_c)
  
  silhouette_values <- rbind(silhouette_values, 
                             data.frame(variables = "dim", 
                                        axis = "col", 
                                        k = k_col, 
                                        avg_sil_width = mean(sil_c[, "sil_width"]),
                                        med_sil_width = median(sil_c[, "sil_width"]),
                                        min_sil_width = min(sil_c[, "sil_width"]),
                                        max_sil_width = max(sil_c[, "sil_width"]),
                                        mean_clust_size = mean(sil_c_summary$clus.sizes)))
}


silhouette_values <- silhouette_values[with(silhouette_values, order(variables, axis)), ]
ggplot(silhouette_values[,], aes(x = k, y = avg_sil_width)) + geom_line() + facet_grid(axis ~ ., scales = "free_y")
ggsave("Plots/Bicluster-dim-noPC1/Silhouette/silhouette-widths.png")

# Heatmap: Group k = 2:10, variable k = 5
for (i in c(2:10)) {
  png(paste0("Plots/Bicluster-dim-noPC1/Heatmap/Heatmap_rowk", i, ".png"))
  par(oma = c(10, 2, 0, 0))
  TwoWayclust(df_clust_dim_noPC1_scaled, group_k = i, variable_k = 3, method = "euclidean")
  dev.off()
}
for (i in c(2:10)) {
  TwoWayclust_means(df_clust_dim_noPC1_scaled, group_k = i, variable_k = 3, method = "euclidean")
  ggsave(paste0("Plots/Bicluster-dim-noPC1/Heatmap-means/Heatmap_rowk", i, ".png"))
}
