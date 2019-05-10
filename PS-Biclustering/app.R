#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(psych)
library(viridis)
#library(fmsb)
library(cluster)
library(ggplot2)
library(reshape)

# To-do: Render table with distribution of cluster age and sex
#        Render plot with cluster dx distribution

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

df_clust <- read.csv("R:/Data June 2018/Data Cleaned/Biclustering_Data.csv", stringsAsFactors = FALSE)
demos <- read.csv("R:/Data June 2018/Data Cleaned/Basic_Demos.csv", stringsAsFactors = FALSE)
demos <- demos[demos$EID %in% df_clust$EID, c("EID", "Sex", "Age")]
row.names(df_clust) <- df_clust$EID
df_clust$EID <- NULL
df_clust_dim <- df_clust[, c(dimensional)]
df_clust_dim_noPC1 <- df_clust[, c(dimensional[dimensional != "PC1"])]
df_clust_dx <- df_clust[, c(categorical_short)]

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

# MergeClusterData
# Get clusters from Data1 based on group-k, and merge with Data2 (e.g. Demographic data)
# Parameters:
#   Data1: dataframe with clustering data
#   Data2: dataframe to be merged with data (needs EID)
#   group_k: k-value for group clustering (default: 3)
#   method: distance method used in dist function (default: "euclidean")
mergeClusterData <- function(Data1, Data2, group_k = 3, method = "euclidean") {
  r_dist <- dist(Data1, method = method)
  hr <- hclust(r_dist, method="ward.D2")
  Sub_Group <- data.frame(EID = names(cutree(hr, k = group_k)), 
                          clust = cutree(hr, k = group_k))
  mycolhr <- rainbow(length(unique(Sub_Group$clust)), start = 0.1, end = 0.9); 
  mycolhr <- mycolhr[as.vector(Sub_Group$clust)]
  Sub_Group$color <- mycolhr
  if (is.null(Data2$EID)) {
    Data2$EID <- row.names(Data2)
  }
  demos <- merge(Data2, Sub_Group, by = "EID")
  demos
}

## Silhouette Values - Dx
silhouette_values_dx <- data.frame(variables = character(), axis = character(), k = numeric(), avg_sil_width = numeric(),
                                med_sil_width = numeric(), min_sil_width = numeric(), max_sil_width = numeric(), 
                                mean_clust_size = numeric(), stringsAsFactors = FALSE)


for (k_row in c(2:10)) {
  
  r_dist <- dist(df_clust_dx, method = "binary")
  hr <- hclust(r_dist, method="ward.D2")
  Sub_Group <- cutree(hr, k = k_row)
  
  sil_r <- silhouette(Sub_Group, r_dist)
  
  sil_r_summary <- summary(sil_r)
  
  silhouette_values_dx <- rbind(silhouette_values_dx, 
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
  
  sil_c <- silhouette(Var_Group, c_dist)
  
  sil_c_summary <- summary(sil_c)
  
  silhouette_values_dx <- rbind(silhouette_values_dx, 
                             data.frame(variables = "dx", 
                                        axis = "col", 
                                        k = k_col, 
                                        avg_sil_width = mean(sil_c[, "sil_width"]),
                                        med_sil_width = median(sil_c[, "sil_width"]),
                                        min_sil_width = min(sil_c[, "sil_width"]),
                                        max_sil_width = max(sil_c[, "sil_width"]),
                                        mean_clust_size = mean(sil_c_summary$clus.sizes)))
}


silhouette_values_dx <- silhouette_values_dx[with(silhouette_values_dx, order(variables, axis)), ]
silhouette_values_dx$axis <- factor(silhouette_values_dx$axis)
levels(silhouette_values_dx$axis) <- c("Subjects", "Variables")


## Silhouette Values - Dimensional
silhouette_values_dim <- data.frame(variables = character(), axis = character(), k = numeric(), avg_sil_width = numeric(),
                                   med_sil_width = numeric(), min_sil_width = numeric(), max_sil_width = numeric(),
                                   mean_clust_size = numeric(), stringsAsFactors = FALSE)
df_clust_dim_scaled <- scale(df_clust_dim)

for (k_row in c(2:10)) {

  r_dist <- dist(df_clust_dim_scaled, method = "euclidean")
  hr <- hclust(r_dist, method="ward.D2")
  Sub_Group <- cutree(hr, k = k_row)

  sil_r <- silhouette(Sub_Group, r_dist)

  sil_r_summary <- summary(sil_r)

  silhouette_values_dim <- rbind(silhouette_values_dim,
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
  
  c_dist <- dist(t(df_clust_dim_scaled), method = "euclidean")
  hc <- hclust(c_dist, method="ward.D2")
  Var_Group <- cutree(hc, k = k_col)

  sil_c <- silhouette(Var_Group, c_dist)

  sil_c_summary <- summary(sil_c)

  silhouette_values_dim <- rbind(silhouette_values_dim,
                                data.frame(variables = "dx",
                                           axis = "col",
                                           k = k_col,
                                           avg_sil_width = mean(sil_c[, "sil_width"]),
                                           med_sil_width = median(sil_c[, "sil_width"]),
                                           min_sil_width = min(sil_c[, "sil_width"]),
                                           max_sil_width = max(sil_c[, "sil_width"]),
                                           mean_clust_size = mean(sil_c_summary$clus.sizes)))
}


silhouette_values_dim <- silhouette_values_dim[with(silhouette_values_dim, order(variables, axis)), ]
silhouette_values_dim$axis <- factor(silhouette_values_dim$axis)
levels(silhouette_values_dim$axis) <- c("Subjects", "Variables")

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    "HBN Processing Speed - Hierarchical Clustering",
    tabPanel("Diagnostic Data",
             # Sidebar with slider inputs for number of clusters
             sidebarLayout(
                sidebarPanel(
                  sliderInput("nclustSubDx",
                              "Number of subject clusters:",
                              min = 2,
                              max = 10,
                              value = 5),
                  sliderInput("nclustVarDx",
                              "Number of variable clusters:",
                              min = 2,
                              max = ncol(df_clust_dx),
                              step = 1,
                              value = 6)
                ),

                # Show a plot of the generated distribution
                mainPanel(
                  h1("Biclustering - Diagnostic Data"),
                  h3("Biclustering Heatmap"),
                  plotOutput("heatmapDx"),
                  h3("Cluster Profiles"),
                  tableOutput("clusterDemosDx"),
                  plotOutput("clusterAgeDx"),
                  plotOutput("clusterSexDx"),
                  plotOutput("clusterPC1Dx"),
                  h3("Mean silhouette values"),
                  plotOutput("silhouettePlotDx")
                )
             )),
    tabPanel("Dimensional Data",
             # Sidebar with slider inputs for number of clusters
             sidebarLayout(
               sidebarPanel(
                 sliderInput("nclustSubDim",
                             "Number of subject clusters:",
                             min = 2,
                             max = 10,
                             value = 5),
                 sliderInput("nclustVarDim",
                             "Number of variable clusters:",
                             min = 2,
                             max = 10,
                             step = 1,
                             value = 6)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 h1("Biclustering - Dimensional Data"),
                 h3("Biclustering Heatmap"),
                 plotOutput("heatmapDim"),
                 h3("Cluster Profiles"),
                 tableOutput("clusterDemosDim"),
                 plotOutput("clusterAgeDim"),
                 plotOutput("clusterSexDim"),
                 plotOutput("clusterDxDim"),
                 plotOutput("clusterPC1Dim"),
                 h3("Mean silhouette values"),
                 plotOutput("silhouettePlotDim")
               )
             ))
   )
)

# Define server logic required to draw plots
server <- function(input, output) {
  # Diagnostic Plots
  output$silhouettePlotDx <- renderPlot({
    ggplot(silhouette_values_dx[,], aes(x = k, y = avg_sil_width)) + 
      geom_line() + 
      geom_point() + 
      facet_grid(axis ~ ., scales = "free_y") +
      labs(y = "Mean Silhouette Width")
  })
  
  output$heatmapDx <- renderPlot({
      # generate number of clusters based on input$nclust from ui.R
      par(oma = c(2, 2, 0, 0))
      TwoWayclust(df_clust_dx, 
                  group_k = input$nclustSubDx, 
                  variable_k = input$nclustVarDx, 
                  method = "binary",
                  gower = FALSE)
  })
  
  output$clusterAgeDx <- renderPlot({
    cluster_demos <- mergeClusterData(df_clust_dx, demos, 
                                      group_k = input$nclustSubDx, 
                                      method = "binary")
    ggplot(cluster_demos, aes(x = factor(clust), y = Age, fill = factor(clust))) + 
      geom_boxplot() + 
      labs(x = "cluster") +
      scale_fill_manual(values = rainbow(length(unique(cluster_demos$color)), 
                                         start=0.1, end=0.9), guide = FALSE)
  })
  
  
  output$clusterSexDx <- renderPlot({
    cluster_demos <- mergeClusterData(df_clust_dx, demos, 
                                      group_k = input$nclustSubDx,
                                      method = "binary")
    
    ggplot(cluster_demos, aes(x = clust, fill = factor(Sex))) +
      geom_bar(aes(fill = factor(Sex)), position = "dodge", stat = "count") +
      labs(x = "Sex", y = "Frequency") + 
      scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("#0067a0", "#ea234b"))
  })
  
  output$clusterDemosDx <- renderTable({
    cluster_demos <- mergeClusterData(df_clust_dx, demos, 
                                      group_k = input$nclustSubDx,
                                      method = "binary")
    cluster_demos_dist <- data.frame(cluster = numeric(), age = character(), n_male = numeric(), n_female = numeric(),
                                     n_total = numeric(), stringsAsFactors = FALSE)
    for (i in 1:input$nclustSubDx) {
      mean_age <- round(mean(cluster_demos$Age[which(cluster_demos$clust == i)]), 2)
      sd_age <- round(sd(cluster_demos$Age[which(cluster_demos$clust == i)]), 2)
      n_male <- sum(cluster_demos$Sex[which(cluster_demos$clust == i)] == 0)
      n_female <- sum(cluster_demos$Sex[which(cluster_demos$clust == i)] == 1)
      n <- length(which(cluster_demos$clust == i))
      row <- data.frame(cluster = i, age = paste0(mean_age, " +/- ", sd_age), 
                        n_male = n_male, n_female = n_female,
                        n_total = n)
      cluster_demos_dist <- rbind(cluster_demos_dist, row)
      remove(row)
    }
    
    cluster_demos_dist
  })
  
  output$clusterPC1Dx <- renderPlot({
    cluster_demos <- mergeClusterData(df_clust_dx, df_clust_dim, 
                                      group_k = input$nclustSubDx,
                                      method = "binary")
    
    ggplot(cluster_demos, aes(x = factor(clust), y = PC1, fill = factor(clust))) + 
      geom_boxplot() + 
      labs(x = "cluster") +
      scale_fill_manual(values = rainbow(length(unique(cluster_demos$color)), 
                                         start=0.1, end=0.9), guide = FALSE)
  })
  
  # Dimensional Plots
  output$heatmapDim <- renderPlot({
    # generate number of clusters based on input$nclust from ui.R
    par(oma = c(8, 2, 0, 0))
    TwoWayclust(df_clust_dim_scaled, 
                group_k = input$nclustSubDim, 
                variable_k = input$nclustVarDim, 
                method = "euclidean",
                gower = FALSE)
  })
  output$silhouettePlotDim <- renderPlot({
    ggplot(silhouette_values_dim[,], aes(x = k, y = avg_sil_width)) + 
      geom_line() + 
      geom_point() + 
      facet_grid(axis ~ ., scales = "free_y") +
      labs(y = "Mean Silhouette Width")
  })
  
  
  output$clusterAgeDim <- renderPlot({
    cluster_demos <- mergeClusterData(df_clust_dim, demos, 
                                     group_k = input$nclustSubDim, 
                                     method = "euclidean")
    ggplot(cluster_demos, aes(x = factor(clust), y = Age, fill = factor(clust))) + 
      geom_boxplot() + 
      labs(x = "cluster") +
      scale_fill_manual(values = rainbow(length(unique(cluster_demos$color)), 
                                         start=0.1, end=0.9), guide = FALSE)
  })
  
  
  output$clusterSexDim <- renderPlot({
    cluster_demos <- mergeClusterData(df_clust_dim, demos, 
                                     group_k = input$nclustSubDim,
                                     method = "euclidean")

    ggplot(cluster_demos, aes(x = clust, fill = factor(Sex))) +
      geom_bar(aes(fill = factor(Sex)), position = "dodge", stat = "count") +
      labs(x = "Sex", y = "Frequency") + 
      scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("#0067a0", "#ea234b"))
  })
  
  output$clusterDemosDim <- renderTable({
    cluster_demos <- mergeClusterData(df_clust_dim, demos, 
                                      group_k = input$nclustSubDim,
                                      method = "euclidean")
    cluster_demos_dist <- data.frame(cluster = numeric(), age = character(), n_male = numeric(), n_female = numeric(),
                                        n_total = numeric(), stringsAsFactors = FALSE)
    for (i in 1:input$nclustSubDim) {
      mean_age <- round(mean(cluster_demos$Age[which(cluster_demos$clust == i)]), 2)
      sd_age <- round(sd(cluster_demos$Age[which(cluster_demos$clust == i)]), 2)
      n_male <- sum(cluster_demos$Sex[which(cluster_demos$clust == i)] == 0)
      n_female <- sum(cluster_demos$Sex[which(cluster_demos$clust == i)] == 1)
      n <- length(which(cluster_demos$clust == i))
      row <- data.frame(cluster = i, age = paste0(mean_age, " +/- ", sd_age), 
                        n_male = n_male, n_female = n_female,
                        n_total = n)
      cluster_demos_dist <- rbind(cluster_demos_dist, row)
      remove(row)
    }
    
    cluster_demos_dist
  })
  
  output$clusterPC1Dim <- renderPlot({
    cluster_demos <- mergeClusterData(df_clust_dim, df_clust_dim, 
                                      group_k = input$nclustSubDim,
                                      method = "euclidean")
    
    ggplot(cluster_demos, aes(x = factor(clust), y = PC1, fill = factor(clust))) + 
      geom_boxplot() + 
      labs(x = "cluster") +
      scale_fill_manual(values = rainbow(length(unique(cluster_demos$color)), 
                                         start=0.1, end=0.9), guide = FALSE)
  })
  
  output$clusterDxDim <- renderPlot({
    cluster_dx <- mergeClusterData(df_clust_dim, df_clust_dx, 
                                     group_k = input$nclustSubDim, 
                                     method = "euclidean")
    cluster_dx <- cluster_dx[, c(categorical_short, "clust")]
    cluster_dx$Ctrl <- as.numeric(rowSums(cluster_dx[, categorical_short]) == 0)
    cluster_dx_dist <- aggregate(cluster_dx[, c(categorical_short, "Ctrl")], 
                                 by = list(cluster = cluster_dx$clust), 
                                 FUN = sum)
    cluster_dx_dist_long <- melt(cluster_dx_dist[, c("cluster", categorical_short, "Ctrl")], 
                                  id.vars = "cluster",
                                  variable_name = "dx")
    ggplot(cluster_dx_dist_long, aes(x = cluster, y = value)) + 
      geom_bar(aes(fill = factor(dx)), position = "dodge", stat = "identity") +
      labs(y = "Frequency") + 
      scale_fill_discrete(name = "Diagnosis")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

