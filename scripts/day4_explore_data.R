library(tidyverse)

## convert data into matrix format

pca_matrix <- trans_cts %>%
  column_to_rownames("gene") %>% 
  as.matrix() %>%     # converts our dataframe to a matrix
  t()       # t is to transpose matrix

sample_pca <- prcomp(pca_matrix)
str(sample_pca)
summary(sample_pca)
class(sample_pca)

pca_matrix[1:10, 1:5]
as_tibble(pca_matrix)
as_tibble(pca_matrix, rownames = "sample")

pc_eigenvalues <- sample_pca$sdev^2  #calculate standard deviation and square it to geht our eigenvalues

pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)),        #shortcut is tidy(sample_pca, matrix = "eigenvalues")
                         variance = pc_eigenvalues) %>% 
  mutate(pct = variance/sum(variance)*100) %>% 
  mutate(pct_cum = cumsum(pct))

#create a pareto plot/chart
pc_eigenvalues %>% 
  ggplot(aes(x = PC))+
  geom_col(aes(y = pct))+
  geom_line(aes(y = pct_cum, group = 1))+
  geom_point(aes(y = pct_cum))+
  labs(x = "Principal component", y= "Fraction variance explained")


pc_scores <- sample_pca$x %>% 
  as_tibble(rownames = ("sample"))

pc_scores %>% 
  ggplot(aes(x = PC1, y = PC2))+
  geom_point()


pc_scores %>% 
  full_join(sample_info, by ="sample")

pca_plot <- pc_scores %>% 
  full_join(sample_info, by="sample") %>% 
  ggplot(aes(x = PC1, y = PC2, color = factor(minute), shape = strain))+
  geom_point()

pc_loadings <- sample_pca$rotation %>% 
  as_tibble(rownames = "gene")

top_genes <- pc_loadings %>% 
  select(gene, PC1, PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
  group_by(PC) %>% 
  arrange(desc(abs(loading))) %>% 
  slice(1:10) %>% 
  pull(gene) %>% 
  unique()

top_loadings <- pc_loadings %>% 
  filter(gene %in% top_genes)


loadings_plot <- ggplot(data = top_loadings)+
    geom_segment(aes(x = 0, y= 0, xend = PC1, yend = PC2), #geomsegment is painting the arrowa
                 arrow = arrow(length = unit(0.1, "in")),
                 color = "brown")+
    geom_text(aes(x= PC1, y= PC2, label = gene),  #geomtext is labeling the arrows
              nudge_y = 0.005, size = 3)+
    scale_x_continuous(expand = c(0.02, 0.02))

library(patchwork)

(pca_plot | loadings_plot) +
  plot_annotation(tag_levels ="A")

library(ggfortify)
autoplot(sample_pca)

autoplot(sample_pca, data = sample_info, color = "minute", shape = "strain")

library(broom)
tidy(sample_pca, matrix = "eigenvalues")

tidy(sample_pca, matrix = "loadings")

#Day5

autoplot(sample_pca, data = sample_info %>% 
           mutate(minute =as.factor(minute)),
         color = "minute",
         shape = "strain")


#Differential expression results

test_result

#gene column -> gene name
# baseMean column -> normalized expression level of a gene
# log2Foldchange column -> amount of change between 2 conditions (between 2 timepoints in this case)
# lfcSE column -> standard error associated to log2foldchange value
# stat column -> statistics value computed as log2foldchange/lfcSE compared to standard normal distribution
# pvalue -> p-value associated with change
# padj -> p-value corrected for multiple hypothesis testing
# comparison -> comparison group

# MA plot
# challenge: generate MA plot (baseMean vs log2FoldChange)
# organize panels by comparison (time point)
#Hint: consider log - transform baseMean




test_result %>% 
  ggplot(aes(x = log10(baseMean), y = log2FoldChange))+
  geom_point(alpha = 0.1)+
  facet_wrap(facets = vars(comparison))         #facet_wrap splits up into the different timepoints -> comparison column

ma_plot <- test_result %>% 
  mutate(sig = ifelse(padj < 0.01, log2FoldChange, NA)) %>% 
  ggplot(aes(x= log10(baseMean), y= log2FoldChange))+
  geom_point(alpha = 0.1)+
  geom_point(aes(y = sig), color = "tomato", size = 1)+
  geom_hline(yintercept = 0, color = "dodgerblue")+
  facet_wrap(facets = vars(comparison))


#visualizing expression trends
# 1. get candidate genes (aka padj < 0.01)

candidate_gene <- test_result %>% 
  filter(padj<0.01) %>% 
  pull(gene) %>%     # extract a column and turn it into a vector similar to: test_result [,"gene] or test_result$gene
  unique()


# 1a get the trans_cts table in long format (haben wir schon gestern gemacht, soll nur zur Wdh hier sein)
trans_cts <- trans_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3, names_to = "sample", values_to = "cts") %>% 
  full_join(sample_info, by = "sample")


# 2. Filter trans_cts_long for candidate genes and compute mean expression value for each gene in each tp and each genotype
trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_gene) %>% 
  group_by(gene, strain, minute) %>% 
  summarise(mean_cts = mean(cts), nrep = n()) %>% 
  ungroup()

# 3. Plot trends
trans_cts_mean %>% 
  ggplot(aes(x = minute, y= mean_cts))+
  geom_line(aes(group = gene), alpha = 0.3)+
  facet_grid(rows = vars(strain))

#4. Scaling data to improve visualization
trans_cts_mean <- trans_cts_long %>% 
  filter (gene %in% candidate_gene) %>% 
  group_by(gene) %>% 
  mutate(cts_scaled = (cts -mean(cts))/sd(cts)) %>% 
  group_by(gene, strain, minute) %>% 
  summarise(mean_cts_scaled = mean(cts_scaled), nrep= n()) %>% 
  ungroup()

trans_cts_mean %>% 
  ggplot(aes(x=minute, y= mean_cts_scaled))+
  geom_line(aes(group = gene), alpha = 0.3)+
  geom_hline(yintercept = 0, color ="brown", linetype = "dashed")+
  facet_grid(rows = vars(strain))

# scale_x_continuous(breaks = unique(trans_cts_mean$minute)), so dass die Minuten so dargestellt werden mit den
# fürs Experiment relevanten Zeitpunkten

trans_cts_mean %>% 
  ggplot(aes(x=minute, y= mean_cts_scaled))+
  geom_line(aes(group = gene), alpha = 0.3)+
  geom_hline(yintercept = 0, color ="brown", linetype = "dashed")+
  facet_grid(rows = vars(strain))+
  scale_x_continuous(breaks = unique(trans_cts_mean$minute))

#Clustering

trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")

# 1. Create a matrix of counts
hclust_matrix <- trans_cts %>% 
  select(-gene) %>% 
  as.matrix()

#assign rownames
rownames(hclust_matrix) <- trans_cts$gene

hclust_matrix <- hclust_matrix [candidate_gene, ]

#transpose the matrix so that genes are as columns
hclust_matrix <- hclust_matrix %>% 
  t() %>% 
  scale() %>%   #aplly scaling to eah column of the matrix(genes), scale calculates the mean and calculates how far away the sample avlue for each gene goes from the mean
  t()           #transpose back so genes are as rows again        

gene_dist <- dist(hclust_matrix)

#hierarchical clustering

gene_hclust <- hclust(gene_dist, method = "complete")
plot(gene_hclust, labels =F)
abline(h = 10, color = "brown", lwd =2)


# make clusters based on the number that I want

cutree(gene_hclust, k = 5)

gene_cluster <- cutree(gene_hclust, k = 5) %>% 
  enframe() %>% 
  rename(gene = name, cluster = value)

trans_cts_cluster <- trans_cts_mean %>% 
  inner_join(gene_cluster, by = "gene")

trans_cts_cluster %>% 
  ggplot(aes(x=minute, y = mean_cts_scaled))+
  geom_line(aes(group = gene))+
  facet_grid(cols = vars(cluster), rows = vars(strain))


library(ComplexHeatmap)

Heatmap(hclust_matrix, show_row_names = F)




  
