library(ggpubr)
df <-read.table("Riskscore_comparison.txt",head=T,sep='\t',check.names = F,row.names = 1)
my_comparisons1 <- list( c("<=65", ">65") )
p1 <- ggboxplot(df, x = "age", y = "riskscore",color = "age", palette ='Set1',add = "jitter", shape = "age")
p1 <- p1 + stat_compare_means(comparisons = my_comparisons1)+ # Add pairwise comparisons p-value
stat_compare_means(label.y = 6)

p2 <- ggboxplot(df, x = "gender", y = "riskscore",color = "gender", palette ='Set1',add = "jitter", shape = "gender")
my_comparisons2 <- list( c("Male", "Female") )
p2 <- p2 + stat_compare_means(comparisons = my_comparisons2)+ stat_compare_means(label.y = 6)

p3 <- ggboxplot(df, x = "grade", y = "riskscore",color = "grade", palette ='Set1',add = "jitter", shape = "grade")
my_comparisons3 <- list( c("G1-2", "G3-4") )
p3 <- p3 + stat_compare_means(comparisons = my_comparisons3)+ stat_compare_means(label.y = 6)

p4 <- ggboxplot(df, x = "stage", y = "riskscore",color = "stage", palette ='Set1',add = "jitter", shape = "stage")
my_comparisons4 <- list( c("Stage I-II", "Stage III-IV") )
p4 <- p4 + stat_compare_means(comparisons = my_comparisons4)+ stat_compare_means(label.y = 6)
library(cowplot)
plot_grid(p1, p2,p3,p4, labels = c('A', 'B',"C","D"), label_size = 12)
ggsave("Riskscore_comparison.pdf")