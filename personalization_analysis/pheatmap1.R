library(pheatmap)


df <- read.csv("final_uni.csv",row.names = 1)

dim(df)
# Row Ann
annotation_row <- data.frame(
  rowAnn = c(rep("Class 1",800),rep("Class 2",800),rep("Class 3",864))
)
rownames(annotation_row) <- rownames(df)

# Col Ann
annotation_col <- data.frame(
  type = rep(c("Type A", "Type B","Type C"), c(30,30,31))
)
rownames(annotation_col) <- colnames(df)

pheatmap(df,
         border_color = 'black',
         cluster_rows = F,
         cluster_cols = F,
         show_rownames = F,
         fontsize_col = 5,
         # scale = 'row',
         annotation_row = annotation_row,
         annotation_col = annotation_col,
         gaps_row = c(800,1600),
         gaps_col = c(30,60))

