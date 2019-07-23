
#读取来自UCSC的文件
data = read.table('UCSC_ncbiref_hg19.txt',sep = '\t',
                  header = T)
dim(data)
summary(data)

# ==========================================================

#读取来自ensemble的数据文件
data1 = read.table('Ensemble_mousecell.gff',sep = '\t',
                   header = F)
dim(data1)


#读取encode的数据
data2 = read.table('Encode_HMM.bed',sep = '\t',
                   header = F)
dim(data2)
