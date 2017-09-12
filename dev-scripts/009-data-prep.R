# Prepare data for 009-expression-heatmaps lesson
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2017-09-12

rm(list = ls())

################################################################################
# Background:
# From series matrix file [ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE68nnn/GSE68849/matrix/GSE68849_series_matrix.txt.gz], 
# pull out expression data via bash:
# `tail -n +61 GSE68849_series_matrix.txt | head -n -1 | sed 's/"//g' > GSE68849-matrix.txt`
# geo2r is results from GEO2R analysis [https://www.ncbi.nlm.nih.gov/geo/geo2r/?acc=GSE68849]

# Read in data
expression <- read.delim(file = "data/GSE68849-matrix.txt", stringsAsFactors = FALSE)
geo2r <- read.delim(file = "data/GSE68849-geo2r.txt", stringsAsFactors = FALSE)

# Drop any data missing a gene symbol
geo2r <- geo2r[geo2r$Gene.symbol != "", ]

# Grab five genes from the top 10
set.seed(20170912)
geo2r.subset <- geo2r[sample(x = 1:10, size = 5, replace = FALSE), ]

# Grab five genes from the bottom 1000
geo2r.subset <- rbind(geo2r.subset,
                      geo2r[sample(x = 30267:31266, size = 5, replace = FALSE), ])
rownames(geo2r.subset) <- NULL

# Just pull out the ID and symbol columns
gene.info <- geo2r.subset[, c("ID", "Gene.symbol")]

# Now extract those columns from the expression object
rows.to.extract <- which(expression$ID_REF %in% gene.info$ID)
exp.subset <- expression[rows.to.extract, ]

# Add column with gene symbol
exp.subset <- merge(x = exp.subset, y = gene.info, by.x = "ID_REF", by.y = "ID")

# Transpose numeric values
exp.transposed <- t(exp.subset[, 2:11])
exp.transposed <- data.frame(exp.transposed)

# Add Gene.symbol as column names
colnames(exp.transposed) <- exp.subset$Gene.symbol

# Use rownames to make subject column
exp.transposed$subject <- rownames(exp.transposed)

# Add treatment column
exp.transposed$treatment <- "control"
exp.transposed$treatment[c(2, 4, 6, 8, 10)] <- "influenza"

# Rearrange columns
exp.transposed <- exp.transposed[, c(11, 12, 1:10)]
rownames(exp.transposed) <- NULL

# Write to data file
write.csv(x = exp.transposed, file = "data/GSE68849-expression.csv", quote = FALSE, row.names = FALSE)
