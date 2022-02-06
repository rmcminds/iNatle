# GBIF.org (06 February 2022) GBIF Occurrence Download  https://doi.org/10.15468/dl.rg6zru

hi <- read.table('0127248-210914110416597.csv',sep='\t',quote='',header=T)
hi2 <- hi[hi$taxonRank == 'GENUS' & hi$taxonomicStatus == 'ACCEPTED' & nchar(hi$genus) <= 8,]
hi2 <- hi2[order(hi2$numberOfOccurrences,decreasing=TRUE),]

words <- hi2[,c('genus','numberOfOccurrences')]

write.table(words, 'words.txt', sep='\t', quote=FALSE, row.names = FALSE, col.names = FALSE)
