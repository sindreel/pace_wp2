table <- read.csv("receiver_summary.csv", sep = ";")
table <- table[(!duplicated(table$newer)),]
