#libraries
library("ggplot2")
library("epitools")


# How many observations?
hits <- 100000


x <- sample( LETTERS[1:2], hits, replace=TRUE, prob=c(0.50, 0.50) )
prop.table(table(x))


y <- sample( LETTERS[c(25,14)], hits, replace=TRUE, prob=c(0.10, 0.90) )
prop.table(table(y))

clicks <- data.frame("Variant" = x, "Clicked" = y)

ggplot(clicks, aes(x=x, y=y)) + geom_bar(stat="identity", fill="blue", color="blue")


#simple 2x2
table(clicks)

prop.test(table(clicks))

#Better?????
fisher.test(table(clicks))

#useful?????
riskratio(table(clicks))
