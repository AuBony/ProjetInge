load("~/2020-2021/PROJET-INGE/complete_clean_data.RData")

test <- dta[,c(seq(1,882000, by = 200), 882001, 882002)]
n <- ncol(test)
plot(1:(n-2), test[3,-c(n-1, n)], type = 'l')

View(test[1:10,c(1:5,4411,4412)])

testcr <- as.data.frame(t(apply(t(test),2,scale)))

