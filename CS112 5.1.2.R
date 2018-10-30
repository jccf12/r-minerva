set.seed(1)


library(Matching)

data(lalonde)


tree.lalonde =tree(u75~ re75, lalonde, method = "class")
summary(tree.lalonde)

plot(tree.lalonde)
text(tree.lalonde, pretty = 0)

lalonde$re75
notzero <- which(lalonde$re75 > 0)
lalonde$u75[notzero]
min(lalonde$re75[notzero])
lalonde$re75[notzero]
set.seed(2)
train=sample (1: nrow(lalonde ), nrow(Boston)/2)
lalonde.test = lalonde[-train ,]
u75.test = lalonde$u75[-train]


cv.lalonde=cv.tree(tree.lalonde)
prune.lalonde=prune.tree(tree.lalonde ,best=2)
plot(prune.lalonde)
text(prune.lalonde , pretty =0)


yhat=predict (tree.lalonde ,newdata=lalonde[-train ,])
lalonde.test=lalonde[-train ,"medv"]
plot(yhat ,lalonde.test)
abline (0,1)
mean((yhat -lalonde.test)^2)





tree.lalonde =tree(u75~ age+educ+black+hisp+married+nodegr, lalonde,subset=train)

tree.pred = predict(tree.lalonde , lalonde.test)
table(tree.pred ,u75.test)