app <- ShinyDriver$new("../../DesigntheLottery.Rmd", seed = 75677)
app$snapshotInit("mytest")

app$snapshot()
app$snapshot()
