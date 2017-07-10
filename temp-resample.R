h <- layerize(lc2009)
fact <- dim(lc2009)[1:2]/dim(landsat)[1:2]
g <- aggregate(h, fact)
f <- resample(g, landsat)

