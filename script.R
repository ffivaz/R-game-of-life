## Cellular automata v0.1
## Par Fabien Fivaz

gen = 500
len = gen * 2 + 1
init = c(rep(0, floor(len/2)), 1, rep(0, floor(len/2)))
init = round(runif(len,0,1))

range = as.list(c("111", "110", "101", "100", "011", "010", "001", "000"))
sol = c(0, 0, 0, 1, 1, 1, 1, 0)

resmat = matrix(NA, ncol = len, nrow = gen+1)

print(init)
resmat[1,] = init
for (i in 1:gen) {
res = rep(NA, length(init))
res[1] = 0
res[len] = 0

for (j in 1:(length(init)-1)) {
	if (j==1) {
		pat = c(round(runif(1,0,1)),init[j:j+1])
	}
	else {
		pat = init[(j-1):(j+1)]
		pat = paste(pat[1], pat[2], pat[3], sep="")
		pat = which(pat == range)
		res[j] = sol[pat]
	}
}
print(res)
init = res
resmat[i+1,] = res
}

#image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
#+  ylab="", axes=FALSE, zlim=c(min,max))

myImagePlot(resmat)
