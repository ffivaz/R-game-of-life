gen = 500
size = 150
min = 2
max = 3
rep = 3

dyn.load("bcl.so")


mat = matrix(0, ncol=size, nrow=size)
#blinker (period 2)
#mat[c(5), c(4:6)] = 1

#Glider
#ctr = c(50,50)
#mat[c((ctr[1]-1):(ctr[1]+1)), c(ctr[2]-1)] = 1
#mat[c(ctr[1]+1), c(ctr[2])] = 1
#mat[c(ctr[1]), c(ctr[2]+1)] = 1

#R-pentomino
#ctr = c(50,50)
#mat[c(ctr[1]), c((ctr[2]-1):(ctr[2]+1))] = 1
#mat[c(ctr[1]+1), c(ctr[2]+1)] = 1
#mat[c(ctr[1]-1), c(ctr[2])] = 1

#Gosper Glider Gun
#a=3
#b=30
#mat[c(a+25), c(b)] = 1
#mat[c(a+23, a+25), c(b-1)] = 1
#mat[c(a+13, a+14, a+21, a+22, a+35, a+36), c(b-2)] = 1
#mat[c(a+12, a+16, a+21, a+22, a+35, a+36), c(b-3)] = 1
#mat[c(a+1, a+2, a+11, a+17, a+21, a+22), c(b-4)] = 1
#mat[c(a+1, a+2, a+11, a+15, a+17, a+18, a+23, a+25), c(b-5)] = 1
#mat[c(a+11, a+17, a+25), c(b-6)] = 1
#mat[c(a+12, a+16), c(b-7)] = 1
#mat[c(a+13, a+14), c(b-8)] = 1

#Clown
#a = 25
#b = 25
#mat[c(a, a+2), c(b-2)] = 1
#mat[c(a, a+2), c(b-1)] = 1
#mat[c(a, a+1, a+2), c(b)] = 1

#Row of seven...
#a = 25a
#b = 25
#mat[c(a+1:7), c(b)] = 1

#pulsar 48-56-72
#a = 25
#b = 25
#mat[c(a+1:5), c(b)] = 1
#mat[c(a+1, a+5), c(b-1)] = 1

#Row of n...
#a = 20
#b = 25
#n = 10
#mat[c(a+1:n), c(b)] = 1

#Row of 5 counters and spaces
a = 50
b = 60
mat[c(a+1:5), c(b)] = 1
mat[c(a+7:11), c(b)] = 1
mat[c(a+13:17), c(b)] = 1
mat[c(a+19:23), c(b)] = 1
mat[c(a+25:29), c(b)] = 1
mat[c(a+31:35), c(b)] = 1
mat[c(a+37:41), c(b)] = 1
mat[c(a+43:47), c(b)] = 1
mat[c(a+49:53), c(b)] = 1


#mat[c(22:24,26,28,30), c(14:16,18,20,22)] = 1
#print(mat)
image(mat, col=c("White", "Black"))
#Sys.sleep(0.5)
ar = c(-1, -1, -1, 0, -1, 1, 0, -1, 0, 1, 1, -1, 1, 0, 1, 1)
pl = rep(NA, gen)

gp = paste("Cycle: 0 / still alive:", sum(mat))
print(gp)

for (g in 1:gen) {
	matg = mat
	
	matg = .Fortran("bcl", m=as.matrix(matg), x=as.integer(dim(matg)[1]), x=as.integer(dim(matg)[2]))
	
	gp = paste("Cycle:", g, "/ still alive:", sum(mat))
	pl[g] = sum(mat)
	par(mfrow=c(1,2))
	image(mat, col=c("White", "Black"))
	plot(pl, type="l")
	print(gp)
	#Sys.sleep(0.025)
	mat = matg
}