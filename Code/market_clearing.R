?knitr
rm(list=ls())
#install.packages("lpSolveAPI")
library(lpSolve)

# Declare the list of demand offers
xd = c(250,300,120,80,40,70,60,45,30,35,25,10) # Quantities
yd = c(200,110,100,90,85,75,65,40,37.5,30,24,15) # Prices

# Declare the list of supply offers
xg = c(120,50,200,400,60,50,60,100,70,50,70,45,50,60,50) # Quantities
yg = c(0,0,15,30,32.5,34,36,37.5,39,40,60,70,100,150,200) # Prices

# Objective Function
obj =  c(yg,-yd)
obj
# Equality Constrainsts
Aeq  = c(rep(1,length(yg)),rep(-1,length(yd)))
beq = 0

# Inequality Constrainsts
A  = diag(rep(1,length(obj)))
b = c(xg,xd)

# All contraints binding them
A = rbind(Aeq,A)
b = c(beq,b)


# Direction of Inequality
f.dir = c("==",rep("<=",(length(obj))))

s = lp("min", obj, A, f.dir, b)
s$solution

eq.price <- lp("min", obj, A, f.dir, b,compute.sens=TRUE)$duals[1]
eq.price


