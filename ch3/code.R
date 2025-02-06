f = function(z){
  z[1]^2 + z[2]^2 - 1
}

f.x = function(z){
  2*z[1]
}

f.y = function(z){
  2*z[2]
}

g = function(z){
  z[1] + z[2]
}

g.x = function(z){
  1
}

g.y = function(z){
  1
}

z = c(3, 4)

for (i in 1:10){
  z = z- solve(matrix(c(f.x(z), f.y(z), g.x(z), g.y(z)), ncol = 2, byrow = TRUE)) %*% c(f(z), g(z))
}

z

?implicitplot3d
?implicitplot3d