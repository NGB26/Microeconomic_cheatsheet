library(graphics)

# Definir la utilidad de cada agente
u_A <- function(x1, x2) {min(x1, x2)}
u_B <- function(x1, x2) {sqrt(x1) * sqrt(x2)}


# Definir las dotaciones iniciales
w_A <- c(3, 1)
w_B <- c(1, 3)

# Función de demanda marshalliana para el agente A
x1_A <- function(p1, p2, M) {(M/p1)^(2/3)}

# Función de demanda marshalliana para el agente B
x1_B <- function(p1, p2, M) {(M/p1)^(1/4) * (p2/p1)^(3/4)}

# Encontrar el precio relativo de equilibrio
p_eq <- (w_A[1]/w_A[2])^(3/5)

# Encontrar las demandas marshallianas en el equilibrio
M_A_eq <- sum(w_A*p_eq)
M_B_eq <- sum(w_B*p_eq)

x1_A_eq <- x1_A(p_eq, 1, M_A_eq)
x2_A_eq <- x2_A_eq <- min(x1_A_eq, M_A_eq - x1_A_eq)
x1_B_eq <- x1_B(p_eq, 1, M_B_eq)
x2_B_eq <- x2_B_eq <- (M_B_eq/p_eq) - x1_B_eq

# Graficar el equilibrio en una caja de Edgeworth
plot(x1_A_eq, x2_A_eq, xlim=c(0, 4), ylim=c(0, 4), xlab="Bienes 1", ylab="Bienes 2", type="p", pch=16, col="blue")
points(x1_B_eq, x2_B_eq, type="p", pch=16, col="red")
abline(v=sum(w_A)/p_eq, lty=2)
abline(h=sum(w_A)/p_eq, lty=2)

# Graficar las curvas de indiferencia de cada agente
curve((x/2)^2, from=0, to=4, col="red", xlab="Bien 1", ylab="Bien 2", ylim=c(0,4))
curve(2/(x/2)^2, from=0.1, to=4, col="blue", add=TRUE)

# Graficar la caja de Edgeworth
polygon(c(0,x1_A_eq,x1_A_eq+x1_B_eq,0), c(0,x2_A_eq,x2_A_eq+x2_B_eq,x2_B_eq), col="gray", border="black")
abline(v=x1_A_eq, col="black", lty=2)
abline(h=x2_B_eq, col="black", lty=2)
points(x1_A_eq, x2_A_eq, col="red", pch=19)
points(x1_B_eq, x2_B_eq, col="blue", pch=19)

# Agregar etiquetas
text(x1_A_eq+x1_B_eq/2, x2_A_eq+x2_B_eq/2, expression(paste(x^1[A], "+", x^1[B], " =", w^1)), col="red", pos=4)
text(x1_A_eq/2, x2_A_eq/2, expression(paste(x^2[A])), col="red")
text(x1_A_eq/2, (x2_A_eq+x2_B_eq)/2, expression(paste(x^2[B])), col="blue")
text(x1_A_eq+x1_B_eq/2, x2_B_eq/2, expression(paste(x^1[A], "+", x^1[B], " =", w^2)), col="blue", pos=2)

