# install.packages("ompr")
# install.packages("ompr.roi")
# install.packages("ROI.plugin.symphony")


library("ompr")
library("ompr.roi")
library("data.table")
library("dplyr")
library(ROI.plugin.symphony)
# library(ROI.plugin.glpk)
library(tictoc)

##################################################!
#Generacion de Datos ####
##################################################!
grupos <- c()

for (i  in 1:10) 
{
  for (j in 1:10) 
  {
    for (k in 1:10) 
      {
      grupos [((i-1)*100+(j-1)*10+k)] <- paste0(i, "-", j, "-", k)
    }  
  }  
}

n<- round(runif(1000, 1000, 3000))

decilCancer <- c(); decilPIF <- c(); decilFraude <- c()
for (i in 1:1000) {
  decilCancer[i] <- as.integer(strsplit(grupos, "-")[[i]][1])
  decilPIF[i] <- as.integer(strsplit(grupos, "-")[[i]][2])
  decilFraude[i] <- as.integer(strsplit(grupos, "-")[[i]][3])
}


parametros <- as.data.table(data.frame(grupos, n, decilCancer, decilPIF, decilFraude))

ventas <- data.frame(seguro = c("cancer", "PIF", "Fraude"), ventas = c(1800, 3000, 2000))
for (i in 1:10) 
{
  parametros[decilCancer == i, efecCancer := (0.02+0.015*i)]
  parametros[decilPIF == i, efecPIF := (0.04+0.011*i)]
  parametros[decilFraude == i, efecFraude := (0.05+0.02*i)]
}

efectividades <- parametros[, mget(c("efecCancer", "efecPIF", "efecFraude"))]

#######################################################!
#Modelo de oprimizacion ####
#######################################################!
seguros <- c("Cancer", "PIF", "Fraude")

modelo <- MIPModel() 
#i: grupos j: seguros
modelo <-
  add_variable(
    modelo,
    x[i, j],
    i = 1:length(parametros$grupos),
    j = 1:length(seguros),
    type = "integer",
    lb = 0
  )
modelo <-
  set_objective(modelo , sum_expr(
    x[i, j],
    i = 1:length(parametros$grupos),
    j = 1:length(seguros)
  ), "min")
modelo <-
  add_constraint(modelo,
                 sum_expr(x[i, j], j = 1:length((seguros))) <= parametros$n[i],
                 i = 1:length(parametros$grupos))
modelo <-
  add_constraint(modelo,
                 sum_expr(efectividades$efecCancer[i] * x[i, j], i = 1:length(parametros$grupos)) >= ventas$ventas[j],
                 j = 1)
modelo <-
  add_constraint(modelo,
                 sum_expr(efectividades$efecPIF[i] * x[i, j], i = 1:length(parametros$grupos)) >= ventas$ventas[j],
                 j = 2)
modelo <-
  add_constraint(modelo,
                 sum_expr(efectividades$efecFraude[i] * x[i, j], i = 1:length(parametros$grupos)) >= ventas$ventas[j],
                 j = 3)

tic()
answer <- solve_model(modelo, with_ROI(solver = "symphony"))
toc()
print(answer$objective_value)
print(get_solution(answer,x[i,j]) %>% filter(value >0))


tic()
answer2 <- solve_model(modelo, with_ROI(solver = "glpk"))
toc()

resultados <- as.data.table(get_solution(answer,x[i,j]) %>% filter(value >0))
resultados[, producto:= seguros[j]]
resultados[, grupo := parametros$grupos[i]]
