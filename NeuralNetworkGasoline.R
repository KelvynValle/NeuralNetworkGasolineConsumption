# Leitura dos dados
combustivel <- read.csv2("C:/Users/kelvy/Desktop/laboratorio ai/lab 1/6/NeuralNetworkGasolineConsumption/gasolina.csv")
# Função para normalizar os dados entre 0 e 1.
normalizar <-function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}
# Normalização dos Dados
maxmindf <-as.data.frame(lapply(combustivel, normalizar))
# Separação dos Dados de Treinamento e Teste já normalizados
treinamento <-maxmindf [1:32,]
teste <-maxmindf [33:40,]

# Carga da Biblioteca “neuralnet”
library(neuralnet)
# Geração da Rede Neural com duas camadas escondidas c(2,1)
rnaConsumo <- neuralnet(consumo ~ capacidade + custo +
                          horas,data=treinamento, hidden=c(2,1), linear.output=TRUE,
                        threshold=0.01)
# Consulta aos Parâmestros gerados para a Rede Neural
rnaConsumo$result.matrix


# Previsão com os Dados de Teste pela Rede
previsao = predict(rnaConsumo,teste)
# Comparação entre o Dado Real e o Valor Previsto
resultadoConsumo <- data.frame(real = teste$consumo, previsto =
                                 previsao)
resultadoConsumo

# Reconvertendo o Previsto
previsto =resultadoConsumo$previsto *
  abs(diff(range(combustivel$consumo))) + min(combustivel$consumo)
# Reconvertendo o Real
real=resultadoConsumo$real * abs(diff(range(combustivel$consumo))) +
  min(combustivel$consumo)
# Comparando o Real e o Previsto convertidos
comparacao=data.frame(previsto,real)
# Resultado da Comparação com os Valores de Teste

# Cálculo da Acurácia com base no desvio
desvio=((real-previsto)/real)
comparacao=data.frame(previsto,real,desvio)
acuracia=1-abs(mean(desvio))
acuracia

# Plotagem da Nova Rede 5,2


# Rede Neural com duas Camadas Ocultas respectivamente 5 e 2 neurônios
rnaConsumo <- neuralnet(consumo ~ capacidade + custo +
                          horas,data=treinamento, hidden=c(5,2), linear.output=TRUE,
                        threshold=0.01)
# Resultado dos Parâmetros da Rede Neural 5,2
rnaConsumo$result.matrix


# Previsão com a Nova Rede com base nos dados de Teste
previsao = predict(rnaConsumo,teste)
# Comparação entre o Valor Real e o Previsto pela Rede
resultadoConsumo <- data.frame(real = teste$consumo, previsto =
                                 previsao)
resultadoConsumo


rnaConsumo <- neuralnet(consumo ~ capacidade + custo +
                          horas,data=treinamento, hidden=c(12,4), linear.output=TRUE,
                        threshold=0.01)
plot(rnaConsumo)
previsao = predict(rnaConsumo,teste)
resultadoConsumo <- data.frame(real = teste$consumo, previsto =
                                 previsao)
resultadoConsumo
previsto=resultadoConsumo$previsto *
  abs(diff(range(combustivel$consumo))) + min(combustivel$consumo)
real=resultadoConsumo$real * abs(diff(range(combustivel$consumo))) +
  min(combustivel$consumo)
comparacao=data.frame(previsto,real)
desvio=((real-previsto)/real)
comparacao=data.frame(previsto,real,desvio)
acuracia=1-abs(mean(desvio))
acuracia
