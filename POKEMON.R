#Estatística pokémon
library('FSA')
library('readxl')

dados <- read_excel('/Users/pietr/OneDrive/Documentos/pastinha/pokemon.xlsx')

#1
freq_rel <- round(prop.table(table(dados$geracao))*100, 2)
freq_rel_ordenada <- sort(freq_rel, decreasing = TRUE,)
print(freq_rel_ordenada)

#2
freq_rel <- round(prop.table(table(dados$tipo))*100, 2)
freq_rel_ordenada <- sort(freq_rel, decreasing = TRUE,)
print(freq_rel_ordenada)

#3
freq_rel <- round(prop.table(table(dados$lendario))*100, 2)
freq_rel_ordenada <- sort(freq_rel, decreasing = TRUE,)
print(freq_rel_ordenada)

total_lendarios <- sum(dados$lendario)
cat("Número de Pokémon lendários no conjunto de dados é:", total_lendarios)

#4 Maior total de status e o menor total de status
maior_total <- max(pokemon$totalStatus)
menor_total <- min(pokemon$totalStatus)

pokemon_maior_total <- pokemon[pokemon$totalStatus == maior_total,"nome"]
pokemon_maior_total
maior_total

pokemon_menor_total <- pokemon[pokemon$totalStatus == menor_total,"nome"]
pokemon_menor_total
menor_total

#5 Os mais velozes
total_velocidade <- head(dados[order(pokemon$velocidade), "nome"],3)
cat("Três Pokémon mais velozes são: \n")
print(total_velocidade)

#6
total_felicidade <- head(pokemon[order(-pokemon$felicidade), "nome"],10)
cat("Top 10 pokémons mais felizes são: \n")
print(total_felicidade)

#7
resumo_ataque <- Summarize(pokemon$ataque ~ pokemon$tipo)
resumo_ataque <- resumo_ataque[order(resumo_ataque$mean, decreasing = TRUE),]
resumo_ataque

#8
resumo_defesa <- Summarize(pokemon$defesa ~ pokemon$tipo)
resumo_defesa <- resumo_defesa[order(resumo_defesa$mean, decreasing = TRUE),]
resumo_defesa

#9
resumo_ataque <- Summarize(dados$ataqueEspecial ~ dados$tipo)
resumo_ataque <- resumo_ataque[order(resumo_ataque$mean, decreasing = TRUE),]
resumo_ataque

#10
resumo_defesa_especial <- Summarize((dados$defesaEspecial ~ dados$tipo))
resumo_defesa_especial <- resumo_defesa_especial[order(resumo_defesa_especial$mean, decreasing = TRUE),]
resumo_defesa_especial

#11
top_felicidade <- head(dados[order(-dados$felicidade),"nome"], 1)
cat("Pokémon com a maior felicidade: \n")
print(top_felicidade)

#12
media_passos <- mean(dados$passosChocar)
media_passos_pokemon <- dados[dados$passosChocar == media_passos, "passosChocar"]
media_passos_pokemon
media_passos

#13
taxa_captura <- sum(dados$taxaCaptura)
cat("Número de Pokémon lendários no conjunto de dados é:", total_lendarios)

#14
t(Summarize(dados$ataque ~ dados$lendario))

#15
t(Summarize(dados$defesa ~ dados$lendario))

#16
t(Summarize(dados$velocidade ~ dados$lendario))

#17
t(Summarize(dados$felicidade ~ dados$lendario))

#18
t(Summarize(dados$ataque ~ dados$geracao))

#19
t(Summarize(dados$defesa ~ dados$geracao))

#20
t(Summarize(dados$velocidade ~ dados$geracao))

#21
t(Summarize(dados$geracao ~ dados$felicidade))

#22
plot(as.double(dados$alturaMetros), dados$totalStatus, col = 'thistle')

#23
plot(as.double(dados$pesoQuilos), dados$totalStatus, col = 'deeppink')

#24
hist(dados$hp, col = 'darkorchid')

#25
boxplot(dados$hp ~ dados$geracao, main = "Histograma", xlab = "hp", ylab = "geração", col = "darkgreen")
