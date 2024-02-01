# Dicas e Desafios para o R
#1
hello_world <- function() {
  print("Olá, mundo!")
}
# Chamar a função
hello_world()

#2
print("Meu nome completo é João da Silva")

#3
print_idade <- function() {
  idade <- 25
  cat("Minha idade é", idade, "anos.\n")
}
# Exemplo de uso
print_idade()

#4
# indice de massa corporal
altura <- 1.75
print(altura)
meu_peso <- 70
print(meu_peso)

#5
calcula_idade_em_meses <- function(anos, meses) {
  idade_em_meses <- anos * 12 + meses
  cat("Sua idade em meses é:", idade_em_meses)
}
# Exemplo de uso:
calcula_idade_em_meses(25, 6)

#6
calcula_IMC <- function(peso, altura){
  IMC <- peso / (altura^2)
  cat("Seu IMC é:", IMC)
}
# Exemplo de uso:
calcula_IMC(70, 1.75)

#7
cria_lista <- function() {
  minha_lista <- list(1:10)
  print(minha_lista)
}
# Exemplo de uso:
cria_lista()

#8
cria_lista_amigos <- function() {
  lista_amigos <- list("João", "Maria", "Pedro")
  print(lista_amigos)
}
# Exemplo de uso:
cria_lista_amigos()

#9
lista_de_frutas <- function() {
  frutas <- c("banana", "maçã", "laranja", "morango", "pera")
  print(frutas)
}
# Exemplo de uso:
lista_de_frutas()

#10
minha_info <- function() {
  nome <- "João da Silva"
  idade <- 30
  altura <- 1.8
  peso <- 75
meu_dicionario <- list(Nome = nome, Idade = idade, Altura = altura, Peso <- peso)
  print(meu_dicionario)
}
# Exemplo de uso:
minha_info()

#11
soma <- function(num1, num2){
  return(num1 + num2)
}
# Exemplo de uso
soma(3,4)

#12
lst <- c(1,3,5)
max_lista <- function(lst) {
  max_val <- lst[[1]]
  for (val in lst) {
    if (val > max_val) {
      max_val <- val
    }
  }
  return(max_val)
}
# Exemplo de uso:
max_lista(lst)

#13
min_lista <- function(lst) {
  min_val <- lst[[1]]
  for (val in lst) {
    if (val < min_val) {
      min_val <- val
    }
  }
  return(min_val)
}
# Exemplo de uso:
min_lista(lst)

#14
media_lista <- function(lst){
  mean(unlist(lst))
}
# Exempo de uso:
lst <- list(1, 2, 3, 4, 5)
media_lista(lst)

#15
ordena_lista <- function(lst) {
  lista_ordenada <- lapply(lst, sort)
  return(lista_ordenada)
}
# Exemplo de uso:
lst <- list(c(3, 1, 4), c(2, 5, 1), c(6, 3, 2))
lista_ordenada <- ordena_lista(lst)
print(lista_ordenada)

# 16
# String reversa
string_reversa <- function(str) {
  return(paste(rev(strsplit(str, "")[[1]]), collapse = ""))
}
# Exemplo de uso:
string_reversa("Olá")

#17
obter_numeros_pares <- function(numeros_lista) {
  numeros_pares <- c()
  for (number in numbers_list) {
    if (numero %% 2 == 0) {
      numeros_pares <- c(numeros_pares, numero)
    }
  }
  return(numeros_pares)
}
# Exemplo de uso:
lst <- c(1,2,3,4,5,6,7)
obter_numeros_pares(lst)

#18
numeros_impares <- function(numeros_lista) {
  impares_lista <- numeros_lista[numeros_lista %% 2 != 0]
  return(impares_lista)
}
# Exemplo de uso:
numeros <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
numeros_impares(numeros)

#19
lst <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
soma_lista <- function(lst){
  sum(lst)
}
# Exemplo de uso
soma_lista(lst)

#20
prod_lista <- function(lst){
  prod(lst)
}
# Exemplo de uso
lst = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
prod_list(lst)

#21
conta_elementos <- function(lst) {
  return(length(lst))
}
# Exemplo de uso:
lst <- c(1, 2, 3, 4, 5)
conta_elementos(lst)

#22
mediana_lista <- function(lst) {
  lista_ordenada <- sort(lst)
  n <- length(lista_ordenada)
  if(n %% 2 == 0) {
    mediana <- (lista_ordenada[n/2] + lista_ordenada[(n/2)+1])/2
  } else {
    mediana <- lista_ordenada[(n+1)/2]
  }
  return(mediana)
}
# Exemplo de uso
lst = c(1, 2, 3, 4, 5)
mediana_lista(lst)

#23
moda_lista <- function(lst) {
  # conta a frequencia de cada valor na lista
  freq <- table(lst)
  # encontra a frequencia máxima
  max_freq <- max(freq)
  # encontra os valores com a máxima frequencia
  modas <- names(freq[freq == max_freq])
  # retorna a(s) moda(s)
  return(modas)
}

#24
desvio_padrao <- function(numeros) {
  sd(numeros)
}
# Exemplo de uso:
numeros <- c(1, 2, 3, 4, 5)
desvio_padrao(numeros)

#25
variancia <- function(numeros) {
  valor_medio <- mean(numeros)
  soma_dos_quadrados <- sum((numeros - valor_medio)^2)
  valor_variancia <- soma_dos_quadrados / length(numeros)
  return(valor_variancia)
}
# Exemplo de uso:
numeros <- c(1, 2, 3, 4, 5)
variancia(numeros)

#26
cv <- function(x) {
  sd(x)/mean(x)*100
}
# Exemplo de uso:
x <- c(1, 2, 3, 4, 5)
cv(x)

#27
lista_ao_quadrado <- function(lst){
  return(lst^2)
}
# Exemplo de uso:
minha_lista <- c(1, 2, 3, 4, 5)
lista_ao_quadrado(minha_lista)

#28
lista_ao_cubo <- function(lst){
  return(lst^3)
}
minha_lista <- c(1, 2, 3, 4, 5)
lista_ao_cubo(minha_lista)

#29
lista_na_potencia <- function(lst, power) {
  nova_lista <- lst^power
  return(nova_lista)
}
# Exemplo de uso:
minha_lista <- c(1, 2, 3, 4, 5)
lista_na_potencia(minha_lista, 2)

#30
numeros_positivos <- function(lista_numeros) {
  lista_positivo <- list()
  for (numero in lista_numeros) {
    if (numero > 0) {
      lista_positivo <- c(lista_positivo, numero)
    }
  }
  return(lista_positivo)
}
# Exemplo de uso:
lista_numeros <- c(1,-2,4,-5)
numeros_positivos(lista_numeros)

#31
numeros_negativos <- function(lista_numeros) {
  return(lista_numeros[lista_numeros < 0])
}
# Exemplo de uso:
lista_numeros <- c(-1,3,4,-7,-9)
numeros_negativos(lista_numeros)


#32
valores_diferentes <- function(lst) {
  unique(lst)
}
# Exemplo de uso:
lst <- c(1, 2, 3, 2, 4, 5, 3,4)
valores_diferentes(lst)

#33
lista_unicos <- function(lst) {
  unique(lst)
}
# Exemplo de uso:
lst <- c(1, 2, 3, 2, 4, 5, 1)
lista_unicos(lst)

#34
lista_numeros_comuns <- function(lista1, lista2) {
  intersect(lista1, lista2)
}
# Exemplo de uso:
lista1 <- c(1, 2, 3, 4, 5)
lista2 <- c(3, 4, 5, 6, 7)
lista_numeros_comuns(lista1, lista2)

#35
mistura_listas <- function(lista1, lista2) {
  lista_misturada <- c(lista1, lista2)
  lista_unica <- unique(lista_misturada)
  return(lista_unica)
}
# Exemplo de uso:
lista1 <- c(1, 2, 3, 4, 5)
lista2 <- c(3, 4, 5, 6, 7)
mistura_listas(lista1,lista2)

#36
eh_ascendente <- function(lst) {
  if (length(lst) <= 1) {
    return(TRUE)
  }
  for (i in 2:length(lst)) {
    if (lst[i] < lst[i-1]) {
      return(FALSE)
    }
  }
  return(TRUE)
}
# Exemplo de uso:
lst <- c(3, 4, 5, 6, 7)
eh_ascendente(lst)

#37
eh_descendente <- function(lst) {
  if(length(lst) <= 1) {
    return(TRUE)
  }
  for(i in 2:length(lst)) {
    if(lst[i] > lst[i-1]) {
      return(FALSE)
    }
  }
  return(TRUE)
}
# Exemplo de uso:
lst <- c(3, 4, 5, 6, 7)
eh_descendente(lst)


#38
estah_ordenada <- function(lst) {
  if (is.null(lst) || length(lst) <= 1) {
    return(TRUE)
  }
  cresce <- TRUE
  decresce <- TRUE
  for (i in 2:length(lst)) {
    if (lst[i] < lst[i-1]) {
      cresce <- FALSE
    }
    if (lst[i] > lst[i-1]) {
      decresce <- FALSE
    }
  }
  return(cresce || decresce)
}

#39
arredonda_lista <- function(lst){
  lst_arredondada <- round(lst)
  return(lst_arredondada)
}
# Exemplo de uso:
lst <- c(1.23,1.6,2.4,5.3)
arredonda_lista(lst)


#40
arredonda_lista_digitos <- function(lst, digitos) {
  lst_arredondada <- round(lst, digitos)
  return(lst_arredondada)
}
# Exemplo de uso
lst <- c(1.238, 1.693, 2.47, 5.396)
arredonda_lista_digitos(lst,2)


#41
ordena_strings <- function(string_lista) {
  lista_ordenada <- sort(string_lista)
  return(lista_ordenada)
}
# Exemplo de uso:
minha_lista <- c("banana", "maçã", "laranja", "morango")
lista_ordenada <- sort_strings(minha_lista)
print(lista_ordenada)

#42
alpha_reversa <- function(lst) {
  return(sort(lst, decreasing = TRUE))
}
# Example usage:
palavras <- c("banana", "maçã", "laranja", "morango")
alpha_reversa(palavras)

#43
ordena_strings <- function(string_lista) {
  lista_ordenada <- string_lista[order(nchar(string_lista))]
  return(lista_ordenada)
}
# Exemplo de uso:
strings <- c("banana", "maçã", "pera", "laranja")
strings_ordenadas <- ordena_strings(strings)
print(strings_ordenadas)

#44
ordem_inversa <- function(lst) {
  lst[order(-nchar(lst))]
}
# Exemplo de uso:
palavras <- c("banana", "maçã", "pera", "laranja", "morango")
ordem_inversa(palavras)

#45
converter_maiuscula <- function(str_list) {
  return(toupper(str_list))
}
# Exemplo de uso:
str_lista <- c("olá", "mundo")
converter_maiuscula(str_lista)

#46
converter_minuscula <- function(str_list) {
  return(lapply(str_lista, tolower))
}
# Exemplo de uso
palavras <- c("Olá", "MUNDO", "Como", "Vai", "Você")
converter_minuscula(palavras)

#47
capitalizaLista <- function(lst) {
  nova_lst <- lapply(lst, function(x) {
    paste(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))), sep = " ")
  })
  return(nova_lst)
}
# Exemplo de uso:
lst <- c("olá", "MUNDO", "iSso", "é", "um", "TEST")
capitalizaLista(lst)


#48
numero_para_vogal <- function(string) {
  vogais <- c("a", "e", "i", "o", "u")
  numeros <- c("4", "3", "1", "0", "5")
  for (i in 1:length(vogais)) {
    string <- gsub(vogais[i], numeros[i], string, ignore.case = TRUE)
  }
  return(string)
}
# Exemplo de uso:
string <- "Brasil"
numero_para_vogal(string)

#49
eh_anagrama <- function(str1, str2) {
  # Remove espaços e converte para minúsculo
  str1 <- tolower(gsub(" ", " ", str1))
  str2 <- tolower(gsub(" ", " ", str2))
  # Verifica se o comprimento das strings é igual
  if (nchar(str1) != nchar(str2)) {
    return(FALSE)
  }
  str1_ordenada <- sort(strsplit(str1, " ")[[1]])
  str2_ordenada <- sort(strsplit(str2, " ")[[1]])
  if (identical(str1_ordenada, str2_ordenada)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
# Exemplo de uso:
eh_anagrama("ROMA", "AMOR") # TRUE

#50
palavras_invertidas <- function(str) {
  palavras <- strsplit(str, " ")[[1]]
  palavras_ordem_inversa <- rev(palavras)
  nova_str <- paste(palavras_ordem_inversa, collapse = " ")
  return(nova_str)
}
# Exemplo de uso:
palavras_invertidas("olá mundo")

#51
extrair_caracteres <- function(string) {
  caracteres <- gsub("[0-9]", "", string)
  return(caracteres)
}

# Example usage
extrair_caracteres("abc123def456") # Returns ""123456"""
