#########################################################################
#                  Base de Dados: Gastos dos Deputados               ####
# Fonte: https://brasil.io/dataset/gastos-deputados/cota_parlamentar/ ###
# Aluna: Thaís Claudino Viana                                         ###
# Curso: Gestão e Análise estratégica de Dados                        ###
#########################################################################


# Função de resgate da base

download_base <- function(dataset, table_name){
  url <- sprintf("https://data.brasil.io/dataset/%s/%s.csv.gz", dataset, table_name)
  tmp <- tempfile()
  download.file(url, tmp)
  response <- read.csv(gzfile(tmp), encoding = "UTF-8")
  unlink(tmp)
  return(response)
}

# Passe o nome da tabela para a funcao, como "caso", "caso_full", "obito_cartorio":
data <- download_base("gastos-deputados", "cota_parlamentar")

#### Utilizando dados de dos três últimos anos

data = subset(data, datemissao >= "2018-01-01")

### Selecionando as colunas de interesse:

data = data[,c("nudeputadoid", "idecadastro", "txnomeparlamentar",  "sgpartido", "sguf", "nulegislatura",
               "codlegislatura","datemissao","indtipodocumento", "numano", "txtcnpjcpf", "txtfornecedor",
               "numsubcota","txtdescricao",  "txtdescricaoespecificacao", "txttrecho", "txtpassageiro",
               "vlrdocumento", "vlrglosa", "vlrliquido", "vlrrestituicao",
               "numespecificacaosubcota", "numparcela", "numressarcimento", "numsubcota")]


### O dicionário de dados está no endereço:
# https://www2.camara.leg.br/transparencia/cota-para-exercicio-da-atividade-parlamentar/explicacoes-sobre-o-formato-dos-arquivos-xml

# Como cada linha corresponde a um serviço, incluo uma coluna-contador para auxiliar
# na obtenção de dados:

data$contador = 1

# breve descrição dos dados:

summary(data)

require(descr)

freq(data$txtdescricao) 
## Algumas descrições são diferentes para o mesmo serviço, irei padronizar:

data$txtdescricao[data$txtdescricao == "Emissão Bilhete Aéreo"] = "PASSAGENS AÉREAS"
data$txtdescricao[data$txtdescricao == "LOCAÇÃO DE VEÍCULOS AUTOMOTORES OU FRETAMENTO DE EMBARCAÇÕES"] = "LOCAÇÃO OU FRETAMENTO DE VEÍCULOS AUTOMOTORES"

#require(descr)
#freq(data$sgpartido)
data$sgpartido[data$sgpartido == "PP**"] = "PP"


#freq(data$txtfornecedor)
# Padronizando os nomes dos fornecedores (todos em caixa alta)
data$txtfornecedor = toupper(data$txtfornecedor)

#freq(data$txnomeparlamentar)
# Padronizando os nomes dos parlamentares (todos em caixa alta)
data$txnomeparlamentar = toupper(data$txnomeparlamentar)

### Padronizando o formato da Data
require(lubridate)
data$datemissao = as.Date(data$datemissao, format = "%Y-%m-%d")
#length(data$idecadastro[data$datemissao > today()]) 

## Retirando dados com data posterior a atual

data = subset(data, datemissao <= "2020-02-13")

#hist(data$vlrliquido)
#summary(data$vlrliquido)


#########################################################################
#       Base de Dados: Cotas Parlamentares por EStado               ####
# Fonte: https://www2.camara.leg.br/transparencia/                  ###
#       acesso-a-informacao/copy_of_perguntas-frequentes/           ###
#       cota-para-o-exercicio-da-atividade-parlamentar             ###
######################################################################

#### Para entender se os gastos estão sendo atingidos ou ultrapassados
### Precisamos saber qual a cota é destinada a cada parlamentar.
### Sabemos que a cota de cada um deles varia de acordo com o Estado pelo qual
### foi eleito. Portanto, vamos resgatar estes valores do site da Camara.
### Como os dados estão em formato de imagem, será necessário uma manipulação 
### para a obtenção dos dados em formato "data frame". Assim:

#install.packages("magick")
require(magick)
input <- image_read("https://www2.camara.leg.br/transparencia/acesso-a-informacao/copy_of_perguntas-frequentes/imagens/cotaporestado.GIF")

text <- input %>%
  image_resize("2000x") %>%
  image_convert(type = 'Grayscale') %>%
  image_trim(fuzz = 40) %>%
  image_write(format = 'png', density = '300x300') %>%
  tesseract::ocr_data()

#View(text)

text = text[,1]
text = data.frame(text)

estados = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53)

UF = text[estados,]
valor = text[-estados,]



Cotas = cbind(UF, valor)
Cotas = data.frame(Cotas)
names(Cotas) = c("UF", "Valor")
str(Cotas)

## Para que o R entenda que "valor" é um campo numérico, precisamos substituir
## a vírgula para "."

require(stringr)

Cotas$Valor = str_replace(Cotas$Valor, "[.]", "")
Cotas$Valor = str_replace(Cotas$Valor, ",", ".")

Cotas$Valor = as.numeric(Cotas$Valor)
str(Cotas)

Cotas$UF[Cotas$UF == "P|"] = "PI"


### Inserindo uma tabela com os nomes dos estados Brasileiros
### a fim de melhorar a visualização nas dashboards

# instalando pacote para a obtenção dos municípios brasileiros
#install.packages("geobr")
require(geobr)

# carregando shape files de todos municipios do Brasil
estados <- read_state(code_state ="all", year=2019)



### Criando uma tabela calendário para auxiliar na manipulação envolvendo datas

menor = min(data$datemissao);menor
maior = max(data$datemissao);maior

Calendário = seq(from = menor, to = maior, by="days")
Calendário = data.frame(Calendário)
names(Calendário) = "Data"


