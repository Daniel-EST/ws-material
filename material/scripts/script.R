# Universidade Federal Fluminense (UFF)
# Uma abordagem prática para o básico de Web scraping.

# Seção 1 ----------- Pacotes
# Instalaremos os seguintes pacotes:
# * rvest ( scrapping de páginas em html sem interatividades em javascript )
# * RSelenium ( scrapping de páginas  com intertividades em javascript )
# * stringr ( manipulação de strings )
# * magrittr ( operadores pipe ' %>% ')
# * readr ( manipulação de arquivos )
# * qdapTools
# * purrr
# * tidyverse
# install.packages(c('rvest', 'RSelenium', 'stringr', 'magrittr', 'purrr', 'qdapTools'))
# install.packages('tidyverse')

# Carregando alguns desses pacotes.
require(stringr)
require(magrittr)
require(readr)

# Seção 2 --- Web scraping
# Seção 2.1 ----------- rvest
# Nosso primeiro exemplo de web scraping será no site IMDB, que oferece informações
# gerais de filmes. ( https://www.imdb.com )
url <- 'https://www.imdb.com'

# Checando robots.txt
robots <-
  paste(url, 'robots.txt', sep = '/') # gerando o link para a página do robots.txt.
robots_txt <-
  read_tsv(robots, col_types = 'c') # lendo o robots.txt e colocando em um tibble.
View(robots_txt) # Visualizando o objeto.
# Pelo robots.txt verificamos que não há restrição sob a coleta de informações de alguns filmes
# de sua base de dados.

# Nosso objetivo agora é utilizar o pacote rvest para coletar informações de algum filme
# presente no site.
# Coletaremos as seguintes informações dos filmes:
# * Título ;
# * Sinópse ;
# * Ano ;
# * Avaliação ;
# * Pôster
# Carregando o pacote.
require(rvest)

# Utilizaremos como exemplo o filme Akira.
url <- 'https://www.imdb.com/title/tt0094625/' # Url do filme Akira

# Carregando a página e colocando-a em um objeto.
html <- read_html(url)

# Coletando título usando a tag 'h1'.
titulo <-
  html %>%
  html_node('h1') %>% # 'h1' é a tag do node que estamos coletando
  html_text() %>% # coletando texto dentro desse node
  str_trim() # utilizando o pacote stringr para cortar os espaços em branco

# Coletando a sínópse usando a class 'summary_text'
sinopse <-
  html %>%
  html_node('.summary_text') %>% # Nas classes adicionamos '.' antes do nome da classe
  html_text() %>%
  str_trim()

# Coletano o ano usando o id 'titleYear'
ano <-
  html %>%
  html_node('#titleYear') %>%  # Nos ids adicionamos '#' antes do nome do id.
  html_text() %>%
  str_extract('\\d+') # usando regex para coletar apenas o ano sem os parênteses.

# Coletando a avalição usando xpath '//*[@id="title-overview-widget"]/div[1]/div[2]/div/div[1]/div[1]/div[1]/strong/span'
# Problemas do xpath:
# O xpath é uma posição fixa, não podemos garantir que em todas as páginas o xpath do que
# queremos será o mesmo. Diferente dos métodos class, tag e id que são 'dinâmicos,.
# qualquer menor alteração na página pode quebrar nosso código.

avaliacao <-
  html %>%
  html_node(xpath = '//*[@id="title-overview-widget"]/div[1]/div[2]/div/div[1]/div[1]/div[1]/strong/span') %>%
  html_text() %>%
  as.numeric()

# A avaliação também pode ser coletada usando a class 'ratingValue', veja abaixo..
html %>%
  html_node('.ratingValue') %>%
  html_text() %>%
  str_trim() %>%
  str_sub(end = -4) %>%
  as.numeric()

# Coletando o poster usando uma class e uma tag.
poster <-
  html %>%
  html_node('.poster img') %>% # perceba que a tag 'img' esta encadeada a class 'poster'.
  html_attr('src') # coletando o atributo 'src' que contem o link da imagem
# Fazendo download da imagem e salvando na pasta 'poster'
dir.create('poster')
download.file(poster,
              paste0('poster//', titulo, '.jpg'),
              mode = 'wb',
              method = 'curl')

# Automatizando
filmes <-
  c(
    'https://www.imdb.com/title/tt2861424',
    'https://www.imdb.com/title/tt0169858',
    'https://www.imdb.com/title/tt0156887',
    'https://www.imdb.com/title/tt0246578',
    'https://www.imdb.com/title/tt1375666',
    'https://www.imdb.com/title/tt0066921'
  )

info_filmes <- function(url)
{
  html <- read_html(url)
  
  titulo <-
    html %>%
    html_node('h1') %>% # 'h1' é a tag do node que estamos coletando
    html_text() %>% # coletando texto dentro desse node
    str_trim()
  
  sinopse <-
    html %>%
    html_node('.summary_text') %>% # Nas classes adicionamos '.' antes do nome da classe
    html_text() %>%
    str_trim()
  
  ano <-
    html %>%
    html_node('#titleYear') %>%  # Nos ids adicionamos '#' antes do nome do id.
    html_text() %>%
    str_extract('\\d+')
  
  avaliacao <-
    html %>%
    html_node('.ratingValue') %>%
    html_text() %>%
    str_trim() %>%
    str_sub(end = -4) %>%
    as.numeric()
  
  poster <-
    html %>%
    html_node('.poster img') %>% # perceba que a tag 'img' esta encadeada a class 'poster'.
    html_attr('src') # coletando o atributo 'src' que contem o link da imagem
  # Fazendo download da imagem e salvando na pasta 'poster'
  download.file(poster,
              paste0('poster//', titulo, '.jpg'),
              mode = 'wb',
              method = 'curl')
  
  Sys.sleep(1) # Dando uma pausa de um segundo para ser gentil com o servidor da IMDB.
  return(t(
    c(
      title = titulo,
      summary = sinopse,
      year = ano,
      rate = avaliacao,
      post = poster
    )
  ))
}

imdb <-
  purrr::map(filmes, info_filmes) %>%
  qdapTools::list_df2df()

View(imdb)

# Seção 2.2 ----------- RSelenium
# Agora utilizaremos o RSelenium para coletar informações de produtos no site da
# submarino ( https://www.submarino.com.br )
url <- 'https://www.submarino.com.br'

# Checando robots.txt
robots <-
  paste(url, 'robots.txt', sep = '/') # gerando o link para a página do robots.txt.
robots_txt <-
  read_tsv(robots, col_types = 'c') # lendo o robots.txt e colocando em um tibble.
View(robots_txt) # Visualizando o objeto.

# Nosso objetivo agora é utilizar o pacote RSelenium para coletar informações de preço
# e frete de algum produto.
# Coletaremos as seguintes informações dos produtos:
# * Nome ;
# * Categoria ;
# * Preço ;
# * Custo do frete mais econômico.

# Carregando o pacote.
require(RSelenium)

# Iniciando nosso navegador automatizado
rD <- rsDriver(browser = c("firefox"))
remDr <- rD$client
url <- 'https://www.submarino.com.br/produto/132806882/'
cepNum <- '26290027'

# Realizando a consulta.
remDr$navigate(url) # Fazendo com que nosso navegador automatizado entre no URL especificado
produto <-
  remDr$findElement(using = 'class',
                    value = 'product-name') # Procurando o elemento por class.
produto <-
  produto$getElementText() %>% unlist() # Coletando o texto desse elemento.

cod <-
  remDr$findElement(using = 'class',
                    value = 'product-id') # Procurando utilizando class.
cod <- cod$getElementText() %>% unlist()

preco <-
  remDr$findElement(using = 'class',
                    value = 'sales-price')
preco <- preco$getElementText() %>% unlist()

cep <-
  remDr$findElement(using = 'id',
                    value = 'input-freight-product')
cep$clickElement() # Clicando no campo para digitar o CEP
cep$sendKeysToElement(list(cepNum,
                           key = "enter")) # inputa o cep no campo desejado.

frete <-
  remDr$findElement(using = 'class',
                    value = 'card-freight')
frete <-
  frete$getElementText() %>%
  unlist() %>%
  str_extract('\nEconômica R\\$ \\d+,\\d+') %>%
  str_extract('R\\$ \\d+,\\d+')


# Agora queremos consultar todos esses links
urls <- c(
  'https://www.submarino.com.br/produto/132806882/',
  'https://www.submarino.com.br/produto/31716174/',
  'https://www.submarino.com.br/produto/31541986/',
  'https://www.submarino.com.br/produto/132490742/',
  'https://www.submarino.com.br/produto/133678229/',
  'https://www.submarino.com.br/produto/399697/',
  'https://www.submarino.com.br/produto/113091965/',
  'https://www.submarino.com.br/produto/128775024/'
)

cepNum <- '26290027' # Cep que utilizaremos


# Agora criei uma função para automatizar
collect <- function(url, cepNum, remDr = rD$client)
{
  remDr$deleteAllCookies()
  remDr$navigate(url) # Fazendo com que nosso navegador automatizado entre no URL especificado
  produto <-
    remDr$findElement(using = 'class',
                      value = 'product-name') # Procurando o elemento por class.
  produto <-
    produto$getElementText() %>% unlist() # Coletando o texto desse elemento.
  
  cod <-
    remDr$findElement(using = 'class',
                      value = 'product-id') # Procurando utilizando class.
  cod <- cod$getElementText() %>% unlist()
  
  preco <-
    remDr$findElement(using = 'class',
                      value = 'sales-price')
  preco <- preco$getElementText() %>% unlist()
  
  cep <-
    remDr$findElement(using = 'id',
                      value = 'input-freight-product')
  cep$clickElement() # Clicando no campo para digitar o CEP
  cep$sendKeysToElement(list(cepNum,
                             key = "enter")) # inputa o cep no campo desejado.
  
  Sys.sleep(1)
  frete <-
    remDr$findElement(using = 'class',
                      value = 'card-freight')
  frete <-
    frete$getElementText() %>%
    unlist() %>%
    str_extract('\nEconômica R\\$ \\d+,\\d+') %>%
    str_extract('R\\$ \\d+,\\d+')
  
  Sys.sleep(1)
  
  return(t(c(
    nome = produto,
    codigo = cod,
    price = preco,
    fre = frete
  )))
}

submarino <-
  purrr::map(urls, collect, cepNum = '26290027') %>%
  qdapTools::list_df2df()
View(submarino)
