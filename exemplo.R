# Pacotes
library(tidyverse)
library(rvest)

url <-
  'https://www.meucarronovo.com.br/buscar/q/HYUNDAI+HB20/tipo-veiculo/carros/origem/combo-home/situacao/novo/cidade/rio-de-janeiro'
# Vetores ##############
marca <-
  c(
    'CHEVROLET',
    'FIAT',
    'FORD',
    'HONDA',
    'HYUNDAI',
    'NISSAN',
    'PEUGEOT',
    'RENAULT',
    'TOYOTA',
    'VW'
  )


chevrolet <-
  c(
    'CELTA',
    'ONIX',
    'COBALT',
    'PRISMA',
    'SPIN'
    )

fiat <-
  c(
    'PALIO',
    'MOBI',
    'UNO',
    'SIENA',
    'PUNTO'
    )

ford <-
  c(
    'KA',
    'FIESTA',
    'FOCUS',
    'ECOSPORT'
    )

honda <-
  c(
    'FIT',
    'CITY',
    'CIVIC'
    )

hyundai <-
  c(
    'HB20',
    'HB20S'
    )


nissan <-
  c(
    'MARCH',
    'SENTRA',
    'VERSA'
    )

peugeot <-
  c(
    '208',
    '308'
    )

renault <-
  c(
    'SANDERO',
    'LOGAN',
    'DUSTER'
    )

toyota <-
  c('ETIOS',
    'COROLLA')

vw <-
  c(
    'GOL',
    'VOYAGE',
    'FOX',
    'UP!',
    'POLO'
    )

#####################

modelo <-
  list(
    chevrolet,
    fiat,
    ford,
    honda,
    hyundai,
    nissan,
    peugeot,
    renault,
    toyota,
    vw
    )

capital <-
  c(
    'rio-de-janeiro',
    'sao-paulo',
    'salvador',
    'porto-alegre',
    'recife',
    'brasilia',
    'belo-horizonte'
  )


gera_link <- function(marca, modelo, capital) {
  gerado <-
    apply(
      expand.grid(
        'https://www.meucarronovo.com.br/buscar/q/',
        marca,
        '+',
        modelo,
        '/origem/combo-home/situacao/novo/cidade/'
      ),
      1,
      paste,
      collapse = ""
    )
  return(gerado)
}

url <- mapply(gera_link, marca, modelo)

url %<>%
  map(~ apply(expand.grid(.x, capital),
              1,
              paste,
              collapse = '')) %>% unlist()

link <-
  map(url,
      function(urls) {
        Sys.sleep(1)
        read_html(urls) %>%
          html_nodes('.padd-r-10 a') %>%
          html_attr('href') %>%
          .[which(str_detect(., '/detalhe/'))]
      }) %>%
  unlist()

nodes <-
  c(
    'h2',
    '.line-h-22',
    '.line-h-22',
    '.line-h-22',
    '.line-h-22',
    '.line-h-22',
    '.bg-white'
  )

variaveis <-
  c(
    'modelo',
    'ano',
    'km',
    'portas',
    'combustivel',
    'cambio',
    'preco'
    )

dat <-
  map(link,
      function(links) {
        resposta <-
          read_html(links)
        Sys.sleep(1)
        map_chr(nodes,
                ~ resposta %>%
                  html_node(css = .x) %>%
                  html_text()) %>%
          set_names(variaveis)
      }) %>%
  set_names(link)

dat_tb <-
  do.call(rbind, dat) %>%
  as_tibble() %>%
  set_names(variaveis) %>%
  add_column(produto = link,
             data_coleta = lubridate::today()) %>%
  mutate(
    preco = preco  %>%
      str_replace('\r\n ', ''),
    
    ano = ano %>%
      str_extract('Ano: \\d+/\\d+') %>%
      str_extract('\\d+/\\d+'),
    
    km = km %>%
      str_extract('Km:\r\n *\\d+') %>%
      str_extract('\\d+'),
    
    portas = portas %>%
      str_extract('Portas: \\d') %>%
      str_extract('\\d'),
    
    combustivel = combustivel %>%
      str_extract('Combustível: (Flex|Gasolina|Gás|Dísel)') %>%
      str_extract('Flex|Gasolina|Gás|Dísel'),
    
    cambio = cambio %>%
      str_extract('Câmbio: (Automático|Manual)') %>%
      str_extract('Automático|Manual')
  )
write.csv2(dat_tb, 'meu-carro-novo.csv')
