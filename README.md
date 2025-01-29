# README

## Nome do Pacote

**educabR** - Acesso facilitado aos dados educacionais do Brasil

## Descrição

O pacote **educaBR** tem como objetivo fornecer uma interface simplificada para acessar e utilizar os dados educacionais disponibilizados pelo Ministério da Educação, incluindo informações do Instituto Nacional de Estudos e Pesquisas Educacionais Anísio Teixeira (INEP) e outros órgãos relacionados. A primeira versão do pacote foca no acesso aos dados do Índice de Desenvolvimento da Educação Básica (IDEB) por escola e município.

## Instalação

Atualmente, o pacote ainda não está disponível no CRAN. Para instalá-lo a partir do GitHub, utilize:

```r
# Instalar o pacote devtools, caso ainda não tenha
install.packages("devtools")

# Instalar o educaBR
devtools::install_github("rodrigoesborges/educabR")
```

## Uso

Após instalar o pacote, você pode carregar e utilizar a função para acessar os dados do IDEB:

```r
library(educaBR)

# Buscar dados do IDEB por município
dados_ideb <- obter_ideb(municipio = "São Paulo")

# Buscar dados do IDEB por escola
dados_ideb_escola <- obter_ideb(escola = "Nome da Escola")

# Visualizar os dados
head(dados_ideb)
```

## Funcionalidades

- `obter_ideb(municipio = "Nome do Município")`: Retorna os dados do IDEB para um município específico.
- `obter_ideb(escola = "Nome da Escola")`: Retorna os dados do IDEB para uma escola específica.

## Contribuição

O desenvolvimento do pacote está em andamento, e contribuições são bem-vindas. Caso encontre problemas ou tenha sugestões, abra uma issue no repositório do GitHub.

## Licença

Este projeto é licenciado sob a licença GPL.

---

# ROADMAP

## Versão 1.0 (Foco no IDEB)

-

## Versão 1.1

-

## Versão 2.0 (Expansão para outros indicadores)

-

## Versão 3.0 (Integração Avançada)

-

