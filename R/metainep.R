#'   metainep - basic metadata table - name , url, group/subject
#'   with auto-generated id - obtaining by scraping inep site at 2025-01
#'
#'   @format A data frame with 636 rows and 5 variables:
#' \describe{
#'   \item{grupo_id}{id do grupo/página superior principal}
#'   \item{grupo}{Nome do Grupo de assuntos/página superior principal}
#'   \item{grupo_url}{url do Grupo/Página superior}
#'   \item{assunto_id}{id do assunto}
#'   \item{assunto}{Assunto - subgrupo de indicadores}
#'   \item{assunto_url}{url do assunto}
#'   \item{id}{id da tabela}
#'   \item{tabela}{Nome da tabela}
#'   \item{periodo}{Periodo de referencia da tabela}
#'   \item{tab_url}{URL da tabela}
#'   }
#' @source \url{https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/}
"metainep"
