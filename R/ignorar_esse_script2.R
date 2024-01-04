library(DBI)
conn <- DBI::dbConnect(odbc::odbc(), Driver = "PostgreSQL Unicode",
                       Database = "projetos", UID = "jose", PWD = Sys.getenv("DBPASSWORD"))

DBI::dbExecute(conn,"set search_path = puc_idec")

api <- dbGetQuery(conn,"select * from api")

api <- api |>
    mutate(data_recebimento = dmy(data_recebimento), tipo= NULL)

writexl::write_xlsx(api, "data/metadados.xlsx")


### polo ativo
q <- "
 WITH cte AS (
         SELECT d.processo,
            jsonb_path_query(d.json, '$.dadosBasicos.polo[0].parte.pessoa'::jsonpath) AS pessoa_at,
            jsonb_path_query(d.json, '$.dadosBasicos.polo[0].parte.pessoa.documento'::jsonpath) AS documento_at
            jsonb_path_query(d.json, '$.dadosBasicos.polo[0].parte.pessoa'::jsonpath) AS pessoa_at,
            jsonb_path_query(d.json, '$.dadosBasicos.polo[0].parte.pessoa.documento'::jsonpath) AS documento_at

        from dados_basicos d
        )
 SELECT cte.processo,
    cte.pessoa ->> 'nome'::text AS parte,
    cte.pessoa ->> 'sexo'::text AS sexo,
    cte.pessoa ->> 'tipoPessoa'::text AS tipo_pessoa,
    cte.documento ->> 'codigoDocumento'::text AS codigo_documento,
    cte.pessoa ->> 'numeroDocumentoPrincipal'::text AS documento_principal
   FROM cte
"


polo_ativo <- dbGetQuery(conn,q)

writexl::write_xlsx(polo_ativo, "data/polo_ativoi.xlsx")

### polo passivo
q <- "
 WITH cte AS (
         SELECT d.processo,
            jsonb_path_query(d.json, '$.dadosBasicos.polo[1].parte.pessoa'::jsonpath) AS pessoa,
            jsonb_path_query(d.json, '$.dadosBasicos.polo[1].parte.pessoa.documento'::jsonpath) AS documento
        from dados_basicos d
        )
 SELECT cte.processo,
    cte.pessoa ->> 'nome'::text AS parte,
    cte.pessoa ->> 'sexo'::text AS sexo,
    cte.pessoa ->> 'tipoPessoa'::text AS tipo_pessoa,
    cte.documento ->> 'codigoDocumento'::text AS codigo_documento,
    cte.pessoa ->> 'numeroDocumentoPrincipal'::text AS documento_principal
   FROM cte
"


polo_passivo <- dbGetQuery(conn,q)


writexl::write_xlsx(polo_passivo, "data/polo_passivo.xlsx")

### Distribuição mensal

q <- "
select
processo,
substring(json -> 'dadosBasicos' ->> 'dataAjuizamento', 1,8) as data
from dados_basicos"

mensal <- DBI::dbGetQuery(conn,q)


