
#### Carrega as bibliotecas ####
library(DBI)
library(tidyverse)
library(flextable)
library(survival)
library(gtsummary)
library(ggsurvfit)
library(tidyquant)

#### Fim do carregamento de bibliotecas ####
#### Estabelece a conexão com PostgreSQL ####
conn <- DBI::dbConnect(odbc::odbc(), Driver = "PostgreSQL Unicode",
                       Database = "projetos", UID = "jose", PWD = Sys.getenv("DBPASSWORD"))
dbExecute(conn,"set search_path = puc_idec")
#### Fim da conexão ####
### Extrai api e assuntos ####
# Para gerar essa tabela, foram aplicados os seguintes filtros:
#
#   1 - Foram obtidas todas as distribuições entre 31/12/2018 e 01/09/2023.
#
# 1 - Aplicou-se um segundo filtro para manter somente os seguintes assuntos:
#
#   Fornecimento de medicamentos
# uti/uci
# Fornecimento de insumos
# Tratamento médico-hospitalar
#
# 3 - Em seguida, foram removidas as distribuições com as seguintes classes:
#
#   Cumprimento de sentença
# Carta precatória
# Liquidação
#
# 4 - Removeram-se as ações em que a administração pública direta ou indireta ocupa o polo passivo ou ativo.

api <-  dbGetQuery(conn,"select * from api
     where exists (
                 select from parte_pa pa
                 where api.processo = pa.processo
                 )
                 and exists (
                 select from parte_at at
                 where api.processo = at.processo
                 )")

api |>
  mutate(assunto = str_to_sentence(assunto)) |>
  count(assunto, sort = T) |>
  mutate(assunto = ifelse(str_detect(assunto, "uti"), "UTI/UCI", assunto)) |>
  ggplot(aes(x = n, y = reorder(assunto, n))) +
  geom_bar(stat = 'identity', fill = "darkblue")+
  geom_label(aes(label = n)) +
  labs(x = "Quantidade",
       y = "Assuntos",
       caption = "Fonte: TJSP")+
  theme_bw()

ggsave("graficos/assuntos.png", width = 10, height  = 6 )

system("cp graficos/assuntos.png ../saudeTJSP/graficos/assuntos.png")


#### fim da API ####
### Polo passivo ####
pa <- dbGetQuery(conn, "select * from parte_pa where tipo_pessoa = 'juridica'") |>
  mutate(documento_principal = coalesce(documento_principal, codigo_documento) |> str_remove_all("\\D")) |>
  mutate(parte1 = case_when(
    str_detect(parte, "(?i)unimed") ~ "Unimed",
    str_detect(parte, "(?i)bradesco") ~ "Bradesco",
    str_detect(parte, "(?i)\\bamil\\b")  ~  "Amil",
    str_detect(parte, "(?i)notre") ~ "NotreDame",
    str_detect(parte, "(?i)interm.dica") ~ "NotreDame",
    str_detect(parte, "(?i)sul\\s?am.rica") ~ "Sul América",
    str_detect(parte, "(?i)prevent")  ~   "Prevent Sênior",
    str_detect(parte,"(?i)Hapvida")  ~ "Hapvida",
    str_detect(parte, "(?i)São Francisco Sistemas? de Saúde") ~ "São Francisco SS",
    str_detect(parte, "(?i)porto seguro") ~ "Porto Seguro",
    str_detect(parte, "(?i)sompo") ~ "Sompo",
    str_detect(parte,"(?i)santa casa") ~ "Santa Casa",
    str_detect(parte,"(?i)santa helena") ~ "Santa Helena",
    str_detect(parte,"(?i)(cassi\\b|banco do brasil)") ~ "Cassi",
    str_detect(parte,"(?i)hpvida") ~ "Hapvida",
    str_detect(parte,"(?i)qualicorp") ~ "Qualicorp",
    str_detect(parte,"(?i)caixa beneficente") ~ "Caixas beneficentes",
    str_detect(parte,"(?i)cruz azul") ~ "Cruz Azul",
    str_detect(parte,"(?i)São Crist[óo]vão") ~ "São Cristóvão",
    str_detect(parte,"(?i)trasmontano") ~ "Trasmontano",
    str_detect(parte,"(?i)ameplan") ~ "Ameplan",
    str_detect(parte,"(?i)sobam") ~ "Sobam",
    str_detect(parte,"(?i)iamspe") ~ "Iamspe",
    str_detect(parte,"(?i)cesp") ~ "Cesp",
    str_detect(parte,"(?i)ana costa") ~ "Ana Costa",
    str_detect(parte,"(?i)mediservice") ~ "Mediservice",
    str_detect(parte,"(?i)ita[úu]") ~ "Itaú",
    str_detect(parte,"(?i)UNIÃO FEDERAL - PRU") ~ "UNIÃO FEDERAL - PRU",
    str_detect(parte,"(?i)(alvorecer|blue med)") ~ "Blue Med",
    str_detect(parte,"(?i)Postal Saúde") ~ "Postal Saúde",
    str_detect(parte,"(?i)biovida") ~ "Biovida",
    str_detect(parte,"(?i)Portuguesa de Beneficência") ~ "Beneficência portuguesa",
    str_detect(parte,"(?i)Uni ?hosp") ~ "Unihosp",
    str_detect(parte,"(?i)hb saúde") ~ "Hb Saúde",
    str_detect(parte,"(?i)green line") ~ "Green Line",
    str_detect(parte,"(?i)Geap") ~ "Geap",
    str_detect(parte,"(?i)omint") ~ "Omint",
    str_detect(parte,"(?i)são lucas") ~ "São Lucas",
    str_detect(parte,"(?i)allianz") ~ "Allianz",
    str_detect(parte,"(?i)qsa[úu]de") ~ "QSaúde",
    str_detect(parte,"(?i)samaritano") ~ "Samaritano",
    str_detect(parte,"(?i)economus") ~ "Economus",
    str_detect(parte,"(?i)(santo andré|medical health)") ~ "Medical Health",
    str_detect(parte,"(?i)(Francisco Xavier|usisa[úu]de)") ~ "Usisaúde",
    TRUE ~ "outros"
  ))

pa |>
  count(parte1, sort = T) |>
  filter(n > 500) |>
  ggplot(aes(x = n, y = reorder(parte1,n))) +
  geom_bar(stat = "identity",fill = "darkblue", show.legend = FALSE) +
  geom_label(aes(label = n))+
  scale_fill_viridis_d(option="A") +
  labs(x = "Quantidade de ações",
       y = "Operadora") +
  theme_bw()

ggsave("graficos/polo_passivo.png", width = 10, height  = 5 )

system("cp graficos/polo_passivo.png ../saudeTJSP/graficos/polo_passivo.png")

saveRDS(pa,"../saudeTJSP/tabelas/polo_passivo.rds")


tb_mes |>
  ggplot(aes(x = mes, y = n)) +
  ggplot2::geom_line(colour = "red") +
  geom_ma(ma_fun = SMA, n = 6) +
  labs(x = "Anos", y = "Número de distribuições",
       caption = "Fonte: TJSP") +
theme_bw()

ggsave("graficos/tb_mes.png", width = 10)

#### Fim do polo passivo ####
### Beneficiários dos planos ####
beneficiarios <- readxl::read_excel("data-raw/Usuarios_SP.xlsx") |>
  slice_tail(n=-4) |>
setNames(c("operadora","beneficiarios","proporcao")) |>
  mutate(beneficiarios = as.integer(beneficiarios)) |>
  mutate(parte1 = case_when(
    str_detect(operadora, "(?i)unimed") ~ "Unimed",
    str_detect(operadora, "(?i)bradesco") ~ "Bradesco",
    str_detect(operadora, "(?i)\\bamil\\b")  ~  "Amil",
    str_detect(operadora, "(?i)notre") ~ "NotreDame",
    str_detect(operadora, "(?i)interm.dica") ~ "NotreDame",
    str_detect(operadora, "(?i)sul\\s?am.rica") ~ "Sul América",
    str_detect(operadora, "(?i)prevent")  ~   "Prevent Sênior",
    str_detect(operadora,"(?i)Hapvida")  ~ "Hapvida",
    str_detect(operadora, "(?i)São Francisco Sistemas? de Saúde") ~ "São Francisco SS",
    str_detect(operadora, "(?i)porto seguro") ~ "Porto Seguro",
    str_detect(operadora, "(?i)sompo") ~ "Sompo",
    str_detect(operadora,"(?i)santa casa") ~ "Santa Casa",
    str_detect(operadora,"(?i)santa helena") ~ "Santa Helena",
    str_detect(operadora,"(?i)(cassi\\b|banco do brasil)") ~ "Cassi",
    str_detect(operadora,"(?i)hpvida") ~ "Hapvida",
    str_detect(operadora,"(?i)qualicorp") ~ "Qualicorp",
    str_detect(operadora,"(?i)caixa beneficente") ~ "Caixas beneficentes",
    str_detect(operadora,"(?i)cruz azul") ~ "Cruz Azul",
    str_detect(operadora,"(?i)São Crist[óo]vão") ~ "São Cristóvão",
    str_detect(operadora,"(?i)trasmontano") ~ "Trasmontano",
    str_detect(operadora,"(?i)ameplan") ~ "Ameplan",
    str_detect(operadora,"(?i)sobam") ~ "Sobam",
    str_detect(operadora,"(?i)iamspe") ~ "Iamspe",
    str_detect(operadora,"(?i)cesp") ~ "Cesp",
    str_detect(operadora,"(?i)ana costa") ~ "Ana Costa",
    str_detect(operadora,"(?i)mediservice") ~ "Mediservice",
    str_detect(operadora,"(?i)ita[úu]") ~ "Itaú",
    str_detect(operadora,"(?i)UNIÃO FEDERAL - PRU") ~ "UNIÃO FEDERAL - PRU",
    str_detect(operadora,"(?i)(alvorecer|blue med)") ~ "Blue Med",
    str_detect(operadora,"(?i)Postal Saúde") ~ "Postal Saúde",
    str_detect(operadora,"(?i)biovida") ~ "Biovida",
    str_detect(operadora,"(?i)Portuguesa de Beneficência") ~ "Beneficência portuguesa",
    str_detect(operadora,"(?i)Uni ?hosp") ~ "Unihosp",
    str_detect(operadora,"(?i)hb saúde") ~ "Hb Saúde",
    str_detect(operadora,"(?i)green line") ~ "Green Line",
    str_detect(operadora,"(?i)Geap") ~ "Geap",
    str_detect(operadora,"(?i)omint") ~ "Omint",
    str_detect(operadora,"(?i)são lucas") ~ "São Lucas",
    str_detect(operadora,"(?i)allianz") ~ "Allianz",
    str_detect(operadora,"(?i)qsa[úu]de") ~ "QSaúde",
    str_detect(operadora,"(?i)samaritano") ~ "Samaritano",
    str_detect(operadora,"(?i)economus") ~ "Economus",
    str_detect(operadora,"(?i)(santo andré|medical health)") ~ "Medical Health",
    str_detect(operadora,"(?i)(Francisco Xavier|usisa[úu]de)") ~ "Usisaúde",
    TRUE ~ "outros"
  ))


operadoras <- beneficiarios |>
  group_by(parte1) |>
  summarize(q = sum(beneficiarios))


pa_op <- pa |>
   select(parte1) |>
   count(parte1) |>
   left_join(operadoras)

pa_op <- pa_op |>
    drop_na()


pa_op <- pa_op |>
    mutate(d_c = ((n/q)*1000) |> round(3))

pa_op <- pa_op |>
  setNames(c("operadora","demandas","beneficiarios","demandas_por_mil"))


pa_op <- pa_op |>
    mutate(perc_demandas = (demandas/sum(demandas)) |> scales::percent(accuracy = 0.1, decimal_mark = ","),
           perc_beneficiarios = (beneficiarios/sum(beneficiarios)) |> scales::percent(accuracy = 0.1,decimal_mark = ",")
                            )

saveRDS(pa_op, "tabelas/operadoras_beneficiarios.rds")

system("cp tabelas/operadoras_beneficiarios.rds ../saudeTJSP/tabelas/operadoras_beneficiarios.rds")

virgula <- scales::label_number(decimal.mark = ",")
pa_op |>
  ggplot(aes(x = demandas_por_mil, y = reorder(operadora,demandas_por_mil))) +
  geom_bar(stat = "identity",fill = "darkblue", show.legend = FALSE) +
  geom_label(aes(label = virgula(demandas_por_mil)))+
  scale_fill_viridis_d(option="A") +
  labs(x = "Demandas por mil beneficiários",
       y = "Operadora") +
  theme_bw()

ggsave("graficos/op_be.png", width = 10,  height = 6)

system("cp graficos/op_be.png ../saudeTJSP/graficos/op_be.png")

#### Fim dos beneficiários de planos ####
#### Distribuição no tempo ####
tb_mes <- dbGetQuery(conn,"select
                 processo,
                 substring(json -> 'dadosBasicos' ->> 'dataAjuizamento', 1,8) as data,
                 assunto
                 from dados_basicos db
                 inner join api using (processo)
                 where exists (
                 select from parte_pa pa
                 where db.processo = pa.processo
                 and pa.tipo_pessoa = 'juridica'
                 )
                 and exists (
                 select from parte_at at
                 where db.processo = at.processo
                 and at.tipo_pessoa = 'fisica'
                 )") |>
  drop_na() |>
  mutate(assunto = str_to_sentence(assunto),
         data = ymd(data)) |>
  mutate(assunto = ifelse(str_detect(assunto, "uti"),"uti/uci",assunto)) |>
  mutate(mes = lubridate::floor_date(data,"month"))

### Para folha de São Paulo ####

mes_a_mes <- tb_mes |>
    dplyr::count(mes)

writexl::write_xlsx(tb_mes, "data/folha_tb_mes.xlsx")
writexl::write_xlsx(mes_a_mes, "data/mes_a_mes.xlsx")

#### fim para Folha de São Paulo ####


vertical <- function(data, descricao){

  data.frame(x1 = lubridate::dmy(data),
             x2 = lubridate::dmy(data),
             y1 =  0,
             y2 = 1000)

}

stj <- data.frame(x1 = as.Date("2022-06-08"),
x2 = as.Date("2022-06-08"),
y1 = 0,
y2 = 1000)


stj1 <- vertical("08/06/2022")

lei <- data.frame(x1 = as.Date("2022-09-21"),
                  x2 = as.Date("2022-09-21"),
                  y1 = 0,
                  y2 = 1000)

resolucao <- data.frame(x1 = as.Date("2022-06-23"),
                  x2 = as.Date("2022-06-23"),
                  y1 = 0,
                  y2 = 1000)

tb_mes |>
  filter(str_detect(assunto,"(?i)m[ée]d")) |>
  filter(mes != as.Date('2023-09-01')) |>
  count(mes, assunto) |>
  ggplot(aes(x = mes, y = n, colour = assunto)) +
  geom_line( show.legend = F) +
  scale_colour_manual(values = c("red","blue"))+
  geom_ma(ma_fun = SMA, n = 6)+
scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")+
  #geom_text(aes(label = n), size = 2)+
  geom_segment(aes(x = x1,
                   xend=x2,
                   y = y1,
                   yend=y2),
                   linetype = "dashed",
                   colour = "black",
data = vertical("08/06/2022"))+
  geom_segment(aes(x = x1,
                   xend=x2,
                   y = y1,
                   yend=y2),
               linetype = "dashed",
               colour = "black",
               data = vertical("03/08/2022"))+
  geom_segment(aes(x = x1,
                   xend=x2,
                   y = y1,
                   yend=y2),
               linetype = "dashed",
               colour = "black",
               data = vertical("21/09/2022"))+
  annotate("text",
           x = as.Date("2022-06-08")+20,
           y = 400,
           label = "
           Decisão STJ",
           size = 2)+
  annotate("text",
           x = as.Date("2022-08-03")+35,
           y = 500,
           label = "Publicação STJ",
           size = 2)+
  annotate("text",
           x = as.Date("2022-09-21")+35,
           y = 800,
           label = "Lei 14.454/2022",
           size = 2)+
  # geom_text(aes(label = "STJ", x = as.Date("2022-6-08"), y = 800),
  #           hjust = 1.5,
  #           angle = 90,
  #           size = 2)+
  labs(x = "Mês", y = "Número de distribuições",
       caption = "Fonte: TJSP",
       colour = "Pedido") +
  theme_bw()+
  theme(axis.text.x  = element_text(angle  = 45, size = 7), legend.position = "bottom")

ggsave("graficos/tb_mes.png", width = 10,  height = 5)

system("cp graficos/tb_mes.png ../saudeTJSP/graficos/tb_mes.png")
#### Fim da distribuição no tempo ####
#### Distribuição das doenças ####
library(quanteda)
documentos <- dbGetQuery(conn,"table peticoes_iniciais")

documentos <- documentos |>
  #rownames_to_column() |>
  mutate(id = paste(processo, id_documento, rowname, sep = "#"))

corpo <- documentos |>
  corpus(docid_field = "id",
         text_field = "texto")

cid <- kwic(corpo, "\\bCID[-:.\\d]?[01]?", 20, valuetype = "regex")

cid <- cid |>
  as_tibble()

cid <- cid |>
  mutate(keyword = ifelse(str_detect(keyword, "(?i)medicinanet"),str_extract(keyword,"\\w+(?=_)"), keyword))

cid <- cid |>
  mutate(keyword = ifelse(str_detect(keyword, "(?i)telemedicina"),str_extract(keyword,"\\w+$"), keyword))
cid <- cid |>
  mutate(keyword = str_remove(keyword, "(?i)cid"))

cid <- cid |>
  mutate(processo = str_extract(docname, "\\d{20}"))

cid <- cid |>
  #as_tibble() |>
  group_by(processo) |>
  unite("j", pre, keyword, post, sep = " ") |>
  reframe(juncao = str_c(j, collapse = "\n"))

cids <- str_extract_all(cid$juncao, "\\b[A-Z].{0,3}\\d+")

url1 <- "https://www.cremesp.org.br/resources/views/site_cid10_tabela.php"

httr::set_config(httr::config(ssl_verifypeer = FALSE))

r1 <- httr::GET(url1)

tb1 <- r1 |>
  httr::content("text") |>
  xml2::read_html() |>
  rvest::html_table() |>
  pluck(1) |>
  janitor::clean_names()

cids <- cids |>
  map(~{
    .x |>
      str_remove_all("\\W") |>
      unique()
  })

tb2 <- tibble(cid1 = cids, processo = cid$processo) |>
  unnest(cid1) |>
  mutate(cid2 = str_sub(cid1, 1,3)) |>
  left_join(tb1, by = c("cid2"="codigo"))

doencas <- tb2 |>
  count(cid2,descricao, sort = T) |>
  drop_na()


doencas |>
  mutate(perc = (n/sum(n)) |> scales::percent()) |>
           View()

system("cp tabelas/doencas.rds ../saudeTJSP/doencas/doencas.rds")


doencas |>
 filter(n > 300) |>
  mutate(descricao = str_wrap(descricao, 40)) |>
  ggplot(aes(x = n, y = reorder(descricao,n))) +
  geom_bar(stat = "identity",fill = "darkblue", show.legend = FALSE) +
  geom_label(aes(label = n))+
  scale_fill_viridis_d(option="A") +
  labs(x = "Quantidade de ações",
       y = "Doença conforme CID-10") +
  theme(axis.text.y = element_text(size = rel(1.5))) +
  theme_bw()

ggsave("graficos/doencas.png", width = 15,  height = 8)

system("cp graficos/doencas.png ../saudeTJSP/graficos/doencas.png")

#### Fim da distribuição por doenças ####
#### Distribuição do autismo no tempo ####

url1 <- "https://raw.githubusercontent.com/ndtj/saudeTJSP/main/data/saude_suplementar_demanda_tjsp.json"

saude <- jsonlite::fromJSON(url1) |>
         filter(assunto == "Tratamento médico-hospitalar") |>
         inner_join(select(tb2, processo, descricao)) |>
        drop_na() |>
        mutate(mes = as.Date(mes)) |>
        mutate(autismo = ifelse(descricao == "Transtornos globais do desenvolvimento", "Transtornos globais do desenvolvimento", "Outra doença"))


autismo <- saude |>
      count(mes, autismo)

autismo <- autismo |>
      pivot_wider(names_from = autismo, values_from = n)

autismo |>
  ggplot(aes(x = mes, y = n, colour = autismo)) +
  geom_line( )+
scale_colour_manual(values = c("red","blue"))+
  geom_ma(ma_fun = SMA, n = 6)+
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y")+
  #geom_text(aes(label = n), size = 2)+
  labs(x = "Mês", y = "Número de distribuições",
       caption = "Fonte: TJSP",
       colour = "Doença") +
  theme_bw()+
  theme(axis.text.x  = element_text(angle  = 45), legend.position = "bottom")

ggsave("graficos/autismo.png", width = 10,  height = 5)

system("cp graficos/autismo.png ../saudeTJSP/graficos/autismo.png")


#### Distribuição dos grupos de doenças ####
tb4 <- read_html("http://tabnet.datasus.gov.br/cgi/sih/mxcid10lm.htm") |>
  html_table() |>
  pluck(3) |>
  tail(-3) |>
  group_by(X1) |>
  filter(row_number() == 1) |>
  head(-1)

tb4 <- tb4 |>
  mutate(l1 = str_extract(X4, "\\p{L}"),
         n1 = str_extract(X4,"\\d+") |> as.integer(),
         l2 = str_extract(X4, "\\p{L}(?=\\d+$)"),
         n2 = str_extract(X4,"\\d+$") |> as.integer()
  )


tb4 <- tb4 |>
mutate(n3 = case_when(
  l1 == l2 ~ paste0(l1, str_pad(n1:n2,width=2, pad="0")) |> str_c(collapse = ";"),
  TRUE ~ c(paste0(l1, str_pad(n1:99,width=2,pad = "0")),
           paste0(l2, str_pad(0:n2, width = 2, pad = "0"))) |> str_c(collapse = ";")
)
)

tb4 <- tb4 |>
  mutate(n4 = str_split(n3, ";")) |>
  select(grupo = X3, cid = n4) |>
  unnest(cid)

doencas <- doencas |>
  left_join(tb4, by = c("cid2"="cid"))

grupos <- doencas |>
  group_by(grupo) |>
  summarize(n = sum(n))

grupos |>
  drop_na() |>
  #mutate(descricao = str_wrap(descricao, 40)) |>
  ggplot(aes(x = n, y = reorder(grupo,n))) +
  geom_bar(stat = "identity",fill = "darkblue", show.legend = FALSE) +
  geom_label(aes(label = n))+
  scale_fill_viridis_d(option="A") +
  labs(x = "Quantidade de ações",
       y = "Grupo de doenças conforme CID-10") +
  theme(axis.text.y = element_text(size = rel(1.5))) +
  theme_bw()

ggsave("graficos/grupos_doencas.png", width = 12,  height = 6)

system("cp graficos/grupos_doencas.png ../saudeTJSP/graficos/grupos_doencas.png")
#### Fim da distribuição dos grupos de doenças ####
#### Análise de série temporaral ####

df <- tb_mes |>
  count(assunto, mes) |>
  filter(str_detect(assunto,"(?i)m[ée]d")) |>
  pivot_wider(names_from =assunto, values_from = n) |>
  set_names(c("mes","medicamento","tratamento")) |>
  mutate(pandemia = case_when(
    mes < as.Date("2020-03-01") ~ 0,
    mes > as.Date("2022-03-01") ~ 0,
    TRUE ~ 1
  ),
  stj = case_when(
    mes < as.Date("2022-06-01") ~ 0,
    mes > as.Date("2022-10-01") ~ 0,
    TRUE ~ 1
  ),
  lei = case_when(
    mes < as.Date("2022-10-01") ~ 0,
    TRUE ~ 1
  ))


df <- df |> ### exclui setembro de 2023 por ora.
  filter(mes != as.Date("2023-09-01"))

demanda_ts <- ts(df$tratamento, start(2019,1), frequency = 12)

m_dummies <- with(df, cbind(pandemia, stj, lei))

auto.arima(demanda_ts, trace = TRUE, approximation = F, xreg = m_dummies)


#### Coleta dados sobre liminares e tempo até liminar ####
q <- "select db.processo, substring(json -> 'dadosBasicos' ->> 'dataAjuizamento', 1,8) as data_judicializao,
      descricao as decisao,
      api.assunto,
      substring(dh,1,8) as data_decisao
      from dados_basicos db
      inner join vmovimentacao using(processo)
      inner join api using (processo)
     where exists (
                 select from parte_pa pa
                 where db.processo = pa.processo
                 )
                 and exists (
                 select from parte_at at
                 where db.processo = at.processo
                 )
    and descricao ~* 'concedida.+?(liminar|antecipa)'
    "
tb <- dbGetQuery(conn,q) |>
  mutate(data_judicializao = ymd(data_judicializao),
         data_decisao = ymd(data_decisao),
         decisao = ifelse(str_detect(decisao, "(?i)não concedida"), "nao concedida", "concedida"),
         assunto = str_to_sentence(assunto)) |>
  mutate(lapso = tjsp::lapso(data_judicializao, data_decisao, unidade = "dia"))

#### Fim da coleta de dados sobre liminar e tempo até liminar #####
#### Gera gráfico sobre liminares ####
tb |>
  select(processo, decisao) |>
  count(decisao, sort = T) |>
  #group_by(decisao) |>
  mutate(per = (n/sum(n)) |> scales::percent(),
         decisao = ifelse(decisao == "concedida", "Concedida", "Não concedida")) |>
 ggplot(aes(x = n, y = reorder(decisao,n))) +
  geom_bar(stat = "identity",fill = "darkblue", show.legend = FALSE) +
  geom_label(aes(label = per))+
  #scale_fill_viridis_d(option="A") +
  labs(x = "Quantidade de ações",
       y = "Decisão",
       title = "Liminares em ações contra operadoras de saúde") +
  theme_bw()

ggsave("graficos/liminar.png", width = 10,  height = 5)

system("cp graficos/liminar.png ../saudeTJSP/graficos/liminar.png")
#### Fim do gráfico sobre liminar #####
### Início da análise e duração em relação à decisão ####

p <- tb |>
  mutate(decisao = ifelse(decisao == "concedida", "Concedida", "Não concedida")) |>
  survfit2(Surv(lapso) ~ decisao, data = _) |>
  ggsurvfit(linewidth = 1) +
  coord_cartesian(xlim = c(0, 60)) +
  scale_color_manual(values = c('red', 'darkblue')) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent
    #expand = c(0.01, 0)
  ) +
  scale_x_continuous(breaks = seq(1, 60, by = 2))+
  #add_confidence_interval() +
  add_risktable(times = c(3,5,10,20,40,60),
                stats_label = c("Pendentes", "Decididos")) +
  add_quantile(y_value = 0.5, color = "blue")+
  #scale_ggsurvfit()+
  labs(x = "Tempo em dias",
       y = "Percentual de pedidos pendentes")

ggsave("graficos/km_decisao.png", width = 16, height = 9)

system("cp graficos/km_decisao.png ../saudeTJSP/graficos/km_decisao.png")

#### Fim da análise de duração em relação à decisão #####
