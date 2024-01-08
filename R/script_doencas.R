#### Distribuição das doenças ####
library(tidyverse)
library(googledrive)
library(quanteda)



drive_download(as_id("https://drive.google.com/file/d/1pnAPKIEXFLooEKFbQz5ZuMN5kPwqGtuA/view?usp=drive_link"),
               path = "documentos.rds")

documentos <- readRDS("documentos.rds")


#### Cria uma coluna chamada id para garantir que cada linha é única.

documentos <- documentos |>
  #rownames_to_column() |>
  mutate(id = paste(processo, id_documento, rowname, sep = "#"))


### Cria um corpus para extração do contexto em que está o CID.
corpo <- documentos |>
  corpus(docid_field = "id",
         text_field = "texto")

#### Extrai os CIDs em seu contexto
cid <- kwic(corpo, "\\bCID[-:.\\d]?[01]?", 20, valuetype = "regex")

### Converte para tibble
cid <- cid |>
  as_tibble()

### Após exame, processa alguns ajueste na coluna keyword.
cid <- cid |>
  mutate(keyword = ifelse(str_detect(keyword, "(?i)medicinanet"),str_extract(keyword,"\\w+(?=_)"), keyword))

cid <- cid |>
  mutate(keyword = ifelse(str_detect(keyword, "(?i)telemedicina"),str_extract(keyword,"\\w+$"), keyword))

cid <- cid |>
  mutate(keyword = str_remove(keyword, "(?i)cid"))

cid <- cid |>
  mutate(processo = str_extract(docname, "\\d{20}"))


### Simplifica a tabela juntando as principais colunas.
cid <- cid |>
  #as_tibble() |>
  group_by(processo) |>
  unite("j", pre, keyword, post, sep = " ") |>
  reframe(juncao = str_c(j, collapse = "\n"))


### Extrai somenrte os CIDS
cids <- str_extract_all(cid$juncao, "\\b[A-Z].{0,3}\\d+")


#### Baixa a tabela de cids do Cremesp
url1 <- "https://www.cremesp.org.br/resources/views/site_cid10_tabela.php"

httr::set_config(httr::config(ssl_verifypeer = FALSE))

r1 <- httr::GET(url1)
### Organiza a tabela de CIDs proveniente do CREMESP

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


doencas <- doencas |>
  mutate(perc = (n/sum(n)) |> scales::percent())

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


#### Fim da distribuição por doenças ####
