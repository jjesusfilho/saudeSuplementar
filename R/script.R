library(tidyverse)
library(readxl)

base <- readxl::read_xlsx("data/base.xlsx")

base <- base |>
  filter(data_recebimento >= between(dmy("31/12/2018"),dmy("01/09/2023"))) |>
  filter(str_detect(parte_at, '(?i)(fazenda|munic[íi]|secretári[oa]|prefei|^estado|diretor.+?departamento|diretor.+secretaria|secretaria|drs|faenda|procurador|uni[ãa]o)', negate = T)) |>
  filter(str_detect(parte_pa, '(?i)(fazenda|munic[íi]|secretári[oa]|prefei|^estado|diretor.+?departamento|diretor.+secretaria|secretaria|drs|faenda|procurador|uni[ãa]o)', negate = T)) |>
  filter(tipo_pessoa_at =="fisica", tipo_pessoa_pa =="juridica") |>
  mutate(documento_principal_pa = coalesce(documento_principal_pa, codigo_documento_pa) |> str_remove_all("\\D")) |>
  mutate(documento_principal_at = coalesce(documento_principal_at, codigo_documento_at) |> str_remove_all("\\D")) |>
  mutate(parte_pa_classificada = case_when(
    str_detect(parte_pa, "(?i)unimed") ~ "Unimed",
    str_detect(parte_pa, "(?i)bradesco") ~ "Bradesco",
    str_detect(parte_pa, "(?i)\\bamil\\b")  ~  "Amil",
    str_detect(parte_pa, "(?i)notre") ~ "NotreDame",
    str_detect(parte_pa, "(?i)interm.dica") ~ "NotreDame",
    str_detect(parte_pa, "(?i)sul\\s?am.rica") ~ "Sul América",
    str_detect(parte_pa, "(?i)prevent")  ~   "Prevent Sênior",
    str_detect(parte_pa,"(?i)Hapvida")  ~ "Hapvida",
    str_detect(parte_pa, "(?i)São Francisco Sistemas? de Saúde") ~ "São Francisco SS",
    str_detect(parte_pa, "(?i)porto seguro") ~ "Porto Seguro",
    str_detect(parte_pa, "(?i)sompo") ~ "Sompo",
    str_detect(parte_pa,"(?i)santa casa") ~ "Santa Casa",
    str_detect(parte_pa,"(?i)santa helena") ~ "Santa Helena",
    str_detect(parte_pa,"(?i)(cassi\\b|banco do brasil)") ~ "Cassi",
    str_detect(parte_pa,"(?i)hpvida") ~ "Hapvida",
    str_detect(parte_pa,"(?i)qualicorp") ~ "Qualicorp",
    str_detect(parte_pa,"(?i)caixa beneficente") ~ "Caixas beneficentes",
    str_detect(parte_pa,"(?i)cruz azul") ~ "Cruz Azul",
    str_detect(parte_pa,"(?i)São Crist[óo]vão") ~ "São Cristóvão",
    str_detect(parte_pa,"(?i)trasmontano") ~ "Trasmontano",
    str_detect(parte_pa,"(?i)ameplan") ~ "Ameplan",
    str_detect(parte_pa,"(?i)sobam") ~ "Sobam",
    str_detect(parte_pa,"(?i)iamspe") ~ "Iamspe",
    str_detect(parte_pa,"(?i)cesp") ~ "Cesp",
    str_detect(parte_pa,"(?i)ana costa") ~ "Ana Costa",
    str_detect(parte_pa,"(?i)mediservice") ~ "Mediservice",
    str_detect(parte_pa,"(?i)ita[úu]") ~ "Itaú",
    str_detect(parte_pa,"(?i)UNIÃO FEDERAL - PRU") ~ "UNIÃO FEDERAL - PRU",
    str_detect(parte_pa,"(?i)(alvorecer|blue med)") ~ "Blue Med",
    str_detect(parte_pa,"(?i)Postal Saúde") ~ "Postal Saúde",
    str_detect(parte_pa,"(?i)biovida") ~ "Biovida",
    str_detect(parte_pa,"(?i)Portuguesa de Beneficência") ~ "Beneficência portuguesa",
    str_detect(parte_pa,"(?i)Uni ?hosp") ~ "Unihosp",
    str_detect(parte_pa,"(?i)hb saúde") ~ "Hb Saúde",
    str_detect(parte_pa,"(?i)green line") ~ "Green Line",
    str_detect(parte_pa,"(?i)Geap") ~ "Geap",
    str_detect(parte_pa,"(?i)omint") ~ "Omint",
    str_detect(parte_pa,"(?i)são lucas") ~ "São Lucas",
    str_detect(parte_pa,"(?i)allianz") ~ "Allianz",
    str_detect(parte_pa,"(?i)qsa[úu]de") ~ "QSaúde",
    str_detect(parte_pa,"(?i)samaritano") ~ "Samaritano",
    str_detect(parte_pa,"(?i)economus") ~ "Economus",
    str_detect(parte_pa,"(?i)(santo andré|medical health)") ~ "Medical Health",
    str_detect(parte_pa,"(?i)(Francisco Xavier|usisa[úu]de)") ~ "Usisaúde",
    TRUE ~ "outros"
  ), .after = parte_pa) |>
  mutate(mes = lubridate::floor_date(data_recebimento,"month"))

### Beneficiários por operadora

beneficiarios <- readxl::read_excel("data/Usuarios_SP.xlsx") |>
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
