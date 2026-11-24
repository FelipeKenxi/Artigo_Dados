install.packages(c("ggplot2", "readr", "dplyr", "tidyr", "purrr"))

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

resultados <- read_delim(
  "resultados_random_forest.csv",
  delim = ";"
)

head(resultados)



metricas <- c(
  "accuracy",
  "precision",
  "recall",
  "f1",
  "roc_auc"
)


resultados_longos <- resultados %>%
  pivot_longer(
    cols = all_of(metricas),
    names_to = "metrica",
    values_to = "valor"
  )

resumo_por_modelo <- resultados_longos %>%
  group_by(modelo, metrica) %>%
  summarise(
    media = mean(valor),
    desvio_padrao = sd(valor),
    mediana = median(valor),
    .groups = "drop"
  )

print(resumo_por_modelo)


#Comparar as métricas
ggplot(
  resultados_longos,
  aes(x = modelo, y = valor, fill = modelo)
) +
  geom_boxplot() +
  facet_wrap(~metrica, scales = "free_y") +
  labs(
    title = "Comparação dos modelos Random Forest",
    x = "Modelo",
    y = "Valor da métrica"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



#Comparar o tempo
tempos_longos <- resultados %>%
  select(modelo, treino_s, predicao_s) %>%
  pivot_longer(
    cols = c(treino_s, predicao_s),
    names_to = "tipo_tempo",
    values_to = "tempo"
  )

resumo_tempos <- tempos_longos %>%
  group_by(modelo, tipo_tempo) %>%
  summarise(
    mediana = median(tempo),
    dp = sd(tempo),
    media = mean(tempo),
    .group = 'drop'
  )

print(resumo_tempos)

ggplot(
  tempos_longos,
  aes(x = modelo, y = tempo, fill = modelo)
) +
  geom_boxplot() +
  facet_wrap(~tipo_tempo, scales = "free_y") +
  labs(
    title = "Comparação dos tempos de execução",
    x = "Modelo",
    y = "Tempo (segundos)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



metricas <- c("accuracy", "precision", "recall", "f1", "roc_auc")

pares <- resultados %>%
  select(repeticao, fold, modelo, all_of(metricas),
         n_treino, n_teste) %>%
  pivot_wider(
    names_from = modelo,
    values_from = all_of(metricas),
    names_glue = "{.value}_{modelo}"
  )


razao_teste_treino <- mean(pares$n_teste / pares$n_treino)

#Teste t pareado corrigido
teste_corrigido <- function(metrica) {
  
  diferencas <- pares[[paste0(metrica, "_URL_HTML")]] -
    pares[[paste0(metrica, "_URL")]]
  
  n <- length(diferencas)
  
  media <- mean(diferencas)
  variancia <- var(diferencas)
  
  erro_padrao <- sqrt(
    (1 / n + razao_teste_treino) * variancia
  )
  
  estatistica_t <- media / erro_padrao
  
  p_valor <- 2 * pt(
    -abs(estatistica_t),
    df = n - 1
  )
  
  tibble(
    metrica = metrica,
    diferenca_media = media,
    estatistica_t = estatistica_t,
    graus_liberdade = n - 1,
    p_valor = p_valor
  )
}




resultados_teste <- map_dfr(metricas, teste_corrigido) %>%
  mutate(
    p_ajustado = p.adjust(p_valor, method = "holm"),
    significativo = p_ajustado < 0.05
  )

print(resultados_teste)

