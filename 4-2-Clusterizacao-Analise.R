###############################################################################
#
# Clusterização dos Municípios
# ANÁLISE
#
###############################################################################

# Resultados do Cluster e junção com dados do IBGE


# Usar para avaliar base gravada
Dados.Plusoft = read.csv("resultados/Cluster-Experim-1.csv", sep=",", dec=".");
Dados.Plusoft = Dados.Plusoft %>% select(-X);
Dados.Plusoft$key = as.character(Dados.Plusoft$key);
Dados.Plusoft$Grupo = factor(Dados.Plusoft$Grupo);

# 
Dados.Municipios = IBGE.Municipios %>%
  left_join(Dados.Plusoft, by=c("key"="key"));


# Avaliação em Box-plot
Variavel = Dados.Plusoft$DD2000;
VariavelStr = "DD2000";
Caption = "Densidade Demográfica";

ggplot(Dados.Plusoft, aes(x=Grupo, y={{Variavel}})) +
  geom_boxplot(fill="Navy", alpha=0.6, col="Navy") +
  labs(x="Grupo", y=Caption) +
  theme_minimal();


# Avaliação Bivariada
ggplot(Dados.Plusoft, aes(x=RPC2000, y=IDH2000, col=Grupo)) +
  geom_point(size=2) +
  theme_minimal();


# Avaliação das Médias
Modelo.Avaliacao = Dados.Plusoft %>%
  group_by(Grupo) %>%
  summarise(Count = n(),
            DD2000 = mean(DD2000),
            RPC2000 = mean(RPC2000),
            IDH2000 = mean(IDH2000),
            ALFA2000 = mean(ALFA2000),
            AE25A2000 = mean(AE25A2000),
            PERC_POP_URB = mean(PERC_POP_URB),
            PERC_POP_25A = mean(PERC_POP_25A));


# Municípios por Grupo
ggplot(Modelo.Avaliacao, aes(x=Grupo, y=Count, label=Count)) +
  geom_bar(stat="identity", fill="Navy", alpha=0.6, col="Navy") +
  geom_label(stat="identity") +
  labs(x="Grupo", y="Municípios") +
  theme_minimal();


# Grupos em Mapa
tm_shape(Dados.Municipios) +
  tm_fill("Grupo", style="cat", title="Grupo", palette="Accent") +
  tm_layout(frame=FALSE) +
  tmap_options(check.and.fix = TRUE) +
  tm_shape(IBGE.Estados) +
  tm_borders(col="White");

# Grupos em Mapa do Estado
tm_shape(Dados.Municipios %>% filter(abbrev_state == "SP")) +
  tm_fill("Grupo", style="cat", title="Grupo", palette="Accent") +
  tm_layout(frame=FALSE, legend.position=c("right", "bottom")) +
  tmap_options(check.and.fix = TRUE) +
  tm_shape(IBGE.Estados) +
  tm_borders(col="White");

tm_shape(Dados.Municipios %>% filter(abbrev_state == "MG")) +
  tm_fill("Grupo", style="cat", title="Grupo", palette="Accent") +
  tm_layout(frame=FALSE, legend.position=c("right", "bottom")) +
  tmap_options(check.and.fix = TRUE) +
  tm_shape(IBGE.Estados) +
  tm_borders(col="White");

tm_shape(Dados.Municipios %>% filter(abbrev_state == "RJ")) +
  tm_fill("Grupo", style="cat", title="Grupo", palette="Accent") +
  tm_layout(frame=FALSE, legend.position=c("right", "bottom")) +
  tmap_options(check.and.fix = TRUE) +
  tm_shape(IBGE.Estados) +
  tm_borders(col="White");

tm_shape(Dados.Municipios %>% filter(abbrev_state == "CE")) +
  tm_fill("Grupo", style="cat", title="Grupo", palette="Accent") +
  tm_layout(frame=FALSE, legend.position=c("right", "bottom")) +
  tmap_options(check.and.fix = TRUE) +
  tm_shape(IBGE.Estados) +
  tm_borders(col="White");


# Sugestões
Modelo.Sugestoes = Dados.Plusoft %>%
  filter(Grupo == 5) %>%
  select(CODIGO,
         MUNICIPIO);

gridExtra::grid.table(Modelo.Sugestoes);

