###############################################################################
#
# Análise Exploratória
#
###############################################################################

Variavel = Dados.Municipios$AE25A2000;
VariavelStr = "AE25A2000";
Caption = "Anos de Estudo, Pop. >25 Anos";


# Mapa de Calor - Variável
tm_shape(Dados.Municipios) +
  tm_fill(VariavelStr, style="cont", title=Caption) +
  tm_layout(frame=FALSE) +
  tmap_options(check.and.fix = TRUE) +
  tm_shape(IBGE.Estados) +
  tm_borders(col="White");


# Mapa de Calor - Percentis da variável
tm_shape(Dados.Municipios) +
  tm_fill(VariavelStr, style="quantile", title=Caption, n=5) +
  tm_layout(frame=FALSE) +
  tmap_options(check.and.fix = TRUE) +
  tm_shape(IBGE.Estados) +
    tm_borders(col="White");


# Box-plot
ggplot(Dados.Municipios, aes(y={{Variavel}})) +
  geom_boxplot(fill="Navy", alpha=0.6) + 
  labs(x="", y=Caption) +
  theme_minimal();


# Histograma
ggplot(Dados.Municipios, aes(x={{Variavel}})) +
  geom_histogram(fill="Navy", color="Navy", alpha=0.6) + 
  labs(x=Caption, y="") +
  theme_minimal();

