###############################################################################
#
# Classificação dos Municípios
# MODELAGEM
#
###############################################################################

# Resultados do Cluster e junção com dados do IBGE
Dados.Plusoft = read.csv("resultados/Cluster-Experim-1.csv", sep=";", dec=".");
# 
Dados.Municipios = IBGE.Municipios %>%
  left_join(Dados.Plusoft, by=c("key"="key"));


# Variáveis do Modelo de Clusterização
ModeloClass.Dados = Dados.Plusoft %>%
  select(DD2000,
         RPC2000,
         IDH2000,
         ALFA2000,
         AE25A2000,
         PERC_POP_URB,
         PERC_POP_25A,
         Grupo);

# Bases de treino e teste (80%-20%)
Indice = order(runif(4405));
ModeloClass.Train = ModeloClass.Dados[Indice,];
ModeloClass.Test = ModeloClass.Dados[-Indice,];

###############################################################################
# Modelagem
###############################################################################
ModeloClass.Fit = C5.0(ModeloClass.Train %>% select(-Grupo), ModeloClass.Train$Grupo, trials=20);

# Visualização da árvore
summary(ModeloClass.Fit);

plot(ModeloClass.Fit);

# Predições (Classificação)
ModeloClass.Pred = C50::predict.C5.0(ModeloClass.Fit, ModeloClass.Test %>% select(-Grupo), type="class");
ModeloClass.Test$GrupoPred = ModeloClass.Pred;

# Predições (Probabilidade da Classificação)
ModeloClass.PredProb = C50::predict.C5.0(ModeloClass.Fit, ModeloClass.Test %>% select(-Grupo), type="prob");
ModeloClass.Test$GrupoPredProb = ModeloClass.PredProb;

###############################################################################
# Avaliação do Modelo
###############################################################################

# Resultado (Matriz de Confusão)
caret::confusionMatrix(ModeloClass.Test$GrupoPred, ModeloClass.Test$Grupo);



