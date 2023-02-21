###############################################################################
#
# Clusterização dos Municípios
# MODELAGEM
#
###############################################################################

# Correlação
Dados.Analise.Correlacao = cor(Dados.Plusoft[,3:29]);
corrplot::corrplot.mixed(Dados.Analise.Correlacao, upper="circle", lower="circle", tl.col="black", tl.pos="lt", tl.cex=0.8, number.cex=0.6);

# Variáveis Selecionadas
Modelo.Dados = Dados.Plusoft %>%
  select(DD2000,
         RPC2000,
         IDH2000,
         ALFA2000,
         AE25A2000,
         PERC_POP_URB,
         PERC_POP_25A);

# Padronização z-scale
Modelo.Dados = scale(Modelo.Dados);

# Teste para Número de Clusters
Modelo.nClusters = NbClust::NbClust(Modelo.Dados,
                                    distance = "euclidean",
                                    min.nc = 2,
                                    max.nc = 10,
                                    method = "complete",
                                    index ="all");


# Métricas de Avaliação
factoextra::fviz_nbclust(Modelo.Dados, hcut, method="silhouette");
factoextra::fviz_nbclust(Modelo.Dados, hcut, method="wss");
factoextra::fviz_nbclust(Modelo.Dados, hcut, method="gap_stat");

# Modelo Hierárquico com X Clusters
Modelo.Hierarq = factoextra::hcut(Modelo.Dados, k=5, stand=TRUE);

plot(Modelo.Hierarq);
rect.hclust(Modelo.Hierarq, k=5, border="red");

plot(Modelo.Hierarq);
rect.hclust(Modelo.Hierarq, k=10, border="red");


# Avaliação dos Grupos (Escolha k=5)
###############################################################################
Modelo.Grupos = cutree(Modelo.Hierarq, k=5);

Dados.Plusoft$Grupo = factor(Modelo.Grupos);

write.csv(Dados.Plusoft, "resultados/Cluster-Experim-1.csv", sep=";", dec=".");
