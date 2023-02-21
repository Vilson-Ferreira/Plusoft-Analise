###############################################################################
#
# Preparação dos Dados
#
###############################################################################


# Carga dos dados
###############################################################################

# Dados da Plusoft
Dados.Plusoft = read.csv("dados/DadosMunicipios.csv", sep=";", dec=",", encoding="UTF-8");

# Dados do IBGE
IBGE.Municipios = geobr::read_municipality(code_muni="all", year="2010");
IBGE.Regioes = geobr::read_region(year=2010);
IBGE.Estados = geobr::read_state(code_state="all", year=2010);


# Novas Variáveis
###############################################################################

# Proporção da População > 25 anos
Dados.Plusoft$PERC_POP_25A = Dados.Plusoft$POP25A2000 / Dados.Plusoft$POPTOT2000 * 100;

# Proporção da População > 65 anos
Dados.Plusoft$PERC_POP_65A = Dados.Plusoft$POP65A2000 / Dados.Plusoft$POPTOT2000 * 100;

# Proporção da População Urbana
Dados.Plusoft$PERC_POP_URB = Dados.Plusoft$POPURB2000 / Dados.Plusoft$POPTOT2000 * 100;

# Proporção da População Rural
Dados.Plusoft$PERC_POP_RUR = Dados.Plusoft$POPRUR2000 / Dados.Plusoft$POPTOT2000 * 100;



# Unificação das variáveis Plusoft com dados do IBGE
###############################################################################

Dados.Plusoft$key = as.character(Dados.Plusoft$CODIGO);
IBGE.Municipios$key = substr(IBGE.Municipios$code_muni, start=1, stop=6);

Dados.Municipios = IBGE.Municipios %>%
  left_join(Dados.Plusoft, by=c("key"="key"));
                               

# Municípios sem Dados
###############################################################################

Dados.Ausentes = Dados.Municipios %>%
  filter(is.na(CODIGO));

write.csv2(Dados.Ausentes, "resultados/Municipios-sem-Dados.csv", sep=";", dec=".");


