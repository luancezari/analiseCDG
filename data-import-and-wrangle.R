##### CARREGA PACOTES #####
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
library(readxl)
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
library(zoo)

##### IMPORTA BP TRANSPOSTO #####
BP_data <- read_xlsx('raw data/LAME4.xlsx', col_names = F)[1, 2:25] %>%
  as.numeric()
BP_data <- as.POSIXct(BP_data*86400, origin="1899-12-30", tz="GMT") %>% as_tibble()

BP_colnames <- read_xlsx('raw data/LAME4.xlsx',  skip = 1, col_names = F)[1:35,1] %>% unlist()

BP_dados <- read_xlsx('raw data/LAME4.xlsx',  skip = 1, col_names = F)[1:35,2:25] %>% 
  t() %>% 
  as_tibble()

BP <- bind_cols(BP_data, BP_dados)  %>% 
  `colnames<-` (c('Data', unlist(BP_colnames))) %>%
  mutate_if(is.character, as.numeric)


##### IMPORTA BP #####

BP2_colnames <-
  read_xlsx('raw data/LAME4.xlsx', col_names = F)[1,-1] %>% as.numeric()
BP2_colnames <- 
  as.POSIXct(BP2_colnames*86400, origin="1899-12-30", tz="GMT") %>% 
  as.yearqtr() %>% 
  as.character()

BP2_dados <- 
  read_xlsx('raw data/LAME4.xlsx', col_names = F, skip = 1)[1:36, 1:25]

BP2 <- BP2_dados %>%
  `colnames<-` (c('Consolidado', unlist(BP2_colnames)))



##### IMPORTA DRE #####
DRE_colnames <-
  read_xlsx('raw data/LAME4.xlsx', col_names = F, sheet = 'DRE modificada')[1,-1] %>% as.numeric()
DRE_colnames <- 
  as.POSIXct(DRE_colnames*86400, origin="1899-12-30", tz="GMT") %>% 
  as.yearqtr() %>% 
  as.character()

DRE_dados <- 
  read_xlsx('raw data/LAME4.xlsx', col_names = F, skip = 39)[1:20, 1:25]

DRE <- DRE_dados %>%
  `colnames<-` (c('Consolidado', unlist(DRE_colnames)))

##### IMPORTA DADOS DE CGL, INDICES DE LIQUIDEZ E CICLOS #####
CGPM <- read_excel('raw data/CGPM.xlsx')

LIQUIDEZ <- read_excel('raw data/LIQUIDEZ.xlsx') %>%
  mutate(Imediata = (BP$`Caixa e equival caixa` + BP$`Aplicacoes financeiras`)/BP$`Passivo Circulante`)
colnames(LIQUIDEZ) <- c('Data', 'Seca', 'Corrente', 'Imediata')

CICLOS <- tibble(Data = CGPM$Data,
                 `Operacional Total` = CGPM$PME + CGPM$PMC,
                 `Financeiro` = `Operacional Total` - CGPM$PMPF)

##### IMPORTA O BP MODIFICADO DE ACORDO COM O MODELO FLEURIET ####
BP_modificado_colnames <-
  read_xlsx('raw data/LAME4.xlsx', col_names = F, sheet = 'Balanço modificado')[1,-1] %>% as.numeric()
BP_modificado_colnames <- 
  as.POSIXct(BP_modificado_colnames*86400, origin="1899-12-30", tz="GMT") %>% 
  as.yearqtr() %>% 
  as.character()

BP_modificado_dados <- 
  read_xlsx('raw data/LAME4.xlsx', col_names = F, sheet = 'Balanço modificado')[-1,]

BP_modificado <- bind_cols(BP_modificado_dados) %>%
  `colnames<-` (c('Consolidado', unlist(BP_modificado_colnames)))

##### IMPORTA O DRE MODIFICADA DE ACORDO COM O MODELO FLEURIET #####
DRE_modificado_colnames <-
  read_xlsx('raw data/LAME4.xlsx', col_names = F, sheet = 'DRE modificada')[1,-1] %>% as.numeric()
DRE_modificado_colnames <- 
  as.POSIXct(DRE_modificado_colnames*86400, origin="1899-12-30", tz="GMT") %>% 
  as.yearqtr() %>% 
  as.character()

DRE_modificado_dados <- 
  read_xlsx('raw data/LAME4.xlsx', col_names = F, sheet = 'DRE modificada')[-1,]

DRE_modificado <- bind_cols(DRE_modificado_dados) %>%
  `colnames<-` (c('Consolidado', unlist(DRE_modificado_colnames)))

##### IMPORTA INDICADORES DO MODELO DE FLEURIET #####
Fleuriet_colnames <- read_xlsx('raw data/LAME4.xlsx', col_names = F, sheet = 'Fleuriet')[-1,1] %>% 
  unlist()

Fleuriet_dados <- read_xlsx('raw data/LAME4.xlsx', col_names = F, sheet = 'Fleuriet')[-1,-1] %>% 
  t() %>% as_tibble()

FLEURIET <- bind_cols(BP_data, Fleuriet_dados) %>%
  `colnames<-` (c('Data', Fleuriet_colnames)) %>%
  mutate_if(is.character, as.numeric)

##### SALVA DADOS EM FORMADO RDA #####
save(BP, file = "rda/BP.rda")
save(DRE, file = "rda/DRE.rda")
save(CGPM, file = 'rda/CGPM.rda')
save(CICLOS, file = 'rda/CICLOS.rda')
save(LIQUIDEZ, file = 'rda/LIQUIDEZ.rda')
save(FLEURIET, file = 'rda/FLEURIET.rda')
save(BP_modificado, file = 'rda/BP_modificado.rda')
save(DRE_modificado, file = 'rda/DRE_modificado.rda')
save(BP2, file = 'rda/BP2.rda')

##### REMOVE DADOS TEMPORARIOS #####
rm(BP_dados, BP_data, BP_colnames, DRE_dados, DRE_colnames, Fleuriet_colnames, Fleuriet_dados, 
   BP_modificado_colnames, BP_modificado_dados, DRE_modificado_colnames, DRE_modificado_dados,
   BP2_colnames, BP2_dados)