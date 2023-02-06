# Criação de um Índice de Reparos dos Distritos da Cidade de São Paulo utilizando PCA. (WIP)
## ***1 - Objetivo***  
&emsp; Encontrar os indicadores sociais que possuem a maior correlação com a quantidade de reparos por distrito da cidade de São Paulo e então criar um índice de reparos utilizando a Análise de Componentes Principais.  
&emsp;
## ***2 - Principais Resultados (WIP)***
## ***3 - Documentação Técnica***
### ***3.1 - Linguagem*** 
  * R  
### ***3.2 - Integrated Development Environment (IDE)*** 
  * RStudio
  
### ***3.3 - Pacotes***  
&emsp; Note que o primeiro tópico de cada os scripts é chamado de "Packages". Ele é responsável por verificar e instalar automaticamente todos os pacotes necessários para o pleno funcionamento de cada script. Abaixo, segue uma lista de todos os pacotes utilizados: 
  * broom
  * DBI
  * lodown
  * PerformanceAnalytics
  * psych
  * raster
  * reshape
  * rgdal
  * spdep
  * tidyverse
  
### ***3.4 - Pastas & Arquivos*** 
&emsp; As pasta estão estruturadas de formas intuitiva, segmentadas pelos tipos de arquivo que estão nelas.   
&emsp; São 3 tipos de arquivos: 

* ***Data:*** armazenas todos os dados que são utilizados pelos scripts. Dessa forma, há dois tipos. 
  + ***csv:*** arquivos que comtêm dados brutos ou limpos relacionados métricas sociais ou de reparos por região da cidade de São Paulo. Dessa forma, seguem os metadados de cada arquivo: 
    + ***census2010-sao-paulo-city-raw:***
      + ***id_sector:*** id do setor censitário, utilizado pelo IBGE;
      + ***qty_property_all:*** quantidade de propriedades privadas;
      + ***qty_property_house:*** quantidade de propriedades privadas do tipo casa;
      + ***qty_property_apartment:*** quantidade de propriedades privadas do tipo apartamento;
      + ***qty_property_rented:*** quantidade de propriedades privadas alugadas;
      + ***qty_property_water:*** quantidade de propriedades privadas com fornecimento de água pela rede pública;
      + ***qty_property_swage:*** quantidade de propriedades privadas com acesso ao sistema de esgoto;
      + ***qty_property_trash:***  quantidade de propriedades privadas com acesso ao sistema de coleta de lixo;
      + ***qty_property_electricity:*** quantidade de propriedades privadas com acesso ao sistema de fornecimento de eletricidade;
      + ***qty_property_people_1_2:*** quantidade de propriedades privadas com 1 ou 2 moradores;
      + ***qty_property_people_3_4:*** quantidade de propriedades privadas com 3 ou 4 moradores;
      + ***qty_property_people_5_p:*** quantidade de propriedades privadas com mais de 5 moradores;
      + ***qty_property_income_0_1:*** quantidade de propriedades privadas com renda per capta entre 0 a 1 salário mínimo;
      + ***qty_property_income_1_5:*** quantidade de propriedades privadas com renda per capta entre 1 a 5 salários mínimo;
      + ***qty_property_income_5_10:*** quantidade de propriedades privadas com renda per capta entre 5 a 10 salários mínimo;
      + ***qty_property_income_10_p:*** quantidade de propriedades privadas com renda per capta maior que 10 salários mínimo;
      + ***qty_residents_all:*** quantidade de residentes;
      + ***qty_residents_14_p:*** quantidade de residentes com 14 anos ou mais;
      + ***qty_residents_14_p_literate:*** quantidade de residentes com 14 anos ou mais e com ensino básico.
    + ***census2010-metrics-sao-paulo-city:***
      +  ***CD_GEOCODD:*** id do distrito, utilizado pelo IBGE;
      +  ***perc_ppt_house:*** percentual de propriedades privadas do tipo casa; 
      +  ***perc_ppt_apartment:*** percentual de propriedades privadas do tipo apartamento; 
      +  ***perc_ppt_rented:*** percentual de propriedades privadas alugadas; 
      +  ***perc_ppt_water:*** percentual de propriedades privadas com fornecimento de água pela rede pública;
      +  ***perc_ppt_swage:*** percentual de propriedades privadas com acesso ao sistema de esgoto;
      +  ***perc_ppt_trash:*** percentual de propriedades privadas com acesso ao sistema de coleta de lixo;
      +  ***perc_ppt_electricity:*** percentual de propriedades privadas com acesso ao sistema de fornecimento de eletricidade;
      +  ***perc_ppt_people_1_2:*** percentual de propriedades privadas com 1 ou 2 moradores;
      +  ***perc_ppt_people_3_4:*** percentual de propriedades privadas com 3 ou 4 moradores;
      +  ***perc_ppt_people_5_p:*** percentual de propriedades privadas com mais de 5 moradores;
      +  ***perc_ppt_income_0_1:*** percentual de propriedades privadas com renda per capta entre 0 a 1 salários mínimo;
      +  ***perc_ppt_income_1_5:*** percentual de propriedades privadas com renda per capta entre 1 a 5 salários mínimo;
      +  ***perc_ppt_income_5_10:*** percentual de propriedades privadas com renda per capta entre 5 a 10 salários mínimo;
      +  ***perc_ppt_income_10_p:*** percentual de propriedades privadas com renda per capta maior que 10 salários mínimo;
      +  ***perc_res_14_p:*** percentual de residentes com 14 anos ou mais;
      +  ***perc_res_14_p_literate:*** percentual de residentes com 14 anos ou mais e com ensino básico.
     + ***repair-metrics-sao-paulo-city*** 
       + ***CD_GEOCODD:*** id do distrito, utilizado pelo IBGE;
       + ***avg_age_months:*** média de tempo de aluguel;
       + ***offrepairs_rate:*** percentual de rescisões com reparos necessários;
       + ***ongrepairs_rate:*** percentual de imóveis com reparos
       + ***ongarea_rate:*** percentual de reparos relativos a áreas externas ou comuns;
       + ***ongeletric_rate:*** percentual de reparos relativos a itens elétricos;
       + ***onghidraulic_rate:*** percentual de reparos relativos a itens hidráulicos;
       + ***onggas_rate:*** percentual de reparos relativos a gás.  
  
   &emsp;&emsp;&ensp; Alguns arquivos foram omitidos em conssonância a Lei Geral de Proteção dos Dados (LGPD).
  
  + ***shapefiles:*** todos os dados geográficos gerados pelo Instituto Brasileiro de Geografia (IBGE) para análise de dados referente ao Censo 2010, os quais foram segmentados em: 
      + ***sp_census_sector:*** dados dos setores censiátios do estado de São Paulo
      + ***sp_distrits:*** dados dos distritos do estado de São Paulo.  

&emsp;&emsp;&ensp; Os shapefiles do IBGE relativos ao Censo 2010 podem ser acessados nesse link [aqui](https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html?=&t=acesso-ao-produto).

* ***scripts:*** algoritmos para transformação de dados bruto em dados limpos. 
  + ***census2010-metrics-sao-paulo-city:*** utilizado para transformar o arquivo census2010-sao-paulo-city-raw no arquivo census2010-metrics-sao-paulo-city. 
    + Nese script, há uma parte do código que está comentada. Descomente e utilize essa parte só se perder o arquivo nomeado como census2010-sao-paulo-city-raw. 
    + Esse SQL consome dados do projeto Base de Dados, disponível nesse link [aqui](basedosdados.org/dataset/br-ibge-censo-demografico?bdm_table=setor_censitario_basico_2010).  
  + ***repair-metrics-sao-paulo-city:*** utilizado para transformar os dados arquivos omitidos para contemplar a LGPD.
  
* ***eda:*** código utilizado para desenvolver as análise necessárias.  
  + ***pca:*** código que contém o algoritmo para consumir os dados transformados e gerar a Análise de Componentes Principais.
  