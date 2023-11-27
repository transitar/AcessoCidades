# AcessoCidades
Este repositório contem os códigos utilizados no processamento e análise de dados do Projeto AcessoCidades, da Frente Nacional de Prefeitos. Para mais informações sobre a pesquisa com seus resultados e metodologia, visite o <a href="https://multimidia.fnp.org.br/biblioteca/documentos/item/978-saiba-mais-projeto-acessocidades)https://multimidia.fnp.org.br/biblioteca/documentos/item/978-saiba-mais-projeto-acessocidades">site do projeto</a>.
# Organização do Repositório

Os scripts em `R` utilizados neste repositório estão organizados em grupos e numeros segundo ordem de processamento.

  - `01`: documentação do tratamento inicial feito às bases
    de dados brutas de informações socieconômicas, de uso do solo,
    transporte e agregação de dados no nível de hexágonos da grade H3 da Uber em resolução 9;
  - `03`: criação das unidades de espaciais de análise (hexágonos) e
    agregação espacial das variáveis demográficas e de uso do solo;
  - `04`: configuração das pastas e arquivos do r5r e cálculo de de matriz de tempo de
    viagem;
  - `05`: cálculo dos indicadores de acessiblidade;
  - `10`: organização da base de dados, mapas e gráficos para publicação.

Além dessa documentação, outros arquivos necessários para o andamento do
projeto estão divididos nas pastas:

  - `./R/fun`: funções e parâmetros úteis para o desenvolvimento do projeto;
  - `./R/microssimulation`: funções e parâmetros úteis para a microssimulação, baseados em <a href="https://github.com/lucasferreira-tp/microsimulation"> Ferreira (2023)</a>;

Pastas referentes aos dados que não estão presentes nesse repositório
porque contém dados grande demais para a plataforma do GitHub:

  - `data-raw`: dados brutos;
  - `data`: dados tratados e organizados;
  - `microssimulacao`: dados populacionais brutos do IBGE e dados microssimulados;
  - `r5`: arquivos utilizados na construção do *router* do
    r5r.
