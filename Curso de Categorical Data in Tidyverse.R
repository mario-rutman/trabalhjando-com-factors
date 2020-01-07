library(tidyverse)
library(readxl)
library(skimr)
library(janitor) # para limpar os nomes das variáveis: clean_names()
library(here)
library(forcats)

# Para ficar tudo em portugês.
Sys.setlocale("LC_ALL", "pt_BR.UTF-8")

# Para não usar notação científica nosnúmeros.
options(scipen=999)

# Curso "Categorical Data in tidyverse".

# Aqui vou usando e descrevendo as funções mais importantes do curso.

# Primeiro importar um df. 
ger_trab_ccat <- read_excel("Planilha dos trabalhos ações na CCAT.xlsx")

# olhar sua estrutura.
str(ger_trab_ccat)
glimpse(ger_trab_ccat)
skim(ger_trab_ccat)

# transformar character em factor de todo ger_trab_ccat.
# a mecânica é a seguinte: se é character a resposta é TRUE, então tranforma em factor.  
ger_trab_ccat_as_factor <- ger_trab_ccat %>% mutate_if(is.character, as.factor)

glimpse(ger_trab_ccat_as_factor)

nlevels(ger_trab_ccat_as_factor$Ação)# vendo a quantidade de levels das colunas.
nlevels(ger_trab_ccat_as_factor$Servidor)

levels(ger_trab_ccat_as_factor$Ação) # dá os nomes dos levels, níveis, de uma coluna de um df.
levels(ger_trab_ccat_as_factor$Servidor)

ger_trab_ccat_as_factor %>% pull(Servidor) %>% # também posso pegar o df 'puxar' a coluna Servidor e separar os levels.
  levels()
ger_trab_ccat_as_factor %>% pull(Ação) %>% # também posso pegar o df 'puxar' a coluna Ação e separar os levels.
  levels()

# Também pode pegar um df e se for factor, sumarizar pelo número de levels.
ger_trab_ccat_as_factor %>% summarise_if(is.factor, nlevels)

# Agora faz uma tabela com as variáveis (variable) e o número de levels (num_levels), níveis. 
quant_niveis <- ger_trab_ccat_as_factor %>% 
  summarise_all(nlevels) %>%
  gather(variavel, quantidade_niveis)

# Duas formas de escolher os maiores.

mtcars %>%
  top_n(2, mpg) # aparecendo toda linha onde estão os 2 maiores mpg.


mtcars %>%
  top_n(5, mpg) %>%  # aparecendo só os 5 maiores mpg.
  pull(mpg)  

summary(ger_trab_ccat_as_factor$Servidor) # Conta a quantidade de cada factor.
summary(ger_trab_ccat_as_factor$Ação)

# Tem ainda a função que conta os fators e dá a proporção.

fct_count(ger_trab_ccat_as_factor$Servidor, sort = TRUE)

f <- factor(sample(letters)[rpois(1000, 10)])
table(f)
fct_count(f)
fct_count(f, sort = TRUE)
fct_count(f, sort = TRUE, prop = TRUE)

# Esse agora é sensacional!!! É do pacote forcats.
# Ordena os gráficos de barra. Aqui a função calculou quantas vezes cada Servidor apareceu.
# Quanto cada um aparece na lista.
# fct_infreq conta as frequências e ordena da menor para a maior.
# fct_rev reverte a ordenação, vai da maior para a menor.

ggplot(ger_trab_ccat_as_factor) + 
  geom_bar(aes(x = fct_rev(fct_infreq(Servidor)))) + 
  coord_flip()

ggplot(ger_trab_ccat_as_factor) + 
  geom_bar(aes(x = fct_rev(fct_infreq(Ação)))) + # com reversão de fatores (fct_rev)
  coord_flip()

ggplot(ger_trab_ccat_as_factor) + 
  geom_bar(aes(x = fct_infreq(Ação))) + # sem reversão de fatores
  coord_flip()

# Também sensacional. Para gráficos de pontos.
# Aqui reordenamos o factor Ação em função dos totais das quantidades de cada Ação. 
# fct_reorder takes three arguments: f = factor you want to reorder, 
# x = the variable in which the order will be based upon,
# o terceiro não é para agora.

ger_trab_ccat_as_factor %>% 
  group_by(Ação) %>% #agrupo por ação para depois fazer a soma.
  summarise(total = sum(Quantidade)) %>% # fazendo a soma por ação.
  ggplot(aes(x = fct_reorder(Ação, total), y = total)) + 
  geom_point() + 
  coord_flip()

# Hadley Wickham diz que é melhor usar o fct_reorder fora do aes. Será que dá o mesmo resultado? R. sim
ger_trab_ccat_as_factor %>% 
  group_by(Ação) %>% #agrupo por ação para depois fazer a soma.
  summarise(total = sum(Quantidade)) %>% # fazendo a soma por ação.
  mutate(Ação = fct_reorder(Ação, total)) %>% # reordenando ação pelo total.
  ggplot(aes(x = Ação, y = total)) + 
  geom_point() + 
  coord_flip()


# Podemos ainda reordenar os fatores de um modo específico ou arbitrário. 
# Vejamos primeiro quais são os fatores. 
levels(ger_trab_ccat_as_factor$Ação)

# Estão em ordem alfabética. Mas esta não interessa agora. Quero outra.
ger_trab_ccat_as_factor_reordenado <- ger_trab_ccat_as_factor %>% 
  mutate(Ação = fct_relevel(Ação, "conciliação GCT/ARR/SIAFE", "cheque não honrado",
                            "recup. doc. não vinvulado",
                            "responder sac-DARJ", "proc. restituição", 
                            "proc. apostilamento", "Termo de Referência",
                            "atualiz. relatório arrecad."))

# Agora os levels estão diferentes. 
levels(ger_trab_ccat_as_factor_reordenado$Ação)

# Podemos recodificar os levels, isto é, dar outros nomes melhores.
# Primeiro o novo nome, depois o antigo, o que vai ser substituído.
  ger_trab_ccat_as_factor %>% 
  mutate(nome_guerra = fct_recode(Servidor,
                                  'Yoko' = "Elaine Yoko",
                                  'Weber' =  "Marcelo Weber",
                                  'Joci' = "Maria Jociliane",
                                  'Rutman' = "Mário Rutman",
                                  'Samuca' = "Samuel Augusto")) %>%
  count(nome_guerra) # contando
 

ger_trab_ccat_as_factor %>% count(Servidor) # contando o Servidor. claro que o resultado foi igual ao do nome_guerra


# Diminuindo o número de levels, níveis, juntando levels, collapsing levels.
# Primeiro collapse alguns em diversos grupos, diminuindo o número de classes.
# Os que ficaram de fora são os 'Other', fct_other. 

ger_trab_ccat_as_factor %>%
  # Criando nova variável, grouped_titles, após juntar levels em Ação.
  mutate(grouped_titles = fct_collapse (Ação, 
                                       "processo" = c("proc. restituição", "proc. apostilamento"), 
                                       "conciliação" = c("recup. doc. não vinvulado", "conciliação GCT/ARR/SIAFE"))) %>%
  
# Chamando os levels que não foram agrupados de outros.
  mutate(grouped_titles = fct_other(grouped_titles, 
                                    keep = c("processo", "conciliação"))) %>% 
  # Get a count of the grouped titles
  count(grouped_titles)



# Este também é importante. Usamos quando temos muitos grupos de pequenos valores.
# Primeiro tiramos os NA depois definimos a classe 'Other' para reunir todos que não chegam à 5% do total.
 

multiple_choice_responses %>%
  # remove NAs of MLMethodNextYearSelect
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # create ml_method, which lumps all those with less than 5% of people into "Other"
  mutate(ml_method = fct_lump(MLMethodNextYearSelect, prop = .05)) %>%
  # count the frequency of your new variable in descending order
  count(ml_method, sort = TRUE) # Faz a contagem e apresenta em ordem decrescente.


# Aqui apresentamos os 5 maiores grupos, e o que sobrou  é other_level = "other method"

multiple_choice_responses %>%
  # remove NAs 
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # create ml_method, retaining the 5 most common methods and renaming others "other method" 
  mutate(ml_method = fct_lump(MLMethodNextYearSelect, n = 5, other_level = "other method")) %>%
  # print the frequency of your new variable in descending order
  count(ml_method, sort = TRUE)




# Fazendo o gráfico de pontos. 
# Mas antes tenho que criar df das frequências de cada Ação.

freq_acao <- ger_trab_ccat_as_factor %>%
  group_by(Ação) %>% 
  summarise(quanti_total = sum(Quantidade)) %>% 
  mutate(quanti_perc = round(quanti_total/sum(quanti_total),3))


# Agora o gráfico.
ggplot(freq_acao, aes(x = Ação, y = quanti_perc)) + 
  geom_point() + 
  # rotate x-axis text by 90 degrees
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  # rename y and x axis labels
  labs(x = "Ação", y = "% da quantidade total") + 
  # change y axis scale to percentage
  scale_y_continuous(labels = scales::percent)


# Mas fica mais fácil de visualizar se tivermos um ordenamento.
# fct_reorder(Ação, quanti_perc)). Lê-se: reordenar o fator Ação por quanti_perc.
# fct_rev lê-se: reverter a ordem (o default é crescente), usando fct_rev fica do maior para o menor.
ggplot(freq_acao, aes(x = fct_rev(fct_reorder(Ação, quanti_perc)), y = quanti_perc)) + 
  geom_point() + 
  # rotate x-axis text by 90 degrees
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  # rename y and x axis labels
  labs(x = "Ação", y = "% da quantidade total") + 
  # change y axis scale to percentage
  scale_y_continuous(labels = scales::percent)


# Sem o fct_rev o ordemanto fica crescente.
ggplot(freq_acao, aes(x = fct_reorder(Ação, quanti_perc), y = quanti_perc)) + 
  geom_point() + 
  # rotate x-axis text by 90 degrees
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  # rename y and x axis labels
  labs(x = "Ação", y = "% da quantidade total") + 
  # change y axis scale to percentage
  scale_y_continuous(labels = scales::percent)

# case_when()
# Cria nova variável (generation) e os nomes de cada faixa etária. 
multiple_choice_responses %>%
  # Eliminate any ages below 10 and above 90
  filter(between(Age, 10, 90)) %>%
  # Create the generation variable based on age
  mutate(generation = case_when(
    between(Age, 10, 22) ~ "Gen Z", 
    between(Age, 23, 37) ~ "Gen Y", 
    between(Age, 38, 52) ~ "Gen X", 
    between(Age, 53, 71) ~ "Baby Boomer", 
    between(Age, 72, 90) ~ "Silent"
  )) %>%
  # Get a count of how many answers in each generation
  count(generation)


# Aqui faz o case_when de mais de uma coluna.
multiple_choice_responses %>%
  # Filter out people who selected Data Scientist as their Job Title
  filter(CurrentJobTitleSelect != "Data Scientist") %>%
  # Create a new variable, job_identity
  mutate(job_identity = case_when(
    CurrentJobTitleSelect == "Data Analyst" & DataScienceIdentitySelect == "Yes" ~ "DS analysts", 
    CurrentJobTitleSelect == "Data Analyst" & DataScienceIdentitySelect %in% c("No", "Sort of (Explain more)") ~ "NDS analyst", 
    CurrentJobTitleSelect != "Data Analyst" & DataScienceIdentitySelect == "Yes" ~ "DS non-analysts", 
    TRUE ~ "NDS non analysts")) %>%
  # Get the average job satisfaction by job_identity, removing NAs
  group_by(job_identity) %>%
  summarize(avg_js = mean(JobSatisfaction, na.rm = TRUE))


# Fazendo o gráfico perfeito.
initial_plot <- ggplot(rude_behaviors, 
 aes(x = fct_reorder(response_var, perc_rude), y = perc_rude)) + 
  # o response_var foi reordenado crescentemente pelos valores do perc_rude
  geom_col() 

titled_plot <- initial_plot + 
  # Add the title, subtitle, and caption
  labs(title = "Hell Is Other People In A Pressurized Metal Tube",
       subtitle = "Percentage of 874 air-passenger respondents who said action is very or somewhat rude",
       caption = "Source: SurveyMonkey Audience", 
       # Remove the x- and y-axis labels. Forma interessante de tirar os dizeres de x e y.
       x = "",
       y = "") 

flipped_plot <- titled_plot + 
  # Flip the axes virou os exixos
  coord_flip() + 
  # Remove the x-axis ticks and labels
  theme(axis.text.x = element_blank(), #tirou os números do eixo x
        axis.ticks.x = element_blank()) # tirou os traços do eixo x


flipped_plot + 
  # Add labels above the bar with the perc value
  geom_text(aes(label = percent(perc_rude), 
                y = perc_rude + .03), 
            position = position_dodge(0.9),
            vjust = 1)
# Contagens. Bem importante.
# Para contarmos Servidores ou ação usamos o group_by e o summarise, ou apenas o count.
ger_trab_ccat_as_factor %>% 
  group_by(Servidor) %>% # agrupando por Servidor
 summarise(num_serv = n()) # 
 
ger_trab_ccat_as_factor %>% count(Servidor) # faz o mesmo que o script anterior

ger_trab_ccat_as_factor %>% count(Servidor, Ação) # pode-se contar também mais de um fator. Note que há um agrupamento.


ger_trab_ccat_as_factor %>% count(Servidor, Ação) %>% 
  count(Servidor) # É uma contagem da contagem, não entendi para qçue funciona.

ger_trab_ccat_as_factor %>% distinct(Ação) # Mostra os fatores

ger_trab_ccat_as_factor %>% count(Servidor == "Maria Jociliane") # Conta quantos são Maria Jociliane (TRUE) e quantos não são.


# O readr domestica as colunas, isto é, na importação força a coluna da ser o que é melhor. 
# Try to cast technical as a number
desserts <- read_csv("desserts.csv",
                     col_types = cols(
                       technical = col_number()) # aqui parse a coluna techinical para número
                     
# Find format to parse uk_airdate 
 parse_date("17 August 2010", format = "%d %B %Y") # Parseando uma só data
                     
# Importando e ao mesmo tempo parseando as colunas.
 desserts <- read_csv("desserts.csv", 
                      na = c("", "NA", "N/A"),
                      col_types = cols(
                        technical = col_number(), # parseando para número
                        uk_airdate = col_date(format = "%d %B %Y"), # parseando para data
                        result = col_factor(levels = NULL) # parseando para factor sem ordenar
                      ))
 
 # Recode, isto é, mudar o nome de valores em determinada coluna.
 desserts <- desserts %>% 
   # criandoa coluna tech_win onde o que é 1 fica 1, o resto (.default) fica 0.
   mutate(tech_win = recode(technical, `1` = 1,
                            .default = 0))
 
 # Recode_factor.
 # Transforma o que está sendo recodificado em factor. Se for fazer gráfico facilita no aes(fill =  )
 ratings <- ratings %>% 
   mutate(bbc = recode_factor(channel, 
                              "Channel 4" = 0,
                              .default = 1))
 
 # Selecionar todas colunas que não terminam com "day". 
 ratings %>% 
   select(- ends_with("day"))
 
 
 # Coloca a coluna channel na frente de todas, e põe todas restantes depois e tira as colunas que terminam em 'day'.
 # Tem que ser nesta ordem.
 ratings %>% 
   select(channel,  everything(), - ends_with("day"))
 
 # Tem um select muito legal. Você seleciona a coluna e muda seu nome ao mesmo tempo.
 freq_acao %>% select(quantidade_total = quanti_total, quantidade_percentual = quanti_perc)
 
 
 # Esse script faz um df de contagem e proporção
 bakers %>% 
   count(gen, sort = TRUE) %>% # conta a coluna gen por categoria e dispõe em ordem decrescente
   mutate(prop = n / sum(n)) # faz a proporção de cada categoria. n é o nome da coluna de contagem
 
 
 # Este script é imoortante
 # Add a line to extract labeled month
 baker_dates_cast <- baker_dates %>% 
   mutate(last_date_appeared_us = dmy(last_date_appeared_us), # 1º transforma a comuna em data.
          # 2º extraí o mes, abreviado (abbr = TRUE), da data
          last_month_us = month(last_date_appeared_us, label = TRUE, abbr = TRUE))  
 
 # Make bar chart by last month
 ggplot(baker_dates_cast, aes(x = last_month_us)) # conclusão: no gráfico as barras vêm com o nome do mes embaixo e na ordem.
 + geom_bar()
 
 getwd()
here() 

f <- factor(sample(letters)[rpois(1000, 10)])
table(f)#> f
#>   b   d   f   g   h   j   l   m   n   o   p   q   r   s   u   v   w   x   y 
#> 131  51  93  42   6   2  11 118   1 107   5  24  51  14  65 122   1  53 103 fct_count(f)#> # A tibble: 19 x 2
#>    f         n
#>    <fct> <int>
#>  1 b       131
#>  2 d        51
#>  3 f        93
#>  4 g        42
#>  5 h         6
#>  6 j         2
#>  7 l        11
#>  8 m       118
#>  9 n         1
#> 10 o       107
#> 11 p         5
#> 12 q        24
#> 13 r        51
#> 14 s        14
#> 15 u        65
#> 16 v       122
#> 17 w         1
#> 18 x        53
#> 19 y       103fct_count(f, sort = TRUE)#> # A tibble: 19 x 2
#>    f         n
#>    <fct> <int>
#>  1 b       131
#>  2 v       122
#>  3 m       118
#>  4 o       107
#>  5 y       103
#>  6 f        93
#>  7 u        65
#>  8 x        53
#>  9 d        51
#> 10 r        51
#> 11 g        42
#> 12 q        24
#> 13 s        14
#> 14 l        11
#> 15 h         6
#> 16 p         5
#> 17 j         2
#> 18 n         1
#> 19 w         1fct_count(f, sort = TRUE, prop = TRUE)#> # A tibble: 19 x 3
#>    f         n     p
#>    <fct> <int> <dbl>
#>  1 b       131 0.131
#>  2 v       122 0.122
#>  3 m       118 0.118
#>  4 o       107 0.107
#>  5 y       103 0.103
#>  6 f        93 0.093
#>  7 u        65 0.065
#>  8 x        53 0.053
#>  9 d        51 0.051
#> 10 r        51 0.051
#> 11 g        42 0.042
#> 12 q        24 0.024
#> 13 s        14 0.014
#> 14 l        11 0.011
#> 15 h         6 0.006
#> 16 p         5 0.005
#> 17 j         2 0.002
#> 18 n         1 0.001
#> 19 w         1 0.001
