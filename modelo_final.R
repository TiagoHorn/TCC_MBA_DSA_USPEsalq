################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp","nortest","flextable")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



 ################################################################################
#           MODELAGEM DE UMA REGRESSÃO LINEAR multipla  BDW (BD3_cw)
###                   Analize preliminar doos dados
################################################################################
list.files()

#no banco de dados consta apenas o indicador de fechamento (close)
#Carregando a base de dados
library(readxl)
BTC23 <- read_excel("BD3_cw.xlsx" )

view(BTC23)
BTC23$Date = NULL
view(BTC23)
summary(BTC23)

####observando os dados no viewer
BTC23%>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)
#função correlation (pearson)
BTC23%>%
  correlation(method = "pearson") %>%
  plot()

###################################################
##grafico para avaliar a distribuição da variavel BTC
ggplotly(
  ggplot(BTC23, aes(x = "", y = BTC)) +
    geom_boxplot(fill = "deepskyblue",    # cor da caixa
                 alpha = 0.7,             # transparência
                 color = "black",         # cor da borda
                 outlier.colour = "red",  # cor dos outliers
                 outlier.shape = 15,      # formato dos marcadores dos outliers
                 outlier.size = 2.5) +    # tamanho dos marcadores dos outliers
    geom_jitter(width = 0.1, alpha = 0.5, size = 1.3, color = "darkorchid") +
    labs(y = "BTC (USD)") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position="none",
          plot.title = element_text(size=15)) +
    ggtitle("")+
    xlab("")
)

##grafico para avaliar a distribuição da variavel FTX
ggplotly(
  ggplot(BTC23, aes(x = "", y = FTX)) +
    geom_boxplot(fill = "deepskyblue",    # cor da caixa
                 alpha = 0.7,             # transparência
                 color = "black",         # cor da borda
                 outlier.colour = "red",  # cor dos outliers
                 outlier.shape = 15,      # formato dos marcadores dos outliers
                 outlier.size = 2.5) +    # tamanho dos marcadores dos outliers
    geom_jitter(width = 0.1, alpha = 0.5, size = 1.3, color = "darkorchid") +
    labs(y = "FTX (USD)") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position="none",
          plot.title = element_text(size=15)) +
    ggtitle("")+
    xlab("")
)

#A função 'pairs.panels' do pacote 'psych' também apresenta as distribuições
#das variáveis, scatters, valores das correlações e suas respectivas
#significâncias
pairs.panels(BTC23[1:4],
             smooth = TRUE,
             lm = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "pearson",
             pch = 1,
             cor = TRUE,
             hist.col = "aquamarine",
             breaks = 12,
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE, alpha = 0.05)

## correlação entre variáveis através do metodo de kendall
pairs.panels(BTC23[1:4],
             smooth = TRUE,
             lm = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "kendall",
             pch = 1,
             cor = TRUE,
             hist.col = "aquamarine",
             breaks = 12,
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE, alpha = 0.05)

##modelo de regressao # Com  a variavel BTC como variavel dependente no periodo entre 26/03/2022 e 26/03/2023
modelo_BTC23 <- lm(formula = BTC ~ .,
                 data = BTC23)
summary(modelo_BTC23)

step_modelo_BTC23 <- step(modelo_BTC23, k = 3.841459)

summary(step_modelo_BTC23)

sf.test(step_modelo_BTC23$residuals)

#Salvando os fitted values na base de dados
BTC23$BTCfit <- step_modelo_BTC23$fitted.values
BTC23
#plotando a aderencia doos dados
BTC23 %>%
  mutate(residuos = step_modelo_BTC23$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..),
                 color = "white",
                 fill = "#287D8EFF",
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm,
                args = list(mean = mean(step_modelo_BTC23$residuals),
                            sd = sd(step_modelo_BTC23$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()
BTC23 %>%
  mutate(residuos = step_modelo_BTC23$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_modelo_BTC23$residuals),
                            sd = sd(step_modelo_BTC23$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#Plotando o Intervalo de Confiança de 95%
ggplotly(
  ggplot(BTC23, aes(x = BTC, y = BTCfit)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x,
                level = 0.95) +
    labs(x = "Bitcoin",
         y = "Bitcoin_fit") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)


#######################################################################################
### Analise do SP23 (BD3_cw) com a variavel dependente SeP500 mesmo periodo ano 22 e 23#
#######################################################################################
library(readxl)
SP23 <- read_excel("BD3_cw.xlsx" )
#load(file = "BTC.xlsx")

SP23 <- BTC23
view(SP23)
SP23$BTCfit <- NULL
view(SP23)

##modelo de regressao 
modelo_SP23 <- lm(formula = `S&P500` ~ .,
                  data = SP23)
summary(modelo_SP23)

step_SP23 <- step(modelo_SP23, k = 3.841459)

summary(step_SP23)

sf.test(step_SP23$residuals)

#Salvando os fitted values na base de dados
SP23$SePfit <- step_SP23$fitted.values
SP23

#grafico da aderencia
SP23 %>%
  mutate(residuos = step_SP23$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..),
                 color = "white",
                 fill = "#287D8EFF",
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm,
                args = list(mean = mean(step_SP23$residuals),
                            sd = sd(step_SP23$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()
SP23 %>%
  mutate(residuos = step_SP23$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_SP23$residuals),
                            sd = sd(step_SP23$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#Plotando o Intervalo de Confiança de 95%
ggplotly(
  ggplot(SP23, aes(x = `S&P500`, y = SePfit)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x,
                level = 0.95) +
    labs(x = "S&P500",
         y = "S&P500_fit") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

####FIM!!!!
