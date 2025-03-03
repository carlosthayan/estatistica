view(tab2_1)

View(tab2_1)

names(tab2_1)

glimpse(tab2_1)

summary(tab2_1$salario)

ni <- table(tab2_1$grau_instrucao) 
fi <- prop.table(ni)
p_fi <- 100*prop.table(ni)

ni <- c(ni, sum(ni))
fi <- c(fi, sum(fi))
p_fi <- c(p_fi, sum(p_fi))
names(ni)[4] <- "Total"

frequencia_relativa <- ni
print(frequencia_relativa)


tab2_2 <- cbind(ni, fi=round(fi, digits = 2), p_fi=round(p_fi, digits = 2))
tab2_2


?cbind

ni<-table(cut(tab2_1$salario, breaks = seq(4,24,by=4),right=FALSE)) # Frequencias por categorias
tab2_4 <- rbind(ni, p_fi = 100*prop.table(ni)) # Frequencias relativas em %
#quebras de linha apenas ilustrativas para facilitar a leitura
tab2_4 <- as.data.frame(
  t(cbind(
    tab2_4,
    c(sum(tab2_4[1,]),sum(tab2_4[2,])
    ))),row.names =c(colnames(tab2_4),"Total")) #Construcao da tabela
tab2_4<-transform(tab2_4,p_fi=round(p_fi,digits=2))
tab2_4

barplot(
  table(tab2_1$grau_instrucao),
  ylab="Frequência",
  cex.names=0.7,
  names.arg = c("Fundamental","Médio", "Superior"),
  col="darkgrey",
  border=NA,
  #main="Figura 2.2: Gráfico em barras para a variável Y: grau de instrução.",
  axes=TRUE,
  ylim=c(0,20)
)

ggplot(tab2_1, aes(x = grau_instrucao))+
  geom_bar()

labs<-paste(1:3,"(",tab2_2[1:3,1],";",round(tab2_2[1:3,3],1),"%)",sep="")
pie(table(tab2_1$grau_instrucao),labels=labs)
#title("Figura 2.3: Gráfico em setores para a variável Y: grau de instrução")
legend(-1.1,-0.8,legend=c("1=Fundamental, 2=Médio, 3=Superior"),border=NA,box.col=NA)



barplot(
  table(tab2_1$n_filhos),
  ylab="Frequência",
  cex.names=0.7,
  col="darkgrey",
  #main="Figura 2.4: Gráfico em barras para a variável Z: Numero de filhos.",
  border=NA)

barplot(
  table(tab2_1$n_filhos),
  ylab = "Frequência",
  cex.names=0.7,
  col="darkgrey",
  border=NA)

ggplot(tab2_1, aes(x = n_filhos))+
  geom_bar()


ggplot(tab2_1, aes(x = factor(n_filhos, levels = 1:5))) +
  geom_bar(fill = "steelblue") +  # Define cor das barras
  scale_x_discrete(limits = as.character(1:5)) +  # Define que o eixo X terá valores de 1 a 5
  labs(x = "Número de Filhos", y = "Frequência", title = "Distribuição do Número de Filhos por Pessoa") +
  theme_minimal()







