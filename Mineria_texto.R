## Ejemplo de Text Mining 

## Librerias
library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(gutenbergr)
library(scales)
library(wordcloud)
library(reshape2)

Sabina<-c("Lo nuestro duró
          Lo que duran dos peces de hielo
          En un güisqui on the rocks
          En vez de fingir
          O, estrellarme una copa de celos
          Le dio por reír
          De pronto me vi
          Como un perro de nadie
          Ladrando, a las puertas del cielo
          Me dejó un neceser con agravios
          La miel en los labios
          Y escarcha en el pelo",
          "Tenían razón
          Mis amantes
          En eso de que, antes
          El malo era yo
          Con una excepción",
          "Esta vez
          Yo quería quererla querer
          Y ella no
          Así que se fue",
          "Me dejó el corazón
          En los huesos
          Y yo de rodillas",
          "Desde el taxi
          Y, haciendo un exceso
          Me tiró dos besos
          Uno por mejilla",
          "Y regresé
          A la maldición
          Del cajón sin su ropa
          A la perdición
          De los bares de copas
          A las cenicientas
          De saldo y esquina
          Y, por esas ventas",
          "Del fino laina
          Pagando las cuentas
          De gente sin alma
          Que pierde la calma
          Con la cocaína
          Volviéndome loco
          Derrochando",
          "La bolsa y la vida
          La fui, poco a poco
          Dando por perdida
          Y eso que yo
          Paro no agobiar con
          Flores a maría
          Para no asediarla
          Con mi antología
          De sábanas frías
          Y alcobas vacías
          Para no comprarla
          Con bisutería",
          "Ni ser el fantoche
          Que va, en romería
          Con la cofradía
          Del santo reproche
          Tanto la quería",
          "Que, tardé, en aprender
          A olvidarla, diecinueve días
          Y quinientas noches
          Dijo hola y adiós
          Y, el portazo, sonó
          Como un signo de interrogación
          Sospecho que, así
          Se vengaba, a través del olvido",
          "Cupido de mi
          No pido perdón
          ¿Para qué? Si me va a perdonar
          Porque ya no le importa
          Siempre tuvo la frente muy alta
          La lengua muy larga
          Y la falda muy corta
          Me abandonó",
          "Como se abandonan
          Los zapatos viejos
          Destrozó el cristal
          De mis gafas de lejos
          Sacó del espejo",
          "Su vivo retrato
          Y, fui, tan torero
          Por los callejones
          Del juego y el vino
          Que, ayer, el portero
          Me echó del casino",
          "De torrelodones
          Qué pena tan grande
          Negaría el santo sacramento
          En el mismo momento
          Que ella me lo mande",
          "Y eso que yo
          Paro no agobiar con
          Flores a maría
          Para no asediarla
          Con mi antología
          De sábanas frías
          Y alcobas vacías
          Para no comprarla
          Con bisutería",
          "Ni ser el fantoche
          Que va, en romería
          Con la cofradía
          Del santo reproche
          Tanto la quería
          Que, tardé, en aprender
          A olvidarla, diecinueve días
          Y quinientas noches
          Y regresé",
         "A la maldición
          Del cajón sin su ropa
          A la perdición
          De los bares de copas
          A las cenicientas
          De saldo y esquina
          Y, por esas ventas
          Del fino laina
          Pagando las cuentas
          De gente sin alma
          Que pierde la calma"
)

Sabina

Sabina_df<-data_frame(line=1:17, text=Sabina)

Sabina_df

Sabina_df%>%
  unnest_tokens(word,text)-> Sabina_df

Sabina_df%>%
  count(word,sort=TRUE)

Sabina_df%>%
  count(word,sort=TRUE)%>%
  filter(n>4)%>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


## Ejemplo con el proyecto gutemberg



hgwells <- gutenberg_download(c(35, 36, 5230, 159))
data("stop_words")
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


tidy_hgwells %>%
  count(word, sort = TRUE)


bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))


tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


tidy_bronte %>%
  count(word, sort = TRUE)


library(tidyr)
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books <- tidy_books %>%
  anti_join(stop_words)


frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)



ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)



cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)


cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)



### Análisis de sentimientos


library(tidytext)

sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)



jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")



pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

pride_prejudice



afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(pride_prejudice %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          pride_prejudice %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)



bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)


get_sentiments("bing") %>% 
  count(sentiment)


bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()



bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


## cloud

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))



tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)



book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

### Ejemplo avanzado

library(tm) # Libreria de text mining 
library(SnowballC) # Algoritmo de derivación de palabras
library(wordcloud) # nube de palabras
library(ggplot2) # Gramática de gráficos
library(dplyr) # Manipulación de datos
library(readr) # Leer archivos
library(cluster) # Clusters
options(encoding = "UTF-8")

Maluma<-c("Ya no sé que hacer 
          No sé con cuál quedarme 
          Todas saben en la cama maltratarme 
          Me tienen bien, de sexo me tienen bien",
          "Estoy enamorado de cuatro babies 
          Siempre me dan lo que quiero 
          Chingan cuando yo les digo 
          Ninguna me pone pero",
          "Dos son casadas 
Hay una soltera 
          La otra medio psycho y si no la llamo se desespera",
          "Estoy enamorado de cuatro babies 
Siempre me dan lo que quiero 
          Chingan cuando yo les digo 
          Ninguna me pone pero",
          "Dos son casadas 
Hay una soltera 
          La otra medio psycho y si no la llamo se desespera",
          "La primera se desespera 
Se encojona si se lo hecho afuera 
          La segunda tiene la funda 
          Y me paga pa' que se lo hunda",
          "Polvos corridos, siempre echamos tres 
A la cuenta de una le bajo la luna 
          Pero ella quiere con Maluma y conmigo a la vez",
          "Estoy enamorado de las cuatro 
Siempre las busco después de las cuatro 
          A las cuatro les encanta en cuatro 
          Y yo nunca fallo como el 24",
          "De los Lakers siempre es la gorras 
De chingar ninguna se enzorra 
          Estoy metio en un lío, ya estoy confundio 
          Porque ninguna de mi mente se borra",
          "Me pongo las gafas Cartier saliendo del aeropuerto 
Vestio de Osiris, zapatos en pie 
Tú tienes tú mi cuenta de banco y el número de la Master Card 
Tú eres mi mujer oficial 
Me tiene enamorado ese culote con ese pelo rubio 
Pero tengo otra pelinegra que siempre quiere chichar",
          "A veces hasta le llega al estudio 
La peliroja chichando es la más que se moja 
          Le encojona que me llame y no lo coja 
          Peleamos y me bota la ropa y tengo que llamar a cotorra pa' que la recoja 
          Tengo una chiquitita nalgona con el pelo corto 
          Me dice papi vente adentro, si me preña (Bryant Myers)",
          "Estoy enamorado de cuatro babies 
Siempre me dan lo que quiero 
          Chingan cuando yo les digo 
          Ninguna me pone pero",
          "Dos son casadas 
Hay una soltera 
          La otra medio psycho y si no la llamo se desespera
          ",
          "Estoy enamorado de cuatro babies 
Siempre me dan lo que quiero 
          Chingan cuando yo les digo 
          Ninguna me pone pero",
          "Dos son casadas 
Hay una soltera 
          La otra medio psycho y si no la llamo se desespera",
          "Ya estoy metio en un lío 
A todas yo quiero darle 
          Me tienen bien confundio 
          Ya no sé ni con cuál quedarme 
          Y es que todas maman bien 
          Todas me lo hacen bien 
          Todas quieren chingarme encima de billetes de cien",
          "Me tienen en un patín 
Comprando en San Valentín 
          Ya me salieron más caras que un reloj de Ulysses Nardin 
          Es que la babies están bunny ninguna las 4 se ha hecho completas 
          Dos tienen maridos y ninguna de las dos al marido respetan",
          "Cuatro chimbitas 
Cuatro personalidades 
          Dos me hablan bonito 
          Dos dicen maldades",
          "Diferentes nacionalidades 
Pero cuando chingan gritan todas por iguales 
          Quiere que la lleve pa' medallo 
          Quiere que la monte en carros del año 
          Que a una la coja 
          A la otra la apriete 
          Y a las otras 2 les dé juntas en el baño",
          "Digan qué más quieren hacer 
El dirty las va a entretener 
          En la casa gigante y un party en el yate que él quiere tener 
          No sé si me entiendes bebé",
          "Estoy enamorado de cuatro babies 
Siempre me dan lo que quiero 
          Chingan cuando yo les digo 
          Ninguna me pone pero",
          "Dos son casadas 
Hay una soltera 
          La otra medio psycho y si no la llamo se desespera
          ",
          "Estoy enamorado de cuatro babies 
Siempre me dan lo que quiero 
          Chingan cuando yo les digo 
          Ninguna me pone pero",
          "Dos son casadas 
Hay una soltera 
          La otra medio psycho y si no la llamo se desespera
          ")



Maluma%>%
  str()
Maluma_df<-data_frame(line=1:24, text=Maluma)

Maluma_df

Maluma_df%>%
  unnest_tokens(word,text)-> Maluma_df

Maluma_df%>%
  count(word,sort=TRUE)

Maluma_df%>%
  count(word,sort=TRUE)%>%
  filter(n>4)%>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



####

Parrafo <- rep(1:ceiling(length(Maluma)/24), each = 24)
Parrafo<-Parrafo[1:length(Maluma)]
Maluma_text<-cbind(Parrafo,Maluma)%>%data.frame()
dim(Maluma_text)
Maluma_text
## Concatenar

Maluma_text<- aggregate(formula = Maluma ~ Parrafo,
                      data = Maluma_text,
                      FUN = paste,
                      collapse = " ")
dim(Maluma_text)



### Limpiar los datos
Maluma_text <- gsub("[[:cntrl:]]", " ", Maluma_text)


## COnvertir todo en minuscula

Maluma_text <- tolower(Maluma_text)


## Eliminar multetillas

Maluma_text <- removeWords(Maluma_text, words = stopwords("spanish"))


## Quitar puntuación

Maluma_text <- removePunctuation(Maluma_text)

# Eliminar Números
Maluma_text <- removeNumbers(Maluma_text)


## Eliminar los espacios vacios

Maluma_text[2] <- stripWhitespace(Maluma_text)


## Crear el corpus

Maluma_corpus <- Corpus(VectorSource(Maluma_text[2]))
Maluma_corpus
Maluma_ptd <- tm_map(Maluma_corpus, PlainTextDocument)

wordcloud(Maluma_ptd, max.words = 30, random.order = F, colors = brewer.pal(name = "Dark2", n = 4))

Maluma_tdm <- TermDocumentMatrix(Maluma_corpus)
Maluma_tdm


nov_mat <- as.matrix(Maluma_tdm)
dim(nov_mat)
nov_mat <- nov_mat %>% rowSums() %>% sort(decreasing = TRUE)
nov_mat <- data.frame(palabra = names(nov_mat), frec = nov_mat)

wordcloud(
  words = nov_mat$palabra, 
  freq = nov_mat$frec, 
  max.words = 80, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)


nov_mat[1:20, ]


## 20 palabras más usadas

nov_mat[1:20, ] %>%
  ggplot(aes(palabra, frec)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = frec)) + 
  coord_flip() + 
  labs(title = "Diez palabras más frecuentes en Niebla",  x = "Palabras", y = "Número de usos")


nov_mat %>%
  mutate(perc = (frec/sum(frec))*100) %>%
  .[1:10, ] %>%
  ggplot(aes(palabra, perc)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = round(perc, 2))) + 
  coord_flip() +
  labs(title = "Diez palabras más frecuentes en Niebla", x = "Palabras", y = "Porcentaje de uso")



### Eliminar palabras dispersas

nov_new <- removeSparseTerms(Maluma_tdm, sparse = .4)
nov_new
Maluma_tdm$nrow
nov_new <- nov_new %>% as.matrix()
nov_new <- nov_new / rowSums(nov_new)
nov_dist <- dist(nov_new, method = "euclidian")
nov_hclust <-  hclust(nov_dist, method = "ward.D")
plot(nov_hclust, main = "Dendrograma del Idio%&/ de Maluma- hclust", sub = "", xlab = "")

plot(nov_hclust, main = " ", sub = "", xlab = "")
rect.hclust(nov_hclust, k = 10, border="blue")



## Aglomeración
nov_agnes <- agnes(nov_dist, method = "average")
plot(nov_agnes, which.plots = 2, main = " ", sub = "", xlab = "")
rect.hclust(nov_agnes, k = 10, border = "blue")





