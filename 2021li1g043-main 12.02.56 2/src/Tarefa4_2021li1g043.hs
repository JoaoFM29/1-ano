{- |
Module      : Tarefa4_2021li1g043
Description : Movimentação do personagem
Copyright   : João Magalhães <a100740@alunos.uminho.pt>;
            : Gonçalo Loureiro <a100535@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g043 where 

import LI12122
import Tarefa1_2021li1g043
import Tarefa2_2021li1g043
import Tarefa3_2021li1g043

{-|
== Definição geral
Aplicamos a função 'correrMovimentos' dando-lhe um jogo e uma lista de movimentos para assim conseguirmos mexer o jogador no mapa.

Para que consigamos fazer isso vamos utilizar a função 'moveJogador'.

== Caso de paragem
Esta função tem como caso de paragem a lista vazia de movimentos, [].

@
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo []      = jogo
correrMovimentos jogo (mov:t) = correrMovimentos (moveJogador jogo mov)
@

== Exemplo de utilização da função 'correrMovimentos':
>>> correrMovimentos (Jogo [[Vazio, Vazio, Vazio, Vazio], [Vazio, Vazio, Caixa, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (3,1) Oeste False)) [InterageCaixa, AndarEsquerda] = (Jogo [[Vazio, Vazio, Caixa, Vazio], [Vazio, Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (2,1) Oeste True))

-}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos j@(Jogo mapa (Jogador (x,y) dir bool)) []      = j
correrMovimentos j@(Jogo mapa (Jogador (x,y) dir bool)) (mov:t) = correrMovimentos (moveJogador j mov) t 

{-|
== Definição Geral
Aplicamos a função 'moveJogador' para conseguirmos movimentar o jogador a partir de 4 movimentos possíveis, fazendo-o assim mover-se no mapa.

Para que consigamos fazer isso vamos utilizar as funções 'podeAndar', 'andaCaixa', 'andaJogador', 'largaCaixa', 'pegaCaixa'

== Caso de paragem
Esta função não tem caso de paragem porque é auxiliar da 'correrMovimentos' logo, o seu caso de paragem está intrínseco a esta.

@
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador j@(Jogo mapa (Jogador (x,y) dir bool)) mov
                                                | mov == AndarDireita && dir == Este && bool == False && podeAndar(Jogo mapa (Jogador (x,y) Este False)) AndarDireita = andaJogador j AndarDireita  --^ Andar sem caixa para o lado Direito.
                                                | mov == AndarDireita && dir == Este && bool == True  && podeAndar(Jogo mapa (Jogador (x,y) Este True)) AndarDireita  = andaJogador j AndarDireita  --^ Andar com caixa para o lado Direito.
                                                | mov == AndarEsquerda && dir == Oeste && bool == False && podeAndar (Jogo mapa (Jogador (x,y) Oeste False)) AndarEsquerda = andaJogador j AndarEsquerda  --^ Andar sem caixa para o lado Esquerdo.                                         
                                                | mov == AndarEsquerda && dir == Oeste && bool == True  && podeAndar (Jogo mapa (Jogador (x,y) Oeste True)) AndarEsquerda  = andaJogador j AndarEsquerda  --^ Andar com caixa para o lado Esquerdo.                                                                         
                                                | mov == Trepar && dir == Este  && bool == False && podeAndar (Jogo mapa (Jogador (x,y) Este  False)) Trepar = (Jogo mapa (Jogador (x+1,y-1) Este  False)) --^ Subir sem caixa para o lado Direito.
                                                | mov == Trepar && dir == Oeste && bool == False && podeAndar (Jogo mapa (Jogador (x,y) Oeste False)) Trepar = (Jogo mapa (Jogador (x-1,y-1) Oeste False)) --^ Subir sem caixa para o lado Esquerdo.                              
                                                | mov == Trepar && dir == Este  && bool == True && podeAndar (Jogo mapa (Jogador (x,y) Este  True)) Trepar = (Jogo (andaCaixa j Trepar) (Jogador (x+1,y-1) Este  True)) --^ Subir com caixa para o lado Direito.
                                                | mov == Trepar && dir == Oeste && bool == True && podeAndar (Jogo mapa (Jogador (x,y) Oeste True)) Trepar = (Jogo (andaCaixa j Trepar) (Jogador (x-1,y-1) Oeste True)) --^ Subir com caixa para o lado Esquerdo. 
                                                | mov == InterageCaixa && dir == Este  && bool == True && (elem(Vazio,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) && not(elem(Porta,(x+1,y)) (constroiMapaPVO(desconstroiMapa mapa))) = (Jogo (largaCaixa (Jogo (constroiMapa(desconstroiMapa mapa)) (Jogador (x,y) Este True)))  (Jogador (x,y) Este  False)) --^ Largar a caixa para o lado Esquerdo.
                                                | mov == InterageCaixa && dir == Oeste && bool == True && (elem(Vazio,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) && not(elem(Porta,(x-1,y)) (constroiMapaPVO(desconstroiMapa mapa))) = (Jogo (largaCaixa (Jogo (constroiMapa(desconstroiMapa mapa)) (Jogador (x,y) Oeste True))) (Jogador (x,y) Oeste False)) --^ Largar a caixa para o lado Direito.
                                                | mov == InterageCaixa && dir == Este  && bool == False && (elem(Caixa,(x+1,y)) (desconstroiMapa mapa)) && (elem(Vazio,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) && (elem(Vazio,(x,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) = (Jogo (pegaCaixa (Jogo (constroiMapa(desconstroiMapa mapa)) (Jogador (x,y) Este  False))) (Jogador (x,y) Este  True)) --^ Pegar a caixa quando esta se encontra do lado Esquerdo.
                                                | mov == InterageCaixa && dir == Oeste && bool == False && (elem(Caixa,(x-1,y)) (desconstroiMapa mapa)) && (elem(Vazio,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) && (elem(Vazio,(x,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) = (Jogo (pegaCaixa (Jogo (constroiMapa(desconstroiMapa mapa)) (Jogador (x,y) Oeste False))) (Jogador (x,y) Oeste True)) --^ Pegar a caixa quando esta se encontra do lado Direito. 
                                                | mov == AndarDireita  && dir == Oeste = moveJogador (Jogo mapa (Jogador (x,y) Este  bool))  AndarDireita --^ Andar para a Direita quando está virado para o lado Esquerdo.
                                                | mov == AndarEsquerda && dir == Este  = moveJogador (Jogo mapa (Jogador (x,y) Oeste bool)) AndarEsquerda --^ Andar para a Esquerdo quando está virado para o lado Direito.
                                                | otherwise = j --^ Se nenhum dos casos anteriores se verificar, devolve o mesmo jogo.

== Exemplo de utilização da função 'moveJogador':
>>> moveJogador (Jogo [[Vazio, Vazio, Vazio, Vazio], [Vazio, Vazio, Caixa, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (3,1) Oeste False)) InterageCaixa = (Jogo [[Vazio, Vazio, Vazio, Caixa], [Vazio, Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (3,1) Oeste True))
-}

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador j@(Jogo mapa (Jogador (x,y) dir bool)) mov 
                                                | mov == AndarDireita  && dir == Este  && bool == False && podeAndar (Jogo mapa (Jogador (x,y) Este False))  AndarDireita  = andaJogador j AndarDireita 
                                                | mov == AndarDireita  && dir == Este  && bool == True  && podeAndar (Jogo mapa (Jogador (x,y) Este True))   AndarDireita  = andaJogador j AndarDireita 
                                                | mov == AndarEsquerda && dir == Oeste && bool == False && podeAndar (Jogo mapa (Jogador (x,y) Oeste False)) AndarEsquerda = andaJogador j AndarEsquerda                                            
                                                | mov == AndarEsquerda && dir == Oeste && bool == True  && podeAndar (Jogo mapa (Jogador (x,y) Oeste True))  AndarEsquerda = andaJogador j AndarEsquerda                                            
                                                | mov == Trepar && dir == Este  && bool == False && podeAndar (Jogo mapa (Jogador (x,y) Este  False)) Trepar = (Jogo mapa (Jogador (x+1,y-1) Este  False))
                                                | mov == Trepar && dir == Oeste && bool == False && podeAndar (Jogo mapa (Jogador (x,y) Oeste False)) Trepar = (Jogo mapa (Jogador (x-1,y-1) Oeste False))
                                                | mov == Trepar && dir == Este  && bool == True  && podeAndar (Jogo mapa (Jogador (x,y) Este  True))  Trepar = (Jogo (andaCaixa j Trepar) (Jogador (x+1,y-1) Este True))
                                                | mov == Trepar && dir == Oeste && bool == True  && podeAndar (Jogo mapa (Jogador (x,y) Oeste True))  Trepar = (Jogo (andaCaixa j Trepar) (Jogador (x-1,y-1) Oeste True))
                                                | mov == InterageCaixa && dir == Este  && bool == True && (elem(Vazio,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) && not(elem(Porta,(x+1,y)) (constroiMapaPVO(desconstroiMapa mapa))) = (Jogo (largaCaixa (Jogo (constroiMapa(desconstroiMapa mapa)) (Jogador (x,y) Este True))) (Jogador (x,y) Este False))
                                                | mov == InterageCaixa && dir == Oeste && bool == True && (elem(Vazio,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) && not(elem(Porta,(x-1,y)) (constroiMapaPVO(desconstroiMapa mapa))) = (Jogo (largaCaixa (Jogo (constroiMapa(desconstroiMapa mapa)) (Jogador (x,y) Oeste True))) (Jogador (x,y) Oeste False))
                                                | mov == InterageCaixa && dir == Este  && bool == False && (elem(Caixa,(x+1,y)) (desconstroiMapa mapa)) && (elem(Vazio,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) && (elem(Vazio,(x,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) = (Jogo (pegaCaixa (Jogo (constroiMapa(desconstroiMapa mapa)) (Jogador (x,y) Este False))) (Jogador (x,y) Este True))
                                                | mov == InterageCaixa && dir == Oeste && bool == False && (elem(Caixa,(x-1,y)) (desconstroiMapa mapa)) && (elem(Vazio,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) && (elem(Vazio,(x,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) = (Jogo (pegaCaixa (Jogo (constroiMapa(desconstroiMapa mapa)) (Jogador (x,y) Oeste False))) (Jogador (x,y) Oeste True))
                                                | mov == AndarDireita  && dir == Oeste = moveJogador (Jogo mapa (Jogador (x,y) Este bool)) AndarDireita
                                                | mov == AndarEsquerda && dir == Este  = moveJogador (Jogo mapa (Jogador (x,y) Oeste bool)) AndarEsquerda
                                                | otherwise = j                                              

{-|
== Definição Geral
Verificar a veracidade do movimento.

== Definição mais específica
Aplicamos a função 'podeAndar' para verificar se o movimento do jogador é possível de ser executado, devolvendo True no caso de o ser. Caso contrário, False.

== Caso de paragem
Esta função não tem caso de paragem porque é auxiliar da 'correrMovimentos' logo, o seu caso de paragem está intrínseco a esta.

podeAndar :: Jogo -> Movimento -> Bool
podeAndar j@(Jogo mapa (Jogador (x,y) dir bool)) mov
                                   | mov == AndarDireita  && dir == Este  && bool == False && ((elem(Vazio,(x+1,y)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x+1,y)) (constroiMapaPVO(desconstroiMapa mapa)))) && ((elem(Bloco,(x+1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Caixa,(x+1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Vazio,(x+1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))))  = True --^ Andar sem caixa para a Direita.
                                   | mov == AndarEsquerda && dir == Oeste && bool == False && ((elem(Vazio,(x-1,y)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x-1,y)) (constroiMapaPVO(desconstroiMapa mapa)))) && ((elem(Bloco,(x-1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Caixa,(x-1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Vazio,(x-1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))))  = True --^ Andar sem caixa para a Esquerda.                                                                                                  
                                   | mov == AndarDireita  && dir == Este  && bool == True && ((elem(Vazio,(x+1,y)) (constroiMapaPVO(desconstroiMapa mapa))) && (elem(Vazio,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)))) && ((elem(Bloco,(x+1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Caixa,(x+1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Vazio,(x+1,y+1)) (constroiMapaPVO(desconstroiMapa mapa)))) = True --^ Andar com caixa para a Direita.
                                   | mov == AndarEsquerda && dir == Oeste && bool == True && ((elem(Vazio,(x-1,y)) (constroiMapaPVO(desconstroiMapa mapa))) && (elem(Vazio,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)))) && ((elem(Bloco,(x-1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Caixa,(x-1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Vazio,(x-1,y+1)) (constroiMapaPVO(desconstroiMapa mapa)))) = True --^ Andar com caixa para a Esquerda.                               
                                   | mov == Trepar && dir == Este  && bool == False && (elem(Vazio,(x,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) && ((elem(Vazio,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)))) && ((elem(Caixa,(x+1,y)) (desconstroiMapa mapa)) || (elem(Bloco,(x+1,y)) (desconstroiMapa mapa))) = True --^ Trepar sem caixa para a Direita. 
                                   | mov == Trepar && dir == Oeste && bool == False && (elem(Vazio,(x,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) && ((elem(Vazio,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)))) && ((elem(Caixa,(x-1,y)) (desconstroiMapa mapa)) || (elem(Bloco,(x-1,y)) (desconstroiMapa mapa))) = True --^ Trepar sem caixa para a Esquerda.                                                        
                                   | mov == Trepar && dir == Este  && bool == True && (elem(Vazio,(x,y-2)) (constroiMapaPVO(desconstroiMapa mapa))) && ((elem(Vazio,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)))) && (elem(Vazio,(x+1,y-2)) (constroiMapaPVO(desconstroiMapa mapa))) && ((elem(Caixa,(x+1,y)) (desconstroiMapa mapa)) || (elem(Bloco,(x+1,y)) (desconstroiMapa mapa))) = True --^ Trepar com caixa para a Direita.
                                   | mov == Trepar && dir == Oeste && bool == True && (elem(Vazio,(x,y-2)) (constroiMapaPVO(desconstroiMapa mapa))) && ((elem(Vazio,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)))) && (elem(Vazio,(x-1,y-2)) (constroiMapaPVO(desconstroiMapa mapa))) && ((elem(Caixa,(x-1,y)) (desconstroiMapa mapa)) || (elem(Bloco,(x-1,y)) (desconstroiMapa mapa))) = True --^ Trepar com caixa para a Esquerda.                                          
                                   | otherwise = False

== Exemplos de utilização da função 'podeAndar':
>>> podeAndar (Jogo [[Vazio, Vazio, Vazio, Vazio], [Vazio, Vazio, Caixa, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (3,1) Oeste False)) AndarEsquerda = False

>>> podeAndar (Jogo [[Vazio, Vazio, Vazio, Caixa], [Vazio, Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (3,1) Oeste True)) AndarEsquerda = True
-}

podeAndar :: Jogo -> Movimento -> Bool
podeAndar j@(Jogo mapa (Jogador (x,y) dir bool)) mov
                                   | mov == AndarDireita  && dir == Este  && bool == False && ((elem(Vazio,(x+1,y)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x+1,y)) (constroiMapaPVO(desconstroiMapa mapa)))) && ((elem(Bloco,(x+1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Caixa,(x+1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Vazio,(x+1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))))  = True
                                   | mov == AndarEsquerda && dir == Oeste && bool == False && ((elem(Vazio,(x-1,y)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x-1,y)) (constroiMapaPVO(desconstroiMapa mapa)))) && ((elem(Bloco,(x-1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Caixa,(x-1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Vazio,(x-1,y+1)) (constroiMapaPVO(desconstroiMapa mapa)))) = True
                                   | mov == AndarDireita  && dir == Este  && bool == True && ((elem(Vazio,(x+1,y)) (constroiMapaPVO(desconstroiMapa mapa))) && (elem(Vazio,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)))) && ((elem(Bloco,(x+1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Caixa,(x+1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Vazio,(x+1,y+1)) (constroiMapaPVO(desconstroiMapa mapa)))) = True
                                   | mov == AndarEsquerda && dir == Oeste && bool == True && ((elem(Vazio,(x-1,y)) (constroiMapaPVO(desconstroiMapa mapa))) && (elem(Vazio,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)))) && ((elem(Bloco,(x-1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Caixa,(x-1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Vazio,(x-1,y+1)) (constroiMapaPVO(desconstroiMapa mapa))))=  True
                                   | mov == Trepar && dir == Este  && bool == False && (elem(Vazio,(x,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) && ((elem(Vazio,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)))) && ((elem(Caixa,(x+1,y)) (desconstroiMapa mapa)) || (elem(Bloco,(x+1,y)) (desconstroiMapa mapa))) = True
                                   | mov == Trepar && dir == Oeste && bool == False && (elem(Vazio,(x,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) && ((elem(Vazio,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)))) && ((elem(Caixa,(x-1,y)) (desconstroiMapa mapa)) || (elem(Bloco,(x-1,y)) (desconstroiMapa mapa))) = True                             
                                   | mov == Trepar && dir == Este  && bool == True && (elem(Vazio,(x,y-2)) (constroiMapaPVO(desconstroiMapa mapa))) && ((elem(Vazio,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)))) && (elem(Vazio,(x+1,y-2)) (constroiMapaPVO(desconstroiMapa mapa))) && ((elem(Caixa,(x+1,y)) (desconstroiMapa mapa)) || (elem(Bloco,(x+1,y)) (desconstroiMapa mapa))) = True
                                   | mov == Trepar && dir == Oeste && bool == True && (elem(Vazio,(x,y-2)) (constroiMapaPVO(desconstroiMapa mapa))) && ((elem(Vazio,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa))) || (elem(Porta,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)))) && (elem(Vazio,(x-1,y-2)) (constroiMapaPVO(desconstroiMapa mapa))) && ((elem(Caixa,(x-1,y)) (desconstroiMapa mapa)) || (elem(Bloco,(x-1,y)) (desconstroiMapa mapa))) = True                              
                                   | otherwise = False
{-|
== Definição Geral
Mover a Caixa.

== Definição mais específica
Através da utilização das funções predefinidas 'take' e 'drop', atualizamos o mapa colocando a peça Caixa na posição a cima da que irá corresponder, futuramente, à do Jogador após o movimento.

== Caso de paragem
Esta função não tem caso de paragem porque é auxiliar da 'correrMovimentos' logo, o seu caso de paragem está intrínseco a esta.

@
andaCaixa :: Jogo -> Movimento -> Mapa
andaCaixa (Jogo mapa (Jogador (x,y) dir True)) mov
                                        | dir == Este  && podeAndar j mov = andaCaixaAux( Jogo (constroiMapa(take((xmaximo + 1) * yqE + x + 1) listatoda ++ [(Caixa,(x+1,yqE))] ++ drop ((xmaximo +1)*yqE + x + 2) listatoda)) (Jogador (x,y) dir True ))
                                        | dir == Oeste && podeAndar j mov = andaCaixaAux( Jogo (constroiMapa(take((xmaximo + 1) * yqO + x - 1) listatoda ++ [(Caixa,(x-1,yqO))] ++ drop ((xmaximo +1)*yqO + x) listatoda)) (Jogador (x,y) dir True ))
                             where xmaximo   = xMax (map snd(desconstroiMapa mapa)) --^ Determina a abcissa máxima do mapa. Ao somar '+1' obtemos o número máximo de Peças. 
                                        listatoda = constroiMapaPVO(desconstroiMapa mapa)
                                        yqE = ((yDoChao (x+1,y) (desconstroiMapa mapa)) - 1) --^ Determina o 'y' respetivo ao jogador depois de se mover para o lado direito. Ao subtrair '-1' obtemos a posiçao da Caixa.
                                        yqO = ((yDoChao (x-1,y) (desconstroiMapa mapa)) - 1) --^ Determina o 'y' respetivo ao jogador depois de se mover para o lado esquerdo. Ao subtrair '-1' obtemos a posiçao da Caixa.
@

== Exemplo de utilização da função 'andaCaixa':
>>> andaCaixa (Jogo [[Vazio, Vazio, Vazio, Caixa], [Vazio, Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (3,1) Oeste True)) AndarEsquerda = [[Vazio, Vazio, Caixa, Vazio], [Vazio, Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco, Bloco]]

== Definição Geral
Auxiliar a função 'andaCaixa'.

== Definição mais específica
Através da utilização das funções predefinidas 'take' e 'drop', atualizamos o mapa colocando a peça Vazio na posição a cima à do Jogador antes do movimento.

== Caso de paragem
Esta função não tem caso de paragem porque é auxiliar da 'andaCaixa' logo, o seu caso de paragem está intrínseco a esta.

@
andaCaixaAux :: Jogo -> Mapa 
andaCaixaAux (Jogo mapa (Jogador (x,y) dir True)) = constroiMapa(take((xmaximo + 1) * (y-1) + x) listatoda ++ [(Vazio,(x,y-1))] ++ drop ((xmaximo +1)*(y-1) + x + 1) listatoda)  
                          where xmaximo   = xMax (map snd(desconstroiMapa mapa)) --^ Determina a abcissa máxima do mapa. Ao somar '+1' obtemos o número máximo de Peças.
                                listatoda = constroiMapaPVO(desconstroiMapa mapa)
@

== Exemplo de utilização da função 'andaCaixaAux':
>>> andaCaixaAux (Jogo [[Vazio, Vazio, Caixa, Caixa], [Vazio, Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (3,1) Oeste True)) = [[Vazio, Vazio, Caixa, Vazio], [Vazio, Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco, Bloco]]
-}

andaCaixa :: Jogo -> Movimento -> Mapa
andaCaixa j@(Jogo mapa (Jogador (x,y) dir True)) mov
                                        | dir == Este  && podeAndar j mov = andaCaixaAux( Jogo (constroiMapa(take((xmaximo + 1) * yqE + x + 1) listatoda ++ [(Caixa,(x+1,yqE))] ++ drop ((xmaximo +1)*yqE + x + 2) listatoda)) (Jogador (x,y) dir True ))
                                        | dir == Oeste && podeAndar j mov = andaCaixaAux( Jogo (constroiMapa(take((xmaximo + 1) * yqO + x - 1) listatoda ++ [(Caixa,(x-1,yqO))] ++ drop ((xmaximo +1)*yqO + x) listatoda)) (Jogador (x,y) dir True ))
                                  where xmaximo   = xMax (map snd(desconstroiMapa mapa))
                                        listatoda = constroiMapaPVO(desconstroiMapa mapa)
                                        yqE = ((yDoChao (x+1,y) (desconstroiMapa mapa)) - 1)
                                        yqO = ((yDoChao (x-1,y) (desconstroiMapa mapa)) - 1)
                                     
andaCaixaAux :: Jogo -> Mapa 
andaCaixaAux (Jogo mapa (Jogador (x,y) dir True)) = constroiMapa(take((xmaximo + 1) * (y-1) + x) listatoda ++ [(Vazio,(x,y-1))] ++ drop ((xmaximo +1)*(y-1) + x + 1) listatoda)  
                          where xmaximo   = xMax (map snd(desconstroiMapa mapa))
                                listatoda = constroiMapaPVO(desconstroiMapa mapa)

{-|
== Definição Geral
Larga a Caixa.

== Definição mais específica
Através da utilização das funções predefinidas 'take' e 'drop', atualizamos o mapa colocando a peça Caixa na posição à frente da do Jogador.

== Caso de paragem
Esta função não tem caso de paragem porque é auxiliar da 'correrMovimentos' logo, o seu caso de paragem está intrínseco a esta.

@
largaCaixa :: Jogo -> Mapa
largaCaixa (Jogo mapa (Jogador (x,y) dir True))
                              | dir == Este  && elem(Vazio,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)) = largaCaixaAux(Jogo (constroiMapa(take((xmaximo + 1) * kE  + x + 1) listatoda    ++ [(Caixa,(x+1,kE))]  ++ drop ((xmaximo +1)* kE + x + 2) listatoda))    (Jogador (x,y) Este  False))
                              | dir == Oeste && elem(Vazio,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)) = largaCaixaAux(Jogo (constroiMapa(take((xmaximo + 1) * kO  + x - 1) listatoda    ++ [(Caixa,(x-1,kO))]  ++ drop ((xmaximo +1)* kO + x) listatoda))        (Jogador (x,y) Oeste  False))                       
                              | otherwise = mapa
                        where xmaximo = xMax (map snd(desconstroiMapa mapa)) --^ Determina a abcissa máxima do mapa. Ao somar '+1' obtemos o número máximo de Peças.                             
                              listatoda = constroiMapaPVO(desconstroiMapa mapa)
                              kE = yDoChao (x+1,y) (desconstroiMapa mapa) --^ y da caixa quando esta cai para o lado Este
                              kO = yDoChao (x-1,y) (desconstroiMapa mapa) --^ y da caixa quando esta cai para o lado Oeste
@

== Exemplo de utilização da função 'largaCaixa':
>>> largaCaixa (Jogo [[Vazio, Vazio, Vazio, Caixa], [Vazio, Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (3,1) Oeste True)) = [[Vazio, Vazio, Vazio, Vazio], [Vazio, Vazio, Caixa, Vazio],[Bloco, Bloco, Bloco, Bloco]]

== Definição Geral
Auxilia a funçao 'largaCaixa'.

== Definição mais específica
Através da utilização das funções predefinidas 'take' e 'drop', atualizamos o mapa colocando a peça Vazio na posição em cima da do Jogador.

== Caso de paragem
Esta função não tem caso de paragem porque é auxiliar da 'largaCaixa' logo, o seu caso de paragem está intrínseco a esta.

@
largaCaixaAux :: Jogo -> Mapa
largaCaixaAux (Jogo mapa (Jogador (x,y) dir bool)) = constroiMapa(take((xmaximo + 1) * (y-1) + x) listatoda ++ [(Vazio,(x,y-1))] ++ drop((xmaximo +1)* (y-1) + x + 1) listatoda)
                        where xmaximo = xMax (map snd(desconstroiMapa mapa)) --^ Determina a abcissa máxima do mapa. Ao somar '+1' obtemos o número máximo de Peças.                              
                              listatoda = constroiMapaPVO(desconstroiMapa mapa)
@

== Exemplo de utilização da função 'largaCaixaAux':
>>> largaCaixaAux (Jogo [[Vazio, Vazio, Vazio, Caixa], [Vazio, Vazio, Caixa, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (3,1) Oeste True)) = [[Vazio, Vazio, Vazio, Vazio], [Vazio, Vazio, Caixa, Vazio],[Bloco, Bloco, Bloco, Bloco]]
-}

largaCaixa :: Jogo -> Mapa
largaCaixa (Jogo mapa (Jogador (x,y) dir True))
                              | dir == Este  && elem(Vazio,(x+1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)) = largaCaixaAux(Jogo (constroiMapa(take((xmaximo + 1) * yqE  + x + 1) listatoda    ++ [(Caixa,(x+1,yqE))]  ++ drop ((xmaximo +1)* yqE + x + 2) listatoda))    (Jogador (x,y) Este  False))
                              | dir == Oeste && elem(Vazio,(x-1,y-1)) (constroiMapaPVO(desconstroiMapa mapa)) = largaCaixaAux(Jogo (constroiMapa(take((xmaximo + 1) * yqO  + x - 1) listatoda    ++ [(Caixa,(x-1,yqO))]  ++ drop ((xmaximo +1)* yqO + x) listatoda))        (Jogador (x,y) Oeste  False))                       
                              | otherwise = mapa
                        where xmaximo = xMax (map snd(desconstroiMapa mapa))                               
                              listatoda = constroiMapaPVO(desconstroiMapa mapa)
                              yqE = yDoChao (x+1,y) (desconstroiMapa mapa) 
                              yqO = yDoChao (x-1,y) (desconstroiMapa mapa) 

largaCaixaAux :: Jogo -> Mapa
largaCaixaAux (Jogo mapa (Jogador (x,y) dir bool)) = constroiMapa(take((xmaximo + 1) * (y-1) + x) listatoda ++ [(Vazio,(x,y-1))] ++ drop((xmaximo +1)* (y-1) + x + 1) listatoda)
                        where xmaximo = xMax (map snd(desconstroiMapa mapa))                               
                              listatoda = constroiMapaPVO(desconstroiMapa mapa)

{-|
== Definição Geral
Pegar a Caixa.

== Definição mais específica
Através da utilização das funções predefinidas 'take' e 'drop', atualizamos o mapa colocando a peça Caixa na posição em cima da do Jogador.

== Caso de paragem
Esta função não tem caso de paragem porque é auxiliar da 'correrMovimentos' logo, o seu caso de paragem está intrínseco a esta.

@
pegaCaixa :: Jogo -> Mapa
pegaCaixa (Jogo mapa (Jogador (x,y) dir bool))
                        | dir == Este  && bool == False = pegaCaixaAux( Jogo (constroiMapa( take((xmaximo + 1) * y + x ) listatoda ++ [(Vazio,(x+1,y))] ++ drop ((xmaximo +1)* (y-1) + x + 1) listatoda)) (Jogador (x,y) Este True))
                        | dir == Oeste && bool == False = pegaCaixaAux( Jogo (constroiMapa( take((xmaximo + 1) * y + x - 1) listatoda ++ [(Vazio,(x-1,y))] ++ drop ((xmaximo +1)* (y-1) + x) listatoda)) (Jogador (x,y) Oeste True))                          
                        | otherwise = mapa
                where  xmaximo   = xMax (map snd(desconstroiMapa mapa)) --^ Determina a abcissa máxima do mapa. Ao somar '+1' obtemos o número máximo de Peças.
                       listatoda = constroiMapaPVO(desconstroiMapa mapa
@

== Exemplo de utilização da função 'pegaCaixa':
>>> pegaCaixa (Jogo [[Vazio, Vazio, Vazio, Vazio], [Vazio, Vazio, Caixa, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (3,1) Oeste True)) = [[Vazio, Vazio, Vazio, Caixa], [Vazio, Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco, Bloco]]

== Definição Geral
Auxilia a funçao 'pegaCaixa'.

== Definição mais específica
Através da utilização das funções predefinidas 'take' e 'drop', atualizamos o mapa colocando a peça Vazio na posição à frente da do Jogador.

== Caso de paragem
Esta função não tem caso de paragem porque é auxiliar da 'pegaCaixa' logo, o seu caso de paragem está intrínseco a esta.

@
pegaCaixaAux :: Jogo -> Mapa
pegaCaixaAux (Jogo mapa (Jogador (x,y) dir bool))  
                               | dir == Este  = constroiMapa( take((xmaximo + 1) * (y-1) + x) listatoda ++ [(Caixa,(x,y-1))] ++ drop ((xmaximo +1)* (y-1) + x + 1) listatoda)
                               | dir == Oeste = constroiMapa( take((xmaximo + 1) * (y-1) + x) listatoda ++ [(Caixa,(x,y-1))] ++ drop ((xmaximo +1)* (y-1) + x + 1) listatoda)
                      where  xmaximo   = xMax (map snd(desconstroiMapa mapa)) --^ Determina a abcissa máxima do mapa. Ao somar '+1' obtemos o número máximo de Peças.
                             listatoda = constroiMapaPVO(desconstroiMapa mapa)
@

== Exemplo de utilização da função 'pegaCaixaAux':
>>> pegaCaixaAux (Jogo [[Vazio, Vazio, Vazio, Caixa], [Vazio, Vazio, Caixa, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (3,1) Oeste True)) = [[Vazio, Vazio, Vazio, Caixa], [Vazio, Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco, Bloco]]
-}

pegaCaixa :: Jogo -> Mapa
pegaCaixa (Jogo mapa (Jogador (x,y) dir bool))
                        | dir == Este  && bool == False = pegaCaixaAux( Jogo (constroiMapa( take((xmaximo + 1) * y + x ) listatoda ++ [(Vazio,(x+1,y))] ++ drop ((xmaximo +1)* (y-1) + x + 1) listatoda)) (Jogador (x,y) Este True))
                        | dir == Oeste && bool == False = pegaCaixaAux( Jogo (constroiMapa( take((xmaximo + 1) * y + x - 1) listatoda ++ [(Vazio,(x-1,y))] ++ drop ((xmaximo +1)* (y-1) + x) listatoda)) (Jogador (x,y) Oeste True))                          
                        | otherwise = mapa
                where  xmaximo   = xMax (map snd(desconstroiMapa mapa))
                       listatoda = constroiMapaPVO(desconstroiMapa mapa)

pegaCaixaAux :: Jogo -> Mapa
pegaCaixaAux (Jogo mapa (Jogador (x,y) dir bool))  
                               | dir == Este  = constroiMapa( take((xmaximo + 1) * (y-1) + x) listatoda ++ [(Caixa,(x,y-1))] ++ drop ((xmaximo +1)* (y-1) + x + 1) listatoda)
                               | dir == Oeste = constroiMapa( take((xmaximo + 1) * (y-1) + x) listatoda ++ [(Caixa,(x,y-1))] ++ drop ((xmaximo +1)* (y-1) + x + 1) listatoda)
                      where  xmaximo   = xMax (map snd(desconstroiMapa mapa))
                             listatoda = constroiMapaPVO(desconstroiMapa mapa)

{-|
== Definição Geral
Atualizar o Jogo depois de um movimento.

== Definição mais específica
Através do auxilio da função 'andaCaixa' e da 'ydoChao', atualizamos o mapa e a respetiva posição do Jogador.

== Caso de paragem
Esta função não tem caso de paragem porque é auxiliar da 'correrMovimentos' logo, o seu caso de paragem está intrínseco a esta.

@
andaJogador :: Jogo -> Movimento -> Jogo
andaJogador j@(Jogo mapa (Jogador (x,y) dir bool )) mov
                                 | mov == AndarEsquerda && bool == True  && ((elem (Porta,(x-1,y)) (desconstroiMapa mapa)) || not(elem (Porta,(x-1,y)) (desconstroiMapa mapa))) = (Jogo (andaCaixa j AndarEsquerda) (Jogador (x-1,yqO) Oeste bool))
                                 | mov == AndarDireita  && bool == True  && ((elem (Porta,(x+1,y)) (desconstroiMapa mapa)) || not(elem (Porta,(x+1,y)) (desconstroiMapa mapa))) = (Jogo (andaCaixa j AndarDireita)  (Jogador (x+1,yqE) Este bool))
                                 | mov == AndarEsquerda && bool == False && ((elem (Porta,(x-1,y)) (desconstroiMapa mapa)) || not(elem (Porta,(x-1,y)) (desconstroiMapa mapa))) = (Jogo mapa (Jogador (x-1,yqO) Oeste bool))
                                 | mov == AndarDireita  && bool == False && ((elem (Porta,(x+1,y)) (desconstroiMapa mapa)) || not(elem (Porta,(x+1,y)) (desconstroiMapa mapa))) = (Jogo mapa (Jogador (x+1,yqE) Este  bool))
                              where yqE = yDoChao (x+1,y) (desconstroiMapa mapa)
                                    yqO = yDoChao (x-1,y) (desconstroiMapa mapa)
@

== Exemplo de utilização da função 'andaJogador':
>>> andaJogador (Jogo [[Vazio, Vazio, Vazio, Caixa], [Vazio, Vazio, Caixa, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (3,1) Oeste True)) = (Jogo [[Vazio, Vazio, Caixa, Vazio], [Vazio, Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco, Bloco]] (Jogador (2,1) Oeste True))
-}

andaJogador :: Jogo -> Movimento -> Jogo
andaJogador j@(Jogo mapa (Jogador (x,y) dir bool )) mov
                                 | mov == AndarEsquerda && bool == True  && ((elem (Porta,(x-1,y)) (desconstroiMapa mapa)) || not(elem (Porta,(x-1,y)) (desconstroiMapa mapa))) = (Jogo (andaCaixa j AndarEsquerda) (Jogador (x-1,yqO) Oeste bool))
                                 | mov == AndarDireita  && bool == True  && ((elem (Porta,(x+1,y)) (desconstroiMapa mapa)) || not(elem (Porta,(x+1,y)) (desconstroiMapa mapa))) = (Jogo (andaCaixa j AndarDireita)  (Jogador (x+1,yqE) Este bool))
                                 | mov == AndarEsquerda && bool == False && ((elem (Porta,(x-1,y)) (desconstroiMapa mapa)) || not(elem (Porta,(x-1,y)) (desconstroiMapa mapa))) = (Jogo mapa (Jogador (x-1,yqO) Oeste bool))
                                 | mov == AndarDireita  && bool == False && ((elem (Porta,(x+1,y)) (desconstroiMapa mapa)) || not(elem (Porta,(x+1,y)) (desconstroiMapa mapa))) = (Jogo mapa (Jogador (x+1,yqE) Este  bool))
                              where yqE = yDoChao (x+1,y) (desconstroiMapa mapa)
                                    yqO = yDoChao (x-1,y) (desconstroiMapa mapa)

{-|
== Definição Geral
Obtêm o y do chao.

== Definição mais específica
A função 'ydoChao' vai correr a lista toda de peças até encontrar uma que respeita as condições apresentadas e, nesse caso dá a ordenada a cima desta mesma peça.

== Caso de paragem
Esta função não tem caso de paragem porque é auxiliar de várias funções já apresentadas e como nunca é usada independentemente, o seu caso de paragem está relacionado com a função onde é implementada.

@
yDoChao :: Coordenadas -> [(Peca,Coordenadas)] -> Int
yDoChao (x,y) ((c,(xs,ys)):t) 
               | ys >= y && xs == x && (c == Bloco || c == Caixa) = (ys-1)
               | otherwise = yDoChao (x,y) t
@

== Exemplo de utilização da função 'ydoChao':
>>> ydoChao (1,0) = [(Vazio,(0,0)), (Vazi,(1,0)), (Vazio,(2,0)), (Vazio,(3,0)), (Vazio,(0,1)),(Vazio,(1,1)), (Bloco,(2,1)), (Vazio,(3,1)), (Vazio,(0,2)), (Vazio,(1,2)), (Bloco,(2,2)), (Vazio,(2,3)), (Bloco,(0,3)), (Bloco,(1,3)), (Bloco,(2,3)), (Bloco,(3,3))] = 2
-} 

yDoChao :: Coordenadas -> [(Peca,Coordenadas)] -> Int
yDoChao (x,y) ((c,(xs,ys)):t) 
               | ys >= y && xs == x && (c == Bloco || c == Caixa) = (ys-1)
               | otherwise = yDoChao (x,y) t
