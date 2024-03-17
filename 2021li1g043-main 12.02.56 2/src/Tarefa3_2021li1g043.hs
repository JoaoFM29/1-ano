{- |
Module      : Tarefa3_2021li1g043
Description : Representação textual do jogo
Copyright   : João Magalhães <a100740@alunos.uminho.pt>;
            : Gonçalo Loureiro <a100535@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g043 where
import           LI12122

{-|
== Definição Geral
Vai ser utlizada para transformar um jogo em texto, para assim poder ser utilizada na instância Show.

Para que isso aconteça vamos utilizar as funções 'jogoptexto' e 'insJogador'.

> show (Jogo mapa jogador) = insJogador jogador (jogoptexto mapa)

==Exemplo da utilização da instância Show:
insJogador (Jogador (1,1) Este False) (jogoptexto [[Porta,Vazio,Vazio,Vazio],[Bloco,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Vazio]])
>>> "Posição ocupada!"

insJogador (Jogador (1,0) Oeste False) (jogoptexto [[Porta,Vazio,Vazio,Vazio],[Bloco,Caixa,Vazio,Bloco],[Bloco,Bloco,Bloco,Vazio]])
>>> "P<  \nXC X\nXXX "
-}

instance Show Jogo where
  show (Jogo mapa jogador) = insJogador jogador (jogoptexto mapa)

{-|
== Definição Geral
Aplicamos a função 'jogoptexto' ,recursivamente, a cada linha. No fim de cada linha adiciona "\n".

Quando só sobrar uma lista no mapa não vai ser adicionado no fim da linha "\n".

Para que os elementos apareçam transformados em texto vamos utilizar a função auxiliar 'jogoptexto2'.

@jogoptexto :: Mapa -> [String]
jogoptexto []      = []
jogoptexto [lnh]   = [jogoptexto2 lnh]
jogoptexto (lnh:t) = (jogoptexto2 lnh ++ "\n") : jogoptexto t
@

==Exemplos da utilização da função 'jogoptexto':
jogoptexto [[Porta,Bloco,Vazio,Vazio],[Bloco,Bloco,Caixa,Vazio],[Bloco,Bloco,Bloco,Bloco]]
>>> ["PX  \n","XXC \n","XXXX"]
-}

jogoptexto :: Mapa -> [String]
jogoptexto []      = []
jogoptexto [lnh]   = [jogoptexto2 lnh]
jogoptexto (lnh:t) = (jogoptexto2 lnh ++ "\n") : jogoptexto t

{-|
==Definição Geral
A função 'jogoptexto2' pega nos elementos de uma linha e transforma-os nos seus correspondentes em texto.

@case h of
      Vazio -> ' '
      Caixa -> 'C'
      Bloco -> 'X'
      Porta -> 'P'
@

==Exemplo da utilização da função 'jogoptexto2':
jogoptexto2 [Bloco,Vazio,Porta,Caixa]
>>> "X PC"
-}

jogoptexto2 :: [Peca] -> String
jogoptexto2  = map
                    (\h
                       -> case h of
                             Vazio -> ' '
                             Caixa -> 'C'
                             Bloco -> 'X'
                             Porta -> 'P')

{-|
==Definição Geral
A função 'insJogador' insere o jogador na lista de Strings formada em 'jogoptexto'.

Este vai ser representado por '<' se estiver virado pra Oeste ou '>' se estiver virado pra Este.

A função vai dividir a lista de strings pelo valor de y, dando-nos assim a lista atual, as anteriores e as seguintes.

Depois disto a string que obtivemos vai ser dividida pelo valor x, dando-nos o espaço que pretendemos, os anteriores e os seguintes.

Se o jogador puder ocupar esse espaço, ele vai ser substituído pela sua representação e as Strings são todas juntas numa só.

Se o jogador tiver a segurar uma caixa, vai acontecer tudo da mesma forma, mas vai aparecer uma representação em cima do jogador para que se saiba que ele carrega uma caixa. Esta vai ser a letra 'C'.

-}

insJogador :: Jogador -> [String] -> String
insJogador _ [] = error "Posição não válida!"
insJogador (Jogador (x,y) direc True) l
                                         | pos == ' ' = juntal l1 ++ (c1 ++ "C" ++ c2) ++ ((a ++ [case direc of Este -> '>'
                                                                                                                Oeste -> '<']) ++ b) ++ juntal l2
                                         | pos == 'P' = juntal l1 ++ (c1 ++ "C" ++ c2) ++ ((a ++ [case direc of Este -> '>'
                                                                                                                Oeste -> '<']) ++ b) ++ juntal l2
                                         | otherwise = error "Posição ocupada!"
                                         where (l1,lnh1:lnh2:l2) = (take (y-1) l, drop (y-1) l)
                                               (c1,caixa:c2) = (take x lnh1, drop x lnh1)
                                               (a,pos:b) = (take x lnh2, drop x lnh2)
insJogador (Jogador (x,y) direc False) l
                                         | pos == ' ' = juntal l1 ++ ((a ++ [case direc of Este -> '>'
                                                                                           Oeste -> '<']) ++ b) ++ juntal l2
                                         | pos == 'P' = juntal l1 ++ ((a ++ [case direc of Este -> '>'
                                                                                           Oeste -> '<']) ++ b) ++ juntal l2
                                         | otherwise = error "Posição ocupada!"
                                         where (l1,lnh1:l2) = (take y l, drop y l)
                                               (a,pos:b) = (take x lnh1, drop x lnh1)

{-|
==Definição Geral
Função auxiliar que vai juntar todas as Strings numa só
-}

juntal :: [[a]] -> [a]
juntal = foldr (++) []
