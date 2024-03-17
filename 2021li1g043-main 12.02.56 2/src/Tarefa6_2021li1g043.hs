{- |
Module      : Tarefa6_2021li1g043
Description : Aplicação gráfica completa
Copyright   : João Magalhães <a100740@alunos.uminho.pt>;
            : Gonçalo Loureiro <a100535@alunos.uminho.pt>;
Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}

module Tarefa6_2021li1g043 where

import           LI12122
import           Tarefa2_2021li1g043
import           Tarefa4_2021li1g043

data TTree x = Empty | Node x (TTree x) (TTree x) (TTree x) (TTree x)
      deriving (Eq)

type CoordenadasPorta = (Int, Int)

{-|
== Definição Geral
Aplicamos a função 'resolveJogo' para, como o seu nome diz, resover um jogo dado.

A função vai receber um número de movimentos e um jogo, ou seja, um mapa e um jogador. Vai pegar nestas informações e vai-nos dar a resolução com o mínimo de movimentos possíveis.

Nesta função vamos usar algumas funções auxiliares, estas são 'fmap', 'simp', 'consSeq', 'coordPorta' e 'desconstroiMapa'.

@resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo 0 _ = Just []
resolveJogo m (Jogo mapa jogador) = fmap simp (consSeq (m+1) (coordPorta (desconstroiMapa mapa)) [Node (Jogo mapa jogador,[]) Empty Empty Empty Empty] [] [])
@

==Exemplos da utilização da função 'resolveJogo':
resolveJogo 60 (Jogo m1r (Jogador (5,4) Este False))
>>> Just [AndarEsquerda,AndarDireita,InterageCaixa,AndarDireita,AndarEsquerda,InterageCaixa,Trepar,InterageCaixa,AndarEsquerda,AndarDireita,InterageCaixa,AndarEsquerda,InterageCaixa,AndarDireita,InterageCaixa,Trepar,Trepar,InterageCaixa,AndarDireita,AndarEsquerda,InterageCaixa,AndarEsquerda,Trepar,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,InterageCaixa,AndarDireita,Trepar,Trepar]

resolveJogo 20 (Jogo m1r (Jogador (5,4) Este False))
>>> Nothing
-}

resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo 0 _ = Just []
resolveJogo m (Jogo mapa jogador) = fmap simp (consSeq (m+1) (coordPorta (desconstroiMapa mapa)) [Node (Jogo mapa jogador,[]) Empty Empty Empty Empty] [] [])

{-|
== Definição Geral
Aplicamos a função 'simp' para inverter o resultado obtido na função 'simpAux'.

A funçao vai receber um número de movimentos e a função 'simpAux' e de seguida vai aplicar a função 'reverse' a isso.

@simp :: [Int] -> [Movimento]
simp m = reverse (simpAux m)
@
-}

simp :: [Int] -> [Movimento]
simp m = reverse (simpAux m)

{-|
== Definição geral
Utilizamos a função 'simpAux' para receber o nome de cada movimento que pode ser utilizado no jogo.

@simpAux :: [Int] -> [Movimento]
simpAux [] = []
simpAux (h:t) = go h
      where go 1 = Trepar: simpAux t
            go 2 = AndarDireita: simpAux t
            go 3 = AndarEsquerda: simpAux t
            go 4 = InterageCaixa: simpAux t
@
-}

simpAux :: [Int] -> [Movimento]
simpAux [] = []
simpAux (h:t) = go h
      where go 1 = Trepar: simpAux t
            go 2 = AndarDireita: simpAux t
            go 3 = AndarEsquerda: simpAux t
            go 4 = InterageCaixa: simpAux t

{-|
== Definição Geral
A função 'consSeq' vai receber um número de movimentos, as coordenadas da porta (onde o jogo finaliza), a primeira árvore que contém as coordenadas onde o jogador vai começar o jogo e uma lista de árvores que vai aumentando à medida em que é necessário.

Para o funcionamento desta função foi utilizada uma função auxiliar que é a 'lTree2'.

@consSeq :: Int -> CoordenadasPorta -> [TTree (Jogo, [Int])] -> [TTree (Jogo, [Int])] -> [Jogo] -> Maybe [Int]
consSeq 0 _ tree _ _ = Nothing
consSeq lTree (x,y) [] lAc pJogos = consSeq (lTree-1) (x,y) (lTree2 lAc pJogos) [] pJogos
consSeq lTree (x,y) (c@(Node (jogoR@(Jogo _ (Jogador cords _ _)), mov) _ _ _ _):t) lAc pJogos = if cords == (x,y) then Just mov else if cords /= (x,y) then consSeq lTree (x,y) t (c:lAc) (sJogo jogoR pJogos) else Nothing
@
-}

consSeq :: Int -> CoordenadasPorta -> [TTree (Jogo, [Int])] -> [TTree (Jogo, [Int])] -> [Jogo] -> Maybe [Int]
consSeq 0 _ tree _ _ = Nothing
consSeq lTree (x,y) [] lAc pJogos = consSeq (lTree-1) (x,y) (lTree2 lAc pJogos) [] pJogos
consSeq lTree (x,y) (c@(Node (jogoR@(Jogo _ (Jogador cords _ _)), mov) _ _ _ _):t) lAc pJogos = if cords == (x,y) then Just mov else if cords /= (x,y) then consSeq lTree (x,y) t (c:lAc) (sJogo jogoR pJogos) else Nothing

{-|
== Definição Geral
A função 'lTree2' cria uma lista de árvores, com nodos como sua raíz, nesses nodos estão jogos que resultam dos movimento aplicados ao nodo anterior.

Para a realização da função 'lTree2' foi utilizada uma função auxiliar que é a função 'filter'.

@lTree2 :: [TTree (Jogo,[Int])] -> [Jogo] -> [TTree (Jogo,[Int])]
lTree2 [] _ = []
lTree2 (h:t) pJogos = filter (/=Empty) (a:b:c:d:lTree2 t pJogos)
            where (Node _ a b c d) = lTree3 h pJogos
@
-}

lTree2 :: [TTree (Jogo,[Int])] -> [Jogo] -> [TTree (Jogo,[Int])]
lTree2 [] _ = []
lTree2 (h:t) pJogos = rest (/=Empty) (a:b:c:d:lTree2 t pJogos)
            where (Node _ a b c d) = lTree3 h pJogos

{-|
== Definição Geral
A função 'rest' vai restringir os elementos que verificam uma determinada condição.

@rest :: (a -> Bool) -> [a] -> [a]
rest p [] = []
rest p (h:t)
    | p h = h : rest p t
    | otherwise = rest p t
@
-}

rest :: (a -> Bool) -> [a] -> [a]
rest p [] = []
rest p (h:t)
    | p h = h : rest p t
    | otherwise = rest p t

{-|
== Definição Geral
A função 'lTree3' vai adicionar mais um de profundidade à árvore.

Para a realização da função 'lTree3' foi utilizada a função auxiliar 'moveJogador'.

@lTree3 :: TTree (Jogo,[Int]) -> [Jogo] -> TTree (Jogo,[Int])
lTree3 Empty _ = Empty
lTree3 (Node (jogoR, mov) Empty Empty Empty Empty) pJogos = Node (jogoR, mov) a b c d
            where a | moveJogador jogoR Trepar `elem` pJogos = Empty
                    | otherwise = Node (moveJogador jogoR Trepar, 1:mov) Empty Empty Empty Empty
                  b | moveJogador jogoR AndarDireita `elem` pJogos = Empty
                    | otherwise = Node (moveJogador jogoR AndarDireita, 2:mov) Empty Empty Empty Empty
                  c | moveJogador jogoR AndarEsquerda `elem` pJogos = Empty
                    | otherwise = Node (moveJogador jogoR AndarEsquerda, 3:mov) Empty Empty Empty Empty
                  d | moveJogador jogoR InterageCaixa `elem` pJogos = Empty
                    | otherwise = Node (moveJogador jogoR InterageCaixa, 4:mov) Empty Empty Empty Empty
@
-}

lTree3 :: TTree (Jogo,[Int]) -> [Jogo] -> TTree (Jogo,[Int])
lTree3 Empty _ = Empty
lTree3 (Node (jogoR, mov) Empty Empty Empty Empty) pJogos = Node (jogoR, mov) a b c d
            where a | moveJogador jogoR Trepar `elem` pJogos = Empty
                    | otherwise = Node (moveJogador jogoR Trepar, 1:mov) Empty Empty Empty Empty
                  b | moveJogador jogoR AndarDireita `elem` pJogos = Empty
                    | otherwise = Node (moveJogador jogoR AndarDireita, 2:mov) Empty Empty Empty Empty
                  c | moveJogador jogoR AndarEsquerda `elem` pJogos = Empty
                    | otherwise = Node (moveJogador jogoR AndarEsquerda, 3:mov) Empty Empty Empty Empty
                  d | moveJogador jogoR InterageCaixa `elem` pJogos = Empty
                    | otherwise = Node (moveJogador jogoR InterageCaixa, 4:mov) Empty Empty Empty Empty

{-|
== Definição Geral
Esta função verifica se um jogo já existe numa lista de jogos, se não existir é adicionado, se já existir não é adicionado.

@sJogo :: Jogo -> [Jogo] -> [Jogo]
sJogo s [] = [s]
sJogo s l
      | s `elem` l = l
      | otherwise = s:l
@
-}

sJogo :: Jogo -> [Jogo] -> [Jogo]
sJogo s [] = [s]
sJogo s l
      | s `elem` l = l
      | otherwise = s:l
{-|
== Definição Geral
Esta função encontra as coordenadas da porta, estas coordenadas vão ser utilizadas na função 'resolveJogo', e vão determinar onde é que a função tem que parar, que é na porta.

@coordPorta :: [(Peca,Coordenadas)] -> Coordenadas
coordPorta ((p,(x,y)):t)
                        | p /= Porta = coordPorta t
                        | otherwise = (x,y)
@
-}
coordPorta :: [(Peca,Coordenadas)] -> Coordenadas
coordPorta ((p,(x,y)):t)
                        | p /= Porta = coordPorta t
                        | otherwise = (x,y)

-- | Mapas disponíveis para o jogo

m1r :: Mapa --(5,4) Este False
m1r =
  [        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
         , [Caixa, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
         , [Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Caixa, Vazio, Vazio, Porta]
         , [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Bloco]
         , [Bloco, Caixa, Caixa, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco]
         , [Bloco, Bloco, Bloco, Vazio, Vazio, Caixa, Bloco, Vazio, Bloco, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m2r :: Mapa --(8,3) Este True
m2r =
  [        [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco]
         , [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
         , [Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Caixa, Bloco]
         , [Bloco, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]
m3r :: Mapa --(4,4) Este False
m3r =
  [        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
         , [Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco]
         , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
         , [Bloco, Porta, Vazio, Bloco, Vazio, Vazio, Caixa, Bloco]
         , [Bloco, Bloco, Vazio, Bloco, Vazio, Bloco, Bloco, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
 ]

m4r :: Mapa --(1,4) Oeste False
m4r =
  [        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
         , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
         , [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa]
         , [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco]
         , [Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]
