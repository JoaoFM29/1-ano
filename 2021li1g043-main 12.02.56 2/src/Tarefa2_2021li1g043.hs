{- |
Module      : Tarefa2_2021li1g043
Description : Construção/Desconstrução do mapa
Copyright   : João Magalhães <a100740@alunos.uminho.pt>;
            : Gonçalo Loureiro <a100535@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g043 where

import LI12122

import Tarefa1_2021li1g043 

import Data.List (length) 

{-| 

== Definição geral
Constroi o Mapa.

== Definição mais específica
Recebe uma lista organizada, devido à função 'constroiMapaPVO', e separa a mesma em linhas, ou seja, por listas, baseando-se apenas no 'y' das coordenadas uma vez que esta ja se encontra organizada.

== Caso de paragem
A função tem como caso de paragem a lista vazia, [].

@
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = separaPorLinhas (constroiMapaPVO l) yy
                   where yy = yMax (map snd l) 
@
    
== Exemplo de utilização da função 'constroiMapa':
>>> constroiMapa constroiMapa [(Bloco,(0,2)),(Caixa,(0,1)),(Bloco,(2,2)),(Porta,(2,1)),(Bloco,(1,2))]
[[Vazio,Vazio,Vazio],[Caixa,Vazio,Porta],[Bloco,Bloco,Bloco]]

1. Seleciona as Pecas de numa lista tendo em conta o seu 'y'
       
== Caso de paragem
A função devolverá uma lista vazia, [], quando a lista dada for vazia, [].

@
separaCoors :: [(Peca,Coordenadas)] -> Int -> [Peca]
separaCoors [] yy = []
separaCoors ((c,(x,y)):t) yy 
                        | y == yy = c : separaCoors t yy
                        | otherwise = separaCoors t yy
@

== Exemplo de utilização da função 'separaCoors':
>>> separaCoors [(Bloco,(2,2)), (Caixa,(1,2)), (Bloco,(1,0))] 2
[Bloco,Caixa]

2. Separar as Pecas em listas até ao valor de 'y' dado
 
== Caso de paragem
Esta função, devido ao facto de ser muito similar à de cima, apresenta o mesmo caso de paragem.

@
separaPorLinhas :: [(Peca,Coordenadas)] -> Int -> [[Peca]]
separaPorLinhas [] yy = []
separaPorLinhas ((c,(x,y)):t) yy
                        | yy >= 0 = separaPorLinhas ((c,(x,y)):t) (yy-1) ++ [separaCoors ((c,(x,y)):t) yy]
                        | otherwise = []
@
        
== Exemplo de utilização da função 'separaPorLinhas':
>> separaPorLinhas [(Bloco,(2,2)), (Caixa,(1,2)), (Bloco,(1,0))] 2
[[Bloco],[],[Bloco,Caixa]]
-}

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = separaPorLinhas (constroiMapaPVO l) yy
                   where yy = yMax (map snd l)

separaCoors :: [(Peca,Coordenadas)] -> Int -> [Peca]
separaCoors [] yy = []
separaCoors ((c,(x,y)):t) yy 
                        | y == yy = c : separaCoors t yy
                        | otherwise = separaCoors t yy

separaPorLinhas :: [(Peca,Coordenadas)] -> Int -> [[Peca]]
separaPorLinhas [] yy = []
separaPorLinhas ((c,(x,y)):t) yy
                        | yy >= 0 = separaPorLinhas ((c,(x,y)):t) (yy-1) ++ [separaCoors ((c,(x,y)):t) yy]
                        | otherwise = []


{-|
== Definição geral
Desconstroi o Mapa. 

== Definição mais específica
Utilizando um método idêntico à 'constroiMapa', esta função recebe um Mapa e devolve este na forma de (Peca,Coordenadas) em uma só lista organizada.
         
== Caso de paragem
A função tem como caso de paragem a lista vazia, [].

@
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)] 
desconstroiMapa [] = []
desconstroiMapa l = colocaCoorLinha l tamanho 
               where tamanho = length l 
@

== Exemplo de utilização da função 'desconstroiMapa':
>> desconstroiMapa [[Vazio,Vazio,Vazio],[Caixa,Vazio,Porta],[Bloco,Bloco,Bloco]]
[(Caixa,(0,1)), (Porta,(2,1)), (Bloco,(0,2)), (Bloco,(1,2)), (Bloco,(2,2))]

1. Recursivamente aplica a função colocaCoors

== Caso de paragem
Esta função tem como caso de paragem a lista vazia, [].

@
colocaCoorLinha :: Mapa -> Int -> [(Peca,Coordenadas)]
colocaCoorLinha [] y = []
colocaCoorLinha l y = colocaCoorLinha (init l) (y-1) ++ colocaCoors (last l) (x-1) y -- ^ Da mesma forma que acontece na função de cima nesta, utiliza-se o (y-1) uma vez que, por exemplo, uma lista composta por 3 colunas ([[1,2],[2,3],[2,0]]) a ordenada máxima é 2
                    where x = length (last l) 
@

== Exemplo de utilização da função 'colocaCoorLinha':
>>> colocaCoorLinha [[Bloco,Porta,Vazio],[Bloco,Bloco,Bloco]] 2
[(Bloco,(0,0)),(Porta,(1,0)),(Bloco,(0,1)),(Bloco,(1,1)),(Bloco,(2,1))]

2. Dada uma lista de Pecas devolve as coordenadas dependendo da coluna (valor de 'x') e do comprimento da lista onde está inserido.

== Caso de paragem
Esta função tem como caso de paragem a lista vazia, [].

@
colocaCoors :: [Peca] -> Int -> Int -> [(Peca,Coordenadas)]
colocaCoors [] x y = []
colocaCoors l x y 
                 | c /= Vazio = colocaCoors (init l) (x-1) y ++ [(c,(x,(y-1)))] -- ^ Utiliza-se o (x-1) porque uma linha comeca com as coordenadas (0,0), ou seja, uma lista que tenha 4 elementos o último elemento terá apenas abcissa 3.
                 | otherwise = colocaCoors (init l) (x-1) y -- ^ Omite as Pecas que são vazias.
             where c = last l
@

== Exemplo de utilização da função 'colocaCoors':
Esta função, neste caso, sozinha não tem muita utilidade por isso, verifiquemos a sua utilização no exemplo da função em cima referida.
-}

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)] 
desconstroiMapa [] = []
desconstroiMapa l = colocaCoorLinha l tamanho 
               where tamanho = length l 

colocaCoors :: [Peca] -> Int -> Int -> [(Peca,Coordenadas)]
colocaCoors [] x y = []
colocaCoors l x y 
                 | c /= Vazio = colocaCoors (init l) (x-1) y ++ [(c,(x,(y-1)))]
                 | otherwise = colocaCoors (init l) (x-1) y 
             where c = last l

colocaCoorLinha :: Mapa -> Int -> [(Peca,Coordenadas)]
colocaCoorLinha [] y = []
colocaCoorLinha l y = colocaCoorLinha (init l) (y-1) ++ colocaCoors (last l) (x-1) y 
                    where x = length (last l) 