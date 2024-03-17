module Tarefa2_2021li1g043_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g043
import Fixtures

-- Tarefa 2
testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa m1" ~: m1r ~=? constroiMapa m1
    , "Tarefa 2 - Teste Construir Mapa m2" ~: m2r ~=? constroiMapa m2
    , "Tarefa 2 - Teste Construir Mapa m3" ~: m3r ~=? constroiMapa m3
    , "Tarefa 2 - Teste Construir Mapa vazio" ~: [] ~=? constroiMapa [] 
    , "Tarefa 2 - Teste Construir Mapa" ~: [[Bloco,Porta,Vazio],[Vazio,Vazio,Caixa]] ~=? constroiMapa [(Bloco,(0,0)),(Caixa,(2,1)),(Porta,(1,0))]
    , "Tarefa 2 - Teste Construir Mapa" ~: [[Vazio,Vazio,Vazio],[Porta,Vazio,Caixa],[Bloco,Bloco,Bloco]] ~=? constroiMapa [(Bloco,(0,2)),(Porta,(0,1)),(Bloco,(1,2)),(Bloco,(2,2)),(Caixa,(2,1))]
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m1 ~=?  sort (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Desconstruir Mapa m2" ~: sort m2 ~=?  sort (desconstroiMapa m2r)
    , "Tarefa 2 - Teste Desconstruir Mapa m3" ~: sort m3 ~=?  sort (desconstroiMapa m3r)
    , "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa []
    , "Tarefa 2 - Teste Identidade m1" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    , "Tarefa 2 - Teste Identidade m2" ~: sort m2 ~=?  sort (desconstroiMapa (constroiMapa m2))
    , "Tarefa 2 - Teste Identidade m3" ~: sort m3 ~=?  sort (desconstroiMapa (constroiMapa m3))
    , "Tarefa 2 - Teste Identidade m1r" ~: m1r ~=?  constroiMapa (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Identidade m2r" ~: m2r ~=?  constroiMapa (desconstroiMapa m2r)
    , "Tarefa 2 - Teste Identidade m3r" ~: m3r ~=?  constroiMapa (desconstroiMapa m3r)
    , "Tarefa 2 - Teste Construir Sobrepor Peças" ~: constroiMapa [(Porta, (7, 4))] ~=?  constroiMapa [(Porta, (7, 4)), (Porta, (7, 4))]
    , "Tarefa 2 - Teste Construir Sobrepor Peças" ~: constroiMapa [(Porta, (1, 0)),(Bloco,(2,1)),(Caixa,(0,0))] ~=?  constroiMapa [(Porta, (1, 0)), (Bloco, (2, 1)),(Bloco,(2,1)),(Caixa,(0,0)),(Caixa,(0,0))]
    ]