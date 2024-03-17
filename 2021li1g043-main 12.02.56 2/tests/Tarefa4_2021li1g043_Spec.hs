module Tarefa4_2021li1g043_Spec where

import Test.HUnit
import LI12122
import Tarefa3_2021li1g043
import Tarefa4_2021li1g043
import Fixtures

testsT4 =
  test
    [ "Tarefa 4 - Teste Move m1e1 Oeste" ~: Jogo m1r (Jogador (5, 3) Oeste False) ~=?  moveJogador m1e1 AndarEsquerda
    , "Tarefa 4 - Teste Move m1e1 Este" ~: Jogo m1r (Jogador (6, 0) Este False) ~=?  moveJogador m1e1 AndarDireita
    , "Tarefa 4 - Teste Move m1e1 Trepar" ~: m1e1 ~=? moveJogador m1e1 Trepar
    , "Tarefa 4 - Teste Move m1e1 InterageCaixa" ~: m1e1 ~=?  moveJogador m1e1 InterageCaixa
    , "Tarefa 4 - Teste Move m1e2 Oeste" ~: Jogo m2r (Jogador (2, 2) Oeste False) ~=?  moveJogador m1e2 AndarEsquerda
    , "Tarefa 4 - Teste Move m1e2 Este" ~: Jogo m2r (Jogador (3, 2) Este False) ~=?  moveJogador m1e2 AndarDireita
    , "Tarefa 4 - Teste Move m1e2 Trepar" ~: m1e2 ~=? moveJogador m1e2 Trepar
    , "Tarefa 4 - Teste Move m1e2 InterageCaixa" ~: m1e2 ~=?  moveJogador m1e2 InterageCaixa
    , "Tarefa 4 - Teste Move m1e3 Oeste" ~: Jogo m3r (Jogador (2, 2) Oeste False) ~=?  moveJogador m1e3 AndarEsquerda
    , "Tarefa 4 - Teste Move m1e3 Este" ~: Jogo m3r (Jogador (2, 2) Este False) ~=?  moveJogador m1e3 AndarDireita
    , "Tarefa 4 - Teste Move m1e3 Trepar" ~: Jogo m3r (Jogador (1, 1) Oeste False) ~=? moveJogador m1e3 Trepar
    , "Tarefa 4 - Teste Move m1e3 InterageCaixa" ~: m1e3 ~=?  moveJogador m1e3 InterageCaixa
    , "Tarefa 4 - Teste movimentos m1e2 Caixa1" ~: Jogo
        [  [Porta, Vazio, Vazio, Vazio, Vazio]
         , [Bloco, Vazio, Vazio, Vazio, Bloco]
         , [Bloco, Bloco, Vazio, Vazio, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Vazio]
        ]
        (Jogador (3, 2) Este False) ~=?  correrMovimentos m1e2 [AndarDireita, Trepar]
    , "Tarefa 4 - Teste movimentos m1e2 Caixa2" ~:
      Jogo
         [ [Porta, Vazio, Vazio, Vazio, Vazio]
         , [Bloco, Vazio, Vazio, Vazio, Bloco]
         , [Bloco, Bloco, Vazio, Vazio, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Vazio]
        ]
        (Jogador (1, 1) Oeste False) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa, AndarEsquerda, Trepar]
    , "Tarefa 4 - Teste movimentos mie3 Caixa3" ~:
      Jogo  
         [  [Porta, Vazio, Bloco, Vazio, Vazio]
          , [Bloco, Vazio, Vazio, Vazio, Bloco]
          , [Bloco, Bloco, Vazio, Bloco, Bloco]
          , [Vazio, Bloco, Bloco, Bloco, Vazio]
         ]
         (Jogador (4, 0) Este False) ~=?  correrMovimentos m1e3 [AndarDireita, Trepar, Trepar]
    ]