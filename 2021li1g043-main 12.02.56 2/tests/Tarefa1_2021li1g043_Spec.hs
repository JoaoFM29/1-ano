module Tarefa1_2021li1g043_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g043
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida função com coordenadas iguais" ~: coordiguais [(0,0),(0,2),(0,0),(0,3)]  ~=? False
    , "Tarefa 1 - Teste Valida função com coordenadas iguais" ~: coordiguais [(0,0),(0,2),(0,3)] ~=? True
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida função com 1 portas" ~: umaPorta [Bloco, Porta, Caixa] ~=?  True
    , "Tarefa 1 - Teste Valida função com caixas flutuantes" ~: caixaFlutuante [(Caixa,(0,0)),(Bloco,(0,1)),(Caixa,(0,2))][(Caixa,(0,0)),(Bloco,(0,1)),(Caixa,(0,2))] ~=? False
    , "Tarefa 1 - Teste Valida função com caixas flutuantes" ~: caixaFlutuante [(Caixa,(0,0)),(Bloco,(0,1)),(Caixa,(0,2)),(Bloco,(0,3))][(Caixa,(0,0)),(Bloco,(0,1)),(Caixa,(0,2)),(Bloco,(0,3))] ~=? True
    , "Tarefa 1 - Teste Valida função com espaços vazios" ~: verificaVazio [(Bloco,(2,2)),(Bloco,(1,2)),(Caixa,(1,0)),(Porta,(0,1))] ~=? True
    , "Tarefa 1 - Teste Valida função com espaços vazios" ~: verificaVazio [(Bloco,(1,1)),(Bloco,(1,0)),(Caixa,(0,1)),(Porta,(0,0))] ~=? False
    , "Tarefa 1 - Teste Valida função com chão" ~: chaoMapa [(Porta,(0,1)),(Bloco,(0,2)),(Bloco,(1,1)),(Bloco,(2,0)),(Caixa,(1,0))] ~=? True
    , "Tarefa 1 - Teste Valida função com chão" ~: chaoMapa [(Porta,(0,0)),(Bloco,(0,1)),(Bloco,(1,1)),(Bloco,(2,1)),(Caixa,(2,0))] ~=? True
    , "Tarefa 1 - Teste Valida função com chão" ~: chaoMapa [(Porta,(2,1)),(Bloco,(0,0)),(Bloco,(1,1)),(Bloco,(2,2)),(Caixa,(1,0))] ~=? True
    , "Tarefa 1 - Teste Valida função com chão" ~: chaoMapa [(Porta,(2,1)),(Bloco,(0,0)),(Bloco,(2,2)),(Caixa,(1,0))] ~=? False
    , "Tarefa 1 - Teste Valida função com chão" ~: chaoMapa [(Bloco,(0,0)),(Bloco,(1,1)),(Caixa,(1,0)),(Bloco,(0,2)),(Bloco,(2,1))] ~=? True
    , "Tarefa 1 - Teste Valida função com chão" ~: chaoMapa [(Bloco,(1,2)),(Bloco,(1,1)),(Caixa,(1,0)),(Bloco,(2,2)),(Bloco,(2,1))] ~=? False
    , "Tarefa 1 - Teste Valida função com chão" ~: chaoMapa [(Bloco,(0,1)),(Bloco,(1,1)),(Caixa,(1,0)),(Bloco,(2,1)),(Bloco,(0,2))] ~=? True
    , "Tarefa 1 - Teste Valida Mapa" ~: validaPotencialMapa m2 ~=? True
    , "Tarefa 1 - Teste Valida Mapa" ~: validaPotencialMapa m3 ~=? True
    , "Tarefa 1 - Teste Valida Mapa" ~: validaPotencialMapa m4 ~=? False    
    ]
