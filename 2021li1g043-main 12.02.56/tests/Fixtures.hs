module Fixtures where
  
import LI12122

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]
m2 :: [(Peca,Coordenadas)]
m2 = 
  [ (Porta,(0,0)),
    (Bloco,(0,1)), 
    (Caixa,(1,1)),                
    (Bloco,(3,1)),
    (Bloco,(0,2)), 
    (Bloco,(1,2)),  
    (Bloco,(2,2))
  ]

m3 :: [(Peca,Coordenadas)]
m3 = 
  [ (Porta,(0,0)),
    (Bloco,(0,1)),
    (Bloco,(0,2)),
    (Bloco,(1,2)),
    (Bloco,(1,3)),
    (Bloco,(2,3)),
    (Bloco,(3,3)),
    (Bloco,(3,2)),
    (Bloco,(4,2)),
    (Bloco,(4,1)),
    (Bloco,(2,0))
  ]

m4 :: [(Peca,Coordenadas)]
m4 = 
  [ (Porta,(0,0)),
    (Bloco,(0,1)),
    (Bloco,(0,2)),
    (Bloco,(1,2)),
    (Bloco,(1,3)),
    (Bloco,(2,3)),
    (Bloco,(3,3)),
    (Bloco,(4,1)),
    (Bloco,(2,0))
  ]

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m2r :: Mapa
m2r = 
  [ [Porta, Vazio, Vazio, Vazio, Vazio],
    [Bloco, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Vazio]
  ]

m3r :: Mapa
m3r =
  [ [Porta, Vazio, Bloco, Vazio, Vazio],
    [Bloco, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Vazio, Bloco, Bloco],
    [Vazio, Bloco, Bloco, Bloco, Vazio]
  ]

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m2r (Jogador (3, 2) Oeste False)

m1e3 :: Jogo
m1e3 = Jogo m3r (Jogador (2,2) Oeste False)