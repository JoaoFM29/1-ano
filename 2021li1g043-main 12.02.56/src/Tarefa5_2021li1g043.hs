{- |
Module      : Tarefa5_2021li1g043
Description : Aplicação gráfica completa
Copyright   : João Magalhães <a100740@alunos.uminho.pt>;
            : Gonçalo Loureiro <a100535@alunos.uminho.pt>;
Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

module Main where 

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import Graphics.Gloss.Juicy (loadJuicy)
import Tarefa1_2021li1g043
import Tarefa2_2021li1g043
import Tarefa3_2021li1g043
import Tarefa4_2021li1g043
import LI12122 

-- | Coordenadas dos objetos que ligam o utilizador ao jogo que, neste caso, é a seta do menu principal e o quadrado no menu dos mapas.

type Estado = (Float, Float)

-- | Estado de um jogo.

type EstadoGloss = (Jogo, Estado, (Menu, [Picture]))

-- | Os vários menus que o jogo possui. 

data Menu = MainMenu | Jogar | Continuar | Mapas | Instrucoes | Sair deriving (Show, Eq)

{-| 
== Definição geral 
Estado do jogo. 

@
estadoInicial :: Estado 
estadoInicial = (0,0) 
@
-}

estadoInicial :: Estado 
estadoInicial = (0,0) 

{-|
== Definição geral
Estado atual do jogo.

@
estadoGlossInicial :: [Picture] -> EstadoGloss
estadoGlossInicial pics@[i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4] = (jogo1, estadoInicial, (MainMenu, pics))
@
-}

estadoGlossInicial :: [Picture] -> EstadoGloss
estadoGlossInicial pics@[i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4] = (jogo1, estadoInicial, (MainMenu, pics))

{-|
== Definição geral
Mofifica o estado do jogo tendo em conta a tecla permida.

@
reageEventoGloss :: Event -> EstadoGloss -> IO EstadoGloss 

=== Modifica o estado quando este se encontra o menu principal.
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    (j, (x,y), (MainMenu, pics))
                                                | y == 0    = return (j, (x,(y-240)), (MainMenu, pics))
                                                | otherwise = return (j, (x,(y+80)) , (MainMenu, pics))
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  (j, (x,y), (MainMenu, pics)) 
                                                | y == (-240) = return (j, (x,0),    (MainMenu, pics)) 
                                                | otherwise   = return (j, (x,y-80), (MainMenu, pics))     
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (j, (x,y), (MainMenu, pics))
                                                | y == 0      = return (j, (x,y),    (Jogar, pics))
                                                | y == (-80)  = return (j, (-40,80), (Mapas, pics))
                                                | y == (-160) = return (j, (x,y),    (Instrucoes,  pics))
                                                | y == (-240) = return (j, (x,y),    (Sair, pics))

=== Modifica o estado quando este se encontra no menu dos mapas.
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)    (j, (x,y), (Mapas, pics)) 
                                                | y == 80   = return (j, (x,(-20)), (Mapas, pics))
                                                | otherwise = return (j, (x,80),     (Mapas, pics))
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    (j, (x,y), (Mapas, pics)) 
                                                | y == 80   = return (j, (x,(-20)), (Mapas, pics))
                                                | otherwise = return (j, (x,80),     (Mapas, pics))
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)    (j, (x,y), (Mapas, pics))
                                                | x == (-40)  = return (j, ((55),y), (Mapas, pics))
                                                | otherwise = return (j, (-40,y),   (Mapas, pics))
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _)    (j, (x,y), (Mapas, pics))
                                                | x == (-40)  = return (j, ((55),y), (Mapas, pics))
                                                | otherwise = return (j, (-40,y),   (Mapas, pics))
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (j, (x,y), (Mapas, pics))
                                                | x == (-40) && y == 80    = return (jogo1, (0,0), (MainMenu, pics))
                                                | x == (-40) && y == (-20) = return (jogo2, (0,0), (MainMenu, pics))
                                                | x == 55 && y == 80       = return (jogo3, (0,0), (MainMenu, pics))
                                                | x == 55 && y == (-20)    = return (jogo4, (0,0), (MainMenu, pics))

=== Modifica o estado quando este se encontra dentro de um dos mapas do jogo.
reageEventoGloss (EventKey (SpecialKey KeyDown)  Down _ _) (jogo@(Jogo mapa (Jogador (xs,ys) dir bool)), (x,y), (Jogar, pics)) = return ((moveJogador jogo InterageCaixa), (x, y), (Jogar, pics))                                                                     
reageEventoGloss (EventKey (SpecialKey KeyUp)    Down _ _) (jogo@(Jogo mapa (Jogador (xs,ys) dir bool)), (x,y), (Jogar, pics)) 
                                                                                        | (xs+1,ys-1) == p && podeAndar jogo Trepar = return (jogo1, estadoInicial, (MainMenu, pics)) --^ É necessário que o jogador consiga alcançar a Porta, por isso, é imposta como condição necessária a função 'podeAndar'.
                                                                                        | (xs-1,ys-1) == p && podeAndar jogo Trepar = return (jogo1, estadoInicial, (MainMenu, pics)) --^ É necessário que o jogador consiga alcançar a Porta, por isso, é imposta como condição necessária a função 'podeAndar'.
                                                                                        | otherwise        = return ((moveJogador jogo Trepar), (x, y), (Jogar, pics))
                                                                                        where p = coordPorta (desconstroiMapa mapa) --^ Determina as coordenadas respetivas da peca, Porta, no mapa.
reageEventoGloss (EventKey (SpecialKey KeyLeft)  Down _ _) (jogo@(Jogo mapa (Jogador (xs,ys) dir bool)), (x,y), (Jogar, pics))
                                                                                        | (xs-1,yqO) == p  && podeAndar jogo AndarEsquerda = return (jogo1, estadoInicial, (MainMenu, pics)) --^ É necessário que o jogador consiga alcançar a Porta, por isso, é imposta como condição necessária a função 'podeAndar'.
                                                                                        | otherwise       = return ((moveJogador jogo AndarEsquerda), (x, y), (Jogar, pics))
                                                                                        where p = coordPorta (desconstroiMapa mapa) --^ Determina as coordenadas respetivas da peca, Porta, no mapa.
                                                                                              yqO = yDoChao (xs-1,ys) (desconstroiMapa mapa) --^ Determina o 'y' respetivo do chão do mapa.
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (jogo@(Jogo mapa (Jogador (xs,ys) dir bool)), (x,y), (Jogar, pics))
                                                                                        | (xs+1,yqE) == p && podeAndar jogo AndarDireita = return (jogo1, estadoInicial, (MainMenu, pics)) --^ É necessário que o jogador consiga alcançar a Porta, por isso, é imposta como condição necessária a função 'podeAndar'.
                                                                                        | otherwise       = return ((moveJogador jogo AndarDireita) , (x, y), (Jogar, pics))
                                                                                        where p = coordPorta (desconstroiMapa mapa) --^ Determina as coordenadas respetivas da peca, Porta, no mapa.
                                                                                              yqE = yDoChao (xs+1,ys) (desconstroiMapa mapa) --^ Determina o 'y' respetivo do chão do mapa.

reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _) jogo@(j, (x,y), (Jogar, pics)) =
                              do writeFile "continuar.txt" (show j)            --^ Grava coordenadas em ficheiro ao pressionar tecla "Space".
                                 return (j, (x,y), (MainMenu, pics))           --^ Volta ao menu principal com o jogo guardado.

reageEventoGloss (EventKey (SpecialKey KeyF12) Down _ _) jogo@(j, (x,y), (s, pics)) --^ Retorna ao menu principal.
                                                | s == MainMenu = return jogo
                                                | otherwise     = return (jogo1, (0,0), (MainMenu, pics))
reageEventoGloss _ s = return s --^ Ignora qualquer outro evento. 
@
-}

reageEventoGloss :: Event -> EstadoGloss -> IO EstadoGloss 
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    (j, (x,y), (MainMenu, pics)) 
                                                | y == 0   = return (j, (x, (-240)), (MainMenu, pics))
                                                | otherwise = return (j, (x,(y+80)) , (MainMenu, pics))
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  (j, (x,y), (MainMenu, pics)) 
                                                | y == (-240) = return (j, (x,0),    (MainMenu, pics)) 
                                                | otherwise   = return (j, (x,y-80), (MainMenu, pics))     
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (j, (x,y), (MainMenu, pics))
                                                | y == 0      = return (j, (x,y),    (Jogar, pics))
                                                | y == (-80)  = return (j, (-40,80), (Mapas, pics))
                                                | y == (-160) = return (j, (x,y),    (Instrucoes,  pics))
                                                | y == (-240) = return (j, (x,y),    (Sair, pics))

reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)    (j, (x,y), (Mapas, pics)) 
                                                | y == 80   = return (j, (x,(-20)), (Mapas, pics))
                                                | otherwise = return (j, (x,80),     (Mapas, pics))
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    (j, (x,y), (Mapas, pics)) 
                                                | y == 80   = return (j, (x,(-20)), (Mapas, pics))
                                                | otherwise = return (j, (x,80),     (Mapas, pics))
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)    (j, (x,y), (Mapas, pics))
                                                | x == (-40)  = return (j, ((55),y), (Mapas, pics))
                                                | otherwise = return (j, (-40,y),   (Mapas, pics))
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _)    (j, (x,y), (Mapas, pics))
                                                | x == (-40)  = return (j, ((55),y), (Mapas, pics))
                                                | otherwise = return (j, (-40,y),   (Mapas, pics))
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (j, (x,y), (Mapas, pics))
                                                | x == (-40) && y == 80    = return (jogo1, (0,0), (MainMenu, pics))
                                                | x == (-40) && y == (-20) = return (jogo2, (0,0), (MainMenu, pics))
                                                | x == 55 && y == 80       = return (jogo3, (0,0), (MainMenu, pics))
                                                | x == 55 && y == (-20)    = return (jogo4, (0,0), (MainMenu, pics))

reageEventoGloss (EventKey (SpecialKey KeyDown)  Down _ _) (jogo@(Jogo mapa (Jogador (xs,ys) dir bool)), (x,y), (Jogar, pics)) = return ((moveJogador jogo InterageCaixa), (x, y), (Jogar, pics))                                                                     
reageEventoGloss (EventKey (SpecialKey KeyUp)    Down _ _) (jogo@(Jogo mapa (Jogador (xs,ys) dir bool)), (x,y), (Jogar, pics)) 
                                                                                        | (xs+1,ys-1) == p && podeAndar jogo Trepar = return (jogo1, estadoInicial, (MainMenu, pics)) 
                                                                                        | (xs-1,ys-1) == p && podeAndar jogo Trepar = return (jogo1, estadoInicial, (MainMenu, pics))
                                                                                        | otherwise  = return ((moveJogador jogo Trepar), (x, y), (Jogar, pics))
                                                                                        where p = coordPorta (desconstroiMapa mapa)
reageEventoGloss (EventKey (SpecialKey KeyLeft)  Down _ _) (jogo@(Jogo mapa (Jogador (xs,ys) dir bool)), (x,y), (Jogar, pics))
                                                                                        | (xs-1,yqO) == p && podeAndar jogo AndarEsquerda = return (jogo1, estadoInicial, (MainMenu, pics))
                                                                                        | otherwise       = return ((moveJogador jogo AndarEsquerda), (x, y), (Jogar, pics))
                                                                                        where p = coordPorta (desconstroiMapa mapa)
                                                                                              yqO = yDoChao (xs-1,ys) (desconstroiMapa mapa)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (jogo@(Jogo mapa (Jogador (xs,ys) dir bool)), (x,y), (Jogar, pics))
                                                                                        | (xs+1,yqE) == p  && podeAndar jogo AndarDireita = return (jogo1, estadoInicial, (MainMenu, pics))
                                                                                        | otherwise       = return ((moveJogador jogo AndarDireita) , (x, y), (Jogar, pics))
                                                                                        where p = coordPorta (desconstroiMapa mapa)
                                                                                              yqE = yDoChao (xs+1,ys) (desconstroiMapa mapa)

reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _) jogo@(j, (x,y), (Jogar, pics)) =
                              do writeFile "continuar.txt" (show j)            
                                 return (j, (x,y), (MainMenu, pics))           

reageEventoGloss (EventKey (SpecialKey KeyF12) Down _ _) jogo@(j, (x,y), (s, pics))
                                                | s == MainMenu = return jogo
                                                | otherwise     = return (jogo1, (0,0), (MainMenu, pics))
reageEventoGloss _ s = return s 

{-|
== Definição Geral
Obtêm as coordenadas da Porta.

== Definição mais específica
A função 'coordPorta' vai correr a lista toda de peças até encontrar uma que respeita as condições apresentadas e, nesse caso dá as coordenadas dessa Peca.

== Caso de paragem
Esta função não tem caso de paragem porque é auxiliar da 'reageEventoGloss' e, por isso, o seu caso de paragem está relacionado com esta função.

@
coordPorta :: [(Peca,Coordenadas)] -> Coordenadas
coordPorta ((c,(x,y)):t) 
                        | c /= Porta = coordPorta t
                        | otherwise = (x,y)
@

== Exemplo de utilização da função 'coordPorta':
>>> coordPorta [(Vazio,(0,0)), (Porta,(1,0)), (Vazio,(2,0)), (Vazio,(3,0)), (Vazio,(0,1)),(Bloco,(1,1)), (Vazio, (2,1)), (Vazio, (3,1)), (Vazio,(0,2)) (Bloco,(1,2)), (Bloco,(2,2)), (Bloco, (3,2)), (Bloco,(0,3)), (Bloco,(1,3)), (Bloco,(2,3)), (Bloco,(3,3))] = (1,0)
-} 

coordPorta :: [(Peca,Coordenadas)] -> Coordenadas
coordPorta ((c,(x,y)):t) 
                        | c /= Porta = coordPorta t
                        | otherwise = (x,y)

-- | À medida que o tempo passa, modifica o estado do jogo.

reageTempoGloss :: Float -> EstadoGloss -> IO EstadoGloss 
reageTempoGloss n e = return e 

-- | Desenha na tela o jogo para que o utilizador possa usufruir do mesmo.

desenhaEstadoGloss :: EstadoGloss -> IO Picture 
desenhaEstadoGloss (_, (x,y), (MainMenu, [i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4])) = return (Pictures (menu ++ [Scale 1.5 1 (Translate (x-180) (y+40) s)]))
desenhaEstadoGloss (_, (x,y), (Instrucoes, [i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4])) = return (Pictures [Scale 1.8 1.8 (Translate x (y+160) i)])
desenhaEstadoGloss estado@((Jogo mapa _), (x,y), (Jogar, [i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4])) = return (Pictures ((desenhaMapa mapa 0 [i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4]) ++   [desenhaJogador estado]))                                   
desenhaEstadoGloss (_, (x,y), (Mapas, [i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4])) = return (Pictures ([Scale 3 3 (Translate (-50) 50 m1), Scale 3 3 (Translate 50 50 m2), Scale 3 3 (Translate (-50) (-50) m3), Scale 3 3 (Translate 50 (-50) m4)] ++ [Scale 3.15 3 (Translate (x-7.5) (y-30) sel)]))

{- |
== Definição geral
Desenha o mapa.

== Definição mais específica
Aplicar a função 'desenhaLinha' recursivamente até desenhar o mapa por completo.

== Caso de paragem
A função acaba quando o mapa é vazio, [].

@
desenhaMapa :: Mapa -> Int -> [Picture] -> [Picture]
desenhaMapa [] y pics = []
desenhaMapa mapa@(h:t) y pics@[i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4] = desenhaMapa t (y-30) pics ++ desenhaLinha h (0,y) (mx,my) pics  --^ São retiradas 30 unidades ao valor de 'y' sempre que uma linha chega ao fim para que o mapa não fique sobreposto.
                  where mx = (xMax (map snd(desconstroiMapa mapa))) --^ Determina o valor de 'x' máximo do mapa.
                        my = (yMax (map snd(desconstroiMapa mapa))) --^ Determina o valor de 'y' máximo do mapa.
@ 

== Exemplo de utilização da função 'desenhaMapa':
>>> desenhaMapa [[Bloco,Vazio,Vazio,Porta], [Bloco, Bloco, Bloco, Bloco]] = [b, v, v, p, b, b, b, b]

== Definição geral
Desenhar uma linha do mapa.

== Definição mais específica
Através da recursividade da função, desenha a linha do mapa que corresponde à lista de peças que recebe.

== Caso de paragem
A função termina quando a lista de peças fica vazia, [].

@
desenhaLinha :: [Peca] -> (Int,Int) -> (Int,Int) -> [Picture] -> [Picture]
desenhaLinha [] (x,y) (mx,my) pics = []
desenhaLinha (h:t) (x,y) (mx,my) pics@[i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4]
                | h == Porta = desenhaLinha t ((x+60),y) (mx,my) pics ++ [(Translate (x1-max1) (y1+may2) p)]  
                | h == Vazio = desenhaLinha t ((x+60),y) (mx,my) pics ++ [(Translate (x1-max1) (y1+may2) v)]
                | h == Bloco = desenhaLinha t ((x+60),y) (mx,my) pics ++ [(Translate (x1-max1) (y1+may2) b)]
                | h == Caixa = desenhaLinha t ((x+60),y) (mx,my) pics ++ [(Translate (x1-max1) (y1+may2) c)]
              where x1 = fromIntegral x --^ Transforma o 'x' (Int) num 'xs' (Float).
                    y1 = fromIntegral y --^ Transforma o 'y' (Int) num 'ys' (Float).
                    max1 = (fromIntegral mx*60) * (1/2) --^ Torna o mapa centrado ao meio tendo em conta o valor do 'x'.
                    may2 = (fromIntegral my*60) * (1/2) --^ Torna o mapa centrado ao meio tendo em conta o valor do 'y'.
@

== Exemplo de utilizaçao da função 'desenhaLinha':
>>> desenhaLinha [Bloco, Porta, Vazio, Vazio, Vazio] (0,0) (4,0) [i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4] = [b, p, v, v]
-}

desenhaMapa :: Mapa -> Int -> [Picture] -> [Picture]
desenhaMapa [] y pics = []
desenhaMapa mapa@(h:t) y pics@[i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4] = desenhaMapa t (y-30) pics ++ desenhaLinha h (0,y) (mx,my) pics 
                  where mx = (xMax (map snd(desconstroiMapa mapa)))
                        my = (yMax (map snd(desconstroiMapa mapa)))

desenhaLinha :: [Peca] -> (Int,Int) -> (Int,Int) -> [Picture] -> [Picture]
desenhaLinha [] (x,y) (mx,my) pics = []
desenhaLinha (h:t) (x,y) (mx,my) pics@[i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4]
                | h == Porta = desenhaLinha t ((x+60),y) (mx,my) pics ++ [(Translate (x1-max1) (y1+may2) p)]  
                | h == Vazio = desenhaLinha t ((x+60),y) (mx,my) pics ++ [(Translate (x1-max1) (y1+may2) v)]
                | h == Bloco = desenhaLinha t ((x+60),y) (mx,my) pics ++ [(Translate (x1-max1) (y1+may2) b)]
                | h == Caixa = desenhaLinha t ((x+60),y) (mx,my) pics ++ [(Translate (x1-max1) (y1+may2) c)]
              where x1 = fromIntegral x
                    y1 = fromIntegral y
                    max1 = (fromIntegral mx*60) * (1/2)
                    may2 = (fromIntegral my*60) * (1/2)
                  
{-|
== Definição geral
Desenha o jogador tendo em conta a sua direção e se este está ou não a segurar uma caixa.

== Caso de paragem
Uma vez que um jogo terá sempre um jogador, esta função não necessita de ter um caso de paragem.

@
desenhaJogador :: EstadoGloss -> Picture
desenhaJogador ((Jogo mapa (Jogador (xs,ys) dir bool)), (x,y), (Jogar, [i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4]))
                             | dir == Oeste && bool == False = Translate ((fromIntegral xs*60)-max1) (-((fromIntegral ys*60)-may2)) p3O
                             | dir == Este  && bool == False = Translate ((fromIntegral xs*60)-max1) (-((fromIntegral ys*60)-may2)) p3E
                             | dir == Oeste && bool == True  = Translate ((fromIntegral xs*60)-max1) (-((fromIntegral ys*60)-may2)) p3CO
                             | dir == Este  && bool == True  = Translate ((fromIntegral xs*60)-max1) (-((fromIntegral ys*60)-may2)) p3CE
                         where  mx = (xMax (map snd(desconstroiMapa mapa))) --^ Determina o 'x' máximo do mapa
                                my = (yMax (map snd(desconstroiMapa mapa))) --^ Determina o 'y' máximo do mapa
                                max1 = (fromIntegral mx*60) * (1/2)  --^ O valor de 'x' é multiplicado pelo tamanho das peças definido, 60. Posteriormente, este é multiplicado por 1/2 uma vez que, o mapa está centrado ao meio. 
                                may2 = (fromIntegral my*60) * (1/2)  --^ O valor de 'y' é multiplicado pelo tamanho das peças definido, 60. Posteriormente, este é multiplicado por 1/2 uma vez que, o mapa está centrado ao meio.                      
@

== Exemplo de utilização da função 'desenhaJogador':
>>> ((Jogo [[Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco]] (Jogador (1,0) Este False)), (x,y), (Jogar, [i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4])) =  Translate 0 30 p3CE
-}

desenhaJogador :: EstadoGloss -> Picture
desenhaJogador ((Jogo mapa (Jogador (xs,ys) dir bool)), (x,y), (Jogar, [i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4]))
                             | dir == Oeste && bool == False = Translate ((fromIntegral xs*60)-max1) (-((fromIntegral ys*60)-may2)) p3O
                             | dir == Este  && bool == False = Translate ((fromIntegral xs*60)-max1) (-((fromIntegral ys*60)-may2)) p3E
                             | dir == Oeste && bool == True  = Translate ((fromIntegral xs*60)-max1) (-((fromIntegral ys*60)-may2)) p3CO
                             | dir == Este  && bool == True  = Translate ((fromIntegral xs*60)-max1) (-((fromIntegral ys*60)-may2)) p3CE
                         where  mx = (xMax (map snd(desconstroiMapa mapa)))
                                my = (yMax (map snd(desconstroiMapa mapa)))
                                max1 = (fromIntegral mx*60) * (1/2)
                                may2 = (fromIntegral my*60) * (1/2)

{-|
== Definição geral
O framerate do jogo, ou seja, é o número de vezes que a função 'reageTempoGloss' é executada num segundo.

@
fr :: Int 
fr = 50
@
-}

fr :: Int 
fr = 50

{-|
== Definição geral
Faz com que o jogo funcione e permite importar as imagens necessárias ao seu desenvolvimento.

@
main :: IO ()
main = do 
    --p <- loadBMP "Imagem.bmp"
    Just i    <- loadJuicy "imagens/instrucoes.png"  --^ Imagem das Instruçoes
    Just p    <- loadJuicy "imagens/porta.png"  --^ Imagem da Porta
    Just b    <- loadJuicy "imagens/bloco.png"  --^ Imagem do Bloco
    Just c    <- loadJuicy "imagens/caixa.png"  --^ Imagem da Caixa
    Just p3E  <- loadJuicy "imagens/manE.png"   --^ Imagem do jogador virado para a direita
    Just p3O  <- loadJuicy "imagens/manO.png"   --^ Imagem do jogador virado para a esquerda
    Just p3CE <- loadJuicy "imagens/mancaixaE.png"  --^ Imagem do jogador virado para a direita a segurar uma caixa
    Just p3CO <- loadJuicy "imagens/mancaixaO.png"  --^ Imagem do jogador virado para a esquerda a segurar uma caixa
    Just v    <- loadJuicy "imagens/vazio.png"  --^ Imagem do Vazio
    Just s    <- loadJuicy "imagens/seta.png"   --^ Imagem da seta
    Just sel  <- loadJuicy "imagens/selecionar.png"  --^ Imagem de um quadrado
    Just m1   <- loadJuicy "imagens/mapa1.png"  --^ Imagem do mapa 1
    Just m2   <- loadJuicy "imagens/mapa2.png"  --^ Imagem do mapa 2
    Just m3   <- loadJuicy "imagens/mapa3.png"  --^ Imagem do mapa 3
    Just m4   <- loadJuicy "imagens/mapa4.png"  --^ Imagem do mapa 4
    playIO FullScreen           --^ Janela onde irá correr o jogo
      (greyN 0.5)               --^ Cor do fundo da janela
      fr                        --^ Frame rate
      (estadoGlossInicial [i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4])   --^ Estado inicial
      desenhaEstadoGloss        --^ Função que desenha o estado do jogo
      reageEventoGloss          --^ Função que reage a um evento
      reageTempoGloss           --^ Função que reage ao passar do tempo
@
-}

main :: IO ()
main = do 
    Just i    <- loadJuicy "imagens/instrucoes.png"
    Just p    <- loadJuicy "imagens/porta.png"
    Just b    <- loadJuicy "imagens/bloco.png"
    Just c    <- loadJuicy "imagens/caixa.png"
    Just p3E  <- loadJuicy "imagens/manE.png"
    Just p3O  <- loadJuicy "imagens/manO.png"
    Just p3CE <- loadJuicy "imagens/mancaixaE.png"
    Just p3CO <- loadJuicy "imagens/mancaixaO.png"
    Just v    <- loadJuicy "imagens/vazio.png"
    Just s    <- loadJuicy "imagens/seta.png"
    Just sel  <- loadJuicy "imagens/selecionar.png"
    Just m1   <- loadJuicy "imagens/mapa1.png"
    Just m2   <- loadJuicy "imagens/mapa2.png"
    Just m3   <- loadJuicy "imagens/mapa3.png"
    Just m4   <- loadJuicy "imagens/mapa4.png"
    playIO FullScreen           
      (greyN 0.5)               
      fr                       
      (estadoGlossInicial [i, p, b, c, p3O, p3E, p3CE, p3CO, v, s, sel, m1, m2, m3, m4])  
      desenhaEstadoGloss      
      reageEventoGloss        
      reageTempoGloss          

-- | Menu inicial do jogo

menu = [Pictures [Translate (-210) 170 (Scale (0.5) (0.5) (Text "BLOCK DUDE")), Translate (-80) (20) (Scale (0.5) (0.5) (Text "Jogar")), Translate (-90) (-60) (Scale (0.5) (0.5) (Text "Mapas")) , Translate (-140) (-140) (Scale (0.5) (0.5) (Text "Instrucoes")) , Translate (-60) (-220) (Scale (0.5) (0.5) (Text "Sair"))]]


-- | Mapas disponíveis para o jogo

jogo1 :: Jogo
jogo1 = (Jogo mapa1 (Jogador (1,4) Oeste False))
mapa1 = [  [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
         , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
         , [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa]
         , [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco]
         , [Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]

jogo2 :: Jogo
jogo2 = (Jogo mapa2 (Jogador (5,4) Este False))
mapa2 = [  [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
         , [Caixa, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
         , [Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Caixa, Vazio, Vazio, Porta]
         , [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Bloco]
         , [Bloco, Caixa, Caixa, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco]
         , [Bloco, Bloco, Bloco, Vazio, Vazio, Caixa, Bloco, Vazio, Bloco, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]

jogo3 :: Jogo
jogo3 = (Jogo mapa3 (Jogador (4,4) Este False))
mapa3 = [  [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
         , [Bloco, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco]
         , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
         , [Bloco, Porta, Vazio, Bloco, Vazio, Vazio, Caixa, Bloco]
         , [Bloco, Bloco, Vazio, Bloco, Vazio, Bloco, Bloco, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]

jogo4 :: Jogo
jogo4 = (Jogo mapa4 (Jogador (8,3) Este True))
mapa4 = [  [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco]
         , [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
         , [Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Caixa, Bloco]
         , [Bloco, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Bloco]
         , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]