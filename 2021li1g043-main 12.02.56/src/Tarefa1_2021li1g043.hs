{- |
Module      : Tarefa1_2021li1g043
Description : Validação de um potencial mapa
Copyright   : João Magalhães <a100740@alunos.uminho.pt>;
            : Gonçalo Loureiro <a100535@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g043 where

import LI12122

{-|
== Definição geral
Validação do mapa através de várias funções acopladas.

== Definição mais específica
A função validaPotencialMapa será utilizada para validar os mapas a partir de várias funções que, juntas, cumprem com as regras de um mapa válido podendo retornar verdadeiro(mapa válido) ou falso(mapa inválido).

== Caso de paragem
A função apenas irá devolver False quando uma das funções nao for cumprida ou até quando for dada à mesma uma lista vazia, [].

@
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False              
validaPotencialMapa l = coordiguais (map snd l)  
                     && umaPorta (map fst l)
                     && caixaFlutuante l l
                     && verificaVazio l
                     && chaoMapa l 
@

== Exemplos de utilização da função 'validaPotencialMapa':
>>> ValidaPotencialMapa [(Porta,(0,0)),(Caixa,(1,0)),(Bloco,(0,1)),(Bloco,(1,1)),(Bloco,(2,1))]
True

>>> ValidaPotencialMapa [(Porta,(0,0)),(Caixa,(1,0)),(Bloco,(0,1)),(Porta,(0,1)),(Bloco,(2,1))] 
False
-}

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False              
validaPotencialMapa l = coordiguais (map snd l)  
                     && umaPorta (map fst l)
                     && caixaFlutuante l l
                     && verificaVazio l
                     && chaoMapa l 

{-| 
== Definição geral
Verificar se há coordenadas iguais.

== Definição mais específica
Esta função a seguir apresentada tem como objetivo principal verificar a repetição de alguma coordenada podendo retornar verdadeiro(coordenadas todas diferentes) ou falso(pelo menos uma coordenada igual).

== Caso de paragem
O caso de paragem de 'coordiguais' é dado apenas quando a função é vazia, [], e nesse caso, devolve False. Quando a lista é percorrida totalmente sem apresentar coordenadas iguais, ao chegar à ultima peça devolve True uma vez que, nada é igual àquela peça.

@ 
coordiguais :: [Coordenadas] -> Bool 
coordiguais [] = False
coordiguais [coor1] = True
coordiguais (coor1:tc) =  if elem(coor1) tc then False else coordiguais tc
@

== Exemplos de utilização da função 'coordiguais':
coordiguais [(0,0),(1,0),(3,2),(0,0)]
>>> False

coordiguais [(0,0),(1,0),(3,2),(5,1)]
>>> True  
-}

coordiguais :: [Coordenadas] -> Bool 
coordiguais [] = False
coordiguais [coor1] = True
coordiguais (coor1:tc) =  if elem(coor1) tc then False else coordiguais tc 

{-| 
== Definição geral
Verificar se há mais do que uma porta no mapa.

== Definição mais específica
A função 'umaPorta' irá percorrer a lista toda até encontrar uma Porta, depois averigua se há mais alguma Porta na restante lista, caso nao exista, a função retorna True.

== Caso de paragem  
O caso de paragem desta função em baixo descrita é dado pela lista vazia, [], uma vez que, a função percorre a lista até encontrar alguma Porta, no caso de não encontrar, devolve False.

@
umaPorta :: [Peca] -> Bool 
umaPorta [] = False
umaPorta (p1 : tp) 
                  | p1 == Porta && (elem(Porta) tp) = False
                  | p1 /= Porta = umaPorta tp  
                  | otherwise = True
@

== Exemplos de utilização da função 'umaPorta':
>>> umaPorta [Porta,Bloco,Caixa,Porta] 
False

>>> umaPorta [Caixa,Bloco,Porta,Bloco]
True 
-}

umaPorta :: [Peca] -> Bool 
umaPorta [] = False
umaPorta (p1 : tp) 
                  | p1 == Porta && (elem(Porta) tp) = False
                  | p1 /= Porta = umaPorta tp  
                  | otherwise = True

{-|
== Definição geral   
Verificar se há alguma caixa flutuante no mapa.

== Definição mais específica
O objetivo desta função é aferir se há alguma caixa ou bloco na posição abaixo da caixa avançando a lista sempre que esta condição se verificar. 

== Caso de paragem
O caso de paragem desta função é dado pela receção de uma lista vazia, [] ou então até encontrar mais do que uma Porta que por sua vez, devolve False senão retorna True. 

@
caixaFlutuante :: [(Peca, Coordenadas)] -> [(Peca,Coordenadas)] -> Bool 
caixaFlutuante [] l = True 
caixaFlutuante ((c,(x,y)):t) l
                       | c == Caixa && elem (Bloco,(x,y+1)) l = caixaFlutuante t l
                       | c == Caixa && elem (Caixa,(x,y+1)) l = caixaFlutuante t l 
                       | c == Caixa && not(elem (Caixa,(x,y+1)) l) = False 
                       | c == Caixa && not(elem (Bloco,(x,y+1)) l) = False
                       | otherwise = caixaFlutuante t l
@

== Exemplos de utilização da função 'caixaFlutuante':
>>> caixaFlutuante [(Porta,(0,0)),(Caixa,(1,0)),(Bloco,(0,1)),(Bloco,(2,1))] 
False
 
>>> caixaFlutuante [(Porta,(0,0)),(Caixa,(1,0)),(Bloco,(0,1)),(Caixa,(1,1)),(Bloco,(1,2))]
True  
-}

caixaFlutuante :: [(Peca, Coordenadas)] -> [(Peca,Coordenadas)] -> Bool 
caixaFlutuante [] l = True 
caixaFlutuante ((c,(x,y)):t) l
                       | c == Caixa && elem (Bloco,(x,y+1)) l = caixaFlutuante t l
                       | c == Caixa && elem (Caixa,(x,y+1)) l = caixaFlutuante t l 
                       | c == Caixa && not(elem (Caixa,(x,y+1)) l) = False 
                       | c == Caixa && not(elem (Bloco,(x,y+1)) l) = False
                       | otherwise = caixaFlutuante t l

{-|
== Definição global da junção das duas funções
Funções auxiliares que encontram a abcissa e a ordenada máxima do mapa para poderem, posteriormente, ser aplicadas nas funções que irão aparecer. 
      
== Caso de paragem
Uma vez que estas funções nunca seram utilizadas sozinhas, o seu caso de paragem vai de encontro com o caso de paragem na função a que estão a auxliar.
  
1. Devolve o valor de 'x' máximo através da comparação das abcissas das coordenadas 

@
xMax :: [Coordenadas] -> Int
xMax [(x,y)] = x
xMax ((x,y):(xs,ys):t) = if x >= xs then xMax ((x,y):t) else xMax ((xs,ys):t)
@

2. Devolve o valor de 'y' máximo através da comparação das ordenadas 

@
yMax :: [Coordenadas] -> Int
yMax [(x,y)] = y
yMax ((x,y):(xs,ys):t) = if y >= ys then yMax((x,y):t) else yMax ((xs,ys):t)
@

== Exemplo de utilização da função 'xMax' e 'yMax', respetivamente:
>>> xMax [(0,0),(2,0),(3,2),(1,0)]
3

>>> yMax [(0,0),(2,0),(3,2),(1,0)]
2
-}

xMax :: [Coordenadas] -> Int
xMax [(x,y)] = x
xMax ((x,y):(xs,ys):t) = if x >= xs then xMax ((x,y):t) else xMax ((xs,ys):t)

yMax :: [Coordenadas] -> Int
yMax [(x,y)] = y
yMax ((x,y):(xs,ys):t) = if y >= ys then yMax((x,y):t) else yMax ((xs,ys):t)

{-|
== Definição geral 
Construção das linhas do mapa.

== Definição mais específica
Constroi uma lista com todas as coordenadas que o mapa pode ter dadas as suas dimenções 'x' e 'y'.

== Caso de paragem
Nestas funções uma vez que, estamos a falar de coordenadas de um mapa, não faz sentido coordenadas negativas logo, o caso de paragem das funções 'linhaMapa' e 'linha' é quando o y<0 e x< 0, respetivamente.

1. Enquanto que y >= 0, a função irá chamar a função 'linha' para um dado valor de 'x', subtraindo sempre o valor "1" por cada linha 

@
linhasMapa :: Int -> Int -> [Coordenadas]
linhasMapa x y
               | (y >= 0) = linhasMapa x (y-1) ++ linha x y  
               | otherwise = [] 
@

== Exemplo de utilização da função 'linhasMapa':
>>> linhasMapa 2 2
[(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(0,2),(1,2),(2,2)]

2. Enquanto que x >= 0 a função irá devolver uma lista de coordenadas do tipo (x,y), subtraíndo sempre o valor "1" a cada coordenada dada (começando na última coordenada da linha), sendo o 'y' constante em toda a linha

@
linha :: Int -> Int -> [Coordenadas]
linha x y
            | (x >= 0) = linha (x-1) y ++ [(x,y)]
            | otherwise = [] 
@

== Exemplo de utilização da função 'linha':
A função sozinha neste caso não tem muita utilidade no entanto, quando junta à função 'linhasMapa' é possível obter inúmeros resultados. Um dos exemplos da sua combinação está referido em cima.
-}

linhasMapa :: Int -> Int -> [Coordenadas]
linhasMapa x y
               | (y >= 0) = linhasMapa x (y-1) ++ linha x y  
               | otherwise = [] 

linha :: Int -> Int -> [Coordenadas]
linha x y
            | (x >= 0) = linha (x-1) y ++ [(x,y)]
            | otherwise = [] 

{-| 
== Definição geral
Devolve uma lista de Vazios omitidos na lista 'l'.

== Definição mais específica
Esta função tem como objetivo auxiliar a função que será apresentada logo após esta, devolvendo todas as pecas que estão omitidas, ou seja, que não são escritas, sendo assim compreendidas como (Vazio,(x,y)).

== Caso de paragem
A função tem como caso de paragem quando uma das duas listas recebidas é vazia, []. 

@
coordVazio :: [Coordenadas] -> [Coordenadas] -> [(Peca,Coordenadas)]
coordVazio _ [] = []
coordVazio [] _ = []
coordVazio ((x,y):t) l = if elem(x,y) l then coordVazio t l else (Vazio,(x,y)) : coordVazio t l
@    

== Exemplo de utilização da função coordVazio
>>> coordVazio [(0,0), (1,0), (0,1), (1,1)] [(0,0), (1,1)]
[(Vazio,(1,0)), (Vazio,(0,1))]
-}

coordVazio :: [Coordenadas] -> [Coordenadas] -> [(Peca,Coordenadas)]
coordVazio _ [] = []
coordVazio [] _ = []
coordVazio ((x,y):t) l = if elem(x,y) l then coordVazio t l else (Vazio,(x,y)) : coordVazio t l

{-| 
== Definição geral
Constroi um mapa ordenado com as peças omitidas.
 
== Definição mais específica
Constroi o mapa ordenado,auxiliada pela função 'ordenaPecas', de peças do tipo Vazio, omitidas, utilizando a função 'coordVazio' para, posteriormente, verificar se há pecas Vazio no mapa.
      
== Caso de paragem
A função tem apenas como caso de paragem a lista vazia, [].

@
constroiMapaPVO :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
constroiMapaPVO ((c,(x,y)):t) = ordenaPecas(coordVazio (linhasMapa xmax ymax) (map snd((c,(x,y)):t)) ++ ((c,(x,y)):t))
                  where xmax = xMax (map snd ((c,(x,y)):t))
                        ymax = yMax (map snd ((c,(x,y)):t))
@

== Exemplo de utilidade da função 'constroiMapaPVO':
>>> constroiMapaPVO [(Bloco,(0,1)),(Porta,(1,1))]
[(Vazio,(0,0)), (Vazio,(1,0)), (Bloco,(0,1)), (Porta,(1,1))]
-}

constroiMapaPVO :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
constroiMapaPVO [] = []
constroiMapaPVO ((c,(x,y)):t) = ordenaPecas(coordVazio (linhasMapa xmax ymax) (map snd((c,(x,y)):t)) ++ ((c,(x,y)):t))
                  where xmax = xMax (map snd ((c,(x,y)):t))
                        ymax = yMax (map snd ((c,(x,y)):t))

{-|
== Definição geral
Verifica se o mapa tem espaços vazios (pecas = Vazio).

== Definição mais específica
A função irá avaliar se a lista de Pecas contêm alguma que é igual a Vazio através da utilização de duas funções de ordem superior, 'any', à primeira parcela do resultado da funçao constroiMapaPVO, dado pelo 'map fst'.

== Caso de paragem
No caso de nenhuma peça ser igual a Vazio a função irá retornar False, caso contrário, True.

@
verificaVazio :: [(Peca,Coordenadas)] -> Bool
verificaVazio [] = []
verificaVazio l = any (Vazio ==) (map fst(constroiMapaPVO l))
@

== Exemplos de utilização da função 'verificaVazio':
>>> verificaVazio [(Bloco,(0,0)), (Porta,(1,1)), (Bloco,(0,1))]
True

>>> verificaVazio [(Bloco,(0,0)), (Caixa,(1,0)), (Porta,(1,1)), (Bloco,(0,1))]
False 
-}

verificaVazio :: [(Peca,Coordenadas)] -> Bool
verificaVazio [] = False
verificaVazio l = any (Vazio ==) (map fst(constroiMapaPVO l))

{-|
== Definção geral
Ordenar uma lista do tipo (Peca,Coordenada).

== Definição mais específica
Com o auxílio da função que será apresentada de seguida, ordena uma lista do tipo (Peca,Coordenada) tendo em conta as suas coordenadas.

== Caso de paragem
A função 'ordenaPecas' irá devolver vazio, [],  quando receber uma lista vazia, [], caso constrário irá devolver a lista recebida ordenadamente.

@
ordenaPecas :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ordenaPecas [] = []
ordenaPecas ((c,(x,y)):t) = ordenaPecas menores ++ (c,(x,y)) : ordenaPecas maiores 
                where (menores,maiores) = receberMaioresEMenores (x,y) t 
@

== Exemplo de utilização da função 'ordenaPecas':
>>> ordenaPecas [(Bloco,(0,2)),(Bloco,(1,1)),(Bloco,(2,3)),(Caixa,(1,0))]
[(Caixa,(1,0)),(Bloco,(1,1)),(Bloco,(0,2)),(Bloco,(2,3))]

1. Esta função abaixo apresentada tem como objetivo, auxiliar a função em cima mencionada, ordenando as peças de forma crescente repartindo a lista em uma par de listas onde dependendo da comparação que é feita com a (Peca, Coordenada) vão para um dos lados do par.
      
== Caso de paragem
O caso de paragem desta função é dado quando 'Coordenadas' é comparado a uma lista vazia, [].

@
receberMaioresEMenores :: Coordenadas -> [(Peca,Coordenadas)] -> ([(Peca,Coordenadas)],[(Peca,Coordenadas)])
receberMaioresEMenores x [] = ([],[])
receberMaioresEMenores (valorX, valorY) ((c,(x,y)):t)
            | y < valorY || (y == valorY && x < valorX) = ((c,(x,y)):as,bs)
            | y > valorY || (y == valorY && x > valorX) = (as,(c,(x,y)):bs)
            | otherwise = (as,bs)
            where (as,bs) = receberMaioresEMenores (valorX, valorY) t
@

== Exemplo de utilização da função 'receberMaioresEMenores':
>>> receberMaioresEMenores (1,1) [(Porta,(0,0)), (Bloco,(1,2)), (Bloco,(1,0)), (Caixa,(2,2))]
([(Porta,(0,0)),(Bloco,(1,0))], [(Bloco,(1,2)),(Caixa,(2,2))])
-}

ordenaPecas :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ordenaPecas [] = []
ordenaPecas ((c,(x,y)):t) = ordenaPecas menores ++ (c,(x,y)) : ordenaPecas maiores 
                where (menores,maiores) = receberMaioresEMenores (x,y) t 

receberMaioresEMenores :: Coordenadas -> [(Peca,Coordenadas)] -> ([(Peca,Coordenadas)],[(Peca,Coordenadas)])
receberMaioresEMenores x [] = ([],[])
receberMaioresEMenores (valorX, valorY) ((c,(x,y)):t)
            | y < valorY || (y == valorY && x < valorX) = ((c,(x,y)):as,bs)
            | y > valorY || (y == valorY && x > valorX) = (as,(c,(x,y)):bs)
            | otherwise = (as,bs)
            where (as,bs) = receberMaioresEMenores (valorX, valorY) t

{-|
== Definição geral
Armazenar as pecas do tipo (Blocos,(x,y))

== Definição mais específica
Armazena apenas as Pecas que são igual a Bloco juntamente com as suas coordenadas, sendo útil em funções seguintes.

== Caso de paragem
A função irá terminar quando a lista ficar vazia, [].

@
coordBloco :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
coordBloco [] = []
coordBloco ((c,(x,y)):t) 
                        | c /= Bloco = coordBloco t
                        | otherwise = (c,(x,y)) : coordBloco t
@

== Exemplo de utilização da função 'coordBloco':
>>> coordBloco [(Bloco,(0,0)), (Caixa,(1,0)), (Caixa,(2,0)), (Bloco,(1,1))]
[(Bloco,(0,0)), (Bloco,(1,1))]
-}

coordBloco :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
coordBloco [] = []
coordBloco ((c,(x,y)):t) 
                        | c /= Bloco = coordBloco t
                        | otherwise = (c,(x,y)) : coordBloco t

{-| 
== Definição geral
Constroi um mapa apenas com Blocos e Vazios ordenado.

== Definição mais específica
Através da junção das funções 'coordBloco' e 'constroiMapaPVO' obtemos um mapa constituído por pecas do tipo, (Bloco,(x,y)) e (Vazio,(x,y)), ordenado com o intuito de facilitar tarefas seguintes.

== Caso de paragem
Esta função tem apenas como caso de paragem a lista vazia, [].

@
constroiMapaBVO :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
constroiMapa BVO [] = []
constroiMapaBVO l = constroiMapaPVO(coordBloco l)
@

== Exemplo de utilização da função 'constroiMapaBVO':
>>> consstroiMapaBVO [(Bloco,(2,0)),(Caixa,(1,0))]
[(Vazio,(0,0)),(Vazio,(1,0)),(Bloco,(2,0))]
-}

constroiMapaBVO :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
consstroiMapaBVO [] = []
constroiMapaBVO l = constroiMapaPVO(coordBloco l)


{-| 
== Definição geral
Examina se a lista fornecida constitui um chão válido.

== Definição mais específica
Ao acoplar a função 'constroiMapaBVO' à 'coordChaoBloco' e respetivas coordenadas máximas, dadas pelas funções 'xMax e yMax', obtemos um resultado, True ou False, dependendo se a lista corresponde ou não a um chão válido.

@
chaoMapa :: [(Peca,Coordenadas)] -> Bool
chaoMapa l = coordChaoBloco (constroiMapaBVO l) xm ym
             where xm = xMax (map snd l)
                   ym = yMax (map snd l)
@

== Exemplos de utilização da função 'chaoMapa':
>>> [ (Porta,(0,0)), (Bloco,(0,1)), (Caixa,(1,1)), (Bloco,(3,1)), (Bloco,(0,2)), (Bloco,(1,2)), (Bloco,(2,2)) ]
True

>>> [ (Porta,(0,0)), (Bloco,(0,1)), (Bloco,(0,2)), (Bloco,(1,2)), (Bloco,(1,3)), (Bloco,(2,3)), (Bloco,(3,3)), (Bloco,(4,1)), (Bloco,(2,0)) ]
False  

1. A função abaixo apresentada serve de auxílio à função principal, 'chaoMapa', verificando se existe algum bloco nas coordenadas máximas do mapa, caso constrário, sobe uma posição para cima (subtraí "1" ao valor do 'y' máximo) até encontrar um Bloco.
      
== Caso de paragem 
A função irá parar até que seja encontrado um bloco na coluna do 'x' máximo do mapa.

@
coordChaoBloco :: [(Peca,Coordenadas)] -> Int -> Int -> Bool
coordChaoBloco [] xmax ymax = False
coordChaoBloco ((b,(x,y)):t) xmax ymax
                                 | elem(b,(xmax,ymax)) ((b,(x,y)):t) = chaoBloco (b,(xmax,ymax)) ((b,(x,y)):t)
                                 | otherwise = coordChaoBloco ((b,(x,y)):t) xmax (ymax-1)
                             where b = Bloco
@

== Exemplos da utilização da função 'coordChaoBloco':
>>> coordChaoBloco [(Porta,(0,0)),(Bloco,(0,3)),(Bloco,(2,0))] 2 2
False  

>>> coordChaoBloco [(Porta,(0,0)),(Bloco,(0,3)),(Bloco,(2,0))] 2 2
True

2. Quando for encontrado um bloco (através da função desenvolvida anteriormente), examina se há blocos consecutivos que constituem um chão verdadeiro ao longo do mapa
      
== Caso de paragem 
A função irá parar quando algum bloco pertencer a um conjunto de blocos continuos e estiver situado na lateral oposta à da coordenada máxima do mapa.

@
chaoBloco :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool
chaoBloco (b,(0,ym)) l = True
chaoBloco (b,(xm,ym)) l
                   | elem(b,(xm-1,ym)) l    = chaoBloco (b,(xm-1,ym)) l
                   | elem(b,(xm-1,ym-1)) l  = chaoBloco (b,(xm-1,ym-1)) l 
                   | elem(b,(xm-1,ym+1)) l  = chaoBloco (b,(xm-1,ym+1)) l
                   | otherwise = False
@

== Exemplos de utilização da função 'chaoBloco':
A função não tem sentido quando utilizada sozinha logo, um exemplo de utilização da mesma está relacionado com a função supramencionada.
-}

chaoMapa :: [(Peca,Coordenadas)] -> Bool
chaoMapa l = coordChaoBloco (constroiMapaBVO l) xm ym
             where xm = xMax (map snd l)
                   ym = yMax (map snd l)

coordChaoBloco :: [(Peca,Coordenadas)] -> Int -> Int -> Bool
coordChaoBloco [] xmax ymax = False
coordChaoBloco ((b,(x,y)):t) xmax ymax
                                 | elem(b,(xmax,ymax)) ((b,(x,y)):t) = chaoBloco (b,(xmax,ymax)) ((b,(x,y)):t)
                                 | otherwise = coordChaoBloco ((b,(x,y)):t) xmax (ymax-1)
                             where b = Bloco

chaoBloco :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool
chaoBloco (b,(0,ym)) l = True
chaoBloco (b,(xm,ym)) l
                   | elem(b,(xm-1,ym)) l    = chaoBloco (b,(xm-1,ym)) l
                   | elem(b,(xm-1,ym-1)) l  = chaoBloco (b,(xm-1,ym-1)) l 
                   | elem(b,(xm-1,ym+1)) l  = chaoBloco (b,(xm-1,ym+1)) l
                   | otherwise = False
