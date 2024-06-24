----------------------------------------------------------------------------------
-- MODELADO
type Recuerdo = String

data Persona = UnaPersona {
    nombre :: String,
    recuerdos :: [Recuerdo]
} deriving (Show,Eq)

suki :: Persona
suki = UnaPersona {
    nombre = "Susana Kitimporta",
    recuerdos = ["abuelita", "escuela primaria", "examen aprobado","novio"]
} 

----------------------------------------------------------------------------------
-- PUNTO 1: PESADILLAS EN SERIE

{-
    Hacer que una persona pase una noche en la que se suceden una serie de pesadillas. Se quiere obtener cómo se encuentra la persona por la mañana siguiente. 
-}

type Pesadilla = [Recuerdo] -> [Recuerdo]

noche :: [Pesadilla] -> Persona ->  Persona
noche pesadillas persona= foldr pasarPesadilla persona pesadillas  

pasarPesadilla :: Pesadilla -> Persona -> Persona
pasarPesadilla  pesadilla persona = persona{recuerdos = pesadilla (recuerdos persona)}

------------ PESADILLAS 

------------ PESADILLA DE MOVIMIENTO
movimiento :: Int -> Int -> Pesadilla 
movimiento posicion1 posicion2 recuerdos =
    (sustitucion posicion1 (recuerdos!!posicion2).sustitucion posicion2 (recuerdos!!posicion1)) recuerdos

------------ PESADILLA DE SUSTITUCION
sustitucion :: Int -> Recuerdo -> Pesadilla
sustitucion posicion recuerdo = agregado posicion recuerdo . quitado posicion

agregado :: Int -> Recuerdo -> [Recuerdo] -> [Recuerdo]
agregado posicion recuerdo recuerdos = take posicion recuerdos ++ [recuerdo] ++ drop posicion recuerdos

quitado :: Int -> [Recuerdo] -> [Recuerdo]
quitado posicion recuerdos = take posicion recuerdos ++ drop (posicion + 1) recuerdos

------------ PESADILLA DESMEMORIZADORA
desmemorizadora :: Recuerdo -> Pesadilla 
desmemorizadora recuerdo = filter (/= recuerdo) 

------------ PESADILLA ESPEJO
inversion :: Pesadilla
inversion = reverse

------------ SUENIO -> PESADILLAN'T
suenio :: Pesadilla
suenio = id


----------------------------------------------------------------------------------
-- PUNTO 2: SITUACIONES EXCEPCIONALES

-- Se quiere experimentar con grupos de voluntarios y analizar cómo cada persona pasa la noche. 
-- Por ahora se sabe que se pueden detectar las siguientes situaciones excepcionales, no excluyentes, pero podría haber otras. 

primerRecuerdo:: Persona-> Recuerdo
primerRecuerdo = head.recuerdos

---- SITUACIONES EXEPCIONALES
type Excepcion = [Pesadilla] -> Persona -> Bool

--Segmentation fault: cuando la cantidad de pesadillas en la noche es mayor a la cantidad de recuerdos de la persona.
segFault :: Excepcion
segFault pesadillas persona = length pesadillas > length (recuerdos persona)

--Bug inicial: sucede cuando ya la primera de las pesadillas de la noche haría que a la persona se le modifique su primer recuerdo
bugInicial :: Excepcion
bugInicial pesadillas persona = primerRecuerdo persona /= primerRecuerdo (pasarPesadilla (head pesadillas) persona) 

--Falsa alarma: cuando pese a que al menos una de las pesadillas de la noche de manera aislada provocaría un cambio en la memoria, por cómo se da la secuencia de toda la noche, la persona se levantaría igual que como se fue a dormir. 
falsaAlarma :: Excepcion
falsaAlarma pesadillas persona = any (flip cambiaRecuerdos persona) pesadillas &&  persona == noche pesadillas persona

cambiaRecuerdos ::  Pesadilla -> Persona -> Bool
cambiaRecuerdos pesadilla persona = recuerdos persona /= recuerdos ( pasarPesadilla pesadilla persona)

-- Para un grupo de personas diferentes, asumiendo que todas van a soñar lo mismo durante la noche, se quiere saber:

-- En cuántas se detectaría una determinada situación.  
cantidadExepciones :: Excepcion -> [Pesadilla] -> [Persona] -> Int
cantidadExepciones exepcion pesadillas = length . filter (exepcion pesadillas) 

--Si alguna de estas situaciones se detecta en todas las personas. 
todasExepciones :: Excepcion -> [Pesadilla] -> [Persona] -> Bool
todasExepciones exepcion pesadillas personas = all (exepcion pesadillas) personas

-- EJEMPLOS NOCHES

nocheLargaTranquila = [suenio, suenio, suenio, suenio, suenio]
nocheIntensa = [movimiento 1 3, desmemorizadora "novio", inversion, movimiento 2 3]
nocheInofensiva = [inversion,suenio, inversion]

-- NOCHE INFINITA
{-
Si una persona pasase una noche con pesadillas infinitas, 
 * bugInicial se podrá evaluar sin ningún problema, se toma sólo el primer elemento de la lista.
 * falsaAlarma y segFault nuncá podrá dar un resultado, se necesita la lista completa 
-}