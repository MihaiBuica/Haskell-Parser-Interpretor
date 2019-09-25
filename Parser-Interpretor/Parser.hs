module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import ClassState

-- ========TASK 2========
-- Definire Program
type Program = [(String, String, ClassState)] --nume clasa, nume parinte, container clasa
tokens = [' ', ':', ',', '(', ')', '=', '.'] --lista de caractere dupa care se face parsarea

--creeaza un program gol
initEmptyProgram :: Program
initEmptyProgram = []

--adaugare clasa Global in programul gol
initProgram = ("Global", "Global", initEmptyClass) : initEmptyProgram

--extrage variabilele din program
getVars :: Program -> [[String]]
getVars [] = []
getVars ((numeClasa, numeParinte, classState) : prog)
   | numeClasa == "Global" = (getValues classState Var)
   | otherwise = getVars prog

--extrage numele claselor din program
getClasses :: Program -> [String]
getClasses [] = []
getClasses ((numeClasa, numeParinte, classState) : prog) = numeClasa : (getClasses prog)

--obtine parintele unei clase din program
getParentClass :: String -> Program -> String
getParentClass strClasa [] = ""
getParentClass strClasa ((numeClasa, numeParinte, classState) : prog)
   | strClasa == numeClasa = numeParinte
   | otherwise = getParentClass strClasa prog

--obtine functiile pentru o anumita clasa din program
getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass strClasa [] = []
getFuncsForClass strClasa ((numeClasa, numeParinte, classState) : prog)
   | numeClasa == strClasa = (getValues classState Func)
   | otherwise = getFuncsForClass strClasa prog

type Instruction = (String, [String]) --nume instructiune, parametrii instructiunii
emptyInstructions = []


-- ====PARSE====
--parsare dupa un caracter : caracter -> string -> string parsat
splitBy :: Char -> String -> [String]
splitBy c [] = [[]]
splitBy c (x:xs)
   | x == c = [] : rest
   | otherwise = (x:(head rest)) : (tail rest)
   where rest = splitBy c xs

--parsare dupa o lista de separatori: lista separatori -> string -> string parsat
splitBy2 :: [Char] -> String -> [String]
splitBy2 tokens [] = [[]]
splitBy2 tokens (x:xs)
   | contains tokens x  == True = [] : rest
   | otherwise = (x:(head rest)) : (tail rest)
   where rest = splitBy2 tokens xs

-- verificare daca un caracter se afla intr-o lista de caractere
-- folosit pentru a cauta un caracter in lista de tokens
contains :: [Char] -> Char -> Bool
contains [] e = False
contains (x : xs) e
   | x == e = True
   | otherwise = contains xs e

-- eliminare string vid dintr-o lista de string-uri
-- lista -> lista fara string-uri vide 
rmvEmpty :: [String] -> [String]
rmvEmpty [] = []
rmvEmpty (x:xs)
   | x == "" = rmvEmpty xs
   | otherwise = x : (rmvEmpty xs)
-- transforma o lista de string-uri intr-o lista de instructiuni
-- ia numele instructiunii si il pune ca prim element, dupa care restul el pe rand
transform :: [[String]] -> [Instruction]
transform [[]] = emptyInstructions
transform [] = emptyInstructions
transform (x:xs) = ((head x), (tail x)) : (transform xs)

-- functia ce realizeaza parsarea, prima data dupa randuri apoi dupa separatori
-- elimina tokenii duplicati, orice ar putea afecta 
parse :: String -> [Instruction]
parse str = transform ( map rmvEmpty ( map (splitBy2 tokens) (rmvEmpty (splitBy '\n' str)) ) )

-- ====INTERPRET====
-- primul cuvant al instructiunii este tipul ei
-- se apeleaza functia specifica comenzii
-- default, daca nu este unul dintre cele 3 cuvinte, este definirea unei functii
interpret :: Instruction -> Program -> Program
interpret (name, param) program
   | name == "class" = initClass param program
   | name == "newvar" = initNewVar param program
   | name == "infer" = initNewInfer param program
   | otherwise = initFunc name param program
 
 -- Functie ce se ocupa cu crearea unei noi clase
 -- verifica daca mai exista clasa in program
 -- in caz ca programul e neinitializat adauga clasa Global
 -- in caz ca era initializat, doar adauga noua clasa si parintele aferent
initClass :: [String] -> Program -> Program
initClass [nume] program = if verifyExistClass nume program == True then program  
                           else if program == initEmptyProgram then (nume, "Global", initEmptyClass) : initProgram
                                   else (nume, "Global", initEmptyClass) : program
initClass [nume, extend, numeParinte] program
   | extend == "extends" = 
    if verifyExistClass nume program == True then program 
    else if verifyExistClass numeParinte program == True then 
           (nume, numeParinte, initEmptyClass) : program
        else if program == initEmptyProgram then (nume, "Global", initEmptyClass) : initProgram
             else (nume, "Global", initEmptyClass) : program
   | otherwise = program
initClass _ program = program   

-- Functie pentru adaugarea unei noi variabile in program
-- Verifica daca exista clasa pentru variabila respectiva, in caz afirmativ o adauga
initNewVar :: [String] -> Program -> Program
initNewVar [simbVar, simbClasa] prog
   | verifyExistClass simbClasa prog == True =( addInProgCS "Global" (Var, [simbVar, simbClasa]) prog )
   | otherwise = prog
initNewVar _ prog = prog

-- Functie pentru adaugarea unei noi functii in program
-- Verifica daca exista toate clasele, sau functii cu acelasi nume inainte de adaugare
initFunc :: String -> [String] -> Program -> Program
initFunc simbReturn (simbClasa : (simbFunctie : listParam)) prog
  | (verifyExistClass simbReturn prog == True) &&
    (verifyExistClass simbClasa prog == True) &&
    (verifyExistClassList listParam prog == True) = 
         ( addInProgCS simbClasa (Func, (simbFunctie : (simbReturn : listParam))) prog )
  | otherwise = prog
initFunc _ _ prog = prog

-- Functie ce verifica daca exista o clasa in program
verifyExistClass :: String -> Program -> Bool
verifyExistClass str [] = False
verifyExistClass str ((className, parentName, container) : xs) 
   | str == className = True
   | otherwise = verifyExistClass str xs

-- Functie ce verifica daca exista o lista de parametri in program
verifyExistClassList :: [String] -> Program -> Bool
verifyExistClassList [] _ = True
verifyExistClassList (param:list) prog 
   | verifyExistClass param prog == True = verifyExistClassList list prog
   | otherwise = False

-- Functie ce adauga o noua "instructiune" in ClassState-ul unei clase
addInProgCS :: String -> (InstrType, [String]) -> Program -> Program
addInProgCS numeClasa param ( (nume, numePar, classState) : xs )
   | numeClasa == nume = ((nume, numePar, (param : classState)) : xs)
   | otherwise = (nume, numePar, classState) : (addInProgCS numeClasa param xs)

-- ========TASK 3========
-- Functie ce realizeaza inferenta de tip
-- Daca este o variabila, se cauta in program iar daca exista se intoarce tipul ei
-- In cazul undei functii, dupa ce se cauta variabila si existenta functiei
--  se prelucreaza parametrii primiti
infer :: Expr -> Program -> Maybe String
infer (Va a) program = verifiExist a (getVars program)
infer (FCall var func listParam) program = if (clasa /= Nothing) then (verifiFunc 
                                                      (func : (prelucrareParam listParam program)) 
                                                      (fromMaybeToString clasa) program )
                                           else Nothing 
                                           where clasa = (verifiExist var (getVars program))

-- Prelucrarea parametrilor consta in faptul ca se obtine o semnatura a functiei
--  asemanatoare cu cea de la punctele precedenta
-- Pentru fiecare parametru se apeleaza infer si se retine tipul rezultat
-- In caz ca inferenta esueaza pe parcurs, se intoarece Nothing
prelucrareParam :: [Expr] -> Program -> [String]
prelucrareParam [] program = []
prelucrareParam (param : listParam) program = ((fromMaybeToString (infer param program)) 
                                                         : prelucrareParam listParam program)  

-- Functie folosita pentru a extrage String-ul dintr-un maybe
fromMaybeToString :: Maybe String -> String
fromMaybeToString Nothing = ""
fromMaybeToString (Just s) = s

-- Cautarea functiei in clasa si pe lantul de mostenire
-- Functia returneaza tipul intors de functie, daca exista sau Nothing. 
verifiFunc :: [String] -> String -> Program -> Maybe String
verifiFunc paramList numeClasa program
   | tipIntors /=Nothing  = tipIntors --de aici o sa iasa tipul intors 
   | numeClasa == "Global" = Nothing
   | otherwise = verifiFunc paramList (getParentClass numeClasa program) program  
  where tipIntors = verfyExistFunc paramList (getFuncsForClass numeClasa program)

-- Functie ce verifica existenta unei functii in program
-- Din semnatura existenta in program, se elimina tipul intors pentru ca acela trebuie returnat
-- Intoarce tipul functiei / Nothing
verfyExistFunc :: [String] -> [[String]] -> Maybe String
verfyExistFunc param [] = Nothing
verfyExistFunc param (x : xs) --trbeuie sa nu verific si tipul intors pentru ca nu il am
   | param == ((head x) : (tail(tail x))) = Just (head(tail x))
   | otherwise = verfyExistFunc param xs

-- Functie ce ferifica existenta undei variabile in program
-- Intoarce tipul variabilei/Nothing
verifiExist :: String -> [[String]] -> Maybe String
verifiExist var [] = Nothing
verifiExist var (x:xs)
   | var == head(x) = Just (head(tail x))
   | otherwise = verifiExist var xs

-- =====BONUS=====
-- Functie pentru crearea unei noi inferente a unei variabile
-- Transforma string-ul primit intr-o Expr si apoi ii realizeaza inferatia 
initNewInfer :: [String] -> Program -> Program
initNewInfer (numeVar : listParam) program
   | tipInfer /= Nothing = initNewVar ([numeVar,(fromMaybeToString (tipInfer) )]) program
   | otherwise = program
   where tipInfer = infer (fromStringToExpr(listParam)) program

-- Functie folosita pentru a transforma o lista de string-uri intr-o Expr
fromStringToExpr :: [String]->Expr
fromStringToExpr (numeClasa : numeFunc : param) = FCall numeClasa numeFunc 
                                                                  (stringToVarList param)

-- Transforma o lista de string-uri intr-o lista de Expr de tip Va
stringToVarList :: [String]->[Expr]
stringToVarList [] = []
stringToVarList (x:xs) = (Va x) : (stringToVarList xs)