module ClassState
where
import Data.Map (Map)
import qualified Data.Map as Map
-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq)

--containerul undei clase -> tipul instructiunii si lista de parametrii
type ClassState = [(InstrType, [String])]
--clasa goala
emptyClassState = []

initEmptyClass :: ClassState
initEmptyClass = emptyClassState

--introduce intr-o clasa o instructiune (var/func) si parametrii ei
insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass classState instr str = (instr, str) : classState

--pentru un element din classState(o clasa) adauga in acumulator instructiuniile de tipul dat
--Tipul instructiunii -> acumulator -> un element din classState -> noua lista
printInfo :: InstrType -> [[String]] -> (InstrType, [String]) -> [[String]]
printInfo tip list (instr, param)
   | instr == tip = param : list
   | otherwise = list

--returneaza informatiile despre o anumita clasa
getValues :: ClassState -> InstrType -> [[String]]
getValues classState tip = foldl (printInfo tip) [] classState