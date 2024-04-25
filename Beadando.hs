module Beadando where
import Data.List

type Coordinate = (Int, Int)
type Sun = Int

data Plant = Peashooter Int | Sunflower Int | Walnut Int | CherryBomb Int deriving (Show, Eq)
data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int deriving (Show, Eq)
data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)] deriving (Show, Eq)

defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2


---------------------          1. feladat


tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
tryPurchase (GameModel a ls ls2) (b,c) noveny 
    | b > 4 || b < 0 || c > 11 || c < 0 = Nothing
    | (length (filter (\xs -> (fst xs) == (b,c)) ls)) == 1 = Nothing
    | noveny == defaultPeashooter && a < 100 = Nothing
    | noveny == defaultWalnut && a < 50 = Nothing
    | noveny == defaultSunflower && a < 50 = Nothing
    | noveny == defaultCherryBomb && a < 150 = Nothing
    | noveny == defaultPeashooter && a >= 100 = Just (GameModel (a-100) ([((b,c), defaultPeashooter)] ++ ls) ls2)
    | noveny == defaultWalnut && a >= 50 = Just (GameModel (a-50) ([((b,c), defaultWalnut)] ++ ls) ls2)
    | noveny == defaultSunflower && a >= 50 = Just (GameModel (a-50) ([((b,c), defaultSunflower)] ++ ls) ls2)
    | noveny == defaultCherryBomb && a >= 150 = Just (GameModel (a-150) ([((b,c), defaultCherryBomb)] ++ ls) ls2)
    

---------------------          2. feladat


placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel a ls ls2) zombie lane
    | lane >= 5 || lane < 0 = Nothing
    | (length (filter (\xs -> (fst xs) == (lane,11)) ls2)) == 1 = Nothing
    | otherwise = Just (GameModel a ls ([((lane,11), zombie)] ++ ls2))


---------------------          3. feladat


plantHealthMinusOne :: Plant -> Plant
plantHealthMinusOne (Peashooter health) = (Peashooter (health-1))
plantHealthMinusOne (Walnut health) = (Walnut (health-1))
plantHealthMinusOne (Sunflower health) = (Sunflower (health-1))
plantHealthMinusOne (CherryBomb health) = (CherryBomb (health-1))

vaultingWithoutJavelin :: Zombie -> Zombie
vaultingWithoutJavelin (Vaulting x 2) = Vaulting x 1

isVaultingTwoSpeed :: Zombie -> Bool
isVaultingTwoSpeed (Basic health speed) = False
isVaultingTwoSpeed (Conehead health speed) = False
isVaultingTwoSpeed (Buckethead health speed) = False
isVaultingTwoSpeed (Vaulting health speed)
    | speed == 2 = True
    | otherwise = False

plantListChanged :: Coordinate -> [(Coordinate, Plant)] -> [(Coordinate, Plant)]
plantListChanged (b,c) [] = []
plantListChanged (b,c) [x]
    | fst(x) == (b,c) = [((b,c), (plantHealthMinusOne (snd(x))))]
    | otherwise = [x]
plantListChanged (b,c) (x:xs) 
    | fst(x) == (b,c) = [((b,c), (plantHealthMinusOne (snd(x))))] ++ plantListChanged (b,c) xs
    | otherwise = [x] ++ plantListChanged (b,c) xs

isHealthZero :: Maybe Plant -> Bool
isHealthZero (Just (Peashooter health))
    | health <= 0 = True
    | otherwise = False
isHealthZero (Just (Walnut health))
    | health <= 0 = True
    | otherwise = False
isHealthZero (Just (Sunflower health))
    | health <= 0 = True
    | otherwise = False
isHealthZero (Just (CherryBomb health))
    | health <= 0 = True
    | otherwise = False

zombieForward :: (Coordinate, Zombie) -> (Coordinate, Zombie)
zombieForward ((b,c),(Basic health speed)) = ((b,c-speed),(Basic health speed)) 
zombieForward ((b,c),(Conehead health speed)) = ((b,c-speed),(Conehead health speed))
zombieForward ((b,c),(Buckethead health speed)) = ((b,c-speed),(Buckethead health speed))
zombieForward ((b,c),(Vaulting health speed)) = ((b,c-speed),(Vaulting health speed))

zombieEncounter :: (Coordinate, Zombie) -> (Coordinate, Zombie)
zombieEncounter ((b,c),(Basic health speed)) = ((b,c),(Basic health speed)) 
zombieEncounter ((b,c),(Conehead health speed)) = ((b,c),(Conehead health speed))
zombieEncounter ((b,c),(Buckethead health speed)) = ((b,c),(Buckethead health speed))
zombieEncounter ((b,c),(Vaulting health speed))
    | speed == 2 = ((b,c-1),(Vaulting health (speed-1)))

laneEnd :: GameModel -> Bool
laneEnd (GameModel sun ls ls2)
    | (lookup (0,0) ls) == Nothing && (lookup (0,0) ls2) /= Nothing = True
    | (lookup (1,0) ls) == Nothing && (lookup (1,0) ls2) /= Nothing = True
    | (lookup (2,0) ls) == Nothing && (lookup (2,0) ls2) /= Nothing = True
    | (lookup (3,0) ls) == Nothing && (lookup (3,0) ls2) /= Nothing = True
    | (lookup (4,0) ls) == Nothing && (lookup (4,0) ls2) /= Nothing = True
    | otherwise = False

zombieMovement :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
zombieMovement [] [] = []
zombieMovement ls [] = []
zombieMovement ls (x:xs)
    | ((lookup (((fst(fst(x)))), ((snd(fst(x)))-1)) ls) /= Nothing) && ((isVaultingTwoSpeed (snd(x))) == True) = [((((fst(fst(x)))), ((snd(fst(x)))-2)),(vaultingWithoutJavelin (snd(x))))] ++ zombieMovement ls xs
    | (lookup (fst(x)) ls) == Nothing = [(zombieForward x)] ++ zombieMovement ls xs
    | ((lookup (fst(x)) ls) /= Nothing) && (isHealthZero((lookup (fst(x)) ls)) == False) = [(zombieEncounter x)] ++ zombieMovement ls xs
    | ((lookup (fst(x)) ls) /= Nothing) && (isHealthZero((lookup (fst(x)) ls)) == True) = [(zombieForward x)] ++ zombieMovement ls xs

plantHappenings :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Plant)]
plantHappenings [] [] = []
plantHappenings ls [] = ls
plantHappenings ls (x:xs)
    | ((lookup (((fst(fst(x)))), ((snd(fst(x)))-1)) ls) /= Nothing) && ((isVaultingTwoSpeed (snd(x))) == True) = plantHappenings ls xs
    | (lookup (fst(x)) ls) == Nothing =  plantHappenings ls xs
    | ((lookup (fst(x)) ls) /= Nothing) && (isHealthZero((lookup (fst(x)) ls)) == False) && ((isVaultingTwoSpeed (snd(x))) == True) =  plantHappenings ls xs
    | ((lookup (fst(x)) ls) /= Nothing) && (isHealthZero((lookup (fst(x)) ls)) == False) =  plantHappenings (plantListChanged (fst(x)) ls) xs
    | ((lookup (fst(x)) ls) /= Nothing) && (isHealthZero((lookup (fst(x)) ls)) == True) =  plantHappenings ls xs
    
performZombieActions :: GameModel -> Maybe GameModel
performZombieActions (GameModel sun [] []) = Just (GameModel sun [] [])
performZombieActions (GameModel sun ls []) = Just (GameModel sun ls [])
performZombieActions (GameModel sun ls ls2)
    | laneEnd (GameModel sun ls ls2) = Nothing
    | otherwise = Just (GameModel sun (plantHappenings ls ls2) (zombieMovement ls ls2))


---------------------          4. feladat


plantHealth :: Plant -> Int
plantHealth (Peashooter health) = health
plantHealth (Walnut health) = health
plantHealth (Sunflower health) = health
plantHealth (CherryBomb health) = health

zombieHealth :: Zombie -> Int
zombieHealth (Basic health speed) = health
zombieHealth (Conehead health speed) = health
zombieHealth (Buckethead health speed) = health
zombieHealth (Vaulting health speed) = health

cleanBoard :: GameModel -> GameModel
cleanBoard (GameModel a [] []) = (GameModel a [] [])
cleanBoard (GameModel a ls []) = (GameModel a (filter (\xs -> (plantHealth (snd(xs))) > 0) ls)  [])
cleanBoard (GameModel a [] ls2) = (GameModel a [] (filter (\xy -> (zombieHealth(snd(xy))) > 0) ls2) )
cleanBoard (GameModel a ls ls2) = (GameModel a (filter (\xs -> (plantHealth(snd(xs))) > 0) ls)  (filter (\xy -> (zombieHealth(snd(xy))) > 0) ls2) ) 

--healths :: GameModel -> [Int]
--healths (GameModel a [] []) = []
--healths (GameModel a ls []) = map (\xs -> (plantHealth(snd(xs)))) ls 
--healths (GameModel a [] ls2) = map (\xs -> (zombieHealth(snd(xs)))) ls2
--healths (GameModel a ls ls2) = (map (\xs -> (plantHealth(snd(xs)))) ls) ++ (map (\xs -> (zombieHealth(snd(xs)))) ls2)

