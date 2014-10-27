{-# LANGUAGE MultiParamTypeClasses #-}

-- GL TYPES
data Type = HUMN |		   -- human
     	    ANIM |		   -- animate
	    ORGN | 		   -- organic
	    ORGZ |		   -- organization
	    PHYS |		   -- physical object
	    ARTF |		   -- artifact
	    EVNT |		   -- event
	    PROP |		   -- proposition
	    INFO |		   -- information
	    SENS |		   -- sensation
	    LOCA |		   -- location
	    TIME |		   -- time period
	    ATTD |		   -- attitude
	    EMOT |		   -- emotion
	    PPTY |		   -- property
	    OBLG |		   -- obligation
	    RULE 		   -- rule
	    deriving (Show, Eq, Enum)

-- CUSTOM DATA TYPES
data Argument = Argument { ttype :: Type, value :: String } deriving (Show, Eq)
data Predicate = Predicate { lemma :: String, arguments :: [Argument] } deriving (Show, Eq)

type Context = [Argument]

-- CREATE SEMANTICALLY TYPED ARGUMENTS AS FOLLOWS
date :: String -> Argument
date s = Argument { ttype = TIME, value = s }

time :: String -> Argument
time s = Argument { ttype = TIME, value = s }

location :: String -> Argument
location s = Argument { ttype = LOCA, value = s }

human :: String -> Argument
human s = Argument { ttype = HUMN, value = s }

phys :: String -> Argument
phys s = Argument { ttype = PHYS, value = s }

artifact :: String -> Argument
artifact s = Argument { ttype = ARTF, value = s }

animate :: String -> Argument
animate s = Argument { ttype = ANIM, value = s }

-- CREATE ENTITIES/PPs AS FOLLOWS
may15 = date "May 15, 2014"
sevenAM = time "7:00"
sandiego = location "San Diego"
john = human "John"
mary = human "Mary"
boston = location "Boston"
ball = phys "ball"
car = artifact "car"
cat = animate "cat"
mouse = animate "mouse"
to_boston = to boston

-- CONTEXT
-- you can consider this the "here and now": a list of facts ("is" predicates) that are true about the current state of the world

context = [
		may15,
		sevenAM,
		sandiego,
		john,
		mary,
		boston,
		ball,
		cat,
		mouse
	  ]

-- HELPER FUNCTIONS
getValue :: Argument -> String
getValue c = value c

getType :: Argument -> Type
getType c = ttype c

isType :: Argument -> Type -> Bool
isType c t = (ttype c == t)

-- CREATE PREPOSITIONS AS FOLLOWS
to :: Argument -> Predicate
to x = Predicate { lemma = "to", arguments = [x] }
		

-- CREATE VERBS AS FOLLOWS

--Verb "Fly"
class Fly a b where
      fly :: a -> b -> Predicate

instance Fly Argument Argument where
      fly x y = Predicate { lemma = "fly", arguments = [x, y] }

instance Fly Argument Predicate where
      fly x y = Predicate { lemma = "fly", arguments = [x, arguments y !! 0] }

--Verb "Catch"
class Catch a b where
	catch :: a -> b -> Predicate

instance Catch Argument Argument where
	catch x y = Predicate {lemma = "catch", arguments = [x, y]}

--Verb "Drive"
class Drive a b where
	drive :: a -> b -> Predicate

instance Drive Argument Argument where
	drive x y = Predicate {lemma = "drive", arguments = [x, y]}

instance Drive Argument Predicate where
     drive x y = Predicate { lemma = "drive", arguments = [x, arguments y !! 0] }

-- POPULATE THE KNOWLEDGE BASE AS FOLLOWS
facts =
	      [fly john to_boston]
	      ++ [catch cat mouse]	-- add your other facts


-- implement functions that can answer the following questions about your fact base
-- where (e.g. qWhere fly john -- boston OR to_boston, depending on how you compute your answer)
-- who (e.g. qWho fly to_boston -- john)
-- what (e.g. qWhat throw boy -- ball)
-- when (e.g. qWhen fly john -- 7:00 AM or at_7, a predicate representing at + 7:00 AM)


getLemmas :: [String]
getLemmas = map lemma facts

--Functions for getting first or second argument, for a predicate
getFirstArg :: Predicate -> Argument
getFirstArg predicate = head (arguments predicate)

getSecondArg :: Predicate -> Argument
getSecondArg predicate = last (arguments predicate)


-- extra credit:
-- how (e.g. qHow john boston -- fly)
-- CREATE QUESTION-ANSWERING FUNCTIONS HERE

qWhere :: (Argument -> Predicate -> Predicate) -> Argument -> [String]
qWhere verb arg = [getValue (getSecondArg (x)) | x <- facts] 






