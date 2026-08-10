import Prelude hiding (Bool, False, True)

data Bool = False | True 

instance Enum Bool where
    fromEnum False = 0
    fromEnum True  = 1

    toEnum 0 = False 
    toEnum 1 = True