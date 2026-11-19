type Potencia = Int 
type Ataque   = Int

data Red = Terminal Potencia 
         | Servidor Potencia Red 
         | Firewall (Ataque -> Bool) Red 
         | Switch (Ataque -> Dir) Red Red

data Dir = Izq | Der | Forward 
type Ruta = [Dir]

addFst :: (Int -> Int) -> (Int,b) -> (Int,b)
addFst f (n,y) = (f n, y)


-- ================================================================================= --
--                                  EJERCICIO 1                                      --
-- ================================================================================= --
--a)
combinarFirewalls :: Red -> Red 
combinarFirewalls (Terminal p)     = Terminal p
combinarFirewalls (Servidor p r)   = Servidor p (combinarFirewalls r)
combinarFirewalls (Firewall f r)   = compFirewall f (combinarFirewalls r)
combinarFirewalls (Switch f r1 r2) = Switch f (combinarFirewalls r1) (combinarFirewalls r2)

compFirewall :: (Ataque -> Bool) -> Red -> Red
compFirewall f (Firewall g r) = Firewall (\a -> f a && g a) r 
compFirewall f r              = Firewall f r

--b)
intensidadRestante :: Ataque -> Red -> Maybe Ataque
intensidadRestante a (Terminal p)     = Just (a-p)
intensidadRestante a (Servidor p r)   = intensidadRestante (a+p) r
intensidadRestante a (Firewall f r)   = if f a then intensidadRestante a r else Nothing
intensidadRestante a (Switch f ri rd) = case f a of 
                                          Der -> case intensidadRestante a rd of 
                                                  Nothing -> Nothing 
                                                  Just a' -> intensidadRestante a' ri
                                          
                                          _   -> case intensidadRestante a ri of 
                                                  Nothing -> Nothing 
                                                  Just a' -> intensidadRestante a' rd


--c)
pasosHastaBloqueo :: Ataque -> Red -> Maybe Int 
pasosHastaBloqueo a r = case recorrido r a of 
                         (i, Nothing) -> Just i 
                         (i, Just x)  -> Nothing 


recorrido :: Red -> Ataque -> (Int, Maybe Ataque)
recorrido (Terminal p) a     = (1, Just (a-p)) 
recorrido (Servidor p r) a   = addFst (+1) (recorrido r (a+p))
recorrido (Firewall f r) a   = if f a then addFst (+1) (recorrido r a) else (0, Nothing)
recorrido (Switch f ri rd) a = case f a of 
                                Der -> case recorrido rd a of 
                                        (p, Nothing) -> (p+1, Nothing)
                                        (p, Just a') -> addFst (+(p+1)) (recorrido ri a')
                                _   -> case recorrido ri a of 
                                        (p, Nothing) -> (p+1, Nothing)
                                        (p, Just a') -> addFst (+(p+1)) (recorrido rd a')


--d)
rutasAServidores :: Ataque -> Red -> [Ruta]
rutasAServidores a r = fst (maybeRutas a r)

maybeRutas :: Ataque -> Red -> ([Ruta], Maybe Ataque)
maybeRutas a (Terminal p)     = ([], Just (a-p)) 
maybeRutas a (Servidor p r)   = if a >= p 
                                then let (rs,m) = maybeRutas (a+p) r in ([] : map (Forward:) rs, m)
                                else let (rs,m) = maybeRutas a r in (map (Forward:) rs, m)
maybeRutas a (Firewall f r)   = if f a 
                                then let (rs,m) = maybeRutas a r in (map (Forward:) rs, m)
                                else ([], Nothing)
maybeRutas a (Switch f ri rd) = case f a of 
                                 Der -> case maybeRutas a rd of 
                                         (rs, Nothing) -> (map (Der:) rs, Nothing)
                                         (rs, Just a') -> let (r2,m) = maybeRutas a' ri in (map (Der:) rs ++ map (Izq:) r2,m)
                                 
                                 _   -> case maybeRutas a ri of 
                                         (rs, Nothing) -> (map (Izq:) rs, Nothing)
                                         (rs, Just a') -> let (r2,m) = maybeRutas a' rd in (map (Izq:) rs ++ map (Der:) r2, m)
