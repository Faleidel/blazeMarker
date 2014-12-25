{-# LANGUAGE OverloadedStrings, FlexibleInstances, ExistentialQuantification #-}

{-|
Following the shell monad ( http://hackage.haskell.org/package/shell-monad ) the ASM monad ( http://wall.org/~lewis/2013/10/15/asm-monad.html ) and the brainfuck monad ( http://hackage.haskell.org/package/brainfuck-monad ) here is a very experimental FreeMarker monad.

Example use:

> test = FM.renderFM $ do
>     bar <- FM.dec $ FM.litS "string blablabla"
>     lol <- FM.und "varName" $ FM.litN 123
>     FM.exlitl
>     FM.litH $ do
>         B5.div $ do
>             R$<< bar
>             th "test2"
>     FM.lit "test"
>     foo <- FM.dec $ FM.true
>     FM.ifel (foo =~ FM.false)
>         (do
>             FM.litH $ do
>                 B5.div $ do
>                     th "ok true div"
>         )
>         (FM.lit "false")
>     FM.list ( FM.litL $ litN 123 >: litN 321 >: [] ) $ \vi -> do
>         D$< vi
>     FM.list ( FM.litNL [12,321,32] ) $ \vi -> do
>         D$< vi
-}
module Control.Monad.FreeMarker where

import Data.Monoid
import Control.Applicative
import Data.String
import Data.List

import Text.Blaze.Internal (preEscapedString)
import qualified Text.Blaze.Html.Renderer.Pretty as BR
import qualified Text.Blaze.Html5 as B5

litH :: B5.Html -> FM ()
litH h = lit $ BR.renderHtml h

type VarCounter = Int

data FM a = FM (VarCounter -> (String, VarCounter, a))

data FEX a b = FEX String

data ENum
data EString
data EBool
data EList a
data EDate
data EHash

data EName
data E

true :: FEX E EBool
true = FEX "true"

false :: FEX E EBool
false = FEX "false"

now :: FEX E EDate
now = FEX ".now"

usF :: String -> FEX a b -> FEX c d
usF s e = FEX ( s ++ parShow e )

usM :: FEX a b -> String -> FEX c d
usM e s = FEX ( parShow e ++ s )

parShow :: FEX a b -> String
parShow a = "(" ++ show a ++ ")"

(>~) :: FEX a b -> FEX c b -> FEX E EBool
a >~ b = FEX ( parShow a ++ " > " ++ parShow b )

(>=~) :: FEX a b -> FEX c b -> FEX E EBool
a >=~ b = FEX ( parShow a ++ " >= " ++ parShow b )

(<~) :: FEX a b -> FEX c b -> FEX E EBool
a <~ b = FEX ( parShow a ++ " < " ++ parShow b )

(<=~) :: FEX a b -> FEX c b -> FEX E EBool
a <=~ b = FEX ( parShow a ++ " <= " ++ parShow b )

(=~) :: FEX a b -> FEX c b -> FEX E EBool
a =~ b = FEX ( parShow a ++ " == " ++ parShow b )

(&~) :: FEX a EBool -> FEX b EBool -> FEX E EBool
a &~ b = FEX ( parShow a ++ " && " ++ parShow b )

instance Show (FEX a b) where
    show (FEX s) = s

--instance IsString (FEX a b) where
--    fromString s = FEX ("\""++s++"\"")
--
--instance Num (FEX a b) where
--    fromInteger i = FEX $ show i
--    negate    (FEX s) = FEX $ "( - ("++s++") )"
--    abs       s = FEX $ "Math.abs" ++ parShow s
--    a - b = FEX ( parShow a ++ " - " ++ parShow b )
--    a + b = FEX ( parShow a ++ " + " ++ parShow b )
--    a * b = FEX ( parShow a ++ " * " ++ parShow b )
--    -- I don't event know what it is supposed to do...
--    signum x = undefined

--instance Show (FM a) where
--    show (FM s _ _) = s

renderFM :: FM a -> String
renderFM f = renderFMW f 0

renderFMW :: FM a -> VarCounter -> String
renderFMW (FM a) v = let (s,_,_) = a v in s

-- This undefined is far from a good idéa... But IsString (FM ()) give me
-- ambiguous decision when using it without type annotation :(
instance IsString (FM a) where
    fromString s = FM (\v -> (s, v, undefined))

lit :: String -> FM ()
lit s = FM (\v -> (s, v, ()))

lite :: String -> FEX a b
lite s = FEX s

size :: FEX a (EList b) -> FEX E ENum
size e = usM e "?size"

has :: FEX a EHash -> String -> FEX E EBool
has e n = usM (FEX ( parShow e ++ "." ++ n )) "??"

data LC b = forall a. LC (FEX a b)

instance Show (LC b) where
    show (LC a) = show a

type LitLET a = FEX E (EList a)

litLEmpty :: FEX E (EList a)
litLEmpty = FEX "[]"

litL :: [LC b] -> FEX E (EList b)
litL l = FEX ( "["++ i ++"]" )
  where
    i = concat $ intersperse "," (map show l)

litNL :: (Num a, Show a) => [a] -> FEX E (EList ENum)
litNL l = litL $ map (LC . litN) l

litSL :: [String] -> FEX E (EList EString)
litSL l = litL $ map (LC . litS) l

litBL :: [Bool] -> FEX E (EList EBool)
litBL l = litL $ map (LC . litB) l

(>:) :: FEX a b -> [LC b] -> [LC b]
e >: l = ( LC e ):l
infixr 1 >:

exlitl = do
    foo <- dec $ litS "afds"
    D$< (litL [ LC $ litS "asd" , LC $ foo ])
    D$< (litL (  litS "asd" >:
                 litS "lol" >: [] ))

list :: FEX a (EList lt) -> (FEX EName lt -> FM b) -> FM ()
list l b = FM (\v ->
    let
        i = FEX ("var" ++ (show v))
        startList = "[#list "++ show l ++" as "++ show i ++"]\n"
        endList = "\n[/#list]"
        code = startList ++ renderFMW (b i) v ++ endList
        in
    (code, succ v, ())
  )


if_ :: FEX a EBool -> FM b -> FM ()
if_ (FEX cond) b1 = FM (\v ->
    let ns = (si cond) ++ renderFMW b1 v ++ ei in
    (ns,v,())
  )
  where
    si x = "[#if "++x++"]"
    ei = "\n[/#if]"

ifel :: FEX a EBool -> FM b -> FM c -> FM ()
ifel (FEX cond) b1 b2 = FM (\v ->
    let ns = (si cond) ++ renderFMW b1 v ++ eli ++ renderFMW b2 v ++ ei in
    (ns, v, ())
  )
  where
    si x = "[#if "++x++"]\n"
    eli = "\n[#else]\n"
    ei = "\n[/#if]"

getVarCounter :: FM VarCounter
getVarCounter = FM ( \v -> ("",v,v) )

dec :: FEX a b -> FM (FEX EName b)
dec ex = namedDec "var" ex

namedDec :: String -> FEX a b -> FM (FEX EName b)
namedDec n ex = do
    vc <- getVarCounter
    unsafeNamedDeclaration (n ++ show vc) ex

und :: String -> FEX a b -> FM (FEX EName b)
und n ex = unsafeNamedDeclaration n ex

unsafeNamedDeclaration :: String -> FEX a b -> FM (FEX EName b)
unsafeNamedDeclaration n ex = FM (\v ->
    let
       name = n
       code = "[#assign " ++ name ++ " = " ++ show ex ++ "/]"
      in
    (code, (succ v), (FEX name))
   )

assign :: FEX EName a -> FEX b a -> FM ()
assign (FEX name) ex = FM (\v -> (code, v, ()))
  where
    code = "[#assign " ++ name ++ " = " ++ show ex ++ "/]"

urlEncode :: FEX a EString -> FEX E EString
urlEncode p = FEX $ parShow p ++ "?url(\"UTF-8\")"

def :: FEX a b -> FEX a b -> FEX a b
def a b = FEX ( parShow a ++ "!" ++ parShow b )

data D = D
data R = R

-- Should be used as : D$< "potatoe"
-- The "D" is only for syntax
($<) :: D -> FEX a b -> FM ()
y $< x = FM ( \v -> (  ("${" ++ show x ++ "}"), v, ()  ) )

($<<) :: R -> FEX a b -> B5.Html
y $<< x = preEscapedString $ renderFM $ FM ( \v -> (  ("${" ++ show x ++ "}"), v, ()  ) )

instance Functor FM where
    fmap f (FM o) = FM (\v -> let (s,v2,a) = o v in (s,v2,f a))

instance Applicative FM where
    pure = return
    (FM f1) <*> (FM f2) = r
      where
        r = FM (\v -> let
                          (s2,v2,a2) = f1 v
                          (s3,v3,a3) = f2 v in
               ( s2 ++ s3 , v2 + v3 , a2 a3 ))

instance Monad FM where
    return x = FM (\v -> ("", v, x) )
    (FM fm1) >>= f = FM (\v ->
      let
         (s,v2,a1)  = fm1 v
         (FM fm2)   = f a1
         (s2,v3,a2) = fm2 v2
       in
      ( s ++ "\n" ++ s2 , v3, a2 )
     )

(-~) :: FEX a ENum -> FEX a ENum -> FEX E ENum
f1 -~ f2 = FEX ( parShow f1 ++ " - " ++ parShow f2 )

(+~) :: FEX a ENum -> FEX a ENum -> FEX E ENum
f1 +~ f2 = FEX ( parShow f1 ++ " + " ++ parShow f2 )

litN :: (Show a, Num a) => a -> FEX E ENum
litN x = FEX $ show x

litS :: String -> FEX E EString
litS x = FEX ( "\"" ++ x ++ "\"" )

litB :: Bool -> FEX E EBool
litB True  = FEX "true"
litB False = FEX "false"

test = do
    foo <- dec $ litB True
    bar <- dec $ litN 123
    assign foo true
    ifel ( litN (-1) =~ bar )
        (do
            "foo :("
        )
        (do
            "foo is : "
            (D$< foo)
        )
    ifel foo
        "t2"
        "t3"
    "test"
    D$< (urlEncode $ litS "<lol>\"\"")
