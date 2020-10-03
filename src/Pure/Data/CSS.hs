{-# LANGUAGE CPP,TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, OverloadedStrings, FlexibleContexts, PatternSynonyms, DeriveFunctor, ViewPatterns, RankNTypes, DataKinds, GADTs, CPP, DeriveLift, TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Pure.Data.CSS where

import Pure.Data.View ( View )
import Pure.Data.View.Patterns ( txt, (<|), (|>), pattern Property, pattern SimpleHTML)
import Ef ( send, Narrative(..) )
import Pure.Data.Txt as Txt ( intercalate, replicate, null, toTxt, Txt )
import Data.Functor.Identity ( Identity(runIdentity) )
import Control.Monad ( void )
import Data.Traversable (for)

data CSS_ k where
  Raw_    :: Txt -> k -> CSS_ k
  Scope_  :: (Txt -> k) -> CSS_ k
  Selection_ :: Txt -> CSS a -> (a -> k) -> CSS_ k
  Wrap_   :: Txt -> CSS a -> (a -> k) -> CSS_ k
  Style_  :: Txt -> Txt -> k -> CSS_ k

instance Functor CSS_ where
  fmap f (Raw_ r k) = Raw_ r (f k)
  fmap f (Scope_ g) = Scope_ (f . g)
  fmap f (Selection_ sel scoped ak) = Selection_ sel scoped (f . ak)
  fmap f (Wrap_ rule scoped ak) = Wrap_ rule scoped (f . ak)
  fmap f (Style_ k v a) = Style_ k v (f a)

type CSS = Narrative CSS_ Identity

-- * Commands

scope :: CSS Txt
scope = send (Scope_ id)

rawCSS :: Txt -> CSS ()
rawCSS r = send (Raw_ r ())

infixr 0 =:
(=:) :: Txt -> Txt -> CSS Txt
(=:) nm val = send (Style_ nm val val)

infixr 0 =*
(=*) :: Txt -> [Txt] -> CSS Txt
(=*) nm (Txt.intercalate " " -> val) = nm =: val

-- designed to maintain the old API
important :: CSS a -> CSS a
important (Return a) = Return a
important (Lift l) = Lift (fmap important l)
important (Do msg) =
  case msg of
    Raw_ r k -> Do (Raw_ r k)
    Scope_ g -> Do (Scope_ (fmap important g))
    Selection_ sel scoped ak -> Do (fmap important (Selection_ sel (important scoped) ak))
    Wrap_ rule scoped ak -> Do (fmap important (Wrap_ rule (important scoped) ak))
    Style_ k v a -> Do (fmap important (Style_ k (v <> " !important") a))


-- | The core mechanism used to implement basic CSS selectors. See `is`, `has`,
-- `next`, `child`.
--
-- > select ".btn" do
-- >   ...
--
-- Produces:
--
-- > .btn {
-- >   ...
-- > }
--
select :: Txt -> CSS a -> CSS a
select sel scoped = send (Selection_ sel scoped id)

-- | `rescope` allows the implementation of @_ queries by re-wrapping
-- the current scope at the root level, making it possible to
-- wrap a scope block with a CSS3 selector:
--
-- > at @SomeClass do
-- >   rescope "@media screen and (min-width: 400px)" do
-- >     ...
--
-- Produces:
--
-- > @media screen and (min-width: 400px) {
-- >   .SomeClass {
-- >     ...
-- >   }
-- > }
rescope :: Txt -> CSS a -> CSS a
rescope rule scoped = send (Wrap_ rule scoped id)

{-# DEPRECATED apply, (.>), (..>) "apply, (.>), and (..>) are no longer necessary to introduce a styling scope as the styling scope and selection scopes have been merged." #-}
apply :: CSS a -> CSS a
apply = id
infixr 0 .>
(.>) = ($)
infixr 0 ..>
(..>) = ($)


-- * Renderers

stylesheet :: CSS a -> Txt
stylesheet = start
  where
    start :: CSS x -> Txt
    start (Return _) = ""
    start (Lift i) = start (runIdentity i)
    start (Do msg) =
      case msg of
        Raw_ r k -> r <> "\n" <> start k
        
        Scope_ f -> start (f "")

        -- Couldn't figure a way to disallow this case; it is an artifact of 
        -- the unification of the css and style DSLs.
        Style_ _ _ k -> start k

        Selection_ sel scoped k -> 
          let (res,a) = selecting 0 sel "" "" scoped 
           in res <> start (k a)
        Wrap_ rule scoped k ->
          let (res,a) = selecting 1 "" "" "" scoped
           in rule <> " {\n" <> res <> "}\n\n" <> start (k a)


    selecting :: Int -> Txt -> Txt -> Txt -> CSS a -> (Txt,a)
    selecting depth sel acc rest (Return a) = 
      case (Txt.null acc,Txt.null rest) of
        (True,True) -> (mempty,a)
        (True,_   ) -> (rest,a)
        _           -> (Txt.replicate depth "\t" <> sel <> " {\n" <> acc <> Txt.replicate depth "\t" <> "}\n\n" <> rest,a)
    selecting depth sel acc rest (Lift l) = selecting depth sel acc rest (runIdentity l)
    selecting depth sel acc rest (Do msg) =
      case msg of
        Scope_ f -> selecting depth sel acc rest (f sel)
        Raw_ r k -> selecting depth sel acc (rest <> r <> "\n") k
        Style_ k v cont -> selecting depth sel (acc <> (Txt.replicate (depth + 1) "\t") <> k <> ": " <> v <> ";\n") rest cont
        Selection_ sel' scoped k -> 
          let (res,a) = selecting depth (sel <> sel') "" "" scoped
           in selecting depth sel acc (rest <> res) (k a)
        Wrap_ rule scoped k ->
          let (res,a) = selecting (depth + 1) sel "" "" scoped
           in selecting depth sel acc (rest <> Txt.replicate depth "\t" <> rule <> " {\n" <> res <> Txt.replicate depth "\t" <> "}\n\n" ) (k a)

css :: CSS a -> View
css = css' False

css' :: Bool -> CSS a -> View
css' scoped sheet = 
  SimpleHTML "style" <| Property "type" "text/css" . Property "scoped" (if scoped then "true" else "") |>
    [ txt (stylesheet sheet) ]

any :: Txt 
any = "*"

active :: Txt
active = ":active"

visited :: Txt
visited = ":visited"

hover :: Txt
hover = ":hover"

focus :: Txt
focus = ":focus"

disabled :: Txt
disabled = ":disabled"

link :: Txt
link = ":link"

empty :: Txt
empty = ":empty"

checked :: Txt
checked = ":checked"

enabled :: Txt
enabled = ":enabled"

firstChild :: Txt
firstChild = ":first-child"

firstOfType :: Txt
firstOfType = ":first-of-type"

inRange :: Txt
inRange = ":in-range"

invalid :: Txt
invalid = ":invalid"

lastChild :: Txt
lastChild = ":last-child"

onlyOfType :: Txt
onlyOfType = ":only-of-type"

onlyChild :: Txt
onlyChild = ":only-child"

optional :: Txt
optional = ":optional"

outOfRange :: Txt
outOfRange = ":out-of-range"

readOnly :: Txt
readOnly = ":read-only"

readWrite :: Txt
readWrite = ":read-write"

required :: Txt
required = ":required"

root :: Txt
root = ":root"

target :: Txt
target = ":target"

valid :: Txt
valid = ":valid"

before :: Txt
before = "::before"

after :: Txt
after = "::after"

firstLetter :: Txt
firstLetter = "::first-letter"

firstLine :: Txt
firstLine = "::first-line"

selection :: Txt
selection = "::selection"

is :: Txt -> CSS a -> CSS ()
is s c = void (select s c)

is' :: Txt -> CSS a -> CSS a
is' = select

and :: (Txt -> CSS a -> CSS b) -> Txt -> CSS a -> CSS b
and f = f

or :: (Txt -> CSS a -> CSS b) -> Txt -> CSS a -> CSS b
or f sel = f (", " <> sel) 

isn't :: Txt -> CSS a -> CSS a
isn't sel = select (":not(" <> sel <> ")")

lang :: Txt -> CSS a -> CSS a
lang sel = select (":lang(" <> sel <> ")")

nthChild :: Int -> CSS a -> CSS a
nthChild i = select (":nth-child(" <> toTxt i <> ")")

nthChildCalc :: Txt -> CSS a -> CSS a
nthChildCalc i = select (":nth-child(" <> i <> ")")

nthLastChild :: Int -> CSS a -> CSS a
nthLastChild i = select (":nth-last-child(" <> toTxt i <> ")")

nthOfType :: Int -> CSS a -> CSS a
nthOfType i = select (":nth-of-type(" <> toTxt i <> ")")

nthLastOfType :: Int -> CSS a -> CSS a
nthLastOfType i = select (":nth-last-of-type(" <> toTxt i <> ")")

use :: (Traversable t, Applicative f) => t (a -> f b) -> a -> f (t b)
use fs x = for fs ($ x)

pseudo :: Txt -> CSS a -> CSS a
pseudo sel = select (":" <> sel)

attr :: Txt -> CSS a -> CSS a
attr sel = select ("[" <> sel <> "]")

child :: Txt -> CSS a -> CSS a
child sel = select (" > " <> sel)

has :: Txt -> CSS a -> CSS a
has sel = select (" " <> sel)

next :: Txt -> CSS a -> CSS a
next sel = select (" + " <> sel)

nexts :: Txt -> CSS a -> CSS a
nexts sel = select (" ~ " <> sel)

atCharset :: Txt -> CSS ()
atCharset cs = rawCSS ("@charset " <> cs)

utf8Charset :: CSS ()
utf8Charset = atCharset "\"UTF-8\";"

iso885915Charset :: CSS ()
iso885915Charset = atCharset "\"iso-8859-15\";"

atImport :: Txt -> CSS ()
atImport i = rescope ("@import " <> i) (pure ())

data Namespace = XHTMLNS | SVGNS

atNamespace :: Namespace -> Maybe Txt -> CSS ()
atNamespace ns mnsv = rescope (namespace_ <> ns_) (pure ())
  where
    ns_ =
      case ns of
        XHTMLNS -> "url(http://www.w3.org/1999/xhtml)"
        SVGNS   -> "url(http://www.w3.org/2000/svg)"

    namespace_ = 
      let n = "@namespace " in maybe n (n <>) mnsv

atMedia :: Txt -> CSS a -> CSS a
atMedia med = rescope ("@media " <> med)

atPage :: Txt -> CSS a -> CSS a
atPage pg = rescope ("@page " <> pg)

atFontFace :: Txt -> CSS a -> CSS a
atFontFace ff = rescope ("@font-face " <> ff)

atKeyframes :: Txt -> CSS a -> CSS a
atKeyframes nm = rescope ("@keyframes " <> nm)
