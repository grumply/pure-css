{-# LANGUAGE CPP,TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, OverloadedStrings, FlexibleContexts, PatternSynonyms, DeriveFunctor, ViewPatterns, RankNTypes, DataKinds, GADTs, CPP, DeriveLift, TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Pure.Data.CSS where

-- from ef
import Ef

-- from pure-core
import Pure.Data.View
import Pure.Data.View.Patterns

-- from pure-default
import Pure.Data.Default

-- from pure-txt
import Pure.Data.Txt as Txt

-- from base
import Data.Char
import Control.Category as C
import Data.Bifunctor
import Data.Foldable as F
import Data.Functor.Identity
import qualified Data.List as List
import Data.Monoid
import Data.String
import Data.Traversable
import Unsafe.Coerce
import Prelude hiding ((.),id)

-- from containers
import qualified Data.Map.Strict as M

-- from template-haskell
import qualified Language.Haskell.TH.Syntax as TH

import Data.String (IsString(..))

data Styles_ k where
  Style_ :: Txt -> Txt -> k -> Styles_ k
  deriving Functor

type Styles = Narrative Styles_ Identity

infixr 0 =:
(=:) :: Txt -> Txt -> Styles Txt
(=:) nm val = send (Style_ nm val val)

infixr 0 =*
(=*) :: Txt -> [Txt] -> Styles Txt
(=*) nm vals = let val = Txt.intercalate " " vals in nm =: val

comment :: Txt -> Styles Txt
comment com = (=:) ("//" <> com) ""

classify :: Txt -> Txt
classify = ("." <>) . toTxt

important :: Styles a -> Styles a
important s = buildn $ \r l d ->
  let go (Style_ k v x) = d (Style_ k (v <> " !important") x)
  in foldn r l go s

-- useful for debugging; keep the styles on the page, but block them with //
-- Can be used to comment out one or more styles
--
-- > ignore $ backgroundColor =: hideousOrange
--
-- > ignore $ do
-- >   backgroundColor =: blue
-- >   fontSize =: ems 1.3
ignore :: Styles a -> Styles a
ignore s = buildn $ \r l d ->
  let go (Style_ k v x) = d (Style_ ("// " <> k) v x)
  in foldn r l go s

renderStyles :: Bool -> Styles a -> ([Txt],a)
renderStyles b = first Prelude.reverse . runIdentity . flip (thread go) []
  where
    go (Style_ k v r) acc =
       r (((if b then "\t\t" else "\t") <> k <> ": " <> v):acc)

styled :: Styles a -> (Features -> Features)
styled = Styles . fst . runIdentity . flip (thread collect) mempty
  where
    collect (Style_ k v r) = r . ((k,v):)

getStyles :: Features -> Styles ()
getStyles = sequence_ . M.mapWithKey (=:) . styles

data CSS_ k where
  CSS_ :: Txt -> Styles a -> (a -> k) -> CSS_ k
  CSS3_ :: Txt -> Txt -> CSS a -> (a -> k) -> CSS_ k
  RawCSS_ :: Txt -> k -> CSS_ k

instance Functor CSS_ where
  fmap f (CSS_ t ss ak) = CSS_ t ss (fmap f ak)
  fmap f (CSS3_ n l cs ak) = CSS3_ n l cs (fmap f ak)
  fmap f (RawCSS_ c k) = RawCSS_ c (f k)

type CSS = Narrative CSS_ Identity
instance Default (CSS ()) where
  def = return ()

selector :: Txt -> Styles a -> CSS a
selector sel ss = send (CSS_ sel ss id)

apply :: Styles a -> CSS a
apply = selector ""

infixr 0 .>
(.>) :: (CSS a -> CSS b) -> Styles a -> CSS b
(.>) f decls = f $ apply decls

reusable :: Monad m => m a -> m (m a,a)
reusable ma = do
  a <- ma
  return (ma,a)

newtype Composable a = Composable { composes :: CSS a }
composable :: CSS a -> CSS (Composable a)
composable css = css >> return (Composable css)

newtype Extendable a = Extendable { extends :: Styles a }
extendable :: Styles a -> Styles (Extendable a)
extendable ss = ss >> return (Extendable ss)

rawCSS_ :: Txt -> CSS ()
rawCSS_ c = Do (RawCSS_ c (Return ()))

select :: Txt -> CSS a -> CSS a
select sel s = buildn $ \r l d ->
  let go (CSS_ suf ss rest) =
        d (CSS_ (sel <> suf) ss rest)

      go (CSS3_ at rule css rest) =
        d (CSS3_ at rule (select sel css) rest)

  in foldn r l go s

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
firstChild = ":fist-child"

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

is :: Txt -> CSS a -> CSS a
is = select

-- equivalent to id; purely for scanning purposes
and :: (Txt -> CSS a -> CSS a) -> Txt -> CSS a -> CSS a
and f sel css = f sel css

or :: (Txt -> CSS a -> CSS a) -> Txt -> CSS a -> CSS a
or f sel css = f (", " <> sel) css

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

compose :: (Category cat, Foldable t) => t (cat a a) -> cat a a
compose = F.foldr (>>>) id

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
atCharset cs = send (CSS3_ "@charset " cs (return ()) id)

utf8Charset :: CSS ()
utf8Charset = atCharset "UTF-8"

iso885915Charset :: CSS ()
iso885915Charset = atCharset "iso-8859-15"

atImport :: Txt -> CSS ()
atImport i = send (CSS3_ "@import " i (Return ()) id)

data Namespace = XHTMLNS | SVGNS

atNamespace :: Namespace -> Maybe Txt -> CSS ()
atNamespace ns mnsv = send (CSS3_ namespace_ ns_ (Return ()) id)
  where
    ns_ =
      case ns of
        XHTMLNS -> "url(http://www.w3.org/1999/xhtml)"
        SVGNS   -> "url(http://www.w3.org/2000/svg)"

    namespace_ =
      maybe "@namespace" ("@namespace " <>) mnsv

atMedia :: Txt -> CSS a -> CSS a
atMedia med c = send (CSS3_ "@media " med c id)

atPage :: Txt -> CSS a -> CSS a
atPage pgsel rls = send (CSS3_ "@page " pgsel rls id)

atFontFace :: Txt -> CSS a -> CSS a
atFontFace ff rls = send (CSS3_ "@font-face " ff rls id)

atWebkitKeyframes :: Txt -> CSS a -> CSS a
atWebkitKeyframes nm kfs = send (CSS3_ "@-webkit-keyframes " nm kfs id)

atKeyframes :: Txt -> CSS a -> CSS a
atKeyframes nm kfs = send (CSS3_ "@keyframes " nm kfs id)

-- data CSSError = InvalidCSSSyntax Txt deriving (Show)
-- instance Exception CSSError

newtype StaticCSS = StaticCSS { cssText :: Txt } deriving (Eq,Ord,TH.Lift)
instance ToTxt StaticCSS where
  {-# INLINE toTxt #-}
  toTxt (StaticCSS csst) = csst
instance FromTxt StaticCSS where
  {-# INLINE fromTxt #-}
  fromTxt = StaticCSS
instance Monoid StaticCSS where
  mempty = fromTxt mempty
#if !MIN_VERSION_base(4,11,0)
  mappend csst1 csst2 = fromTxt $ toTxt csst1 <> "\n" <> toTxt csst2
#else
instance Semigroup StaticCSS where
  (<>) csst1 csst2 = fromTxt $ toTxt csst1 <> "\n" <> toTxt csst2
#endif
mkRawCSS :: CSS () -> TH.Q TH.Exp
mkRawCSS c = [| rawCSS_ $(TH.lift $ toTxt c) |]

instance ToTxt (CSS a) where
  toTxt = fst . go "\n" False
    where
      go acc b (Return a) = (acc,a)
      go acc b (Lift s) = go acc b (runIdentity s)
      go acc b (Do msg) =
        case msg of
          CSS3_ atRule sel css k ->
            case css of
              Return a ->
                 go (acc <> atRule <> sel <> ";\n") False (k a)
              _ ->
                let (b,a) = go mempty True (unsafeCoerce css)
                in go (acc <> atRule <> sel <> " {\n" <> b <> "\n}\n\n") False (unsafeCoerce k a)
          CSS_ sel ss k ->
            let (s,a) = renderStyles b (unsafeCoerce ss)
                t = sel <> " {\n" <> Txt.intercalate ";\n" s <> if b then "\n\t}\n\n" else "\n}\n\n"
            in go (acc <> if b then "\t" <> t else t) b (unsafeCoerce k a)
          RawCSS_ css k ->
            go (acc <> css <> "\n") b k

staticCSS :: CSS a -> StaticCSS
staticCSS = fromTxt . toTxt

unindent :: Txt -> Txt
unindent =
  Txt.concat .
  removeIndentation .
  trimLastLine .
  removeLeadingEmptyLine .
  lines_
  where
    isEmptyLine :: Txt -> Bool
    isEmptyLine = Txt.all isSpace

    lines_ :: Txt -> [Txt]
    lines_ s =
      if Txt.null s
      then []
      else
        case Txt.span (/= '\n') s of
          (first, rest) ->
            case Txt.uncons rest of
              Just ('\n', more) -> (first <> Txt.pack "\n") : lines_ rest
              _ -> first : lines_ rest

    removeLeadingEmptyLine :: [Txt] -> [Txt]
    removeLeadingEmptyLine xs = case xs of
      y:ys | isEmptyLine y -> ys
      _ -> xs

    trimLastLine :: [Txt] -> [Txt]
    trimLastLine (a : b : r) = a : trimLastLine (b : r)
    trimLastLine [a] = if Txt.all (== ' ') a
      then []
      else [a]
    trimLastLine [] = []

    removeIndentation :: [Txt] -> [Txt]
    removeIndentation ys = List.map (dropSpaces indentation) ys
      where
        dropSpaces 0 s = s
        dropSpaces n s =
          case Txt.uncons s of
            Just (' ',r) -> dropSpaces (n - 1) r
            _ -> s
        indentation = minimalIndentation ys
        minimalIndentation =
            safeMinimum 0
          . List.map (Txt.length . Txt.takeWhile (== ' '))
          . removeEmptyLines
        removeEmptyLines = List.filter (not . isEmptyLine)

        safeMinimum :: Ord a => a -> [a] -> a
        safeMinimum x xs = case xs of
          [] -> x
          _ -> List.minimum xs

css :: Narrative CSS_ Identity a -> View
css = css' False

css' :: forall a e. Bool -> Narrative CSS_ Identity a -> View
css' b nar = SimpleHTML "style" <| Property "type" "text/css" . Property "scoped" (if b then "true" else "") |> ("\n" : (fst $ go False [] nar))
  where
    go :: forall a. Bool -> [View] -> Narrative CSS_ Identity a -> ([View],a)
    go b acc (Return a) = (acc,a)
    go b acc (Lift s) = go b acc (runIdentity s)
    go b acc c@(Do msg) =
      case msg of
        CSS3_ atRule sel css k ->
          case css of
            Return a ->
              go False (acc ++ [ txt (atRule <> sel <> ";\n") ]) (k a)
            _ ->
              let (c,a) = go True [] css
              in go False (acc ++ ( txt (atRule <> sel <> " {\n") : c) ++ [ "\n}\n\n" ]) (k a)
        CSS_ sel ss r ->
          let (s,a) = renderStyles b ss
          in
            go b  ( acc ++ [ txt ( (if b then "\t" else mempty)
                                      <> sel
                                      <> " {\n"
                                      <> (Txt.intercalate (if b then ";\n\t" else ";\n") s)
                                      <> (if b then "\n\t}\n\n" else "\n}\n\n")
                                  )
                           ]
                  ) (r a)
        RawCSS_ css k -> go b (acc ++ [ txt (css <> "\n") ]) k

scss :: StaticCSS -> View
scss = scss' False

scss' :: Bool -> StaticCSS -> View
scss' b = RawView Nothing "style" def { properties = M.fromList [("type","text/css"),("scoped",if b then "true" else "")] } . cssText

inlineCSS :: Narrative CSS_ Identity a -> View
inlineCSS = css' True . classify
  where
    classify :: forall a. Narrative CSS_ Identity a -> Narrative CSS_ Identity a
    classify (Return r) = Return r
    classify (Lift sup) = Lift (fmap classify sup)
    classify (Do e) =
      case e of
        CSS_ sel ss k ->
          Do (CSS_ (Txt.cons '.' sel) ss (classify . k))
        CSS3_ at sel css k ->
          Do (CSS3_ at sel (classify css) (classify . k))
        RawCSS_ css k ->
          Do (RawCSS_ css (classify k))

