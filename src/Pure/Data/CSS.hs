{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, OverloadedStrings, FlexibleContexts, PatternSynonyms, DeriveFunctor, ViewPatterns, RankNTypes, DataKinds, GADTs, CPP #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
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
#ifdef USE_TEMPLATE_HASKELL
import qualified Language.Haskell.TH.Syntax as TH
#endif

data Styles_ k where
  Style_ :: Txt -> Txt -> k -> Styles_ k
  deriving Functor

type Styles = Narrative Styles_ Identity

infixr 5 =:
(=:) :: Txt -> Txt -> Styles Txt
(=:) nm val = send (Style_ nm val val)

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

infixr 1 .>
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

before :: Txt
before = ":before"

after :: Txt
after = ":after"

active :: Txt
active = ":active"

hovered :: Txt
hovered = ":hover"

focused :: Txt
focused = ":focus"

disabled :: Txt
disabled = "[disabled]"

is :: Txt -> CSS a -> CSS a
is = select

is_ :: Txt -> CSS a -> CSS ()
is_ t c = void (is t c)

-- equivalent to id; purely for scanning purposes
and :: (Txt -> CSS a -> CSS a) -> Txt -> CSS a -> CSS a
and f sel css = f sel css

or :: (Txt -> CSS a -> CSS a) -> Txt -> CSS a -> CSS a
or f sel css = f (", " <> sel) css

isn't :: Txt -> CSS a -> CSS a
isn't sel = select (":not(" <> sel <> ")")

isn't_ :: Txt -> CSS a -> CSS ()
isn't_ sel css = void (isn't sel css)

compose :: (Category cat, Foldable t) => t (cat a a) -> cat a a
compose = F.foldr (>>>) id

use :: (Traversable t, Applicative f) => t (a -> f b) -> a -> f (t b)
use fs x = for fs ($ x)

pseudo :: Txt -> CSS a -> CSS a
pseudo sel = select (":" <> sel)

pseudo_ :: Txt -> CSS a -> CSS ()
pseudo_ sel c = void (pseudo sel c)

attr :: Txt -> CSS a -> CSS a
attr sel = select ("[" <> sel <> "]")

attr_ :: Txt -> CSS a -> CSS ()
attr_ sel c = void (attr sel c)

child :: Txt -> CSS a -> CSS a
child sel = select (" > " <> sel)

child_ :: Txt -> CSS a -> CSS ()
child_ sel c = void (child sel c)

has :: Txt -> CSS a -> CSS a
has sel = select (" " <> sel)

has_ :: Txt -> CSS a -> CSS ()
has_ sel css = void (has sel css)

next :: Txt -> CSS a -> CSS a
next sel = select (" + " <> sel)

next_ :: Txt -> CSS a -> CSS ()
next_ sel css = void (next sel css)

nexts :: Txt -> CSS a -> CSS a
nexts sel = select (" ~ " <> sel)

nexts_ :: Txt -> CSS a -> CSS ()
nexts_ sel css = void (nexts sel css)

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

atMedia_ :: Txt -> CSS a -> CSS ()
atMedia_ med c = void (atMedia med c)

atPage :: Txt -> CSS a -> CSS a
atPage pgsel rls = send (CSS3_ "@page " pgsel rls id)

atPage_ :: Txt -> CSS a -> CSS ()
atPage_ med c = void (atPage med c)

atFontFace :: Txt -> CSS a -> CSS a
atFontFace ff rls = send (CSS3_ "@font-face " ff rls id)

atFontFace_ :: Txt -> CSS a -> CSS ()
atFontFace_ ff rls = void (atFontFace ff rls)

atWebkitKeyframes :: Txt -> CSS a -> CSS a
atWebkitKeyframes nm kfs = send (CSS3_ "@-webkit-keyframes " nm kfs id)

atWebkitKeyframes_ :: Txt -> CSS a -> CSS ()
atWebkitKeyframes_ nm kfs = void (atWebkitKeyframes nm kfs)

atKeyframes :: Txt -> CSS a -> CSS a
atKeyframes nm kfs = send (CSS3_ "@keyframes " nm kfs id)

atKeyframes_ :: Txt -> CSS a -> CSS ()
atKeyframes_ nm kfs = void (atKeyframes nm kfs)

-- data CSSError = InvalidCSSSyntax Txt deriving (Show)
-- instance Exception CSSError

data StaticCSS = StaticCSS { cssText :: {-# UNPACK #-} !Txt } deriving (Eq,Ord)
instance ToTxt StaticCSS where
  toTxt (StaticCSS csst) = csst
instance FromTxt StaticCSS where
  fromTxt = StaticCSS
instance Monoid StaticCSS where
  mempty = fromTxt mempty
  mappend csst1 csst2 = fromTxt $ toTxt csst1 <> "\n" <> toTxt csst2
#ifdef USE_TEMPLATE_HASKELL
instance TH.Lift StaticCSS where
  lift (StaticCSS csst) = [| StaticCSS csst |]
mkRawCSS :: CSS () -> TH.Q TH.Exp
mkRawCSS c =
  let x = toTxt (staticCSS c)
  in [|rawCSS_ x|]
#endif

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
css' b nar = SimpleHTML "style" <| Property "type" "text/css" . Property "scoped" (if b then "true" else "") |> ((txt "\n"): (fst $ go False [] nar))
  where
    go :: forall a. Bool -> [View] -> Narrative CSS_ Identity a -> ([View],a)
    go b acc (Return a) = (acc,a)
    go b acc (Lift s) = go b acc (runIdentity s)
    go b acc c@(Do msg) =
      case msg of
        CSS3_ atRule sel css k ->
          case css of
            Return a ->
              go False (acc ++ [ text (atRule <> sel <> ";\n") ]) (k a)
            _ ->
              let (c,a) = go True [] css
              in go False (acc ++ ( txt (atRule <> sel <> " {\n") : c) ++ [ txt "\n}\n\n" ]) (k a)
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

