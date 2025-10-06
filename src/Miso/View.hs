{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Miso.View (
    V,
    InputType (..),
    toView,
    liftV,
    div,
    p,
    table,
    thead,
    tr,
    th,
    tbody,
    span,
    input,
    i,
    button,
    form,
    h1,
    submit,
    toHtml,
    toDiv,
    class_,
    nav,
    section,
    liftAttr,
    role,
    liftVF,
    txt,
    for,
    (>$<),
    ($<),
    td,
    if_,
    type_,
    onInput,
    name,
    label,
    wrapAttr,
    mapChildren,
    (>>),
) where

import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import Miso.String hiding (map, span)
import Miso.Types (Attribute, View, text)
import Prelude hiding ((>>), div, span, unwords)
import Data.String (IsString (..))
import Data.Profunctor (Profunctor (..), Choice (..))
import Control.Category (Category, (>>>))

data Elem p a = Elem
    { attrs :: [Attribute a]
    , children :: [View p a]
    }
    deriving (Functor)

instance Semigroup (Elem p a) where
    (Elem a1 c1) <> (Elem a2 c2) = Elem (a1 <> a2) (c1 <> c2)

instance Monoid (Elem p a) where
    mempty = Elem mempty mempty

-- the type variables in View are improperly named. 'View model action' should be named 'View parent action'

-- | Alternative type for constructing views using do notation
newtype V p m a = V { apV :: m -> Elem p a }
    deriving newtype (Semigroup, Monoid)

instance Profunctor (V p) where
    dimap f g (V h) = V $ dimap f (fmap g) h

instance Functor (V p m) where
    fmap = rmap

instance IsString (V p m a) where
    fromString = liftV . text . fromString

-- This is used for QualifiedDo
(>>) :: V p m a -> V p m a -> V p m a
(>>) = (<>)

-- | Convert from a V interface to a View interface
toView :: V p m a -> ([Attribute a] -> [View p a] -> View p a) -> m -> View p a
toView v f m = f `apElem` (v `runV` m)

toHtml :: V p m a -> m -> View p a
toHtml = flip toView H.html_

toDiv :: V p m a -> m -> View p a
toDiv = flip toView H.div_

mapChildren :: (V p m a -> V p m a) -> V p m a -> V p m a
mapChildren f v = V $ \m ->
    let
        -- apChildren :: Elem p a -> Elem p a
        apChildren = dimap (V . pure) (`runV` m) f
    in
        apChildren $ v `apV` m

runV :: V p m a -> m -> Elem p a
runV v m = v`apV` m

liftV :: View p a -> V p m a
liftV = V . pure . Elem [] . pure

liftVF :: (m -> View p a) -> V p m a
liftVF = V . fmap (Elem [] . pure)

wrapAttr :: ([Attribute a] -> View p a) -> V p m a -> V p m a
wrapAttr = wrapV . (\f a _ -> f a)

liftElem :: View p a -> Elem p a
liftElem = Elem [] . pure

liftAttr :: Attribute a -> V p m a
liftAttr = V . pure . flip Elem [] . pure

{- | lift a View interface into a V interface

This should not be exported as it's used mostly for internal reasons
-}
wrapV :: ([Attribute a] -> [View p a] -> View p a) -> V p m a -> V p m a
wrapV f (V x) = V $ fmap (liftElem . apElem f) x

apElem :: ([Attribute a] -> [View p a] -> View p a) -> Elem p a -> View p a
apElem f Elem{children, attrs} = f attrs children

class_ :: [MisoString] -> V p m a
class_ = liftAttr . P.class_ . unwords

role :: MisoString -> V p m a
role = liftAttr . P.role_

txt :: V p JSString a
txt = liftVF text

for :: V p m a -> V p [m] a
for = V . foldMap . apV 

div :: V p m a -> V p m a
div = wrapV H.div_

nav :: V p m a -> V p m a
nav = wrapV H.nav_

section :: V p m a -> V p m a
section = wrapV H.section_

span :: V p m a -> V p m a
span = wrapV H.span_

p :: V p m a -> V p m a
p = wrapV H.p_

h1 :: V p m a -> V p m a
h1 = wrapV H.h1_

form :: V p m a -> V p m a
form = wrapV H.form

{- | Primitive HTML input types.

> input Text "first-name" :: V p MisoString MisoString
-}
data InputType
    = Text
    | TextArea
    | File
    | Submit

input :: InputType -> V p m a -> V p m a
input t x = wrapAttr H.input_ $ type_ t <> x

submit :: V p m a -> V p m a
submit = input Submit

textInput :: V p m a -> V p m a
textInput = input Text

textAreaInput :: V p m a -> V p m a
textAreaInput = input TextArea

type_ :: InputType -> V p m a
type_ t = liftAttr $ P.type_ $ case t of
    Text -> "text"
    TextArea -> "textarea"
    File -> "file"
    Submit -> "submit"

onInput :: V p m MisoString
onInput = liftAttr $ E.onInput id

name :: MisoString -> V p m a
name = liftAttr . P.name_

label :: V p m a -> V p m a
label = wrapV H.label_

table :: V p m a -> V p m a
table = wrapV H.table_

thead :: V p m a -> V p m a
thead = wrapV H.thead_

tr :: V p m a -> V p m a
tr = wrapV H.tr_

th :: V p m a -> V p m a
th = wrapV H.th_

td :: V p m a -> V p m a
td = wrapV H.td_

tbody :: V p m a -> V p m a
tbody = wrapV H.tbody_

i :: V p m a -> V p m a
i = wrapV H.i_

button :: V p m a -> V p m a
button = wrapV H.button_

if_ :: (m -> Bool) -> V p m a -> V p m a -> V p m a
if_ p_ (V t) (V f) = V $ ifP p_ t f

-- Contravariant operators in the profunctor form

infixl 4 >$<
(>$<) :: Profunctor p => (a -> b) -> p b x -> p a x
(>$<) = lmap

infixl 4 $<
($<) :: Profunctor p => p a x -> a -> p b x
($<) = flip $ lmap . const

ifP :: (Profunctor p, Choice p, Category p) => (a -> Bool) -> p a x -> p a x -> p a x
ifP p_ t f = dimap (predToEither p_) (either id id) $ left' t >>> right' f

predToEither :: (a -> Bool) -> a -> Either a a
predToEither p_ a = if p_ a then Left a else Right a
