module InitialAlgebra where

-- The Initial Algebra in the category of F algebras

-- We consider the type system of Haskell as a Category where types
-- are the objects and functions are the morphism. Lets call it Hask or H for sort.
-- (Modulo the weirdness lazyness, bottoms, type families, etc)

-- Let's define a recursive datatype. This will helps us soon.
data IntTree
  = Leaf Int
  | Branch IntTree Int IntTree

-- Let's define a functor and a weird data structure

data Fix f = Fix (f (Fix f))

-- To build intuition about the Fix f:
-- If we use Maybe with Fix we get something very similar to a list.

weirdList1 :: Fix Maybe
weirdList1 = Fix $ Just $ Fix $ Just $ Fix Nothing

-- This is not very useful, lets take another step:

weirdList2 :: Fix ((,) Int)
weirdList2 = Fix (1, Fix (2, {-oh this needs to go forever, hmmm-} weirdList2 {-will do it-}))

-- it is getting crazier, but don't loose hope my friend, we can seal that recursion

-- oh no, we would need to apply the type variable twice, or what? Grrr
-- weirdList3 :: Fix (Either ((,Int)))
---weirdList3 = ?

-- I want a list before I can go to the tree.

data WList r = WN | WC Char r

weirdList3 :: Fix WList
weirdList3 = Fix $ WC '1' $ Fix $ WC '2' $ Fix WN

-- So Fix uses the type parameter in the container like functor to introduce new recursion...
-- But how that helps us? IntTree is also a recursive structure but it doesn't have a type
-- parameter. Lets make one!

data IntTreeF r -- We call it F for historical reasons - A functor that describes the shape
                -- of one layer in the recusive structure (ShapeFunctor)
  = LeafF Int       -- No recursion here, stupid naming :)
  | BranchF r Int r -- Two recursive places here, still stupid naming.

weirdTree :: Fix IntTreeF -- I am sure you can think of a simple tree, go on write one that type-checks
weirdTree = undefined

-- To talk about the Initial Algebra in the category of F algebras, first we need to set the
-- actual category and understand what the algebra is.

-- What is an algebra? To create an Algebra we need a category C and an endofunctor F.

-- If you have a functor F which goes/maps from C to C, a category to the same category, you get an
-- endofunctor. Every Functor in Haskell is an endofunctor. We can't really write any other
-- functor in Haskell that would product a Haskell type.

-- So: Maybe gets a Haskell type and produces a Haskell type. For example:
-- Int is a Haskell type, (Maybe Int) is also a Haskell type.
-- String is a Haskell type, (Maybe String) is also a Haskell type.
-- etc, I think you know where this is going...

-- Again what is an algebra?

-- If you have F : C -> C then an algebra is a morphism in C such that, for an (not any!) object
-- in C such as 'a' we have: F a -> a.

-- Happy? - No...

-- So a -> F a is a morphism in C because it goes from the object 'F a' to the object 'a'.
-- You know what, lets see it in Haskell. No, not with the Maybe, it was weird enough in the List part...
-- So we need a functor, lets pick WList. But Wlist is not a functor! Not yet...

instance Functor WList where
  fmap f WN       = WN
  fmap f (WC x r) = WC x (f r)

-- OK it is a functor. But it looks weird. In the list functor we have apply f in the first element
-- of the cons. - Yes! - But this is not the list functor. This is the Shape functor of the list. You
-- know we used in the creation of the weridList3 where the 'r' type parameter represented the recursion
-- point in the data structure. - OK ok, we have a functor... Show me some algebra! - I show you one!

lengthAlgebra :: WList Int -> Int
lengthAlgebra WN       = 0
lengthAlgebra (WC _ l) = l + 1

-- Voila, the length Algebra! It tells you about the connection between the length of a wierdList and
-- the constructors at one level. The length of the empty list must be zero and the length of the
-- non empty list must be the length of the list in the recursive occurrence + 1.

-- But I can't use that to determine the length of the wierdList. Types don't match up!
-- Try to implement the length using the lengthAlgebra.

weirdLength :: Fix WList -> Int
weirdLength (Fix w) = lengthAlgebra $ fmap weirdLength w

-- This is confusing! - Yes I know... But this is the piece you have to understand yourself. I can't
-- help you there. Grab pen and paper and work out how this functions behaves on the weirdList2

-- Why this is important? - Just do it. - OK.

-- But it feels very weird, can't I use the normal list from Haskell? - Sure you can!

normalListAlgebra :: WList [Char] -> [Char]
normalListAlgebra WN        = []
normalListAlgebra (WC x ls) = x : ls

-- No I meant just the Normal list? - I don't understand your question. Maybe you want this:

normalListAlgebra2 :: WList (Fix WList) -> Fix WList
normalListAlgebra2 WN        = Fix WN
normalListAlgebra2 (WC x ls) = Fix (WC x ls)

-- No! This is even crazier! - Ah let me help it!

normalListAlgebra3 :: WList (Fix WList) -> Fix WList
normalListAlgebra3 = Fix

-- Whaaaaat???? No, no and no! - I can't even make this simpler....
-- Ah you mean you want to use the list from Haskell??? - (facepalm) YES!
-- Well you just have to write a conversion function:

listToWList :: [Char] -> Fix WList
listToWList []     = Fix WN
listToWList (x:xs) = Fix $ WC x $ listToWList xs

-- and the other way around... Wait we have already have written that! Or at least one part of it...

unFix :: Fix f -> f (Fix f)
unFix (Fix f) = f

wlistToList :: Fix WList -> [Char]
wlistToList = normalListAlgebra . fmap wlistToList . unFix

-- Weird in another way...
weirdLength2 :: [Char] -> Int
weirdLength2 = weirdLength . listToWList

-- So you say [Char] in Haskell and Fix WList are isomorphic? - Yes!

-- And weirdLength and wlistToList are very similar. We used the algebra and we recursively called
-- the function using fmap of the shape functor. Could you write an abstract function for these two?
-- It would be very boring to write the same structure all over again... - Let me try!

-- I need something like this, but I need an 'a' from somewhere.
-- weirdFunction :: Fix WList -> a
-- Ah I have the 'a' in the algebras
-- weirdFunction :: (WList a -> a) -> Fix WList -> a
-- Yes! - Go on, implement it

weirdFunction :: (WList a -> a) -> Fix WList -> a
weirdFunction alg = alg . fmap (weirdFunction alg) . unFix

wlistToList2 :: Fix WList -> [Char]
wlistToList2 = weirdFunction normalListAlgebra

weirdLength3 :: Fix WList -> Int
weirdLength3 = weirdFunction lengthAlgebra

-- Oh that's cool! If I pass an algebra in, the weirdFunction will take care of the
-- recursion. I only have to think in one layer at a time!

-- Wait, we forgot something...
-- The Initial Algebra in the category of F algebras
-- OK Now I understand what is the F algebra and we created some of the WList algebras
-- Yes
-- What I don't understand how that maps to the F algebras?
-- The definition is vague here. When we say the category of F algebras, we don't talk about
-- the category of all the functors and all the algebras. We only talk about one F!
-- ...
-- We fixed the F to WList!
-- ...
-- ...
-- Ahhhh we talk about the algebras of WList, such as lengthAlgebra and normalListAlgebra, and
-- every other function that goes from 'WList o -> o'
-- Yes!
-- OK we have objects of WList algebras. How that forms a category?
-- Lets call this category a 'WLCat'
-- The objects in WLCat are 'WList o -> o' algebras. Remember 'o' and 'WList o' are types in Haskell
-- and thus objects in Hask, but the Objects in WLCat are functions of signature 'WList o -> o'
-- Ooook - but what are the morphism than?
-- They are also Haskell functions! Of type (a -> b)
-- Wait! This can't be right! Not all the functions are suitable for this...
-- Yes you are right! - Only the ones which are well behaving in terms of the WLCat objects such as:
-- if we have two objects in WLCat: o1 :: WList a -> a and o2 : WList b -> b, and the f : a -> b
-- function. 'f' is a morphism in WLCat if it is well behaving, no matter if we apply the function first
-- inside the WList or after the computation. So 'the following diagram commutes':
-- f . o1 = o2 . fmap f

-- Can you give me an example?
-- You already have one!
-- Do I!?!
-- Yes. What is the morphism between the normalListAlgebra and the lengthAlgebra
-- .... errrm ... errrm ... it must be the length from the prelude.
-- Yes!
-- But that is confusing.
-- Just a bit! Next question: Can you find me a function from Int -> [Char] between
-- lengthAlgebra and normalListAlgebra?
-- Sure! Here it is:
aaaa :: Int -> [Char]
aaaa 0 = []
aaaa n = 'a' : aaaa (n-1)

-- Are you sure?
-- Yes absolutely, look!
-- o1 = lengthAlgebra     : WList Int    -> Int
-- o2 = normalListAlgebra : WList [Char] -> [Char]
-- m  = aaaa
-- m . o1 == o2 . fmap m
-- aaaa . lengthAlgebra == normalListAlgebra . fmap aaa

-- I can show you a counter example! That test it!
test1 = aaaa . lengthAlgebra
test2 = normalListAlgebra . fmap aaaa
testAAAA = (test1 (WC 'b' 2), test2 (WC 'b' 2))

-- Oh, sorry!

-- The WList [Char] is very interesting structure. Actually if you give any other 'WList o -> o'
-- algebra, based on the structure of the [Char] we will be always able to find a function in
-- Haskell 'm : [Char] -> a' which will makes sense, work as expected, makes the diagram commute for
-- the morphism in WLCat.

-- That's sounds familiar...
-- Yes?
-- Ah that is the definition of the initial object in a category.
-- So?
-- It means the 'normalListAlgebra' is the initial object of the WLCat category.
-- Yes!
-- But what about the normalListAlgebra2 and normalListAlgebra3 ?
-- What do you mean?
-- They are isomorphic to normalListAlgebra.
-- Yes.
-- Than we have more than one initial object! That can't be right...
-- You forgot the definition of the initial object in the category: Up to isomorphism. If they
-- behave the same, we can't really distinguish them when talking about the properties of the
-- structure of WList. In other words it doesn't even matter.

-- OK.
-- Do you see any abstraction in the implementation of the weirdFunction? Any opportunity to
-- generalize it, to make it more useful?
-- Let me try: Ah yes! We don't really use WList constructors in the definitions, just fmap, so

weirdFunction2 :: Functor f => (f a -> a) -> Fix f -> a
weirdFunction2 alg = alg . fmap (weirdFunction2 alg) . unFix

-- Well that is a really weird name. Let me rename it. I feel greek today. Cata preffix means
-- collapsing in greek. As we collapse our recursive type layer by layer.

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- Nice! But you forget something! You started with the IntTree. Oh my god, we have to go through
-- this craziness again!
-- Nonsense! Look! Use the 'F' Luke. You remember we have the IntTreeF already defined.

--data IntTreeF r
--  = LeafF Int
--  | BranchF r Int r

instance Functor IntTreeF where
  fmap f (LeafF x)       = LeafF x
  fmap f (BranchF l x r) = BranchF (f l) x (f r)

-- Please I need more help...
-- (Sigh) Could you write an algebra which will be the initial object in the cateogry of IntTreeF algebras?
-- Let me try...

initialIntTreeAlg :: IntTreeF IntTree -> IntTree
initialIntTreeAlg (LeafF x)         = Leaf x
initialIntTreeAlg (BranchF tl x tr) = Branch tl x tr

-- Good! And you can use the cata(morphism) too.
