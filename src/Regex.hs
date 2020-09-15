{-# LANGUAGE BangPatterns #-}

module Regex where

-- From https://github.com/Gabriel439/slides/blob/master/regex/regex.md

import Data.Array.Unboxed (Array, UArray, (!))
import Data.Bits ((.|.), (.&.))
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Word (Word8, Word64)
import Foreign (peek, plusPtr, withForeignPtr)

import qualified Data.Array.Base
import qualified Data.Array.Unboxed       as Array
import qualified Data.Bits                as Bits
import qualified Data.ByteString          as ByteString
import qualified Data.ByteString.Internal as ByteString.Internal
import qualified GHC.Arr

integerShiftL :: Integer -> Int -> Integer
integerShiftL = Bits.unsafeShiftL
{-# INLINE integerShiftL #-}

sizeOfWord :: Int
sizeOfWord = Bits.finiteBitSize (0 :: Word)
{-# INLINE sizeOfWord #-}

integerFoldl' :: (b -> Int -> b) -> b -> Integer -> b
integerFoldl' f acc0 bits = go acc0 (Bits.popCount bits) 0
  where
    go !acc 0 _ = acc
    go !acc n b =
        if Bits.testBit bits b
        then go (f acc b) (n - 1) (b + 1)
        else go    acc     n      (b + 1)
{-# INLINE integerFoldl' #-}

data Regex i = Regex
    { _numberOfStates         :: Int

    -- Fast path, if the number of states is less than or equal to the number of
    -- bits in a `Word`
    , _startingStates         :: Word
    , _transitionFunction     :: i -> Int -> Word
    , _acceptingStates        :: Word

    -- Slow path, if the number of states is greater than the number of bits in
    -- a `Word`
    --
    -- This is ~10x slower
    , _startingStatesSlow     :: Integer
    , _transitionFunctionSlow :: i -> Int -> Integer
    , _acceptingStatesSlow    :: Integer
    }

instance Num (Regex i) where
    fromInteger n
        | 0 == n    = Regex 0 0 f 0 0 g 0
        | 0 <  n    = Regex 1 1 f 1 1 g 1
        | otherwise = error "fromInteger[Regex]: Negative numbers unsupported"
      where
        f _ _ = 0
        g _ _ = 0
    {-# INLINE fromInteger #-}

    Regex nL asL fL bsL csL gL dsL + Regex nR asR fR bsR csR gR dsR =
        Regex n as f bs cs g ds
      where
        n  = nL + nR

        as = Bits.unsafeShiftL asR nL .|. asL

        f i j =
            if j < nL
            then fL i j
            else Bits.unsafeShiftL (fR i (j - nL)) nL

        bs = Bits.unsafeShiftL bsR nL .|. bsL

        cs = integerShiftL csR nL .|. csL

        g i j =
            if j < nL
            then gL i j
            else integerShiftL (gR i (j - nL)) nL

        ds = integerShiftL dsR nL .|. dsL
    {-# INLINE (+) #-}

    Regex nL asL fL bsL csL gL dsL * Regex nR asR fR bsR csR gR dsR =
        asR' `seq` csR' `seq` Regex n as f bs cs g ds
      where
        n = nL + nR

        asR' = Bits.unsafeShiftL asR nL

        as =
            if asL .&. bsL == 0
            then asL
            else asL .|. asR'

        f i j =
            if j < nL
            then
                if s .&. bsL == 0
                then s
                else s .|. asR'
            else Bits.unsafeShiftL (fR i (j - nL)) nL
          where
            s = fL i j

        bs = Bits.unsafeShiftL bsR nL

        csR' = integerShiftL csR nL

        cs =
            if csL .&. dsL == 0
            then csL
            else csR' .|. csL

        g i j =
            if j < nL
            then
                if s .&. dsL == 0
                then s
                else s .|. csR'
            else integerShiftL (gR i (j - nL)) nL
          where
            s = gL i j

        ds = integerShiftL dsR nL
    {-# INLINE (*) #-}

star :: Regex i -> Regex i
star (Regex n as f bs cs g ds) = Regex n as f' as cs g' cs
  where
    f' i j =
        let s = f i j
        in  if s .&. bs == 0
            then s
            else s .|. as

    g' i j =
        let s = g i j
        in  if s .&. ds == 0
            then s
            else s .|. cs
{-# INLINE star #-}

plus :: Regex i -> Regex i
plus (Regex n as f bs cs g ds) = Regex n as f' bs cs g' ds
  where
    f' i j =
        let s = f i j
        in  if s .&. bs == 0
            then s
            else s .|. as

    g' i j =
        let s = g i j
        in  if s .&. ds == 0
            then s
            else s .|. cs
{-# INLINE plus #-}

match :: Regex i -> [i] -> Bool
match (Regex n as f bs cs g ds) is
    -- Fast path (Bit arithmetic on `Word`s)
    | n <= sizeOfWord = bs .&. foldl' step  as is /= 0
    -- Slow path (Bit arithmetic on `Integer`s)
    | otherwise       = ds .&. foldl' step' cs is /= 0
  where
    step s0 i = go 0 s0
      where
        go !acc 0 = acc
        go !acc s = go (acc .|. f i j) (Bits.clearBit s j)
          where
            j = Bits.countTrailingZeros s

    step' s0 i = integerFoldl' (\acc j -> acc .|. g i j) 0 s0
{-# INLINE match #-}

satisfy :: (i -> Bool) -> Regex i
satisfy predicate = Regex 2 1 f 2 1 g 2
  where
    f c 0 | predicate c = 2
    f _ _               = 0

    g c 0 | predicate c = 2
    g _ _               = 0
{-# INLINE satisfy #-}

once :: Eq i => i -> Regex i
once x = satisfy (== x)
{-# INLINE once #-}

dot :: Regex i
dot = satisfy (\_ -> True)
{-# INLINE dot #-}

chars :: Regex i
chars = Regex 1 1 f 1 1 g 1
  where
    f _ _ = 1
    g _ _ = 1
{-# INLINE chars #-}

bytes :: ByteString -> Regex Word8
bytes w8s = Regex (n + 1) 1 f (Bits.unsafeShiftL 1 n) 1 g (integerShiftL 1 n)
  where
    n = fromIntegral (ByteString.length w8s)

    f w8 i
        | i == n                                      =
            0
        | ByteString.index w8s (fromIntegral i) == w8 =
            Bits.unsafeShiftL 1 (i + 1)
        | otherwise                                   =
            0

    g w8 i
        | i == n                                      =
            0
        | ByteString.index w8s (fromIntegral i) == w8 =
            integerShiftL 1 (i + 1)
        | otherwise                                   =
            0

matchBytes :: Regex Word8 -> ByteString -> Bool
matchBytes (Regex n as f bs cs g ds) (ByteString.Internal.PS fp off len)
    | n <= sizeOfWord = do
        ByteString.Internal.accursedUnutterablePerformIO
            (withForeignPtr fp (\p ->
                loop as (p `plusPtr` off) (p `plusPtr` (off+len)) ))
    | otherwise       = do
        ByteString.Internal.accursedUnutterablePerformIO
            (withForeignPtr fp (\p ->
                loop' cs (p `plusPtr` off) (p `plusPtr` (off+len)) ))
  where
    loop  0  _  _   = return False
    loop !z !p !q
        | p == q    = return (bs .&. z /= 0)
        | otherwise = do
            x <- peek p
            loop (step z x) (p `plusPtr` 1) q

    step :: Word -> Word8 -> Word
    step !s0 i0 = go 0 s0
      where
        go :: Word -> Word -> Word
        go !acc 0 = acc
        go !acc s = go acc' s'
          where
            acc' = acc .|. m
            m    = Data.Array.Base.unsafeAt table ix
            ix   = GHC.Arr.unsafeIndex bounds (i0, j)
            s'   = s .&. Bits.complement (Bits.unsafeShiftL 1 j)
            j    = Bits.countTrailingZeros s

        bounds :: ((Word8, Int), (Word8, Int))
        bounds = ((0, 0), (255, n - 1))

        table :: UArray (Word8, Int) Word
        table =
            Array.listArray bounds
                [ f i j
                | i <- [0..255]
                , j <- [0..n-1]
                ]

    loop'  0  _  _  = return False
    loop' !z !p !q
        | p == q    = return (ds .&. z /= 0)
        | otherwise = do
            x <- peek p
            loop' (step' z x) (p `plusPtr` 1) q

    step' :: Integer -> Word8 -> Integer
    step' !s0 i0 = integerFoldl' (\acc j -> acc .|. table ! (i0, j)) 0 s0
      where
        table :: Array (Word8, Int) Integer
        table =
            Array.listArray ((0, 0), (255, n - 1))
                [ g i j
                | i <- [0..255]
                , j <- [0..n-1]
                ]

hasBytes :: Regex Word8 -> ByteString -> Bool
hasBytes (Regex n as f bs cs g ds) (ByteString.Internal.PS fp off len)
    | n <= sizeOfWord = do
        ByteString.Internal.accursedUnutterablePerformIO
            (withForeignPtr fp (\p ->
                loop as (p `plusPtr` off) (p `plusPtr` (off+len)) ))
    | otherwise       = do
        ByteString.Internal.accursedUnutterablePerformIO
            (withForeignPtr fp (\p ->
                loop' cs (p `plusPtr` off) (p `plusPtr` (off+len)) ))
  where
    loop !z !p !q
        | bs .&. z /= 0 = return True
        | p == q        = return False
        | otherwise     = do
            x <- peek p
            loop (step z x .|. as) (p `plusPtr` 1) q

    step :: Word -> Word8 -> Word
    step !s0 i0 = go 0 s0
      where
        go :: Word -> Word -> Word
        go !acc 0 = acc
        go !acc s = go acc' s'
          where
            acc' = acc .|. m
            m    = Data.Array.Base.unsafeAt table ix
            ix   = GHC.Arr.unsafeIndex bounds (i0, j)
            s'   = s .&. Bits.complement (Bits.unsafeShiftL 1 j)
            j    = Bits.countTrailingZeros s

        bounds :: ((Word8, Int), (Word8, Int))
        bounds = ((0, 0), (255, n - 1))

        table :: UArray (Word8, Int) Word
        table =
            Array.listArray bounds
                [ f i j
                | i <- [0..255]
                , j <- [0..n-1]
                ]

    loop' !z !p !q
        | ds .&. z /= 0 = return True
        | p == q        = return False
        | otherwise     = do
            x <- peek p
            loop' (step' z x .|. cs) (p `plusPtr` 1) q

    step' :: Integer -> Word8 -> Integer
    step' !s0 i0 = integerFoldl' (\acc j -> acc .|. table ! (i0, j)) 0 s0
      where
        table :: Array (Word8, Int) Integer
        table =
            Array.listArray ((0, 0), (255, n - 1))
                [ g i j
                | i <- [0..255]
                , j <- [0..n-1]
                ]
