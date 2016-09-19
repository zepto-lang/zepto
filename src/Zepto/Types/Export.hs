module Zepto.Types.Export (
                            module Data.Ratio,
                            module Data.Fixed,
                            module Data.Dynamic,
                            module Data.List,
                            module Data.Complex,
                            module Zepto.Types,

                            InternalRegex,
                            InternalMap,
                            InternalBVector,

                            emptyBVec,
                            singletonBVec,
                            packBVec,
                            unpackBVec,
                            consBVec,
                            snocBVec,
                            appendBVec,
                            headBVec,
                            unconsBVec,
                            unsnocBVec,
                            lastBVec,
                            tailBVec,
                            initBVec,
                            nullBVec,
                            lengthBVec,
                            mapBVec,
                            reverseBVec,
                            intersperseBVec,
                            intercalateBVec,
                            transposeBVec,
                            foldlBVec,
                            foldlBVec',
                            foldl1BVec,
                            foldl1BVec',
                            foldrBVec,
                            foldr'BVec,
                            foldr1BVec,
                            foldr1'BVec,
                            concatBVec,
                            concatMapBVec,
                            anyBVec,
                            allBVec,
                            maximumBVec,
                            minimumBVec,
                            scanlBVec,
                            scanl1BVec,
                            scanrBVec,
                            scanr1BVec,
                            mapAccumLBVec,
                            mapAccumRBVec,
                            replicateBVec,
                            unfoldrBVec,
                            unfoldrNBVec,
                            takeBVec,
                            dropBVec,
                            splitAtBVec,
                            takeWhileBVec,
                            dropWhileBVec,
                            spanBVec,
                            spanEndBVec,
                            breakBVec,
                            breakEndBVec,
                            groupBVec,
                            groupByBVec,
                            initsBVec,
                            tailsBVec,
                            splitBVec,
                            splitWithBVec,
                            isPrefixOfBVec,
                            isSuffixOfBVec,
                            isInfixOfBVec,
                            breakSubstringBVec,
                            findSubstringBVec,
                            findSubstringsBVec,
                            elemBVec,
                            notElemBVec,
                            findBVec,
                            filterBVec,
                            partitionBVec,
                            indexBVec,
                            elemIndexBVec,
                            elemIndicesBVec,
                            elemIndexEndBVec,
                            findIndexBVec,
                            findIndicesBVec,
                            countBVec,
                            zipBVec,
                            zipWithBVec,
                            unzipBVec,
                            sortBVec,
                            copyBVec,
                            packCStringBVec,
                            packCStringLenBVec,
                            useAsCStringBVec,
                            useAsCStringLenBVec,

                            nullMap,
                            sizeMap,
                            mapMap,
                            mapWithKeyMap,
                            mapAccumMap,
                            mapAccumWithKeyMap,
                            mapAccumRWithKeyMap,
                            mapKeysMonotonicMap,
                            foldWithKeyMap,
                            foldrWithKeyMap,
                            foldlWithKeyMap,
                            elemsMap,
                            keysMap,
                            keysSetMap,
                            assocsMap,
                            toListMap,
                            toAscListMap,
                            toDescListMap,
                            fromDistinctAscListMap,
                            filterMap,
                            filterWithKeyMap,
                            partitionMap,
                            partitionWithKeyMap,
                            mapMaybeMap,
                            mapMaybeWithKeyMap,
                            mapEitherMap,
                            mapEitherWithKeyMap,
                            elemAtMap,
                            updateAtMap,
                            deleteAtMap,
                            findMinMap,
                            findMaxMap,
                            deleteMinMap,
                            deleteMaxMap,
                            deleteFindMinMap,
                            deleteFindMaxMap,
                            updateMinMap,
                            updateMaxMap,
                            updateMinWithKeyMap,
                            updateMaxWithKeyMap,
                            minViewMap,
                            maxViewMap,
                            minViewWithKeyMap,
                            maxViewWithKeyMap,
                            fromListMap,
                            lookupMap,
                            deleteMap,
                            findWithDefaultMap,
                          ) where
import Data.Complex
import Data.Dynamic
import Data.Fixed
import Data.List
import Data.Ratio

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Text.Regex.PCRE.Light.Base as Reg

import Zepto.Types

type InternalRegex = Reg.Regex
type InternalMap = M.Map
type InternalBVector = B.ByteString

emptyBVec = B.empty
singletonBVec = B.singleton
packBVec = B.pack
unpackBVec = B.unpack
consBVec = B.cons
snocBVec = B.snoc
appendBVec = B.append
headBVec = B.head
unconsBVec = B.uncons
unsnocBVec = B.unsnoc
lastBVec = B.last
tailBVec = B.tail
initBVec = B.init
nullBVec = B.null
lengthBVec = B.length
mapBVec = B.map
reverseBVec = B.reverse
intersperseBVec = B.intersperse
intercalateBVec = B.intercalate
transposeBVec = B.transpose
foldlBVec = B.foldl
foldlBVec' = B.foldl'
foldl1BVec = B.foldl1
foldl1BVec' = B.foldl1'
foldrBVec = B.foldr
foldr'BVec = B.foldr'
foldr1BVec = B.foldr1
foldr1'BVec = B.foldr1'
concatBVec = B.concat
concatMapBVec = B.concatMap
anyBVec = B.any
allBVec = B.all
maximumBVec = B.maximum
minimumBVec = B.minimum
scanlBVec = B.scanl
scanl1BVec = B.scanl1
scanrBVec = B.scanr
scanr1BVec = B.scanr1
mapAccumLBVec = B.mapAccumL
mapAccumRBVec = B.mapAccumR
replicateBVec = B.replicate
unfoldrBVec = B.unfoldr
unfoldrNBVec = B.unfoldrN
takeBVec = B.take
dropBVec = B.drop
splitAtBVec = B.splitAt
takeWhileBVec = B.takeWhile
dropWhileBVec = B.dropWhile
spanBVec = B.span
spanEndBVec = B.spanEnd
breakBVec = B.break
breakEndBVec = B.breakEnd
groupBVec = B.group
groupByBVec = B.groupBy
initsBVec = B.inits
tailsBVec = B.tails
splitBVec = B.split
splitWithBVec = B.splitWith
isPrefixOfBVec = B.isPrefixOf
isSuffixOfBVec = B.isSuffixOf
isInfixOfBVec = B.isInfixOf
breakSubstringBVec = B.breakSubstring
findSubstringBVec = B.findSubstring
findSubstringsBVec = B.findSubstrings
elemBVec = B.elem
notElemBVec = B.notElem
findBVec = B.find
filterBVec = B.filter
partitionBVec = B.partition
indexBVec = B.index
elemIndexBVec = B.elemIndex
elemIndicesBVec = B.elemIndices
elemIndexEndBVec = B.elemIndexEnd
findIndexBVec = B.findIndex
findIndicesBVec = B.findIndices
countBVec = B.count
zipBVec = B.zip
zipWithBVec = B.zipWith
unzipBVec = B.unzip
sortBVec = B.sort
copyBVec = B.copy
packCStringBVec = B.packCString
packCStringLenBVec = B.packCStringLen
useAsCStringBVec = B.useAsCString
useAsCStringLenBVec = B.useAsCStringLen

nullMap = M.null
sizeMap = M.size
mapMap = M.map
mapWithKeyMap = M.mapWithKey
mapAccumMap = M.mapAccum
mapAccumWithKeyMap = M.mapAccumWithKey
mapAccumRWithKeyMap = M.mapAccumRWithKey
mapKeysMonotonicMap = M.mapKeysMonotonic
foldMap = M.fold
foldWithKeyMap = M.foldWithKey
foldrWithKeyMap = M.foldrWithKey
foldlWithKeyMap = M.foldlWithKey
elemsMap = M.elems
keysMap = M.keys
keysSetMap = M.keysSet
assocsMap = M.assocs
toListMap = M.toList
toAscListMap = M.toAscList
toDescListMap = M.toDescList
fromDistinctAscListMap = M.fromDistinctAscList
filterMap = M.filter
filterWithKeyMap = M.filterWithKey
partitionMap = M.partition
partitionWithKeyMap = M.partitionWithKey
mapMaybeMap = M.mapMaybe
mapMaybeWithKeyMap = M.mapMaybeWithKey
mapEitherMap = M.mapEither
mapEitherWithKeyMap = M.mapEitherWithKey
elemAtMap = M.elemAt
updateAtMap = M.updateAt
deleteAtMap = M.deleteAt
findMinMap = M.findMin
findMaxMap = M.findMax
deleteMinMap = M.deleteMin
deleteMaxMap = M.deleteMax
deleteFindMinMap = M.deleteFindMin
deleteFindMaxMap = M.deleteFindMax
updateMinMap = M.updateMin
updateMaxMap = M.updateMax
updateMinWithKeyMap = M.updateMinWithKey
updateMaxWithKeyMap = M.updateMaxWithKey
minViewMap = M.minView
maxViewMap = M.maxView
minViewWithKeyMap = M.minViewWithKey
maxViewWithKeyMap = M.maxViewWithKey
fromListMap :: [(Simple, LispVal)] -> M.Map Simple LispVal
fromListMap = M.fromList
lookupMap :: Simple -> M.Map Simple LispVal -> Maybe LispVal
lookupMap = M.lookup
deleteMap :: Simple -> M.Map Simple LispVal -> M.Map Simple LispVal
deleteMap = M.delete
findWithDefaultMap :: LispVal -> Simple -> M.Map Simple LispVal -> LispVal
findWithDefaultMap = M.findWithDefault
