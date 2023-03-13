# socialranking 1.0.0

This major update brings a lot of breaking changes that are hopefully justified.

* The structure of `PowerRelation` objects has been changed to improve usability and speed.
  * `$elements` is, as in the previous version, a vector of sorted, unique elements
  * `$equivalenceClasses` has been renamed to `$eqs`
  * `$rankingCoalitions` and `$rankingComparators` has been removed, they served no particular purpose
  * Added `$coalitionLookup()` to make it easy (and fast) to find equivalence class indexes that a coalition appears in
  * Added `$elementLookup()` to make it easy (and fast) to find coalitions that an element appears in
  * Changed coalitions to no longer be `sets::set` objects, but just normal vectors. This allows for one element to appear multiple times in the same coalition, which is more consistent with the ability to have a coalition appear multiple times in a `PowerRealtion` object.
* Added `PowerRelation()` and `as.PowerRelation()`
* Added `SocialRanking()`
* Added `coalitionLookup()` and `elementLookup()`
* Added `L1Scores()` and `L1Ranking()`, along with its aliases `lexcel1...()`
* Added `makePowerRelationMonotonic()` and `appendMissingCoalitions()`
* Added `powerRelationGenerator()` and `generateNextPartition()`
* Changed `createPowerset()`, replaced parameters logicals `copyToClipboard` and `writeLines` with character `result` 
* Changed `ordinalBanzhafScores()` such that it returns a third value indicating how many comparisons were not made
* Deprecated `newPowerRelation()` and `newPowerRelationFromString()`
* Deprecated `SocialRankingSolution` class
* Removed `mathjaxr` and `sets` from dependencies


# socialranking 0.1.2 (2022-10-11)

* Changed maintainer's email address


# socialranking 0.1.1 (2022-04-25)

 * Added parameter `equivalenceClasses` to `newPowerRelation()`
