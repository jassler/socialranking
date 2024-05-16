# socialranking 1.2.0

* Added `generateRandomPowerRelation()`
* Added S3 class `DualLexcelScores`

# socialranking 1.1.0 (2023-11-29)

* Added `L2Scores()` and `L2Ranking()`, along with aliases `lexcel2...()`
* Added `LPScores()` and `LPRanking()`, along with aliases `lexcelP...()`
* Added `LPSScores()` and `LPSRanking()`, along with aliases `lexcelPS...()`
* Fixed `kramerSimpsonScores()` implementation which was not in line with the official definition
* Changed succeq to succsim notation in documentation to reflect modern literature

# socialranking 1.0.1 (2023-08-23)

Added -package alias to package description (fixing problem listed [here](https://github.com/r-lib/roxygen2/issues/1491)).

# socialranking 1.0.0 (2023-03-13)

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
