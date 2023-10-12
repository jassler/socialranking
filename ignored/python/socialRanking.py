import re

class DictLookup(dict):

    def __init__(self, *args, keys_are_lists = False):
        self.update(*args)
        self.keys_are_lists = keys_are_lists

    def __getitem__(self, key):
        if not self.keys_are_lists:
            return dict.__getitem__(self, str(key))
        if not isinstance(key, list):
            key = [key]
        key = tuple([str(k) for k in key])
        return dict.__getitem__(self, key)

class PowerRelation:
    def __init__(self, equivalenceClasses: list):
        self.eqs = [[[str(item) for item in coalition] for coalition in eqc] for eqc in equivalenceClasses]
        self.elements = set([item for eqc in self.eqs for coalition in eqc for item in coalition])
        self.coalitionLookup = DictLookup(keys_are_lists=True)
        self.elementLookup = { e: [] for e in self.elements }
        self.only_single_chars = all([ len(e) == 1 for e in self.elements ])

        for i, eqc in enumerate(self.eqs):
            for j, coalition in enumerate(eqc):
                self.coalitionLookup[tuple(coalition)] = i
                for item in coalition:
                    self.elementLookup[item].append((i, j))
        self.elementLookup = DictLookup(self.elementLookup)

    @staticmethod
    def from_string(s: str):
        s = re.compile('[^a-zA-Z0-9>~\u227B\u223C]').sub('', s)

        eqs = []
        eq = []
        coal = []

        for c in s:
            if c == '~' or c == '\u223C':
                eq.append(coal)
                coal = []
            elif c == '>' or c == '\u227B':
                eq.append(coal)
                eqs.append(eq)
                eq = []
                coal = []
            else:
                coal.append(c)
        
        eq.append(coal)
        eqs.append(eq)
        
        return PowerRelation(eqs)

    @staticmethod
    def from_list(*args, comparators = ['>']):
        coalitions = [a if isinstance(a, list) else [a] for a in args]
        if comparators == ['>']:
            return PowerRelation([[coalition] for coalition in coalitions])
        eqcs = []
        eqc = []
        for i, coalition in enumerate(coalitions):
            eqc.append(coalition)
            if comparators[i % len(comparators)] == '>':
                eqcs.append(eqc)
                eqc = []
        if len(eqc) > 0:
            eqcs.append(eqc)
        return PowerRelation(eqcs)

    def __str__(self) -> str:
        j_str =\
            (lambda c: ''.join(c) if len(c) > 0 else '{}') if self.only_single_chars else\
            (lambda c: '{' + ', '.join(c) + '}')
        stringified = [
            [j_str(coalition) for coalition in eqc]
            for eqc in self.eqs
        ]
        stringified = map(lambda m: m[0] if len(m) == 1 else '(' + ' ~ '.join(m) + ')', stringified)
        return ' > '.join(stringified)


if __name__ == '__main__':
    pr = PowerRelation([[[1,2,3],[1,2],[1]],[[2]],[[3]],[[2,3],[],[1,3]]])
    assert pr.__str__() == '(123 ~ 12 ~ 1) > 2 > 3 > (23 ~ {} ~ 13)'
    assert pr.elementLookup[1] == [(0,0),(0,1),(0,2),(3,2)]
    assert pr.elementLookup[2] == [(0,0),(0,1),(1,0),(3,0)]
    assert pr.elementLookup[3] == [(0,0),(2,0),(3,0),(3,2)]
    assert pr.coalitionLookup[[]] == 3
    assert pr.coalitionLookup[[1]] == 0
    assert pr.coalitionLookup[[2]] == 1
    assert pr.coalitionLookup[[1,3]] == 3
    assert pr.coalitionLookup[[1,2,3]] == 0
    assert pr.elements == set(['1','2','3'])

    pr = PowerRelation([[['ab','cd','e'],['cd','e']],[['ab','cd'],['e']], [[]]])
    assert pr.__str__() == '({ab, cd, e} ~ {cd, e}) > ({ab, cd} ~ {e}) > {}'

# def coalitionsAreIndifferent(powerRelation, c1, c2):
#     pass

# def equivalenceClassIndex(powerRelation, coalition):
#     pass

# def elementLookup(powerRelation, element):
#     pass

# class SocialRanking:
#     def __init__(l):
#         pass

# def doRanking(scores, compare = None, decreasing = True):
#     pass

# def appendMissingCoalitions(powerRelation, includeEmptySet = True):
#     pass

# def ordinalBanzhafScores(powerRelation, elements = powerRelation$elements):
#     pass

# def ordinalBanzhafRanking(powerRelation):
#     pass
# def copelandScores(powerRelation, elements = powerRelation$elements):
#     pass
# def copelandRanking(powerRelation):
#     pass
# def cpMajorityComparison(powerRelation, e1, e2, strictly = False, includeEmptySet = True):
#     pass
# def cpMajorityComparisonScore(powerRelation, e1, e2, strictly = False, includeEmptySet = True):
#     pass
# def createPowerset(elements, includeEmptySet = True, result = c('return', 'print', 'copy', 'printCompact', 'copyCompact')):
#     pass
# def makeListCopyable(elements, l, compact):
#     pass
# def cumulativeScores(powerRelation, elements = powerRelation$elements):
#     pass
# def cumulativelyDominates(powerRelation, e1, e2, strictly = False):
#     pass
# def release_bullets():
#     pass
# def dominates(powerRelation, e1, e2, strictly = False, includeEmptySet = True):
#     pass
# def powerRelationGenerator(coalitions, startWithLinearOrder = False):
#     pass
# def generateNextPartition(gen):
#     pass
# def kramerSimpsonScores(powerRelation, elements = powerRelation$elements, compIvsI = False):
#     pass
# def kramerSimpsonRanking(powerRelation, compIvsI = False):
#     pass
# def lexcelScores(powerRelation, elements = powerRelation$elements):
#     pass
# def lexcelRanking(powerRelation):
#     pass
# def dualLexcelRanking(powerRelation):
#     pass
# def L1Scores(powerRelation, elements = powerRelation$elements):
#     pass
# def L1Ranking(powerRelation):
#     pass
# def makePowerRelationMonotonic(powerRelation, addMissingCoalitions = True):
#     pass
# def testRelation(powerRelation, e1):
#     pass
# def powerRelationMatrix(powerRelation, domainNames = c("pretty", "numericPrec", "numeric")):
#     pass
# def transitiveClosure(powerRelation):
#     pass
