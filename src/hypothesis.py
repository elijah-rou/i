import pandas as pd

class Hypothesis:
    H = []
    def __init__(self, row):
        for r in row:
            H.extend(h)
    
class HypothesisSet:
    H = [[]]
    def __init__(self, hypotheses, dataset):
        data = pd.read_csv(dataset)
        for row in hypotheses:
            HSet.extend(Hypothesis(H))

    @classmethod
    def general(n):
        for i in range(0, n):
            H.extend(Hypothesis())

