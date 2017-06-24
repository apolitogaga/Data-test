__author__ = "codingMonkey"
__project__ = "ChessML"
# import pandas as pd


if __name__ == "__main":

    # b = pd.read_csv("people-involved.csv")
    # a = pd.read_csv("train.csv")
    # merged = a.merge(b, on='GridID')
    # merged.to_csv("output_merged.csv", index=False)

    with open('people-involved.csv', 'r') as f:
        reader = csv.reader(f)
        pepol = list(reader)

    with open('train.csv', 'r') as f:
        reader = csv.reader(f)
        train = list(reader)


