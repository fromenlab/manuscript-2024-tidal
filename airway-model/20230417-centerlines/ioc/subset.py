import pandas as pd

# coords = pd.read_csv(r'ioc/ioc-internal-cl-sp.csv')

# groups_unmerged = [0, 1, 2, 3, 97, 4, 58, 164, 60, 159, 61]

# for _ in groups_unmerged:
#     df = coords[coords["GroupIds"] == _]
#     df.to_csv(fr"ioc/coord-groups/ioc-internal-cl-sp-g{_}.csv")

def subset(coords, groups, prefix):
    for _ in groups:
        df = coords[coords["GroupIds"] == _]
        df.to_csv(fr"{prefix}{_}.csv")

if __name__ == "__main__":
    subset(pd.read_csv(r'ioc/ioc-internal-cl-sp-merged.csv'), groups = [0, 2, 4, 97, 58, 164, 60, 159, 62, 157], prefix='ioc/merged-groups/g')