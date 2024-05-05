import pandas as pd
from vmtk import vmtkscripts

centerlineReader = vmtkscripts.vmtkSurfaceReader()
centerlineReader.InputFileName = r'ioc/ioc-internal-clipped-cl.vtp'
centerlineReader.Execute()

centerlines = centerlineReader.Surface

def to_df(centerline):
    clNumpyAdaptor = vmtkscripts.vmtkCenterlinesToNumpy()
    clNumpyAdaptor.Centerlines = centerline
    clNumpyAdaptor.ConvertCellToPoint = 1
    clNumpyAdaptor.Execute()
    numpyCenterlines = clNumpyAdaptor.ArrayDict

    df = pd.DataFrame({k: v for k, v in numpyCenterlines['PointData'].items() if len(v.shape) < 2})
    df2 = pd.DataFrame(data=numpyCenterlines['Points'], columns=['x', 'y', 'z'])
    return pd.concat((df2, df), axis=1)

to_df(centerlines).to_csv(r'ioc/ioc-internal-clipped-cl.csv', sep=',')