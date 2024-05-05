import pandas as pd
from vmtk import vmtkscripts

centerlineReader = vmtkscripts.vmtkSurfaceReader()
centerlineReader.InputFileName = r'ioc/ioc-internal-cl-sp.vtp'
centerlineReader.Execute()

centerlines = centerlineReader.Surface

centerlineMerge = vmtkscripts.vmtkCenterlineMerge()
centerlineMerge.Centerlines = centerlines
centerlineMerge.Execute()

centerlineWriter = vmtkscripts.vmtkSurfaceWriter()
centerlineWriter.Surface = centerlineMerge.Centerlines
centerlineWriter.OutputFileName = r'ioc/ioc-internal-cl-sp-merged.vtp'
centerlineWriter.Execute()

def to_df(centerline):
    clNumpyAdaptor = vmtkscripts.vmtkCenterlinesToNumpy()
    clNumpyAdaptor.Centerlines = centerline
    clNumpyAdaptor.ConvertCellToPoint = 1
    clNumpyAdaptor.Execute()
    numpyCenterlines = clNumpyAdaptor.ArrayDict

    df = pd.DataFrame({k: v for k, v in numpyCenterlines['PointData'].items() if len(v.shape) < 2})
    df2 = pd.DataFrame(data=numpyCenterlines['Points'], columns=['x', 'y', 'z'])
    return pd.concat((df2, df), axis=1)

to_df(centerlineMerge.Centerlines).to_csv(r'ioc/ioc-internal-cl-sp-merged.csv', sep=',')

centerlineLabeler = vmtkscripts.vmtkCenterlineLabeler()
centerlineLabeler.Centerlines = centerlineMerge.Centerlines
centerlineLabeler.LabelIdsArrayName = "GroupIds"
centerlineLabeler.Execute()