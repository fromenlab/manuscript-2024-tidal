import pandas as pd
from vmtk import vmtkscripts

surfaceReader = vmtkscripts.vmtkSurfaceReader()
surfaceReader.InputFileName = r'ioc/ioc-internal.stl'
surfaceReader.Execute()

# To make centerlines from input surface
centerlineMaker = vmtkscripts.vmtkCenterlines()
centerlineMaker.Surface = surfaceReader.Surface
centerlineMaker.SeedSelectorName = 'openprofiles'
centerlineMaker.AppendEndPoints = 1
centerlineMaker.Execute()

# To write centerlines
centerlineWriter = vmtkscripts.vmtkSurfaceWriter()
centerlineWriter.Surface = centerlineMaker.Centerlines
centerlineWriter.OutputFileName = r'ioc/ioc-internal-cl.vtp'
centerlineWriter.Execute()