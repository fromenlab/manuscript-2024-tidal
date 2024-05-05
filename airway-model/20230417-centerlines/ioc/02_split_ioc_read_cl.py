import pandas as pd
from vmtk import vmtkscripts

surfaceReader = vmtkscripts.vmtkSurfaceReader()
surfaceReader.InputFileName = r'ioc/ioc-internal.stl'
surfaceReader.Execute()

# To read centerlines from existing file
centerlineReader = vmtkscripts.vmtkSurfaceReader()
centerlineReader.InputFileName = r'ioc/ioc-internal-cl.vtp'
centerlineReader.Execute()
# Need to also update later centerline references for branch splitting

# To make centerlines from input surface
# centerlineMaker = vmtkscripts.vmtkCenterlines()
# centerlineMaker.Surface = surfaceReader.Surface
# centerlineMaker.SeedSelectorName = 'openprofiles'
# centerlineMaker.AppendEndPoints = 1
# centerlineMaker.Execute()

# To write centerlines
# centerlineWriter = vmtkscripts.vmtkSurfaceWriter()
# centerlineWriter.Surface = centerlineMaker.Centerlines
# centerlineWriter.OutputFileName = r'ioc/ioc-internal-cl.vtp'
# centerlineWriter.Execute()

centerlines = centerlineReader.Surface

# To extract branches -- split and group centerlines
branchExtractor = vmtkscripts.vmtkBranchExtractor()
branchExtractor.Centerlines = centerlines
branchExtractor.RadiusArrayName = 'MaximumInscribedSphereRadius'
branchExtractor.Execute()

# Save the extracted branches
branchWriter = vmtkscripts.vmtkSurfaceWriter()
branchWriter.Surface = branchExtractor.Centerlines
branchWriter.OutputFileName = r'ioc/ioc-internal-cl-sp.vtp'
branchWriter.Execute()