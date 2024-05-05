from vmtk import vmtkscripts

surfaceReader = vmtkscripts.vmtkSurfaceReader()
surfaceReader.InputFileName = r'ioc/ioc-internal.stl'
surfaceReader.Execute()

# To read centerlines from existing file
centerlineReader = vmtkscripts.vmtkSurfaceReader()
centerlineReader.InputFileName = r'ioc/ioc-internal-cl-sp.vtp'
centerlineReader.Execute()

centerlines = centerlineReader.Surface

# Option to rename any groups
# centerlineLabeler = vmtkscripts.vmtkCenterlineLabeler()
# centerlineLabeler.Centerlines = branchExtractor.Centerlines
# centerlineLabeler.LabelIdsArrayName = "GroupIds"
# centerlineLabeler.Execute()

# Clip branches from model
branchClipper = vmtkscripts.vmtkBranchClipper()
branchClipper.Surface = surfaceReader.Surface
branchClipper.Centerlines = centerlines
branchClipper.RadiusArrayName = 'MaximumInscribedSphereRadius'
branchClipper.Interactive = True
branchClipper.Execute()

# Write clipped branches to surface
clipSurfaceWriter = vmtkscripts.vmtkSurfaceWriter()
clipSurfaceWriter.Surface = branchClipper.Surface
clipSurfaceWriter.OutputFileName = r'ioc/ioc-internal-clipped.vtp'
clipSurfaceWriter.Execute()

# Write clipped branches centerlines
clipSurfaceWriter.OutputFileName = r'ioc/ioc-internal-clipped-cl.vtp'
clipSurfaceWriter.Surface = branchClipper.Centerlines
clipSurfaceWriter.Execute()

# Write clipped branches to STL -- necessary for recognizing format
# https://github.com/vmtk/vmtk/blob/e9ebb8c420d7e2aba7a7e68c11355794e63f3716/vmtkScripts/vmtksurfacewriter.py#L242
clipSurfaceWriter = vmtkscripts.vmtkSurfaceWriter()
clipSurfaceWriter.Surface = branchClipper.Surface
clipSurfaceWriter.OutputFileName = r'ioc/ioc-internal-clipped.stl'
clipSurfaceWriter.Execute()

# View clipped branches
clipSurfaceViewer = vmtkscripts.vmtkSurfaceViewer()
clipSurfaceViewer.Surface = branchClipper.Surface
clipSurfaceViewer.ArrayName = 'GroupIds'
clipSurfaceViewer.Execute()