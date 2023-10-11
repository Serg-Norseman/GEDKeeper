using System.Reflection;
using System.Runtime.CompilerServices;
using GKCore;

[assembly: AssemblyTitle("GKCore")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyProduct(GKData.APP_TITLE)]
[assembly: AssemblyCopyright(GKData.APP_COPYRIGHT)]
[assembly: AssemblyVersion(GKData.APP_VERSION_3X)]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

[assembly: InternalsVisibleTo("GKComponents")]
