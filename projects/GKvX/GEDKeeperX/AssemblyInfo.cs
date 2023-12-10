using System.Reflection;
using System.Runtime.CompilerServices;
using GKCore;
using Xamarin.Forms.Xaml;

[assembly: AssemblyTitle("GEDKeeperX")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyProduct(GKData.APP_TITLE)]
[assembly: AssemblyCopyright(GKData.APP_COPYRIGHT)]
[assembly: AssemblyVersion(GKData.APP_VERSION_3X)]
[assembly: AssemblyCulture("")]

[assembly: XamlCompilation(XamlCompilationOptions.Compile)]

[assembly: InternalsVisibleTo("GEDKeeperX.Android")]
[assembly: InternalsVisibleTo("GEDKeeperX.iOS")]
