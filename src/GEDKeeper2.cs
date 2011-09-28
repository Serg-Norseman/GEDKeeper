using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GKUI;

[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2009-2011 Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyTitle("GEDKeeper2")]
[assembly: AssemblyVersion("2.0.437.1")]

[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile("")]
[assembly: AssemblyKeyName("")]

[assembly: ComVisible(false)]
//[assembly: Guid("")]
//[assembly: TypeLibVersion(1, 0)]

namespace GEDKeeper2
{
	internal sealed class Program
	{
		[STAThread]
		private static void Main(string[] args)
		{
			Application.EnableVisualStyles();
			Application.SetCompatibleTextRenderingDefault(false);
			Application.Run(new TfmGEDKeeper(args));
		}
	}
}
