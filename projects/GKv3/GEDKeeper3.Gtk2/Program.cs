using System;
using System.Reflection;
using System.Resources;
using Eto;
using Eto.Forms;

[assembly: AssemblyTitle("GEDKeeper3.Gtk2")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2009-2017 by Sergey V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyVersion("3.0.0.0")]
[assembly: NeutralResourcesLanguage("en")]

namespace GEDKeeper3.Gtk2
{
    public class Program
    {
        [STAThread]
        public static void Main(string[] args)
        {
            new Application(Platforms.Gtk2).Run(new BaseWinSDI());
        }
    }
}
