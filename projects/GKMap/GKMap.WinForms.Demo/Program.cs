using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Windows.Forms;

[assembly: AssemblyTitle("GKMap.WinForms.Demo")]
[assembly: AssemblyDescription("Demo for GKMap (WinForms)")]
[assembly: AssemblyProduct("GKMap")]
[assembly: ComVisible(false)]
[assembly: AssemblyCulture("")]
[assembly: AssemblyCopyright("Copyright © 2009-2018 by radioman")]
[assembly: AssemblyVersion("1.8.0")]

namespace GKMap.WinForms.Demo
{
    static class Program
    {
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new MainForm());
        }
    }
}
