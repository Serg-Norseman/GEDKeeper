using System;
using System.Reflection;
using System.Runtime.InteropServices;

using GKCore.Interfaces;

[assembly: AssemblyTitle("GKTreeVizPlugin")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2014, Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: ComVisible(false)]
// The assembly version has following format: Major.Minor.Build.Revision
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

namespace GKTreeVizPlugin
{
    public class Plugin : IPlugin
    {
        private string fDisplayName = "3D Визуализатор деревьев";
        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return this.fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        public void Execute()
        {
            //frmP2Main frm = new frmP2Main(this);
            //frm.ShowDialog();
            
        	IBase curBase = this.fHost.GetCurrentFile(true);
			using (TreeVizViewer viewer = new TreeVizViewer(curBase, 2/*decimal.ToInt32(this.edMinGens.Value)*/))
			{
				viewer.ShowDialog();
			}
        }

        public void OnHostClosing(ref bool cancelClosing) {}
		public void OnHostActivate() {}
		public void OnHostDeactivate() {}

        public void OnLanguageChange()
        {
        	try
        	{
        		this.fLangMan = this.fHost.CreateLangMan(this);
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKTreeVizPlugin.OnLanguageChange(): " + ex.Message);
        	}
        }

        public bool Startup(IHost host)
        {
        	bool result = true;
        	try
        	{
        		this.fHost = host;
        		// Implement any startup code here
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKTreeVizPlugin.Startup(): " + ex.Message);
        		result = false;
        	}
        	return result;
        }

        public bool Shutdown()
        {
        	bool result = true;
        	try
        	{
        		// Implement any shutdown code here
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKTreeVizPlugin.Shutdown(): " + ex.Message);
        		result = false;
        	}
        	return result;
        }

    }
}
