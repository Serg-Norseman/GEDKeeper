using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using GKCore.Interfaces;

[assembly: AssemblyTitle("GKTreeVizPlugin")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2014, Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(false)]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

namespace GKTreeVizPlugin
{
    public sealed class Plugin : IPlugin
    {
        private const string DISPLAY_NAME = "GKTreeVizPlugin";

        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return DISPLAY_NAME; } }
        public IHost Host { get { return this.fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        public void Execute()
        {
        	using (frmTVPSettings frm = new frmTVPSettings(this))
        	{
        		if (frm.ShowDialog() == DialogResult.OK)
        		{
        			IBaseWindow curBase = this.fHost.GetCurrentFile(true);

        			using (TreeVizViewer viewer = new TreeVizViewer(curBase, frm.MinGens))
        			{
        				viewer.ShowDialog();
        			}
        		}
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
