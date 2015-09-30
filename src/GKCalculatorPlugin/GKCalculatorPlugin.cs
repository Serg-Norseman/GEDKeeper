using System;
using System.Reflection;
using System.Runtime.InteropServices;

using GKCommon;
using GKCore.Interfaces;

[assembly: AssemblyTitle("GKCalculatorPlugin")]
[assembly: AssemblyDescription("GEDKeeper2 Calculator plugin")]
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

namespace GKCalculatorPlugin
{
    public enum PLS
    {
		/* 031 */ LSID_MICalc,
		/* 167 */ LSID_CopyResultToClipboard,
    }
    
    public class Plugin : IPlugin, IWidget
    {
        private string fDisplayName = "GKCalculatorPlugin";
        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        private TfmCalcWidget frm;
        
        public void Execute()
        {
        	if (!this.fHost.IsWidgetActive(this)) {
				frm = new TfmCalcWidget(this);
				frm.Show();
			} else {
				frm.Close();
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
        		this.fDisplayName = this.fLangMan.LS(PLS.LSID_MICalc);
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKCalculatorPlugin.OnLanguageChange(): " + ex.Message);
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
        		fHost.LogWrite("GKCalculatorPlugin.Startup(): " + ex.Message);
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
        		fHost.LogWrite("GKCalculatorPlugin.Shutdown(): " + ex.Message);
        		result = false;
        	}
        	return result;
        }

    	#region IWidget common

    	void IWidget.WidgetInit(IHost host) {}
        void IWidget.BaseChanged(IBase aBase) {}
        void IWidget.BaseClosed(IBase aBase) {}

        void IWidget.WidgetEnable()
        {
        	if (frm != null) {
        		NativeMethods.EnableWindow(this.frm.Handle, true);
        	}
        }

        #endregion
    }
}
