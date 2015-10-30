using System;
using System.Reflection;
using System.Runtime.InteropServices;

using GKCore.Interfaces;

[assembly: AssemblyTitle("GKTimeLinePlugin")]
[assembly: AssemblyDescription("GEDKeeper2 TimeLine plugin")]
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

namespace GKTimeLinePlugin
{
    public enum PLS
    {
		/* 032 */ LSID_MITimeLine,
		/* 130 */ LSID_TimeScale,
		/* 131 */ LSID_CurrentYear,
    }
    
    public sealed class Plugin : IPlugin, IWidget
    {
        private string fDisplayName = "GKTimeLinePlugin";
        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        private TfmTimeLine frm;
        
        public void Execute()
        {
			if (!this.fHost.IsWidgetActive(this)) {
				frm = new TfmTimeLine(this);
				frm.Show();
			} else {
				frm.Close();
				frm = null;
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
        		this.fDisplayName = this.fLangMan.LS(PLS.LSID_MITimeLine);
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKTimeLinePlugin.OnLanguageChange(): " + ex.Message);
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
        		fHost.LogWrite("GKTimeLinePlugin.Startup(): " + ex.Message);
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
        		fHost.LogWrite("GKTimeLinePlugin.Shutdown(): " + ex.Message);
        		result = false;
        	}
        	return result;
        }

        #region IWidget support

        void IWidget.WidgetInit(IHost host) {}

        void IWidget.BaseChanged(IBaseWindow aBase)
        {
        	if (frm != null) {
        		frm.BaseChanged(aBase);
        	}
    	}
        
        void IWidget.BaseClosed(IBaseWindow aBase)
        {
        	if (frm != null) {
        		frm.BaseChanged(null);
        	}
        }

     	void IWidget.WidgetEnable() {}

        #endregion
    }
}
