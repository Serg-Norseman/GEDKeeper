using System;
using System.Reflection;
using System.Runtime.InteropServices;

using GKCore.Interfaces;

[assembly: AssemblyTitle("GKNamesBookPlugin")]
[assembly: AssemblyDescription("GEDKeeper2 NamesBook plugin")]
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

namespace GKNamesBookPlugin
{
    public enum NLS
    {
		/* 032 */ LSID_MINamesBook
    }
    
    public sealed class Plugin : IPlugin, IWidget
    {
        private string fDisplayName = "GKNamesBookPlugin";
        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        private TfmNamesBook frm;
        
        public void Execute()
        {
			if (!this.fHost.IsWidgetActive(this)) {
				frm = new TfmNamesBook(this);
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
        		this.fDisplayName = this.fLangMan.LS(NLS.LSID_MINamesBook);
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKNamesBookPlugin.OnLanguageChange(): " + ex.Message);
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
        		fHost.LogWrite("GKNamesBookPlugin.Startup(): " + ex.Message);
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
        		fHost.LogWrite("GKNamesBookPlugin.Shutdown(): " + ex.Message);
        		result = false;
        	}
        	return result;
        }

        #region IWidget support

        void IWidget.WidgetInit(IHost host) {}
        void IWidget.BaseChanged(IBaseWindow aBase) {}
        void IWidget.BaseClosed(IBaseWindow aBase) {}
        void IWidget.WidgetEnable() {}

        #endregion
    }
}
