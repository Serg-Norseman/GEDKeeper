using System;
using System.ComponentModel;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GKCore.Interfaces;

[assembly: AssemblyTitle("GKNamesBookPlugin")]
[assembly: AssemblyDescription("GEDKeeper2 NamesBook plugin")]
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

namespace GKNamesBookPlugin
{
    public enum NLS
    {
		/* 032 */ LSID_MINamesBook
    }
    
    public class Plugin : IPlugin, IWidget
    {
        private string fDisplayName = "Тестовый плагин";
        private IHost fHost;
        private ILangMan fLangMan;
    	private MenuItem fMenuItem; // for IWidget

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        private TfmNamesBook frm;
        
        public void Execute()
        {
			if (!this.fMenuItem.Checked) {
				frm = new TfmNamesBook(this);
				frm.Show();
			} else {
				frm.Close();
			}
        }

        public void OnHostClosing(object sender, CancelEventArgs e) {}
		public void OnHostActivated(object sender, EventArgs e) {}
		public void OnHostDeactivate(object sender, EventArgs e) {}

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

    	#region IWidget common

    	MenuItem IWidget.MenuItem
    	{
    		get { return this.fMenuItem; }
    	}

    	void IWidget.WidgetInit(IHost host, MenuItem menuItem)
    	{
    		this.fMenuItem = menuItem;
    	}

        void IWidget.BaseChanged(IBase aBase) {}
        void IWidget.WidgetEnable() {}

        #endregion
    }
}
