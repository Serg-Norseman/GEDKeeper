using System;
using System.Reflection;
using System.Runtime.InteropServices;

using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

[assembly: AssemblyTitle("GKTextSearchPlugin")]
[assembly: AssemblyDescription("GEDKeeper2 TextSearch plugin")]
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

namespace GKTextSearchPlugin
{
    public enum TLS
    {
    	LSID_PluginTitle,
    	LSID_SearchIndexRefreshing,
    	LSID_SearchResults
    }

    public class Plugin : IPlugin, ISubscriber
    {
        private const string DISPLAY_NAME = "GKTextSearchPlugin";

        private IHost fHost;
        private ILangMan fLangMan;
        
        private SearchManager fSearchMan;

        public string DisplayName {
        	get {
        	    return (fLangMan == null) ? DISPLAY_NAME : this.fLangMan.LS(TLS.LSID_PluginTitle);
        	}
        }
        
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

		public SearchManager SearchMan
		{
			get { return this.fSearchMan; }
		}

        public void Execute()
        {
            //PluginForm frm = new PluginForm(this);
            //frm.ShowDialog();
			IBaseWindow curBase = fHost.GetCurrentFile();
		    if (curBase == null) return;

		    TfmTextSearch tsDlg = new TfmTextSearch(this, curBase);
		    //ts_dlg.MdiParent = this;
		    tsDlg.Show();
        }

        public void NotifyRecord(IBaseWindow aBase, object record, RecordAction action)
        {
        	if (aBase == null || record == null || this.fSearchMan == null) return;
        	
        	switch (action) {
        		case RecordAction.raEdit:
                    this.fSearchMan.UpdateRecord(aBase, (GEDCOMRecord)record);
        			break;

        		case RecordAction.raDelete:
                    this.fSearchMan.DeleteRecord(aBase, ((GEDCOMRecord)record).XRef);
        			break;
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
        		fHost.LogWrite("GKTextSearchPlugin.OnLanguageChange(): " + ex.Message);
        	}
        }
        
        public bool Startup(IHost host)
        {
        	bool result = true;
        	try
        	{
        		this.fHost = host;
        		this.fLangMan = this.fHost.CreateLangMan(this);
        		this.fSearchMan = new SearchManager(this);
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKTextSearchPlugin.Startup(): " + ex.Message);
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
        		//if (this.fSearchMan != null) this.fSearchMan.Dispose();
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKTextSearchPlugin.Shutdown(): " + ex.Message);
        		result = false;
        	}
        	return result;
        }

    }
}
