using System;
using System.ComponentModel;
using System.Reflection;
using System.Runtime.InteropServices;
using GKCommon.GEDCOM;
using GKCore;
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
[assembly: ComVisible(false)]
// The assembly version has following format: Major.Minor.Build.Revision
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
        private string fDisplayName = "Полнотекстовый поиск";
        private IHost fHost;
        private ILangMan fLangMan;
        
        private SearchManager fSearchMan;

        public string DisplayName {
        	get {
        		if (fLangMan == null) {
        			return this.fDisplayName;
        		} else {
        			return this.fLangMan.LS(TLS.LSID_PluginTitle);
        		}
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
			IBase curBase = fHost.GetCurrentFile();
		    if (curBase == null) return;

		    TfmTextSearch ts_dlg = new TfmTextSearch(this, curBase);
		    //ts_dlg.MdiParent = this;
		    ts_dlg.Show();
        }

        public void NotifyRecord(IBase aBase, object record, RecordAction action)
        {
        	if (aBase == null || record == null || this.fSearchMan == null) return;
        	
        	switch (action) {
        		case RecordAction.raEdit:
        			this.fSearchMan.UpdateRecord(aBase, record as GEDCOMRecord);
        			break;

        		case RecordAction.raDelete:
        			this.fSearchMan.DeleteRecord(aBase, (record as GEDCOMRecord).XRef);
        			break;
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
