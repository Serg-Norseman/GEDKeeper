using System;
using System.Reflection;
using System.Runtime.InteropServices;

using GKCore.Interfaces;

[assembly: AssemblyTitle("GKCalendarPlugin")]
[assembly: AssemblyDescription("GEDKeeper2 Calendar plugin")]
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

namespace GKCalendarPlugin
{
    public enum PLS
    {
		/* 033 */ LSID_MICalendar,
		/* 140 */ LSID_Date,
		/* 157 */ LSID_Cal_Gregorian,
		/* 158 */ LSID_Cal_Julian,
		/* 159 */ LSID_Cal_Hebrew,
		/* 160 */ LSID_Cal_Islamic,
		/* 161 */ LSID_Cal_Persian,
		/* 162 */ LSID_Cal_Indian,
		/* 163 */ LSID_Cal_Bahai,
    }
    
    public sealed class Plugin : IPlugin, IWidget
    {
        private string fDisplayName = "GKCalendarPlugin";
        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        private TfmCalendar frm;
        
        public void Execute()
        {
			if (!this.fHost.IsWidgetActive(this)) {
				frm = new TfmCalendar(this);
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
        		this.fDisplayName = this.fLangMan.LS(PLS.LSID_MICalendar);
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKCalendarPlugin.OnLanguageChange(): " + ex.Message);
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
        		fHost.LogWrite("GKCalendarPlugin.Startup(): " + ex.Message);
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
        		fHost.LogWrite("GKCalendarPlugin.Shutdown(): " + ex.Message);
        		result = false;
        	}
        	return result;
        }

    	#region IWidget common

    	void IWidget.WidgetInit(IHost host) {}
        void IWidget.BaseChanged(IBaseWindow aBase) {}
        void IWidget.BaseClosed(IBaseWindow aBase) {}
        void IWidget.WidgetEnable() {}

        #endregion
    }
}
