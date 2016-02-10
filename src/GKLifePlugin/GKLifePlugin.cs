using System;
using System.Reflection;
using System.Runtime.InteropServices;

using ConwayLife;
using GKCore.Interfaces;

[assembly: AssemblyTitle("GKLifePlugin")]
[assembly: AssemblyDescription("GEDKeeper2 LifeGame plugin")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2009, Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(false)]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

namespace GKLifePlugin
{
	public enum LLS
	{
		/* 000 */ LSID_LifeGame,
		/* 001 */ LSID_Step,
		/* 002 */ LSID_Start,
		/* 003 */ LSID_Stop,
		/* 004 */ LSID_SetCells,
		/* 005 */ LSID_Clear,
		/* 006 */ LSID_Random,
		/* 007 */ LSID_Options,
	}

	public class Plugin : IPlugin
    {
        private string fDisplayName = "Conway's Game of Life";
        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        public void Execute()
        {
            LifeForm frm = new LifeForm();

			frm.Viewer.Options.LS_LifeGame = fLangMan.LS(LLS.LSID_LifeGame);
			frm.Viewer.Options.LS_Step = fLangMan.LS(LLS.LSID_Step);
			frm.Viewer.Options.LS_Start = fLangMan.LS(LLS.LSID_Start);
			frm.Viewer.Options.LS_Stop = fLangMan.LS(LLS.LSID_Stop);
			frm.Viewer.Options.LS_SetCells = fLangMan.LS(LLS.LSID_SetCells);
			frm.Viewer.Options.LS_Clear = fLangMan.LS(LLS.LSID_Clear);
			frm.Viewer.Options.LS_Random = fLangMan.LS(LLS.LSID_Random);
			frm.Viewer.Options.LS_Options = fLangMan.LS(LLS.LSID_Options);

			frm.ShowDialog();
        }

        public void OnHostClosing(ref bool cancelClosing) {}
		public void OnHostActivate() {}
		public void OnHostDeactivate() {}

		public void OnLanguageChange()
        {
        	try
        	{
        		this.fLangMan = this.fHost.CreateLangMan(this);
        		this.fDisplayName = this.fLangMan.LS(LLS.LSID_LifeGame);
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKLifePlugin.OnLanguageChange(): " + ex.Message);
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
        		fHost.LogWrite("GKLifePlugin.Startup(): " + ex.Message);
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
        		fHost.LogWrite("GKLifePlugin.Shutdown(): " + ex.Message);
        		result = false;
        	}
        	return result;
        }

    }
}
