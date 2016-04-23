using System;
using System.Reflection;
using System.Runtime.InteropServices;

using GKCore.Interfaces;

[assembly: AssemblyTitle("GKImageViewerPlugin")]
[assembly: AssemblyDescription("GEDKeeper2 ImageViewer plugin")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2015, Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(false)]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

namespace GKImageViewerPlugin
{
	public enum IVLS
	{
		/* 000 */ LSID_ImgViewer
	}

    public class Plugin : IPlugin
    {
        private string fDisplayName = "ImageViewer";
        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        public void Execute()
        {
        	try
        	{
        		ImageViewer frm = new ImageViewer(this);
        		this.fHost.ShowMDI(frm);
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKImageViewerPlugin.Execute(): " + ex.Message);
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
        		this.fDisplayName = this.fLangMan.LS(IVLS.LSID_ImgViewer);
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKImageViewerPlugin.OnLanguageChange(): " + ex.Message);
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
        		fHost.LogWrite("GKImageViewerPlugin.Startup(): " + ex.Message);
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
        		fHost.LogWrite("GKImageViewerPlugin.Shutdown(): " + ex.Message);
        		result = false;
        	}
        	return result;
        }

    }
}
