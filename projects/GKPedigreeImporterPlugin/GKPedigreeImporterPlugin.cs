/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Reflection;
using System.Runtime.InteropServices;

using GKCore.Interfaces;

[assembly: AssemblyTitle("GKPedigreeImporterPlugin")]
[assembly: AssemblyDescription("GEDKeeper2 PedigreeImporter plugin")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2014,2016, Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(false)]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.2.0.0")]
[assembly: AssemblyFileVersion("1.2.0.0")]

namespace GKPedigreeImporterPlugin
{
    public enum ILS
    {
    	LSID_PluginTitle,
    	LSID_File, /* 0 */
    	LSID_DlgSelect, /* 100 */
		LSID_FormatUnsupported, /* 396 */
		LSID_DataLoadError, 
		LSID_ParseError_LineSeq, 
		LSID_PersonParsed, 
		LSID_Generation, 
		LSID_ParseError_AncNotFound, 
		LSID_ParseError_DateInvalid,
		LSID_ParseError_NumDuplicate
    }
    
    public class PlugIn : IPlugin
    {
        private const string DISPLAY_NAME = "GKPedigreeImporterPlugin";

        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName {
        	get {
                return (fLangMan == null) ? DISPLAY_NAME : this.fLangMan.LS(ILS.LSID_PluginTitle);
        	}
        }

        public IHost Host { get { return this.fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        public void Execute()
        {
            frmPedigreeImporter frm = new frmPedigreeImporter(this);
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
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKPedigreeImporterPlugin.OnLanguageChange(): " + ex.Message);
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
        		fHost.LogWrite("GKPedigreeImporterPlugin.Startup(): " + ex.Message);
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
        		fHost.LogWrite("GKPedigreeImporterPlugin.Shutdown(): " + ex.Message);
        		result = false;
        	}
        	return result;
        }

    }
}
