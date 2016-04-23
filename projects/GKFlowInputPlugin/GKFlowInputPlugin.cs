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

[assembly: AssemblyTitle("GKFlowInputPlugin")]
[assembly: AssemblyDescription("GEDKeeper2 FlowInput plugin")]
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

namespace GKFlowInputPlugin
{
    public enum FLS
    {
		/* 023 */ LSID_PluginTitle,
		
		/* 067 */ LSID_SexM,
		/* 068 */ LSID_SexF,
		/* 085 */ LSID_Surname,
		/* 086 */ LSID_Name,
		/* 087 */ LSID_Patronymic,

		/* 100 */ LSID_DlgClose,
		/* 102 */ LSID_DlgAppend,
		/* 109 */ LSID_Note,
		/* 110 */ LSID_Source,
		/* 111 */ LSID_Page,
		/* 123 */ LSID_BirthDate,
		/* 124 */ LSID_DeathDate,
		/* 151 */ LSID_Father,
		/* 152 */ LSID_Mother,

		/* 217 */ LSID_Spouse,

		/* 302 */ LSID_FullName,
		/* 303 */ LSID_BirthPlace,
		/* 304 */ LSID_DeathPlace,
		/* 306 */ LSID_Age,
		/* 322 */ LSID_Birth,
		/* 333 */ LSID_Death,
		/* 347 */ LSID_RK_Unk,

		/* 486 */ LSID_InputSimple,
		/* 487 */ LSID_InputSource,
		/* 488 */ LSID_SourceKind,
		/* 489 */ LSID_SK_Rev,
		/* 490 */ LSID_SK_Met,
		/* 491 */ LSID_Year,
		/* 492 */ LSID_Settlement,
		/* 493 */ LSID_EventDate,
		/* 494 */ LSID_EventType,
		/* 495 */ LSID_Join,
		/* 496 */ LSID_Comment,

		/* 504 */ LSID_PLPerson,
		/* 505 */ LSID_PLGodparent,
		/* 506 */ LSID_Child,
		/* 507 */ LSID_NameInvalid,
		/* 508 */ LSID_BasePersonInvalid,
		/* 509 */ LSID_SourceYearInvalid
    }

    public class Plugin : IPlugin
    {
        private const string DISPLAY_NAME = "GKFlowInputPlugin";

        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName {
        	get {
                return (fLangMan == null) ? DISPLAY_NAME : this.fLangMan.LS(FLS.LSID_PluginTitle);
        	}
        }

        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        public void Execute()
        {
			IBaseWindow curBase = fHost.GetCurrentFile();
		    if (curBase == null) return;

		    using (TfmFlowInput frm = new TfmFlowInput(this, curBase))
		    {
           		frm.ShowDialog();
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
        		fHost.LogWrite("GKFlowInputPlugin.OnLanguageChange(): " + ex.Message);
        	}
        }
        
        public bool Startup(IHost host)
        {
        	bool result = true;
        	try
        	{
        		this.fHost = host;
        		this.fLangMan = this.fHost.CreateLangMan(this);
        	}
        	catch (Exception ex)
        	{
        		fHost.LogWrite("GKFlowInputPlugin.Startup(): " + ex.Message);
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
        		fHost.LogWrite("GKFlowInputPlugin.Shutdown(): " + ex.Message);
        		result = false;
        	}
        	return result;
        }

    }
}
