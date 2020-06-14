/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using BSLib.Design.Graphics;
using GKCore;
using GKCore.Interfaces;
using GKCore.Plugins;

namespace GKSamplePlugin
{
    public class PersonDialogPlugin : OrdinaryPlugin, IDialogReplacement
    {
        private const string DISPLAY_NAME = "PersonDialogPlugin";

        private bool fEnabled;
        private ILangMan fLangMan;

        public override string DisplayName { get { return DISPLAY_NAME; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.DialogReplacement; } }

        public bool Enabled
        {
            get { return fEnabled; }
            set { fEnabled = value; }
        }


        public override void Execute()
        {
            // dummy
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
            } catch (Exception ex) {
                Logger.WriteError("PersonDialogPlugin.OnLanguageChange()", ex);
            }
        }

        #region IDialogReplacement implementation

        public Type GetDialogType()
        {
            return typeof(PersonEditDlgEx);
        }

        public ICommonDialog CreateDialog(params object[] parameters)
        {
            IBaseWindow baseWin = (parameters.Length > 0) ? (IBaseWindow)parameters[0] : null;
            return new PersonEditDlgEx(baseWin);
        }

        #endregion
    }
}
