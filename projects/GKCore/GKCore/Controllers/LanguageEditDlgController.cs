/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore.MVP;
using GKCore.MVP.Views;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class LanguageEditDlgController : DialogController<ILanguageEditDlg>
    {
        private GEDCOMLanguageID fLanguageID;

        public GEDCOMLanguageID LanguageID
        {
            get { return fLanguageID; }
            set {
                if (fLanguageID != value) {
                    fLanguageID = value;
                    UpdateView();
                }
            }
        }

        public LanguageEditDlgController(ILanguageEditDlg view) : base(view)
        {
            for (var lid = GEDCOMLanguageID.Unknown; lid < GEDCOMLanguageEnum.LastVal; lid++) {
                fView.LanguageCombo.AddItem(GEDCOMLanguageEnum.Instance.GetStrValue(lid), lid);
            }
        }

        public override bool Accept()
        {
            try {
                fLanguageID = (GEDCOMLanguageID)fView.LanguageCombo.SelectedTag;
                return true;
            } catch (Exception ex) {
                Logger.LogWrite("LanguageEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.LanguageCombo.Text = GEDCOMLanguageEnum.Instance.GetStrValue(fLanguageID);
        }
    }
}
