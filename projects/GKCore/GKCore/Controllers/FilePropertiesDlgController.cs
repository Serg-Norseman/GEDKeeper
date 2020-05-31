﻿/*
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
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class FilePropertiesDlgController : DialogController<IFilePropertiesDlg>
    {

        public FilePropertiesDlgController(IFilePropertiesDlg view) : base(view)
        {
        }

        public override bool Accept()
        {
            try {
                fBase.Context.Tree.Header.Language = GEDCOMUtils.GetLanguageVal(fView.Language.Text);

                GDMSubmitterRecord submitter = fBase.Context.Tree.GetSubmitter();
                submitter.Name.StringValue = fView.Name.Text;
                submitter.Address.SetAddressArray(fView.Address.Lines);

                if (submitter.Address.PhoneNumbers.Count > 0) {
                    submitter.Address.PhoneNumbers[0].StringValue = fView.Tel.Text;
                } else {
                    submitter.Address.AddPhoneNumber(fView.Tel.Text);
                }

                fBase.NotifyRecord(submitter, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("FilePropertiesDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.Language.Text = GEDCOMUtils.GetLanguageStr(fBase.Context.Tree.Header.Language);

            GDMSubmitterRecord submitter = fBase.Context.Tree.GetSubmitter();
            fView.Name.Text = submitter.Name.FullName;
            fView.Address.Text = submitter.Address.Lines.Text;

            if (submitter.Address.PhoneNumbers.Count > 0) {
                fView.Tel.Text = submitter.Address.PhoneNumbers[0].StringValue;
            }

            // update stats
            int[] stats = fBase.Context.Tree.GetRecordStats();
            fView.RecordStats.ClearItems();
            for (int i = 1; i < stats.Length; i++) {
                fView.RecordStats.AddItem(null, LangMan.LS(GKData.RecordTypes[i]), stats[i].ToString());
            }
        }

        public void ChangeLanguage()
        {
            using (var dlg = AppHost.ResolveDialog<ILanguageEditDlg>()) {
                dlg.LanguageID = fBase.Context.Tree.Header.Language;

                if (dlg.ShowModalX(this)) {
                    // Assignment in control, instead of the header's property to work Cancel.
                    fView.Language.Text = GEDCOMUtils.GetLanguageStr(dlg.LanguageID);
                }
            }
        }
    }
}
