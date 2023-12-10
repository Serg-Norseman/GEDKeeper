/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class SexCheckController : DialogController<ISexCheckDlg>
    {
        public GDMSex Sex
        {
            get {
                if (GetControl<IRadioButton>("rbMale").Checked) {
                    return GDMSex.svMale;
                }
                if (GetControl<IRadioButton>("rbFemale").Checked) {
                    return GDMSex.svFemale;
                }
                return GDMSex.svUnknown;
            }
            set {
                switch (value) {
                    case GDMSex.svUnknown:
                    case GDMSex.svIntersex:
                        GetControl<IRadioButton>("rbNone").Checked = true;
                        break;

                    case GDMSex.svMale:
                        GetControl<IRadioButton>("rbMale").Checked = true;
                        break;

                    case GDMSex.svFemale:
                        GetControl<IRadioButton>("rbFemale").Checked = true;
                        break;
                }
            }
        }

        public SexCheckController(ISexCheckDlg view) : base(view)
        {
        }

        public override void UpdateView()
        {
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.WinCheckSex);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);

            GetControl<IGroupBox>("grpSex").Text = LangMan.LS(LSID.Sex);
            GetControl<IRadioButton>("rbNone").Text = " ? ";
            GetControl<IRadioButton>("rbMale").Text = LangMan.LS(LSID.SexM);
            GetControl<IRadioButton>("rbFemale").Text = LangMan.LS(LSID.SexF);
        }
    }
}
