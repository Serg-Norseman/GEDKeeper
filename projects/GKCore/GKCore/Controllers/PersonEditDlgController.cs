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

#define SEX_SYMBOLS

using System;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PersonEditDlgController : DialogController<IPersonEditDlg>
    {
        private GEDCOMIndividualRecord fPerson;

        public GEDCOMIndividualRecord Person
        {
            get { return fPerson; }
            set {
                if (fPerson != value) {
                    fPerson = value;
                    UpdateView();
                }
            }
        }


        public PersonEditDlgController(IPersonEditDlg view) : base(view)
        {
            for (GEDCOMRestriction res = GEDCOMRestriction.rnNone; res <= GEDCOMRestriction.rnPrivacy; res++) {
                fView.RestrictionCombo.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            for (GEDCOMSex sx = GEDCOMSex.svNone; sx <= GEDCOMSex.svUndetermined; sx++) {
                string name = GKUtils.SexStr(sx);
                IImage image = null;
                #if SEX_SYMBOLS
                switch (sx) {
                    case GEDCOMSex.svMale:
                        image = AppHost.GfxProvider.LoadResourceImage("sym_male.png", true);
                        break;
                    case GEDCOMSex.svFemale:
                        image = AppHost.GfxProvider.LoadResourceImage("sym_female.png", true);
                        break;
                }
                #endif
                fView.SexCombo.AddItem(name, null, image);
            }
        }

        public override bool Accept()
        {
            try {
                return true;
            } catch (Exception ex) {
                Logger.LogWrite("PersonEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
        }
    }
}
