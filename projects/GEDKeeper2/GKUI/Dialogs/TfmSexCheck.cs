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

using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmSexCheck : Form
    {
        public TfmSexCheck()
        {
            this.InitializeComponent();

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_WinCheckSex);
            this.GroupBox1.Text = LangMan.LS(LSID.LSID_Sex);
            this.sbMale.Text = LangMan.LS(LSID.LSID_SexM);
            this.sbFemale.Text = LangMan.LS(LSID.LSID_SexF);
        }
        
        public string IndividualName
        {
            get { return this.edName.Text; }
            set { this.edName.Text = value; }
        }
        
        public GEDCOMSex Sex
        {
            get
            {
                if (this.sbMale.Checked) {
                    return GEDCOMSex.svMale;
                }
                if (this.sbFemale.Checked) {
                    return GEDCOMSex.svFemale;
                }
                return GEDCOMSex.svNone;
            }
            set
            {
                switch (value)
                {
                    case GEDCOMSex.svNone:
                    case GEDCOMSex.svUndetermined:
                        this.sbNone.Checked = true;
                        break;

                    case GEDCOMSex.svMale:
                        this.sbMale.Checked = true;
                        break;

                    case GEDCOMSex.svFemale:
                        this.sbFemale.Checked = true;
                        break;
                }
            }
        }
    }
}
