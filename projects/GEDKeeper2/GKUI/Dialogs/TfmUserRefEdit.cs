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
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmUserRefEdit : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;

        private GEDCOMUserReference fUserRef;

        public GEDCOMUserReference UserRef
        {
            get { return this.fUserRef; }
            set { this.SetUserRef(value); }
        }

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        private void SetUserRef(GEDCOMUserReference value)
        {
            this.fUserRef = value;
            this.EditRef.Text = this.fUserRef.StringValue;
            this.EditType.Text = this.fUserRef.ReferenceType;
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.fUserRef.StringValue = this.EditRef.Text;
                this.fUserRef.ReferenceType = this.EditType.Text;
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TfmUserRefEdit.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        public TfmUserRefEdit(IBaseWindow aBase)
        {
            this.InitializeComponent();
            this.fBase = aBase;

            for (SpecialUserRef ur = SpecialUserRef.urCustom; ur <= SpecialUserRef.urLast; ur++)
            {
                this.EditRef.Items.Add(GKData.SpecialUserRefs[(int)ur]);
            }

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_WinUserRefEdit);
            this.lblReference.Text = LangMan.LS(LSID.LSID_Reference);
            this.lblRefType.Text = LangMan.LS(LSID.LSID_Type);
        }
    }
}
