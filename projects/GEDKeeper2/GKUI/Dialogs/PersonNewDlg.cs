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
using GKCore.Cultures;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class PersonNewDlg : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;
        private GEDCOMIndividualRecord fTarget;
        private TargetMode fTargetMode;

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        public GEDCOMIndividualRecord Target
        {
            get { return this.fTarget; }
            set { this.SetTarget(value); }
        }

        public TargetMode TargetMode
        {
            get { return this.fTargetMode; }
            set { this.fTargetMode = value; }
        }

        private void SetTarget(GEDCOMIndividualRecord value)
        {
            try
            {
                this.fTarget = value;

                if (this.fTarget != null)
                {
                    string iFamily, iName, iPatronymic;
                    this.fTarget.GetNameParts(out iFamily, out iName, out iPatronymic);
                    this.txtSurname.Text = iFamily;
                    INamesTable names = MainWin.Instance.NamesTable;
                    GEDCOMSex sx = (GEDCOMSex)this.cmbSex.SelectedIndex;

                    switch (this.fTargetMode) {
                        case TargetMode.tmParent:
                            if (sx == GEDCOMSex.svFemale) {
                                this.txtSurname.Text = RussianCulture.GetRusWifeSurname(iFamily);
                            }
                            this.cmbPatronymic.Items.Add(names.GetPatronymicByName(iName, GEDCOMSex.svMale));
                            this.cmbPatronymic.Items.Add(names.GetPatronymicByName(iName, GEDCOMSex.svFemale));
                            this.cmbPatronymic.Text = names.GetPatronymicByName(iName, sx);
                            break;

                        case TargetMode.tmChild:
                            switch (sx) {
                                case GEDCOMSex.svMale:
                                    this.txtName.Text = names.GetNameByPatronymic(iPatronymic);
                                    break;
                                case GEDCOMSex.svFemale:
                                    this.txtSurname.Text = '(' + RussianCulture.GetRusWifeSurname(iFamily) + ')';
                                    break;
                            }
                            break;
                            
                        case TargetMode.tmWife:
                            this.txtSurname.Text = '(' + RussianCulture.GetRusWifeSurname(iFamily) + ')';
                            break;
                    }
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("PersonNewDlg.SetTarget("+this.fTargetMode.ToString()+"): " + ex.Message);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            base.DialogResult = DialogResult.OK;
        }

        private void edFamily_KeyDown(object sender, KeyEventArgs e)
        {
            TextBox tb = (sender as TextBox);
            
            if (tb != null && e.KeyCode == Keys.Down && e.Control)
            {
                tb.Text = GEDCOMUtils.NormalizeName(tb.Text);
            }
        }

        private void edFamily_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == '/')
            {
                e.Handled = true;
            }
        }

        public PersonNewDlg(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.btnAccept.Image = global::GKResources.iBtnAccept;
            this.btnCancel.Image = global::GKResources.iBtnCancel;

            this.fBase = aBase;

            for (GEDCOMSex sx = GEDCOMSex.svNone; sx <= GEDCOMSex.svUndetermined; sx++)
            {
                this.cmbSex.Items.Add(GKUtils.SexStr(sx));
            }

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_WinPersonNew);
            this.lblSurname.Text = LangMan.LS(LSID.LSID_Surname);
            this.lblName.Text = LangMan.LS(LSID.LSID_Name);
            this.lblPatronymic.Text = LangMan.LS(LSID.LSID_Patronymic);
            this.lblSex.Text = LangMan.LS(LSID.LSID_Sex);
        }
    }
}
