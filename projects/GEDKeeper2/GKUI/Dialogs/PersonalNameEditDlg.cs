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
using GKCore.Options;
using GKCore.Types;

namespace GKUI.Dialogs
{
    public partial class PersonalNameEditDlg: Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;

        private GEDCOMPersonalName fPersonalName;

        public GEDCOMPersonalName PersonalName
        {
            get { return this.fPersonalName; }
            set { this.SetPersonalName(value); }
        }

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }


        private void SetPersonalName(GEDCOMPersonalName value)
        {
            this.fPersonalName = value;
            try
            {
                this.UpdateControls();
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("PersonalNameEditDlg.SetPersonalName(): " + ex.Message);
            }
        }

        private bool IsExtendedWomanSurname()
        {
            GEDCOMIndividualRecord iRec = this.fPersonalName.Parent as GEDCOMIndividualRecord;

            bool result = (GlobalOptions.Instance.WomanSurnameFormat != WomanSurnameFormat.wsfNotExtend) &&
                (iRec.Sex == GEDCOMSex.svFemale);
            return result;
        }

        private void UpdateControls()
        {
            string surname, name, patronymic;
            GKUtils.GetRusNameParts(this.fPersonalName, out surname, out name, out patronymic);

            this.txtSurname.Text = surname;
            this.txtName.Text = name;
            this.txtPatronymic.Text = patronymic;
            this.cmbNameType.SelectedIndex = (sbyte)this.fPersonalName.NameType;

            this.txtNamePrefix.Text = fPersonalName.Pieces.Prefix;
            this.txtNickname.Text = fPersonalName.Pieces.Nickname;
            this.txtSurnamePrefix.Text = fPersonalName.Pieces.SurnamePrefix;
            this.txtNameSuffix.Text = fPersonalName.Pieces.Suffix;

            this.txtMarriedSurname.Text = fPersonalName.Pieces.MarriedName;

            if (!this.IsExtendedWomanSurname()) {
                this.lblSurname.Text = LangMan.LS(LSID.LSID_Surname);
                this.txtMarriedSurname.Enabled = false;
            } else {
                this.lblSurname.Text = LangMan.LS(LSID.LSID_MaidenSurname);
                this.txtMarriedSurname.Enabled = true;
            }
        }

        private void AcceptChanges()
        {
            GKUtils.SetRusNameParts(fPersonalName, this.txtSurname.Text, this.txtName.Text, this.txtPatronymic.Text);

            GEDCOMPersonalNamePieces pieces = fPersonalName.Pieces;
            pieces.Nickname = this.txtNickname.Text;
            pieces.Prefix = this.txtNamePrefix.Text;
            pieces.SurnamePrefix = this.txtSurnamePrefix.Text;
            pieces.Suffix = this.txtNameSuffix.Text;

            this.fPersonalName.NameType = (GEDCOMNameType)this.cmbNameType.SelectedIndex;
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.AcceptChanges();
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("PersonalNameEditDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        public PersonalNameEditDlg(IBaseWindow aBase)
        {
            InitializeComponent();

            this.btnAccept.Image = global::GKResources.iBtnAccept;
            this.btnCancel.Image = global::GKResources.iBtnCancel;

            this.fBase = aBase;

            for (GEDCOMNameType nt = GEDCOMNameType.ntNone; nt <= GEDCOMNameType.ntMarried; nt++)
            {
                this.cmbNameType.Items.Add(LangMan.LS(GKData.NameTypes[(int)nt]));
            }

            this.SetLang();
        }

        public void SetLang()
        {
            this.Text = LangMan.LS(LSID.LSID_Name);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.lblSurname.Text = LangMan.LS(LSID.LSID_Surname);
            this.lblMarriedSurname.Text = LangMan.LS(LSID.LSID_MarriedSurname);
            this.lblName.Text = LangMan.LS(LSID.LSID_Name);
            this.lblPatronymic.Text = LangMan.LS(LSID.LSID_Patronymic);
            this.lblNickname.Text = LangMan.LS(LSID.LSID_Nickname);
            this.lblSurnamePrefix.Text = LangMan.LS(LSID.LSID_SurnamePrefix);
            this.lblNamePrefix.Text = LangMan.LS(LSID.LSID_NamePrefix);
            this.lblNameSuffix.Text = LangMan.LS(LSID.LSID_NameSuffix);
            this.lblType.Text = LangMan.LS(LSID.LSID_Type);
        }
    }
}
