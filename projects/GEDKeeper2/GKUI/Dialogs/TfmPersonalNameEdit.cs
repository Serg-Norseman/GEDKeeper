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

namespace GKUI.Dialogs
{
    public partial class TfmPersonalNameEdit: Form, IBaseEditor
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
                this.fBase.Host.LogWrite("TfmPersonalNameEdit.SetPersonalName(): " + ex.Message);
            }
        }

        private void UpdateControls()
        {
            string fam, nam, pat;
            fPersonalName.GetRusNameParts(out fam, out nam, out pat);

            this.edSurname.Text = fam;
            this.edName.Text = nam;
            this.edPatronymic.Text = pat;
            this.cbNameType.SelectedIndex = (sbyte)this.fPersonalName.NameType;

            this.edPiecePrefix.Text = fPersonalName.Pieces.Prefix;
            this.edPieceNickname.Text = fPersonalName.Pieces.Nickname;
            this.edPieceSurnamePrefix.Text = fPersonalName.Pieces.SurnamePrefix;
            this.edPieceSuffix.Text = fPersonalName.Pieces.Suffix;
        }

        private void AcceptChanges()
        {
            fPersonalName.SetNameParts(
                this.edName.Text.Trim() + " " + this.edPatronymic.Text.Trim(),
                this.edSurname.Text.Trim(), fPersonalName.LastPart);

            GEDCOMPersonalNamePieces pieces = fPersonalName.Pieces;
            pieces.Nickname = this.edPieceNickname.Text;
            pieces.Prefix = this.edPiecePrefix.Text;
            pieces.SurnamePrefix = this.edPieceSurnamePrefix.Text;
            pieces.Suffix = this.edPieceSuffix.Text;

            this.fPersonalName.NameType = (GEDCOMNameType)this.cbNameType.SelectedIndex;
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
                this.fBase.Host.LogWrite("TfmPersonalNameEdit.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        public TfmPersonalNameEdit(IBaseWindow aBase)
        {
            InitializeComponent();
            this.fBase = aBase;

            for (GEDCOMNameType nt = GEDCOMNameType.ntNone; nt <= GEDCOMNameType.ntMarried; nt++)
            {
                this.cbNameType.Items.Add(LangMan.LS(GKData.NameTypes[(int)nt]));
            }

            this.SetLang();
        }

        public void SetLang()
        {
            this.Text = LangMan.LS(LSID.LSID_Name);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Label1.Text = LangMan.LS(LSID.LSID_Surname);
            this.Label2.Text = LangMan.LS(LSID.LSID_Name);
            this.Label3.Text = LangMan.LS(LSID.LSID_Patronymic);
            this.Label7.Text = LangMan.LS(LSID.LSID_Nickname);
            this.Label8.Text = LangMan.LS(LSID.LSID_SurnamePrefix);
            this.Label6.Text = LangMan.LS(LSID.LSID_NamePrefix);
            this.Label9.Text = LangMan.LS(LSID.LSID_NameSuffix);
            this.lblType.Text = LangMan.LS(LSID.LSID_Type);
        }
    }
}
