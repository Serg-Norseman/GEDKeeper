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
    public partial class AssociationEditDlg : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;
        private GEDCOMAssociation fAssociation;
        private GEDCOMIndividualRecord fTempInd;

        public GEDCOMAssociation Association
        {
            get { return this.fAssociation; }
            set { this.SetAssociation(value); }
        }

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        private void SetAssociation(GEDCOMAssociation value)
        {
            this.fAssociation = value;
            this.EditRelation.Text = this.fAssociation.Relation;
            string st = ((this.fAssociation.Individual == null) ? "" : this.fAssociation.Individual.GetNameString(true, false));
            this.EditPerson.Text = st;
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                string rel = this.EditRelation.Text.Trim();
                if (rel != "" && MainWin.Instance.Options.Relations.IndexOf(rel) < 0)
                {
                    MainWin.Instance.Options.Relations.Add(rel);
                }

                this.fAssociation.Relation = this.EditRelation.Text;
                this.fAssociation.Individual = this.fTempInd;
                this.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TfmAssociationEdit.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            this.fTempInd = this.fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
            this.EditPerson.Text = ((this.fTempInd == null) ? "" : this.fTempInd.GetNameString(true, false));
        }

        public AssociationEditDlg(IBaseWindow aBase)
        {
            this.InitializeComponent();
            this.fBase = aBase;

            int num = MainWin.Instance.Options.Relations.Count;
            for (int i = 0; i < num; i++)
            {
                this.EditRelation.Items.Add(MainWin.Instance.Options.Relations[i]);
            }

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_Association);
            this.Label1.Text = LangMan.LS(LSID.LSID_Relation);
            this.Label2.Text = LangMan.LS(LSID.LSID_Person);
        }
    }
}
