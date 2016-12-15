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
            this.cmbRelation.Text = this.fAssociation.Relation;
            string st = ((this.fAssociation.Individual == null) ? "" : GKUtils.GetNameString(this.fAssociation.Individual, true, false));
            this.txtPerson.Text = st;
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                string rel = this.cmbRelation.Text.Trim();
                if (rel != "" && GlobalOptions.Instance.Relations.IndexOf(rel) < 0)
                {
                    GlobalOptions.Instance.Relations.Add(rel);
                }

                this.fAssociation.Relation = this.cmbRelation.Text;
                this.fAssociation.Individual = this.fTempInd;
                this.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("AssociationEditDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            this.fTempInd = this.fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
            this.txtPerson.Text = ((this.fTempInd == null) ? "" : GKUtils.GetNameString(this.fTempInd, true, false));
        }

        public AssociationEditDlg(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.btnPersonAdd.Image = GKResources.iRecNew;
            this.btnAccept.Image = GKResources.iBtnAccept;
            this.btnCancel.Image = GKResources.iBtnCancel;

            this.fBase = aBase;

            int num = GlobalOptions.Instance.Relations.Count;
            for (int i = 0; i < num; i++)
            {
                this.cmbRelation.Items.Add(GlobalOptions.Instance.Relations[i]);
            }

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_Association);
            this.lblRelation.Text = LangMan.LS(LSID.LSID_Relation);
            this.lblPerson.Text = LangMan.LS(LSID.LSID_Person);

            this.toolTip1.SetToolTip(this.btnPersonAdd, LangMan.LS(LSID.LSID_PersonAttachTip));
        }
    }
}
