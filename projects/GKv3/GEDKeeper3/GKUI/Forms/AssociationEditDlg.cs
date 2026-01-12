/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class AssociationEditDlg : CommonDialog<IAssociationEditDlg, AssociationEditDlgController>, IAssociationEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private Label lblRelation;
        private ComboBox cmbRelation;
        private Label lblPerson;
        private TextBox txtPerson;
        private Button btnPersonAdd;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMAssociation Association
        {
            get { return fController.Association; }
            set { fController.Association = value; }
        }

        #region View Interface

        IComboBox IAssociationEditDlg.Relation
        {
            get { return GetControlHandler<IComboBox>(cmbRelation); }
        }

        ITextBox IAssociationEditDlg.Person
        {
            get { return GetControlHandler<ITextBox>(txtPerson); }
        }

        #endregion

        public AssociationEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new AssociationEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            fController.SetPerson();
        }
    }
}
