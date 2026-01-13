/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class AssociationEditDlg : CommonDialog<IAssociationEditDlg, AssociationEditDlgController>, IAssociationEditDlg
    {
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
            InitializeComponent();

            fController = new AssociationEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            fController.SetPerson();
        }
    }
}
