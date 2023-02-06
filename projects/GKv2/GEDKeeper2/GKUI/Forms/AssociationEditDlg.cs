/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore.Controllers;
using GKCore.Design.MVP.Controls;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;

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

            btnPersonAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new AssociationEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            fController.SetPerson();
        }
    }
}
