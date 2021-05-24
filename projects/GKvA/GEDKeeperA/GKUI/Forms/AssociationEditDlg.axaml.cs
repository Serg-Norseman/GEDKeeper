/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;

namespace GKUI.Forms
{
    public sealed partial class AssociationEditDlg : EditorDialog, IAssociationEditDlg
    {
        private readonly AssociationEditDlgController fController;

        public GDMAssociation Association
        {
            get { return fController.Association; }
            set { fController.Association = value; }
        }

        #region Design

        private TextBlock lblRelation;
        private ComboBox cmbRelation;
        private TextBlock lblPerson;
        private TextBox txtPerson;
        private Button btnPersonAdd;
        private Button btnAccept;
        private Button btnCancel;

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);

            lblRelation = this.FindControl<TextBlock>("lblRelation");
            cmbRelation = this.FindControl<ComboBox>("cmbRelation");
            lblPerson = this.FindControl<TextBlock>("lblPerson");
            txtPerson = this.FindControl<TextBox>("txtPerson");
            btnPersonAdd = this.FindControl<Button>("btnPersonAdd");
            btnAccept = this.FindControl<Button>("btnAccept");
            btnCancel = this.FindControl<Button>("btnCancel");
        }

        #endregion

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

        public AssociationEditDlg() : this(null)
        {
        }

        public AssociationEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            //btnPersonAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            //btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            //btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            // SetLang()
            btnAccept.Content = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Content = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_Association);
            lblRelation.Text = LangMan.LS(LSID.LSID_Relation);
            lblPerson.Text = LangMan.LS(LSID.LSID_Person);

            SetToolTip(btnPersonAdd, LangMan.LS(LSID.LSID_PersonAttachTip));

            fController = new AssociationEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnAccept_Click(object sender, RoutedEventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        private void btnPersonAdd_Click(object sender, RoutedEventArgs e)
        {
            fController.SetPerson();
        }
    }
}
