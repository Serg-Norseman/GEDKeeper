/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class EventDefEditDlg : CommonDialog<IEventDefEditDlg, EventDefEditDlgController>, IEventDefEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private Label lblName;
        private TextBox txtName;
        private Label lblTag;
        private ComboBox cmbTag;
        private Label lblType;
        private TextBox txtType;
        private CheckBox chkEnabled;
        private Label lblDesc;
        private TextBox txtDesc;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public EventDef EventDef
        {
            get { return fController.EventDef; }
            set { fController.EventDef = value; }
        }

        #region View Interface

        ITextBox IEventDefEditDlg.NameText
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        IComboBox IEventDefEditDlg.TagCombo
        {
            get { return GetControlHandler<IComboBox>(cmbTag); }
        }

        ITextBox IEventDefEditDlg.TypeText
        {
            get { return GetControlHandler<ITextBox>(txtType); }
        }

        ICheckBox IEventDefEditDlg.EnabledCheck
        {
            get { return GetControlHandler<ICheckBox>(chkEnabled); }
        }

        ITextBox IEventDefEditDlg.DescText
        {
            get { return GetControlHandler<ITextBox>(txtDesc); }
        }

        #endregion

        public EventDefEditDlg()
        {
            XamlReader.Load(this);

            fController = new EventDefEditDlgController(this);
        }

        private void edName_KeyPress(object sender, KeyEventArgs e)
        {
            if (e.KeyChar == '/') {
                e.Handled = true;
            }
        }
    }
}
