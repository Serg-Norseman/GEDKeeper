/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Events;

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
