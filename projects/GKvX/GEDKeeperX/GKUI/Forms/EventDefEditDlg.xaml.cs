/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Events;

namespace GKUI.Forms
{
    public sealed partial class EventDefEditDlg : CommonDialog<IEventDefEditDlg, EventDefEditDlgController>, IEventDefEditDlg
    {
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
            InitializeComponent();

            fController = new EventDefEditDlgController(this);
        }
    }
}
