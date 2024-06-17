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

using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;

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
