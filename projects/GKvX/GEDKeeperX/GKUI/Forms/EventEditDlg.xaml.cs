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

using System;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;

namespace GKUI.Forms
{
    public sealed partial class EventEditDlg : CommonDialog<IEventEditDlg, EventEditDlgController>, IEventEditDlg
    {
        public GDMCustomEvent Event
        {
            get { return fController.Event; }
            set { fController.Event = value; }
        }

        #region View Interface

        ISheetList IEventEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList IEventEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ISheetList IEventEditDlg.SourcesList
        {
            get { return fSourcesList; }
        }

        IComboBox IEventEditDlg.EventType
        {
            get { return GetControlHandler<IComboBox>(cmbEventType); }
        }

        IDateControl IEventEditDlg.Date
        {
            get { return GetControlHandler<IDateControl>(dateCtl); }
        }

        IComboBox IEventEditDlg.Attribute
        {
            get { return GetControlHandler<IComboBox>(txtAttribute); }
        }

        ITextBox IEventEditDlg.Place
        {
            get { return GetControlHandler<ITextBox>(txtEventPlace); }
        }

        IComboBox IEventEditDlg.Cause
        {
            get { return  GetControlHandler<IComboBox>(txtEventCause); }
        }

        IComboBox IEventEditDlg.Agency
        {
            get { return  GetControlHandler<IComboBox>(txtEventOrg); }
        }

        #endregion

        public EventEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new EventEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnAddress_Click(object sender, EventArgs e)
        {
            fController.ModifyAddress();
        }

        private void btnPlaceAdd_Click(object sender, EventArgs e)
        {
            fController.AddPlace();
        }

        private void btnPlaceDelete_Click(object sender, EventArgs e)
        {
            fController.RemovePlace();
        }

        private void EditEventType_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (cmbEventType.Text == FreqCollection<string>.LineItem) {
                cmbEventType.SelectedIndex = 0;
                return;
            }

            fController.ChangeEventType();
        }

        public void SendData(string signature, string data)
        {
            fController.SendData(signature, data);
        }
    }
}
