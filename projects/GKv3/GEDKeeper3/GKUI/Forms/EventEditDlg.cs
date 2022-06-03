﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.ComponentModel;
using BSLib.Design.MVP.Controls;
using Eto.Drawing;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class EventEditDlg : CommonDialog<IEventEditDlg, EventEditDlgController>, IEventEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageSources;
        private Button btnAddress;
        private TabPage pageCommon;
        private Label lblEvent;
        private Label lblPlace;
        private Label lblDate;
        private Label lblCause;
        private Label lblOrg;
        private Label lblAttrValue;
        private ComboBox cmbEventType;
        private TextBox txtEventName;
        private TextBox txtEventPlace;
        private TextBox txtEventCause;
        private TextBox txtEventOrg;
        private ComboBox txtAttribute;
        private Button btnPlaceAdd;
        private Button btnPlaceDelete;
        private GKDateControl dateCtl;
        private GKSheetList fNotesList;
        private GKSheetList fMediaList;
        private GKSheetList fSourcesList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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

        ITextBox IEventEditDlg.EventName
        {
            get { return  GetControlHandler<ITextBox>(txtEventName); }
        }

        ITextBox IEventEditDlg.Cause
        {
            get { return  GetControlHandler<ITextBox>(txtEventCause); }
        }

        ITextBox IEventEditDlg.Agency
        {
            get { return  GetControlHandler<ITextBox>(txtEventOrg); }
        }

        #endregion

        public EventEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new EventEditDlgController(this);
            fController.Init(baseWin);
        }

        public void SetLocationMode(bool active)
        {
            if (active) {
                txtEventPlace.ReadOnly = true;
                txtEventPlace.BackgroundColor = SystemColors.Control;
                btnPlaceAdd.Enabled = false;
                btnPlaceDelete.Enabled = true;
            } else {
                txtEventPlace.ReadOnly = false;
                txtEventPlace.BackgroundColor = SystemColors.WindowBackground;
                btnPlaceAdd.Enabled = true;
                btnPlaceDelete.Enabled = false;
            }
        }

        private void btnAddress_Click(object sender, EventArgs e)
        {
            fController.ModifyAddress();
        }

        private void EditEventPlace_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Down && e.Control) {
                txtEventPlace.Text = txtEventPlace.Text.ToLower();
            }
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
            fController.ChangeEventType();
        }
    }
}
