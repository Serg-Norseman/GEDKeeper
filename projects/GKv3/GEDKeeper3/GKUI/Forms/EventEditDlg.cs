/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class EventEditDlg : EditorDialog, IEventEditDlg
    {
        private readonly EventEditDlgController fController;

        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fSourcesList;

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

        IComboBoxHandler IEventEditDlg.EventType
        {
            get { return GetControlHandler<IComboBoxHandler>(cmbEventType); }
        }

        IComboBoxHandler IEventEditDlg.EventDateType
        {
            get { return GetControlHandler<IComboBoxHandler>(cmbEventDateType); }
        }

        ICheckBoxHandler IEventEditDlg.Date1BC
        {
            get { return GetControlHandler<ICheckBoxHandler>(btnBC1); }
        }

        ICheckBoxHandler IEventEditDlg.Date2BC
        {
            get { return GetControlHandler<ICheckBoxHandler>(btnBC2); }
        }

        IComboBoxHandler IEventEditDlg.Date1Calendar
        {
            get { return GetControlHandler<IComboBoxHandler>(cmbDate1Calendar); }
        }

        IComboBoxHandler IEventEditDlg.Date2Calendar
        {
            get { return GetControlHandler<IComboBoxHandler>(cmbDate2Calendar); }
        }

        IDateBoxHandler IEventEditDlg.Date1
        {
            get { return GetControlHandler<IDateBoxHandler>(txtEventDate1); }
        }

        IDateBoxHandler IEventEditDlg.Date2
        {
            get { return GetControlHandler<IDateBoxHandler>(txtEventDate2); }
        }

        IComboBoxHandler IEventEditDlg.Attribute
        {
            get { return GetControlHandler<IComboBoxHandler>(txtAttribute); }
        }

        ITextBoxHandler IEventEditDlg.Place
        {
            get { return GetControlHandler<ITextBoxHandler>(txtEventPlace); }
        }

        ITextBoxHandler IEventEditDlg.EventName
        {
            get { return  GetControlHandler<ITextBoxHandler>(txtEventName); }
        }

        ITextBoxHandler IEventEditDlg.Cause
        {
            get { return  GetControlHandler<ITextBoxHandler>(txtEventCause); }
        }

        ITextBoxHandler IEventEditDlg.Agency
        {
            get { return  GetControlHandler<ITextBoxHandler>(txtEventOrg); }
        }

        #endregion

        public EventEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnPlaceAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnPlaceDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");

            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);
            fSourcesList = new GKSheetList(pageSources);

            // SetLang()
            Title = LangMan.LS(LSID.LSID_Event);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            btnAddress.Text = LangMan.LS(LSID.LSID_Address) + @"...";
            pageCommon.Text = LangMan.LS(LSID.LSID_Common);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            pageSources.Text = LangMan.LS(LSID.LSID_RPSources);
            lblEvent.Text = LangMan.LS(LSID.LSID_Event);
            lblAttrValue.Text = LangMan.LS(LSID.LSID_Value);
            lblPlace.Text = LangMan.LS(LSID.LSID_Place);
            lblDate.Text = LangMan.LS(LSID.LSID_Date);
            lblCause.Text = LangMan.LS(LSID.LSID_Cause);
            lblOrg.Text = LangMan.LS(LSID.LSID_Agency);

            SetToolTip(btnPlaceAdd, LangMan.LS(LSID.LSID_PlaceAddTip));
            SetToolTip(btnPlaceDelete, LangMan.LS(LSID.LSID_PlaceDeleteTip));

            fController = new EventEditDlgController(this);
            fController.Init(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(baseWin, fController.LocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(baseWin, fController.LocalUndoman);
            fSourcesList.ListModel = new SourceCitationsListModel(baseWin, fController.LocalUndoman);
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

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try {
                fController.Cancel();
                CancelClickHandler(sender, e);
            } catch (Exception ex) {
                Logger.LogWrite("EventEditDlg.btnCancel_Click(): " + ex.Message);
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

        // FIXME: GKv3 DevRestriction
        /*private void EditEventDate1_DragOver(object sender, DragEventArgs e)
        {
            e.Effect = e.Data.GetDataPresent(typeof(string)) ? DragDropEffects.Move : DragDropEffects.None;
        }

        private void EditEventDate1_DragDrop(object sender, DragEventArgs e)
        {
            try {
                if (e.Data.GetDataPresent(typeof(string))) {
                    string txt = e.Data.GetData(typeof(string)) as string;
                    string[] dt = ((MaskedTextBox)sender).Text.Split('.');
                    ((MaskedTextBox)sender).Text = dt[0] + '.' + dt[1] + '.' + txt;
                }
            } catch (Exception ex) {
                Logger.LogWrite("EventEditDlg.DragDrop(): " + ex.Message);
            }
        }*/

        private void EditEventType_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.ChangeEventType();
        }

        private void EditEventDateType_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.ChangeDateType();
        }
    }
}
