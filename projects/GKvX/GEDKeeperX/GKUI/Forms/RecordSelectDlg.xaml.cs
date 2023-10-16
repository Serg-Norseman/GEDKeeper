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
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class RecordSelectDlg : CommonDialog, IRecordSelectDialog
    {
        private readonly RecordSelectDlgController fController;

        private GKListView fListRecords;


        public GDMRecord ResultRecord { get; set; }

        #region View Interface

        IComboBox IRecordSelectDialog.FilterBox
        {
            get { return GetControlHandler<IComboBox>(txtFastFilter); }
        }

        IFilterControl IRecordSelectDialog.FilterCtl
        {
            get { return fltCtl; }
        }

        IListView IRecordSelectDialog.RecordsList
        {
            get { return fListRecords; }
        }

        #endregion


        public RecordSelectDlg(IBaseWindow baseWin, GDMRecordType recType)
        {
            InitializeComponent();

            fController = new RecordSelectDlgController(this);
            fController.Init(baseWin);
            fController.RecType = recType;

            fltCtl.ParamsChanged += txtFastFilter_TextChanged;

            UpdateRecordsView();
        }

        private void UpdateRecordsView()
        {
            if (fListRecords != null) {
                fListRecords.ListMan = null;
                //fListRecords.Dispose();
                fListRecords = null;
            }
            fListRecords = UIHelper.CreateRecordsView(panList, fController.Base.Context, fController.RecType, true);
        }

        private void btnSelect_Click(object sender, EventArgs e)
        {
            try {
                ResultRecord = fListRecords.GetSelectedData() as GDMRecord;
                //DialogResult = DialogResult.Ok;
            } catch (Exception ex) {
                Logger.WriteError("RecordSelectDlg.btnSelect_Click()", ex);
                ResultRecord = null;
                //DialogResult = DialogResult.None;
            }
        }

        private void btnCreate_Click(object sender, EventArgs e)
        {
            try {
                GDMRecord rec = BaseController.AddRecord(this, fController.Base, fController.RecType, fController.Target);
                if (rec != null) {
                    ResultRecord = rec;
                    //DialogResult = DialogResult.Ok;
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordSelectDlg.btnCreate_Click()", ex);
                ResultRecord = null;
                //DialogResult = DialogResult.None;
            }
        }

        private void txtFastFilter_TextChanged(object sender, EventArgs e)
        {
            fController.UpdateView();
        }

        /*private void txtFastFilter_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Enter) {
                fController.ChangeFilter();
                e.Handled = true;
            }
        }*/

        public void SetTarget(TargetMode mode, GDMIndividualRecord target, GDMSex needSex, string defFilter = "*")
        {
            fController.SetTarget(mode, target, needSex, defFilter);
        }
    }
}
