/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class RecordSelectDlg : CommonDialog, IRecordSelectDialog
    {


        private GKListView fListRecords;
        private readonly RecordSelectDlgController fController;


        public GDMRecord ResultRecord { get; set; }

        #region View Interface

        IComboBox IRecordSelectDialog.FilterCombo
        {
            get { return null; }
        }

        ITextBox IRecordSelectDialog.FilterText
        {
            get { return GetControlHandler<ITextBox>(txtFastFilter); }
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

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }

        private void UpdateRecordsView()
        {
            if (fListRecords != null) {
                fListRecords.ListMan = null;
                fListRecords = null;
            }
            fListRecords = UIHelper.CreateRecordsView(panList, fController.Base.Context, fController.RecType, true);
        }

        private async void btnSelect_Click(object sender, EventArgs e)
        {
            try {
                ResultRecord = fListRecords.GetSelectedData() as GDMRecord;
                await Close(DialogResult.Ok);
            } catch (Exception ex) {
                Logger.WriteError("RecordSelectDlg.btnSelect_Click()", ex);
                ResultRecord = null;
            }
        }

        private async void btnCreate_Click(object sender, EventArgs e)
        {
            try {
                GDMRecord rec = await BaseController.AddRecord(this, fController.Base, fController.RecType, fController.Target);
                if (rec != null) {
                    ResultRecord = rec;
                    await Close(DialogResult.Ok);
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordSelectDlg.btnCreate_Click()", ex);
                ResultRecord = null;
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
