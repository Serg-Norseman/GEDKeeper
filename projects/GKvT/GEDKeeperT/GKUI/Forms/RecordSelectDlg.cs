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
using GKCore.Locales;
using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    public sealed partial class RecordSelectDlg : CommonDialog, IRecordSelectDialog
    {
        public delegate void UpdateDelegate();

        #region Design components

        private GKListView fListRecords;
        private ContextMenu contextMenu;

        #endregion

        private readonly RecordSelectDlgController fController;


        public GDMRecord ResultRecord { get; set; }

        #region View Interface

        IComboBox IRecordSelectDialog.FilterCombo
        {
            get { return GetControlHandler<IComboBox>(txtFastFilter); }
        }

        ITextBox IRecordSelectDialog.FilterText
        {
            get { return null; }
        }

        IFilterControl IRecordSelectDialog.FilterCtl
        {
            get { return null; }
        }

        IListView IRecordSelectDialog.RecordsList
        {
            get { return fListRecords; }
        }

        #endregion


        public RecordSelectDlg(IBaseWindow baseWin, GDMRecordType recType)
        {
            InitializeComponent();

            //TabIndexChanged += Form_TabIndexChanged;

            fController = new RecordSelectDlgController(this);
            fController.Init(baseWin);
            fController.RecType = recType;

            //fltCtl.ParamsChanged += txtFastFilter_TextChanged;

            var miDetails = new MenuItem();
            miDetails.Title = LangMan.LS(LSID.Details);
            miDetails.Action += miDetails_Click;

            contextMenu = new ContextMenu();
            contextMenu.MenuItems = new MenuBarItem("Actions", new MenuItem[] {
                miDetails
            });

            UpdateRecordsView();
        }

        private void Form_TabIndexChanged(object sender, EventArgs e)
        {
            /*if (fltCtl.CanFocus) {
                fListRecords.SetFocus();
                fListRecords.SelectItem(0);
            }*/
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fChangeTimer != null) {
                    fChangeTimer.Stop();
                    fChangeTimer.Dispose();
                    fChangeTimer = null;
                }
            }
            base.Dispose(disposing);
        }

        private void UpdateRecordsView()
        {
            if (fListRecords != null) {
                fListRecords.ListMan = null;
                fListRecords.Dispose();
                fListRecords = null;
            }
            fListRecords = UIHelper.CreateRecordsView(panList, fController.Base.Context, fController.RecType, true);
            fListRecords.MouseClick += (s, args) => {
                if (args.MouseEvent.Flags.HasFlag(MouseFlags.Button3Clicked)) {
                    contextMenu.Position = new Point(args.MouseEvent.X, args.MouseEvent.Y);
                    contextMenu.Show();
                }
            };
        }

        private void miDetails_Click(object sender, EventArgs e)
        {
            fController.ShowDetails();
        }

        private void btnSelect_Click(object sender, EventArgs e)
        {
            try {
                ResultRecord = fListRecords.GetSelectedData() as GDMRecord;
                //Close(DialogResult.Ok);
                DialogResult = DialogResult.Ok;
            } catch (Exception ex) {
                Logger.WriteError("RecordSelectDlg.btnSelect_Click()", ex);
                ResultRecord = null;
                DialogResult = DialogResult.None;
            }
        }

        private async void btnCreate_Click(object sender, EventArgs e)
        {
            try {
                GDMRecord rec = await BaseController.AddRecord(this, fController.Base, fController.RecType, fController.Target);
                if (rec != null) {
                    ResultRecord = rec;
                    //Close(DialogResult.Ok);
                    DialogResult = DialogResult.Ok;
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordSelectDlg.btnCreate_Click()", ex);
                ResultRecord = null;
                DialogResult = DialogResult.None;
            }
        }

        private System.Timers.Timer fChangeTimer;

        private void txtFastFilter_TextChanged(object sender, string e)
        {
            if (fChangeTimer == null) {
                fChangeTimer = new System.Timers.Timer(500);
                fChangeTimer.AutoReset = false;
                fChangeTimer.Elapsed += (sdr, args) => {
                    fController.UpdateView();
                };
            } else {
                fChangeTimer.Stop();
            }
            fChangeTimer.Start();
        }

        private void txtFastFilter_KeyDown(object sender, KeyEventEventArgs e)
        {
            if (e.KeyEvent.Key == Key.Enter) {
                fController.ChangeFilter();
                e.Handled = true;
            }
            fController.UpdateView();
        }

        public void SetTarget(TargetMode mode, GDMIndividualRecord target, GDMSex needSex, string defFilter = "*")
        {
            fController.SetTarget(mode, target, needSex, defFilter);
        }
    }
}
