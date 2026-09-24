/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Linq;
using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Sync;
using GKCore.Utilities;
using GKUI.Components;

namespace GKUI.Forms
{
#if !NETCOREAPP
    using System.Drawing;
    using System.Windows.Forms;
#else
    using Eto.Forms;
    using Eto.Serialization.Xaml;
#endif

    public partial class TSForm : Dialog, ILocalizable
    {
#if NETCOREAPP
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051
        private Panel panel1;
        private GroupBox groupBox1;
        private Button btnSelectFile;
        private Label lblFile;
        private TextBox txtFile;
        private ComboBox cmbRecordTypes;
        private RadioButton rbSyncSelected;
        private RadioButton rbSyncAll;
        private CheckBox chkOnlyModified;
        private GKListView lvRecords;
        private TextArea mSyncRes;
#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
#endif

        private readonly IBaseWindow fBase;
        private readonly SyncTool fSyncTool;
        private readonly DiffRecordsModel fListModel;

        public TSForm()
        {
#if !NETCOREAPP
            InitializeComponent();
#else
            XamlReader.Load(this);
            UIHelper.FixRadioButtons(this, groupBox1);
#endif
        }

        public TSForm(IBaseWindow curBase) : this()
        {
            for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                var name = LangMan.LS(GKData.RecordTypes[(int)rt].Name);
                cmbRecordTypes.Items.Add(name);
            }

            fBase = curBase;
            fSyncTool = new SyncTool();

#if !NETCOREAPP
            lvRecords.CheckBoxes = true;
#endif
            fListModel = new DiffRecordsModel(fBase.Context);
            lvRecords.ListMan = fListModel;
            lvRecords.CellDoubleClick += lvRecords_CellDoubleClick;
        }

        public void SetLocale()
        {
        }

        private async void btnSelectFile_ClickAsync(object sender, EventArgs e)
        {
            string fileName = await AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (string.IsNullOrEmpty(fileName)) {
                txtFile.Text = string.Empty;
                return;
            }

            txtFile.Text = fileName;
            fSyncTool.LoadOtherFile(fBase.Context.Tree, fileName);
            fSyncTool.CompareTrees(GetRecordType());
            UpdateLists();
        }

        private void rbSyncRecords_CheckedChanged(object sender, EventArgs e)
        {
#if NETCOREAPP
            if (sender is RadioButton radBtn && radBtn.HasFocus)
#endif
                cmbRecordTypes.Enabled = !rbSyncAll.Checked;
        }

        private void chkOnlyModified_CheckStateChanged(object sender, EventArgs e)
        {
            UpdateLists();
        }

        private GDMRecordType GetRecordType()
        {
            return rbSyncAll.Checked ? GDMRecordType.rtNone : (GDMRecordType)(cmbRecordTypes.SelectedIndex + 1);
        }

        private void lvRecords_CellDoubleClick(object sender, GridCellMouseEventArgs e)
        {
            var item = lvRecords.GetSelectedData() as DiffRecord;
            if (item == null) return;

            var records = fSyncTool.Results;
            if (item.Status >= DiffStatus.Modified)
                records = records.Where((x) => x.Status >= DiffStatus.Modified).ToList();

            int selectedIndex = records.IndexOf(item);
            using (var detailForm = new TSDetailForm(fBase, fSyncTool, records, selectedIndex)) {
                detailForm.ShowModal();
            }
        }

        private void UpdateLists()
        {
#if !NETCOREAPP
            fListModel.ShowOnlyModified = chkOnlyModified.Checked;
#else
            fListModel.ShowOnlyModified = chkOnlyModified.Checked.Value;
#endif
            fListModel.DataSource = fSyncTool.Results;
            lvRecords.UpdateContents();
        }

        #region List Model

        private sealed class DiffRecordsModel : SimpleListModel<DiffRecord>
        {
            public bool ShowOnlyModified { get; set; }


            public DiffRecordsModel(BaseContext baseContext) :
                base(baseContext, CreateListColumns())
            {
            }

            public static ListColumns CreateListColumns()
            {
                var result = new ListColumns(GKListType.ltNone);
                result.AddColumn("Sync", DataType.dtBool, 40, true);
                result.AddColumn("#", DataType.dtInteger, 40, true);
                result.AddColumn("XRef 1", DataType.dtString, 100, true);
                result.AddColumn("XRef 2", DataType.dtString, 100, true);
                result.AddColumn("Name 1", DataType.dtString, 400, true);
                result.AddColumn("Name 2", DataType.dtString, 400, true);
                return result;
            }

            public override bool CheckFilter()
            {
                bool res = (!ShowOnlyModified || fFetchedRec.Status != DiffStatus.Equal);
                return res;
            }

            // fetched data
            private string item1, item2;

            public override void Fetch(DiffRecord aRec)
            {
                base.Fetch(aRec);

                string diffChar = DiffUtil.GetStatusChar(fFetchedRec.Status);
                switch (fFetchedRec.Status) {
                    case DiffStatus.Equal:
                    default:
                        item1 = diffChar + " " + fFetchedRec.Obj1.XRef;
                        item2 = diffChar + " " + fFetchedRec.Obj2.XRef;
                        break;

                    case DiffStatus.Deleted:
                        item1 = diffChar + " " + fFetchedRec.Obj1.XRef;
                        item2 = " ";
                        break;

                    case DiffStatus.Inserted:
                        item1 = " ";
                        item2 = diffChar + " " + fFetchedRec.Obj2.XRef;
                        break;

                    case DiffStatus.Modified:
                    case DiffStatus.DeepModified:
                        item1 = diffChar + " " + fFetchedRec.Obj1.XRef;
                        item2 = diffChar + " " + fFetchedRec.Obj2.XRef;
                        break;
                }
            }

            protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
            {
                object result = null;
                switch (colType) {
                    case 0:
                        result = fFetchedRec.Checked;
                        break;
                    case 1:
                        result = fFetchedRec.Num;
                        break;
                    case 2:
                        result = item1;
                        break;
                    case 3:
                        result = item2;
                        break;
                    case 4:
                        result = GKUtils.GetRecordName(fBaseContext.Tree, fFetchedRec.Obj1, false);
                        break;
                    case 5:
                        result = GKUtils.GetRecordName(fBaseContext.Tree, fFetchedRec.Obj2, false);
                        break;
                }
                return result;
            }

            public override IColor GetBackgroundColor(int itemIndex, object rowData)
            {
                DiffRecord diffRecord = rowData as DiffRecord;
                return SyncTool.GetDiffColor(diffRecord.Status);
            }

            protected override void SetColumnValueEx(DiffRecord item, int colIndex, object value)
            {
                if (item != null && colIndex == 0 && value is bool chk)
                    item.Checked = chk;
            }
        }

        #endregion
    }
}
