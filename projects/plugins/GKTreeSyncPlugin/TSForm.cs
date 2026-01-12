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
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Tools;
using GKCore.Utilities;

namespace GKTreeSyncPlugin
{
#if !NETCOREAPP
    using System.Drawing;
    using System.Windows.Forms;
#else
    using Eto.Forms;
    using Eto.Serialization.Xaml;
    using GKUI.Components;
#endif

    public partial class TSForm : Form, ILocalizable
    {
#if NETCOREAPP
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
#endif

        private readonly IBaseWindow fBase;
        private readonly SyncTool fSyncTool;
        private readonly DiffListModel fListModel;

        public TSForm()
        {
#if !NETCOREAPP
            InitializeComponent();
#else
            XamlReader.Load(this);
            UIHelper.FixRadioButtons(this, groupBox1);
#endif
        }

        public TSForm(Plugin plugin, IBaseWindow curBase) : this()
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
            fListModel = new DiffListModel(fBase.Context);
            lvRecords.ListMan = fListModel;
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
            fSyncTool.CompareRecords(GetRecordType());
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
    }


    public sealed class DiffListModel : SimpleListModel<DiffRecord>
    {
        public bool ShowOnlyModified { get; set; }


        public DiffListModel(BaseContext baseContext) :
            base(baseContext, CreateListColumns())
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.ltNone);
            result.AddColumn("Sync", DataType.dtBool, 40, true);
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
        private char diffChar;
        private int backColor;

        public override void Fetch(DiffRecord aRec)
        {
            base.Fetch(aRec);

            diffChar = DiffUtil.GetStatusChar(fFetchedRec.Status);

            switch (fFetchedRec.Status) {
                case DiffStatus.Equal:
                default:
                    item1 = diffChar + " " + fFetchedRec.Obj1.XRef;
                    item2 = diffChar + " " + fFetchedRec.Obj2.XRef;
                    backColor = GKColors.White;
                    break;

                case DiffStatus.Deleted:
                    item1 = diffChar + " " + fFetchedRec.Obj1.XRef;
                    item2 = " ";
                    backColor = GKColors.Coral;
                    break;

                case DiffStatus.Inserted:
                    item1 = " ";
                    item2 = diffChar + " " + fFetchedRec.Obj2.XRef;
                    backColor = GKColors.LightBlue;
                    break;

                case DiffStatus.Modified:
                case DiffStatus.DeepModified:
                    item1 = diffChar + " " + fFetchedRec.Obj1.XRef;
                    item2 = diffChar + " " + fFetchedRec.Obj2.XRef;
                    backColor = (fFetchedRec.Status == DiffStatus.Modified) ? GKColors.Yellow : GKColors.Orange;
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
                    result = item1;
                    break;
                case 2:
                    result = item2;
                    break;
                case 3:
                    result = GKUtils.GetRecordName(fBaseContext.Tree, fFetchedRec.Obj1, false);
                    break;
                case 4:
                    result = GKUtils.GetRecordName(fBaseContext.Tree, fFetchedRec.Obj2, false);
                    break;
            }
            return result;
        }

        public override IColor GetBackgroundColor(int itemIndex, object rowData)
        {
            return ChartRenderer.GetColor(backColor);
        }

        protected override void SetColumnValueEx(DiffRecord item, int colIndex, object value)
        {
            if (item != null && colIndex == 0 && value is bool chk)
                item.Checked = chk;
        }
    }
}
