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
using GKCore.Locales;
using GKCore.Sync;
using GKCore.Utilities;
using GKUI.Components;

namespace GKTreeSyncPlugin
{
#if !NETCOREAPP
    using System.Drawing;
    using System.Windows.Forms;
#else
    using Eto.Forms;
    using Eto.Serialization.Xaml;
#endif

    public partial class TSForm : Form, ILocalizable
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

            var modifiedRecords = fSyncTool.Results.Where((x) => x.Status >= DiffStatus.Modified).ToList();
            int selectedIndex = modifiedRecords.IndexOf(item);
            using (var detailForm = new TSDetailForm(fBase, modifiedRecords, selectedIndex)) {
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
    }
}
