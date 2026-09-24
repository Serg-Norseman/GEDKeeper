/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Linq;
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

    public partial class TSDetailForm : Dialog, ILocalizable
    {
#if NETCOREAPP
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051
        private Panel panel1;
        private Button btnPrevRecord;
        private Button btnNextRecord;
        private Button btnMerge;
        private GKListView lvContents;
        private Label lblRecordInfo;
        private HyperView hvLeftRecord;
        private HyperView hvRightRecord;
#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
#endif

        private readonly IBaseWindow fBase;
        private readonly List<DiffRecord> fRecordsList;
        private readonly DiffContentsModel fListModel;
        private readonly SyncTool fSyncTool;

        private int fCurrentIndex;
        private DiffRecord fCurrentRecord;

        public TSDetailForm()
        {
#if !NETCOREAPP
            InitializeComponent();
#else
            XamlReader.Load(this);
#endif

            lvContents.SelectedItemsChanged += lvContents_SelectedItemsChanged;
        }

        public TSDetailForm(IBaseWindow curBase, SyncTool syncTool, List<DiffRecord> recordsList, int currentIndex) : this()
        {
            fBase = curBase;
            fSyncTool = syncTool;
            fRecordsList = recordsList;
            fCurrentIndex = currentIndex;

            if (fRecordsList != null && fRecordsList.Count > 0 && fCurrentIndex >= 0 && fCurrentIndex < fRecordsList.Count) {
                fCurrentRecord = fRecordsList[fCurrentIndex];
                fListModel = new DiffContentsModel(fBase.Context);
                lvContents.ListMan = fListModel;

                UpdateView();
            }
        }

        public void SetLocale()
        {
            // TODO: Implement localization
        }

        private void UpdateView()
        {
            if (fCurrentRecord == null) return;

            // Update record info label
            string recName1 = GKUtils.GetRecordName(fBase.Context.Tree, fCurrentRecord.Obj1, false);
            string recName2 = GKUtils.GetRecordName(fBase.Context.Tree, fCurrentRecord.Obj2, false);
            lblRecordInfo.Text = string.Format("Record: {0} [{1}] ↔ {2} [{3}]",
                recName1, fCurrentRecord.Obj1.XRef,
                recName2, fCurrentRecord.Obj2.XRef);

            // Compare records and update list
            var diffTags = fSyncTool.CompareRecords(fCurrentRecord);
            fListModel.DataSource = diffTags;
            lvContents.UpdateContents();

            // Update navigation buttons
            btnPrevRecord.Enabled = (fCurrentIndex > 0);
            btnNextRecord.Enabled = (fCurrentIndex < fRecordsList.Count - 1);
            btnMerge.Enabled = (fCurrentIndex >= 0 && fCurrentIndex < fRecordsList.Count);
        }

        private void lvContents_SelectedItemsChanged(object sender, EventArgs e)
        {
            var diffTag = lvContents.GetSelectedData() as DiffTag;
            if (diffTag == null) return;

            GDMObjectsDescriber.GetFullDescription(fBase.Context.Tree, diffTag.Obj1, hvLeftRecord.Lines);
            GDMObjectsDescriber.GetFullDescription(fBase.Context.Tree, diffTag.Obj2, hvRightRecord.Lines);
        }

        private void btnPrevRecord_Click(object sender, EventArgs e)
        {
            if (fCurrentIndex > 0) {
                fCurrentIndex--;
                fCurrentRecord = fRecordsList[fCurrentIndex];
                UpdateView();
            }
        }

        private void btnNextRecord_Click(object sender, EventArgs e)
        {
            if (fCurrentIndex < fRecordsList.Count - 1) {
                fCurrentIndex++;
                fCurrentRecord = fRecordsList[fCurrentIndex];
                UpdateView();
            }
        }

        private void btnMerge_Click(object sender, EventArgs e)
        {
            var tagsDiff = fListModel.DataSource.Where((x) => x.Checked).ToList();
            if (tagsDiff == null || tagsDiff.Count <= 0) return;

            if (fSyncTool.Merge(fCurrentRecord.Obj1, fCurrentRecord.Obj2, tagsDiff))
                UpdateView();
        }

        #region List Model

        private sealed class DiffContentsModel : SimpleListModel<DiffTag>
        {
            public DiffContentsModel(BaseContext baseContext) :
                base(baseContext, CreateListColumns())
            {
            }

            public static ListColumns CreateListColumns()
            {
                var result = new ListColumns(GKListType.ltNone);
                result.AddColumn("Sync", DataType.dtBool, 40, true);
                result.AddColumn("#", DataType.dtInteger, 40, true);
                result.AddColumn("Content 1", DataType.dtString, 400, true);
                result.AddColumn("Content 2", DataType.dtString, 400, true);
                return result;
            }

            // fetched data
            private string prefix1, prefix2;

            public override void Fetch(DiffTag aRec)
            {
                base.Fetch(aRec);

                string diffChar = DiffUtil.GetStatusChar(fFetchedRec.Status);
                switch (fFetchedRec.Status) {
                    case DiffStatus.Equal:
                    default:
                        prefix1 = diffChar + " ";
                        prefix2 = diffChar + " ";
                        break;

                    case DiffStatus.Deleted:
                        prefix1 = diffChar + " ";
                        prefix2 = " ";
                        break;

                    case DiffStatus.Inserted:
                        prefix1 = " ";
                        prefix2 = diffChar + " ";
                        break;

                    case DiffStatus.Modified:
                    case DiffStatus.DeepModified:
                        prefix1 = diffChar + " ";
                        prefix2 = diffChar + " ";
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
                        result = prefix1 + GDMObjectsDescriber.GetBriefDescription(fBaseContext.Tree, fFetchedRec.Obj1);
                        break;
                    case 3:
                        result = prefix2 + GDMObjectsDescriber.GetBriefDescription(fBaseContext.Tree, fFetchedRec.Obj2);
                        break;
                }
                return result;
            }

            public override IColor GetBackgroundColor(int itemIndex, object rowData)
            {
                return SyncTool.GetDiffColor(((DiffTag)rowData).Status);
            }

            protected override void SetColumnValueEx(DiffTag item, int colIndex, object value)
            {
                if (item != null && colIndex == 0 && value is bool chk)
                    item.Checked = chk;
            }
        }

        #endregion
    }
}
