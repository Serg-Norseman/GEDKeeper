/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using GKCore;
using GKCore.Design;
using GKCore.Locales;
using GKCore.Sync;
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

    public partial class TSDetailForm : Dialog, ILocalizable
    {
#if NETCOREAPP
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051
        private Panel panel1;
        private Button btnPrevRecord;
        private Button btnNextRecord;
        private Button btnCopyToLeft;
        private Button btnCopyToRight;
        private GKListView lvContents;
        private Label lblRecordInfo;
#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
#endif

        private readonly IBaseWindow fBase;
        private readonly List<DiffRecord> fRecordsList;
        private readonly DiffContentsModel fListModel;

        private int fCurrentIndex;
        private DiffRecord fCurrentRecord;

        public TSDetailForm()
        {
#if !NETCOREAPP
            InitializeComponent();
#else
            XamlReader.Load(this);
#endif

            // TODO
            btnCopyToLeft.Enabled = false;
            btnCopyToRight.Enabled = false;
        }

        public TSDetailForm(IBaseWindow curBase, List<DiffRecord> recordsList, int currentIndex) : this()
        {
            fBase = curBase;
            fRecordsList = recordsList;
            fCurrentIndex = currentIndex;

            if (fRecordsList != null && fRecordsList.Count > 0 && fCurrentIndex >= 0 && fCurrentIndex < fRecordsList.Count) {
                fCurrentRecord = fRecordsList[fCurrentIndex];
                fListModel = new DiffContentsModel(fBase.Context, SyncTool.CommonTagContentHandler);
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
            var diffTags = SyncTool.CompareRecords(fCurrentRecord);
            fListModel.DataSource = diffTags;
            lvContents.UpdateContents();

            // Update navigation buttons
            btnPrevRecord.Enabled = (fCurrentIndex > 0);
            btnNextRecord.Enabled = (fCurrentIndex < fRecordsList.Count - 1);
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

        private void btnCopyToLeft_Click(object sender, EventArgs e)
        {
            // TODO: Implement copying from right to left
            // This would involve applying changes from fCurrentRecord.Obj2 to fCurrentRecord.Obj1
        }

        private void btnCopyToRight_Click(object sender, EventArgs e)
        {
            // TODO: Implement copying from left to right
            // This would involve applying changes from fCurrentRecord.Obj1 to fCurrentRecord.Obj2
        }
    }
}
