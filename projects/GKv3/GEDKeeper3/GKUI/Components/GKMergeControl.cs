/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public partial class GKMergeControl : Panel
    {
        private GEDCOMRecord fRec1;
        private GEDCOMRecord fRec2;

        private IBaseWindow fBase;
        private GEDCOMRecordType fMergeMode;
        private bool fBookmark;

        public IBaseWindow Base
        {
            get { return fBase; }
            set { fBase = value; }
        }

        public bool Bookmark
        {
            get { return fBookmark; }
            set { fBookmark = value; }
        }

        public GEDCOMRecordType MergeMode
        {
            get { return fMergeMode; }
            set { fMergeMode = value; }
        }

        public GEDCOMRecord Rec1
        {
            get { return fRec1; }
        }

        public GEDCOMRecord Rec2
        {
            get { return fRec2; }
        }

        public GKMergeControl()
        {
            InitializeComponent();

            AdjustControls();
            SetRec1(null);
            SetRec2(null);

            btnRec1Select.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            btnRec2Select.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
        }

        private void AdjustControls()
        {
            /*if (fView1 == null || fView2 == null) return;

            int y = Edit1.Top + Edit1.Height + 8;
            int h = btnMergeToLeft.Top - y - 8;
            int w = (btnRec1Select.Left + btnRec1Select.Width) - Edit1.Left;

            fView1.Location = new Point(Edit1.Left, y);
            fView1.Size = new Size(w, h);

            fView2.Location = new Point(Edit2.Left, y);
            fView2.Size = new Size(w, h);*/
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            AdjustControls();
            base.OnSizeChanged(e);
        }

        private void RecordMerge(GEDCOMRecord targetRec, GEDCOMRecord sourceRec)
        {
            if (targetRec == null)
                throw new ArgumentNullException("targetRec");

            if (sourceRec == null)
                throw new ArgumentNullException("sourceRec");

            var repMap = new XRefReplacer();
            try
            {
                repMap.AddXRef(sourceRec, sourceRec.XRef, targetRec.XRef);

                GEDCOMTree tree = fBase.Context.Tree;
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    tree[i].ReplaceXRefs(repMap);
                }

                sourceRec.MoveTo(targetRec, false);
                BaseController.DeleteRecord(fBase, sourceRec, false);

                if (targetRec.RecordType == GEDCOMRecordType.rtIndividual && fBookmark) {
                    ((GEDCOMIndividualRecord)targetRec).Bookmark = true;
                }

                fBase.NotifyRecord(targetRec, RecordAction.raEdit);
                fBase.RefreshLists(false);
            }
            finally
            {
                repMap.Dispose();
            }
        }

        private void UpdateMergeButtons()
        {
            btnMergeToLeft.Enabled = (fRec1 != null && fRec2 != null);
            btnMergeToRight.Enabled = (fRec1 != null && fRec2 != null);
        }

        public void SetRec1(GEDCOMRecord value)
        {
            fRec1 = value;
            UpdateMergeButtons();

            if (fRec1 == null) {
                Lab1.Text = @"XXX1";
                Edit1.Text = "";
                fView1.Lines.Clear();
            } else {
                Lab1.Text = fRec1.XRef;
                Edit1.Text = GKUtils.GetRecordName(fRec1, false);
                fView1.Lines.Assign(fBase.GetRecordContent(fRec1));
            }
        }

        public void SetRec2(GEDCOMRecord value)
        {
            fRec2 = value;
            UpdateMergeButtons();

            if (fRec2 == null) {
                Lab2.Text = @"XXX2";
                Edit2.Text = "";
                fView2.Lines.Clear();
            } else {
                Lab2.Text = fRec2.XRef;
                Edit2.Text = GKUtils.GetRecordName(fRec2, false);
                fView2.Lines.Assign(fBase.GetRecordContent(fRec2));
            }
        }

        private void btnRec1Select_Click(object sender, EventArgs e)
        {
            GEDCOMRecord irec = fBase.Context.SelectRecord(fMergeMode, null);
            if (irec != null) SetRec1(irec);
        }

        private void btnRec2Select_Click(object sender, EventArgs e)
        {
            GEDCOMRecord irec = fBase.Context.SelectRecord(fMergeMode, null);
            if (irec != null) SetRec2(irec);
        }

        private void btnMergeToLeft_Click(object sender, EventArgs e)
        {
            RecordMerge(fRec1, fRec2);
            SetRec1(fRec1);
            SetRec2(null);
        }

        private void btnMergeToRight_Click(object sender, EventArgs e)
        {
            RecordMerge(fRec2, fRec1);
            SetRec1(null);
            SetRec2(fRec2);
        }

        #region Design

        private Button btnMergeToRight;
        private Button btnMergeToLeft;
        private Button btnRec2Select;
        private Button btnRec1Select;
        private TextBox Edit2;
        private TextBox Edit1;
        private Label Lab2;
        private Label Lab1;
        private HyperView fView1;
        private HyperView fView2;

        /// <summary>
        /// This method is required for Windows Forms designer support.
        /// Do not change the method contents inside the source code editor. The Forms designer might
        /// not be able to load this method if it was changed manually.
        /// </summary>
        private void InitializeComponent()
        {
            SuspendLayout();

            Lab1 = new Label();
            Lab1.Text = "XXX1";

            Lab2 = new Label();
            Lab2.Text = "XXX2";

            Edit1 = new TextBox();
            Edit1.ReadOnly = true;
            //Edit1.Size = new Size(366, 24);

            Edit2 = new TextBox();
            Edit2.ReadOnly = true;
            //Edit2.Size = new Size(373, 24);

            btnRec1Select = new Button();
            btnRec1Select.Size = new Size(80, 26);
            btnRec1Select.Text = "btnRec1Select";
            btnRec1Select.Click += btnRec1Select_Click;

            btnRec2Select = new Button();
            btnRec2Select.Size = new Size(80, 26);
            btnRec2Select.Text = "btnRec2Select";
            btnRec2Select.Click += btnRec2Select_Click;

            btnMergeToLeft = new Button();
            btnMergeToLeft.Enabled = false;
            btnMergeToLeft.Size = new Size(80, 26);
            btnMergeToLeft.Text = "<<<";
            btnMergeToLeft.Click += btnMergeToLeft_Click;

            btnMergeToRight = new Button();
            btnMergeToRight.Enabled = false;
            btnMergeToRight.Size = new Size(80, 26);
            btnMergeToRight.Text = ">>>";
            btnMergeToRight.Click += btnMergeToRight_Click;

            fView1 = new HyperView();
            fView2 = new HyperView();

            Content = new DefTableLayout() {
                Rows = {
                    new TableRow {
                        Cells = { Lab1, Lab2 }
                    },
                    new TableRow {
                        Cells = { TableLayout.Horizontal(Edit1, btnRec1Select), TableLayout.Horizontal(Edit2, btnRec2Select) }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { fView1, fView2 }
                    },
                    new TableRow {
                        Cells = { TableLayout.Horizontal(null, btnMergeToLeft), TableLayout.Horizontal(btnMergeToRight, null) }
                    }
                }
            };
            //((DefTableLayout)Content).SetColumnScale(0, true);
            //((DefTableLayout)Content).SetColumnScale(1, true);

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }

        #endregion
    }
}
