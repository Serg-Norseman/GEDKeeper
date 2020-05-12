﻿/*
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
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.Tools;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKMergeControl : Panel, IMergeControl
    {
        private GDMRecord fRec1;
        private GDMRecord fRec2;

        private HyperView fView1;
        private HyperView fView2;

        private IBaseWindow fBase;
        private GDMRecordType fMergeMode;
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

        public GDMRecordType MergeMode
        {
            get { return fMergeMode; }
            set { fMergeMode = value; }
        }

        public GDMRecord Rec1
        {
            get { return fRec1; }
        }

        public GDMRecord Rec2
        {
            get { return fRec2; }
        }

        public GKMergeControl()
        {
            InitializeComponent();

            SetRec1(null);
            SetRec2(null);

            btnRec1Select.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            btnRec2Select.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
        }

        private void UpdateMergeButtons()
        {
            btnMergeToLeft.Enabled = (fRec1 != null && fRec2 != null);
            btnMergeToRight.Enabled = (fRec1 != null && fRec2 != null);
        }

        public void Activate()
        {
            Focus();
        }

        public void SetRec1(GDMRecord value)
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

        public void SetRec2(GDMRecord value)
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
            GDMRecord irec = fBase.Context.SelectRecord(fMergeMode, null);
            if (irec != null) SetRec1(irec);
        }

        private void btnRec2Select_Click(object sender, EventArgs e)
        {
            GDMRecord irec = fBase.Context.SelectRecord(fMergeMode, null);
            if (irec != null) SetRec2(irec);
        }

        private void btnMergeToLeft_Click(object sender, EventArgs e)
        {
            TreeTools.MergeRecord(fBase, fRec1, fRec2, fBookmark);
            SetRec1(fRec1);
            SetRec2(null);
        }

        private void btnMergeToRight_Click(object sender, EventArgs e)
        {
            TreeTools.MergeRecord(fBase, fRec2, fRec1, fBookmark);
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

        private void InitializeComponent()
        {
            SuspendLayout();

            Lab1 = new Label();
            Lab1.Text = "XXX1";

            Lab2 = new Label();
            Lab2.Text = "XXX2";

            Edit1 = new TextBox();
            Edit1.ReadOnly = true;
            Edit1.Width = 350;

            Edit2 = new TextBox();
            Edit2.ReadOnly = true;
            Edit2.Width = 350;

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

            var contPan = new DefTableLayout(2, 4);
            contPan.SetRowScale(0, false);
            contPan.SetRowScale(1, false);
            contPan.SetRowScale(2, true);
            contPan.SetRowScale(3, false);
            contPan.SetColumnScale(0, true);
            contPan.SetColumnScale(1, true);

            contPan.Add(Lab1, 0, 0);
            contPan.Add(Lab2, 1, 0);
            contPan.Add(TableLayout.Horizontal(Edit1, null, btnRec1Select), 0, 1);
            contPan.Add(TableLayout.Horizontal(Edit2, null, btnRec2Select), 1, 1);
            contPan.Add(fView1, 0, 2);
            contPan.Add(fView2, 1, 2);
            contPan.Add(TableLayout.Horizontal(null, btnMergeToLeft), 0, 3);
            contPan.Add(TableLayout.Horizontal(btnMergeToRight, null), 1, 3);

            Content = contPan;

            UIHelper.SetControlFont(this, UIHelper.GetDefaultFont());
            ResumeLayout();
        }

        #endregion
    }
}
