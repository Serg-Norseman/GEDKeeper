﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Lists;

namespace GKUI.Components
{
    public class GKFilterControl : UserControl, IFilterControl, ILocalizable, IMessageFilter
    {
        private readonly IContainer fComponents;
        private readonly ToolTip fToolTip;

        private bool fPopupShown;
        private QuickFilterParams fParams;

        public QuickFilterParams Params
        {
            get { return fParams; }
            set {
                fParams = value;
                UpdateView();
            }
        }

        public event EventHandler ParamsChanged;

        public GKFilterControl()
        {
            fComponents = new Container();
            fToolTip = new ToolTip(this.fComponents);

            InitializeComponent();
            SetLocale();

            Application.AddMessageFilter(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                Application.RemoveMessageFilter(this);

                if (fComponents != null) fComponents.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Activate()
        {
            Focus();
        }

        public void SetLocale()
        {
            fToolTip.SetToolTip(btnSettings, LangMan.LS(LSID.MIOptions));
            chkIndistinctMatching.Text = LangMan.LS(LSID.RM_IndistinctMatching);
        }

        private void Control_LostFocus(object sender, EventArgs e)
        {
            if (fPopupShown && !frmPopup.Focused) {
                SetPopupVisible(false);
            }
        }

        private void SetPopupVisible(bool visible)
        {
            frmPopup.Visible = visible;
            fPopupShown = visible;
        }

        private void PopupButton_Click(object sender, EventArgs e)
        {
            if (!fPopupShown) {
                frmPopup.Location = Parent.PointToScreen(new Point(Bounds.X, Bounds.Bottom));
                SetPopupVisible(true);
            } else {
                SetPopupVisible(false);
            }
        }

        private void chkIndistinctMatching_CheckedChanged(object sender, EventArgs e)
        {
            OnParamsChanged();
        }

        private void numIndistinctThreshold_ValueChanged(object sender, EventArgs e)
        {
            OnParamsChanged();
        }

        private void UpdateView()
        {
            if (fParams == null) {
                chkIndistinctMatching.Checked = false;
                numIndistinctThreshold.Value = 100;
            } else {
                chkIndistinctMatching.Checked = (fParams.Type == MatchType.Indistinct);
                numIndistinctThreshold.Value = (int)Math.Round(fParams.IndistinctThreshold * 100);
            }
            numIndistinctThreshold.Enabled = chkIndistinctMatching.Checked;
        }

        private void OnParamsChanged()
        {
            if (fParams != null) {
                fParams.Type = !chkIndistinctMatching.Checked ? MatchType.REMask : MatchType.Indistinct;
                fParams.IndistinctThreshold = ((float)numIndistinctThreshold.Value / 100.0f);
            }

            ParamsChanged?.Invoke(this, EventArgs.Empty);
        }

        public bool PreFilterMessage(ref Message m)
        {
            if (fPopupShown && (Form.ActiveForm == null || !Form.ActiveForm.Equals(frmPopup))) {
                SetPopupVisible(false);
            }
            return false;
        }

        #region Design

        private Button btnSettings;
        private Form frmPopup;
        private CheckBox chkIndistinctMatching;
        private NumericUpDown numIndistinctThreshold;

        private void InitializeComponent()
        {
            SuspendLayout();

            chkIndistinctMatching = new CheckBox();
            chkIndistinctMatching.CheckedChanged += chkIndistinctMatching_CheckedChanged;
            chkIndistinctMatching.AutoSize = true;

            numIndistinctThreshold = new NumericUpDown();
            numIndistinctThreshold.Minimum = 0;
            numIndistinctThreshold.Maximum = 100;
            numIndistinctThreshold.Width = 60;
            numIndistinctThreshold.ValueChanged += numIndistinctThreshold_ValueChanged;

            frmPopup = new Form();
            frmPopup.FormBorderStyle = FormBorderStyle.None;
            frmPopup.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            frmPopup.AutoSize = true;
            frmPopup.StartPosition = FormStartPosition.Manual;
            frmPopup.ShowInTaskbar = false;
            frmPopup.Size = new Size(1, 1);
            frmPopup.SuspendLayout();
            frmPopup.Controls.Add(new FlowLayoutPanel() {
                BorderStyle = BorderStyle.FixedSingle,
                Dock = DockStyle.Fill,
                Padding = new Padding(8),
                FlowDirection = FlowDirection.LeftToRight,
                WrapContents = false,
                AutoSizeMode = AutoSizeMode.GrowAndShrink,
                AutoSize = true,
                Controls = { chkIndistinctMatching, numIndistinctThreshold }
            });
            frmPopup.ResumeLayout(true);
            frmPopup.LostFocus += Control_LostFocus;

            btnSettings = new Button();
            btnSettings.Location = new Point(0, 0);
            btnSettings.Image = UIHelper.LoadResourceImage("Resources.btn_tools.gif");
            btnSettings.Size = new Size(36, 32);
            btnSettings.Click += PopupButton_Click;

            Size = btnSettings.Size;
            Controls.Add(btnSettings);
            LostFocus += Control_LostFocus;

            ResumeLayout(false);
            PerformLayout();
        }

        #endregion
    }
}
