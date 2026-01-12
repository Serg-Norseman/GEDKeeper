/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Drawing;
using Eto.Forms;
using GKCore.Design.Controls;
using GKCore.Filters;
using GKCore.Locales;

namespace GKUI.Components
{
    public class GKFilterControl : Panel, IFilterControl, ILocalizable
    {
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
            InitializeComponent();
            SetLocale();
        }

        public void Activate()
        {
            Focus();
        }

        public void SetLocale()
        {
            btnSettings.ToolTip = LangMan.LS(LSID.MIOptions);
            chkIndistinctMatching.Text = LangMan.LS(LSID.RM_IndistinctMatching);
        }

        private void Control_LostFocus(object sender, EventArgs e)
        {
            if (fPopupShown && !frmPopup.HasFocus) {
                frmPopup.Visible = false;
                fPopupShown = !fPopupShown;
            }
        }

        private void PopupButton_Click(object sender, EventArgs e)
        {
            if (!fPopupShown) {
                //frmPopup.Width = this.Width;
                //fPopupForm.Height = 100;
                frmPopup.Location = Point.Round(PointToScreen(Content.Bounds.BottomLeft));
                frmPopup.Visible = true;
            } else {
                frmPopup.Visible = false;
            }
            fPopupShown = !fPopupShown;
        }

        private void chkIndistinctMatching_CheckedChanged(object sender, EventArgs e)
        {
            numIndistinctThreshold.Enabled = chkIndistinctMatching.Checked.Value;
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
                numIndistinctThreshold.Value = 100.0f;
            } else {
                chkIndistinctMatching.Checked = (fParams.Type == MatchType.Indistinct);
                numIndistinctThreshold.Value = Math.Round(fParams.IndistinctThreshold * 100);
            }
        }

        private void OnParamsChanged()
        {
            if (fParams != null) {
                fParams.Type = !chkIndistinctMatching.Checked.Value ? MatchType.REMask : MatchType.Indistinct;
                fParams.IndistinctThreshold = (float)(numIndistinctThreshold.Value / 100.0f);
            }

            ParamsChanged?.Invoke(this, EventArgs.Empty);
        }

        #region Design

        private Button btnSettings;
        private Form frmPopup;
        private CheckBox chkIndistinctMatching;
        private NumericStepper numIndistinctThreshold;

        private void InitializeComponent()
        {
            SuspendLayout();

            chkIndistinctMatching = new CheckBox();
            chkIndistinctMatching.CheckedChanged += chkIndistinctMatching_CheckedChanged;

            numIndistinctThreshold = new NumericStepper();
            numIndistinctThreshold.MinValue = 0;
            numIndistinctThreshold.MaxValue = 100;
            numIndistinctThreshold.Width = 60;
            numIndistinctThreshold.ValueChanged += numIndistinctThreshold_ValueChanged;

            frmPopup = new Form();
            frmPopup.WindowStyle = WindowStyle.None;
            frmPopup.ShowActivated = false;
            frmPopup.Resizable = false;
            //fPopupForm.CanFocus = false;
            frmPopup.ShowInTaskbar = false;
            frmPopup.Content = new Scrollable() {
                Border = BorderType.Bezel,
                Content = new StackLayout() {
                    Padding = 8,
                    Spacing = 8,
                    Orientation = Orientation.Horizontal,
                    Items = {
                        chkIndistinctMatching,
                        numIndistinctThreshold
                    }
                }
            };

            btnSettings = new Button();
            btnSettings.Style = "iconBtn";
            btnSettings.Image = UIHelper.LoadResourceImage("Resources.btn_tools.gif");
            btnSettings.Click += PopupButton_Click;

            Content = btnSettings;
            LostFocus += Control_LostFocus;

            ResumeLayout();
        }

        #endregion
    }
}
