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

using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKUI.Components
{
    public sealed class ACOptionsControl : Panel, IOptionsControl
    {
        private CircleChartOptions fOptions;

        public IOptions Options
        {
            get {
                return fOptions;
            }
            set {
                var circleOptions = value as CircleChartOptions;
                if (circleOptions == null) return;

                fOptions = circleOptions;
                UpdateControls();
            }
        }

        public ACOptionsControl()
        {
            InitializeComponent();
            SetLang();
        }

        public void SetLang()
        {
            //Title = LangMan.LS(LSID.LSID_MIOptions);

            //SheetAncCircle.Text = LangMan.LS(LSID.LSID_AncestorsCircle);
            acb0.Text = LangMan.LS(LSID.LSID_Circle) + @" 0";
            acb1.Text = LangMan.LS(LSID.LSID_Circle) + @" 1";
            acb2.Text = LangMan.LS(LSID.LSID_Circle) + @" 2";
            acb3.Text = LangMan.LS(LSID.LSID_Circle) + @" 3";
            acb4.Text = LangMan.LS(LSID.LSID_Circle) + @" 4";
            acb5.Text = LangMan.LS(LSID.LSID_Circle) + @" 5";
            acb6.Text = LangMan.LS(LSID.LSID_Circle) + @" 6";
            acb7.Text = LangMan.LS(LSID.LSID_Circle) + @" 7";
            acbText.Text = LangMan.LS(LSID.LSID_TextColor);
            acbBack.Text = LangMan.LS(LSID.LSID_BackColor);
            acbLine.Text = LangMan.LS(LSID.LSID_LinesColor);
            chkHideEmptySegments.Text = LangMan.LS(LSID.LSID_HideEmptySegments);
            chkArcText.Text = LangMan.LS(LSID.LSID_ArcText);
        }

        public void AcceptChanges()
        {
            if (fOptions == null) return;

            fOptions.BrushColor[ 0] = UIHelper.ConvertColor(acb0.BackgroundColor);
            fOptions.BrushColor[ 1] = UIHelper.ConvertColor(acb1.BackgroundColor);
            fOptions.BrushColor[ 2] = UIHelper.ConvertColor(acb2.BackgroundColor);
            fOptions.BrushColor[ 3] = UIHelper.ConvertColor(acb3.BackgroundColor);
            fOptions.BrushColor[ 4] = UIHelper.ConvertColor(acb4.BackgroundColor);
            fOptions.BrushColor[ 5] = UIHelper.ConvertColor(acb5.BackgroundColor);
            fOptions.BrushColor[ 6] = UIHelper.ConvertColor(acb6.BackgroundColor);
            fOptions.BrushColor[ 7] = UIHelper.ConvertColor(acb7.BackgroundColor);
            fOptions.BrushColor[ 8] = UIHelper.ConvertColor(acbText.BackgroundColor);
            fOptions.BrushColor[ 9] = UIHelper.ConvertColor(acbBack.BackgroundColor);
            fOptions.BrushColor[10] = UIHelper.ConvertColor(acbLine.BackgroundColor);

            fOptions.HideEmptySegments = chkHideEmptySegments.Checked.GetValueOrDefault();
            fOptions.ArcText = chkArcText.Checked.GetValueOrDefault();
            //fOptions.Apply();
        }

        public void UpdateControls()
        {
            if (fOptions == null) return;
            //PanDefFont.Text = FOptions.ChartOptions.DefFont_Name + ", " + FOptions.ChartOptions.DefFont_Size.ToString();

            acb0.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[0]);
            acb1.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[1]);
            acb2.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[2]);
            acb3.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[3]);
            acb4.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[4]);
            acb5.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[5]);
            acb6.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[6]);
            acb7.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[7]);
            acbText.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[8]);
            acbBack.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[9]);
            acbLine.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[10]);

            chkHideEmptySegments.Checked = fOptions.HideEmptySegments;
            chkArcText.Checked = fOptions.ArcText;
        }

        /*private void PanDefFont_Click(object sender, EventArgs e)
        {
            if (FontDialog1.ShowDialog() == DialogResult.OK)
            {
                //FOptions.ChartOptions.DefFont_Name = FontDialog1.Font.Name;
                //FOptions.ChartOptions.DefFont_Size = (int)checked((long)Math.Round((double)FontDialog1.Font.Size));
            }
            UpdateControls();
        }*/

        private void lblColorClick(object sender, MouseEventArgs e)
        {
            Label lbl = sender as Label;
            if (lbl == null) return;
            lbl.BackgroundColor = UIHelper.ConvertColor(AppHost.StdDialogs.SelectColor(UIHelper.ConvertColor(lbl.BackgroundColor)));
        }

        #region Design

        private Label acb0;
        private Label acb1;
        private Label acb2;
        private Label acb3;
        private Label acb4;
        private Label acb5;
        private Label acb6;
        private Label acb7;
        private Label acbText;
        private Label acbBack;
        private Label acbLine;
        private CheckBox chkHideEmptySegments;
        private CheckBox chkArcText;

        private void InitializeComponent()
        {
            SuspendLayout();

            acbLine = new Label();
            //acbLine.BorderStyle = BorderStyle.Fixed3D;
            acbLine.Cursor = Cursors.Pointer;
            acbLine.TextColor = Colors.White;
            acbLine.Size = new Size(100, 20);
            acbLine.Text = "Line color";
            acbLine.MouseDown += lblColorClick;

            acbBack = new Label();
            //acbBack.BorderStyle = BorderStyle.Fixed3D;
            acbBack.Cursor = Cursors.Pointer;
            acbBack.Size = new Size(100, 20);
            acbBack.Text = "Background color";
            acbBack.MouseDown += lblColorClick;

            acbText = new Label();
            //acbText.BorderStyle = BorderStyle.Fixed3D;
            acbText.Cursor = Cursors.Pointer;
            acbText.TextColor = Colors.White;
            acbText.Size = new Size(100, 20);
            acbText.Text = "Text color";
            acbText.MouseDown += lblColorClick;

            acb7 = new Label();
            //acb7.BorderStyle = BorderStyle.Fixed3D;
            acb7.Cursor = Cursors.Pointer;
            acb7.Size = new Size(100, 20);
            acb7.Text = "Circle 7";
            acb7.MouseDown += lblColorClick;

            acb6 = new Label();
            //acb6.BorderStyle = BorderStyle.Fixed3D;
            acb6.Cursor = Cursors.Pointer;
            acb6.Size = new Size(100, 20);
            acb6.Text = "Circle 6";
            acb6.MouseDown += lblColorClick;

            acb5 = new Label();
            //acb5.BorderStyle = BorderStyle.Fixed3D;
            acb5.Cursor = Cursors.Pointer;
            acb5.Size = new Size(100, 20);
            acb5.Text = "Circle 5";
            acb5.MouseDown += lblColorClick;

            acb4 = new Label();
            //acb4.BorderStyle = BorderStyle.Fixed3D;
            acb4.Cursor = Cursors.Pointer;
            acb4.Size = new Size(100, 20);
            acb4.Text = "Circle 4";
            acb4.MouseDown += lblColorClick;

            acb3 = new Label();
            //acb3.BorderStyle = BorderStyle.Fixed3D;
            acb3.Cursor = Cursors.Pointer;
            acb3.Size = new Size(100, 20);
            acb3.Text = "Circle 3";
            acb3.MouseDown += lblColorClick;

            acb2 = new Label();
            //acb2.BorderStyle = BorderStyle.Fixed3D;
            acb2.Cursor = Cursors.Pointer;
            acb2.Size = new Size(100, 20);
            acb2.Text = "Circle 2";
            acb2.MouseDown += lblColorClick;

            acb1 = new Label();
            //acb1.BorderStyle = BorderStyle.Fixed3D;
            acb1.Cursor = Cursors.Pointer;
            acb1.Size = new Size(100, 20);
            acb1.Text = "Circle 1";
            acb1.MouseDown += lblColorClick;

            acb0 = new Label();
            //acb0.BorderStyle = BorderStyle.Fixed3D;
            acb0.Cursor = Cursors.Pointer;
            acb0.Size = new Size(100, 20);
            acb0.Text = "Circle 0";
            acb0.MouseDown += lblColorClick;

            chkHideEmptySegments = new CheckBox();
            //chkHideEmptySegments.Size = new Size(328, 24);
            chkHideEmptySegments.Text = "chkHideEmptySegments";

            chkArcText = new CheckBox();
            //chkArcText.Size = new Size(328, 24);
            chkArcText.Text = "chkArcText";

            Content = new DefTableLayout() {
                Rows = {
                    new TableRow {
                        Cells = { acb0, acb1, acb2, acb3 }
                    },
                    new TableRow {
                        Cells = { acb4, acb5, acb6, acb7 }
                    },
                    new TableRow {
                        Cells = { acbText, acbBack, acbLine }
                    },
                    new TableRow {
                        Cells = { chkHideEmptySegments }
                    },
                    new TableRow {
                        Cells = { chkArcText }
                    },
                    null
                }
            };

            UIHelper.SetControlFont(this, UIHelper.GetDefaultFont());
            ResumeLayout();
        }

        #endregion
    }
}
