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

using Eto.Forms;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKUI.Components
{
    public partial class ACOptionsControl : Panel, IOptionsControl
    {
        private AncestorsCircleOptions fOptions;

        public IOptions Options
        {
            get {
                return fOptions;
            }
            set {
                var ancestorsCircleOptions = value as AncestorsCircleOptions;
                if (ancestorsCircleOptions == null) return;

                fOptions = ancestorsCircleOptions;
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

            fOptions.HideEmptySegments = chkHideEmptySegments.Checked;
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

            ColorDialog1.Color = lbl.BackgroundColor;

            if (ColorDialog1.ShowDialog(this) == DialogResult.Ok) {
                lbl.BackgroundColor = ColorDialog1.Color;
            }
        }
    }
}
