/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Windows.Forms;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Charts
{
    public partial class ACOptionsControl : UserControl, IOptionsControl
    {
        private AncestorsCircleOptions fOptions;

        public IOptions Options
        {
            get {
                return this.fOptions;
            }
            set {
                if (value is AncestorsCircleOptions) {
                    this.fOptions = value as AncestorsCircleOptions;
                    this.UpdateControls();
                }
            }
        }

        public ACOptionsControl()
        {
            this.InitializeComponent();
            this.SetLang();
        }

        public void SetLang()
        {
            this.Text = LangMan.LS(LSID.LSID_MIOptions);

            //this.SheetAncCircle.Text = LangMan.LS(LSID.LSID_AncestorsCircle);
            this.acb0.Text = LangMan.LS(LSID.LSID_Circle) + @" 0";
            this.acb1.Text = LangMan.LS(LSID.LSID_Circle) + @" 1";
            this.acb2.Text = LangMan.LS(LSID.LSID_Circle) + @" 2";
            this.acb3.Text = LangMan.LS(LSID.LSID_Circle) + @" 3";
            this.acb4.Text = LangMan.LS(LSID.LSID_Circle) + @" 4";
            this.acb5.Text = LangMan.LS(LSID.LSID_Circle) + @" 5";
            this.acb6.Text = LangMan.LS(LSID.LSID_Circle) + @" 6";
            this.acb7.Text = LangMan.LS(LSID.LSID_Circle) + @" 7";
            this.acbText.Text = LangMan.LS(LSID.LSID_TextColor);
            this.acbBack.Text = LangMan.LS(LSID.LSID_BackColor);
            this.acbLine.Text = LangMan.LS(LSID.LSID_LinesColor);
        }

        public void AcceptChanges()
        {
            this.fOptions.BrushColor[ 0] = this.acb0.BackColor;
            this.fOptions.BrushColor[ 1] = this.acb1.BackColor;
            this.fOptions.BrushColor[ 2] = this.acb2.BackColor;
            this.fOptions.BrushColor[ 3] = this.acb3.BackColor;
            this.fOptions.BrushColor[ 4] = this.acb4.BackColor;
            this.fOptions.BrushColor[ 5] = this.acb5.BackColor;
            this.fOptions.BrushColor[ 6] = this.acb6.BackColor;
            this.fOptions.BrushColor[ 7] = this.acb7.BackColor;
            this.fOptions.BrushColor[ 8] = this.acbText.BackColor;
            this.fOptions.BrushColor[ 9] = this.acbBack.BackColor;
            this.fOptions.BrushColor[10] = this.acbLine.BackColor;
            
            this.fOptions.Apply();
        }

        public void UpdateControls()
        {
            //this.PanDefFont.Text = this.FOptions.ChartOptions.DefFont_Name + ", " + this.FOptions.ChartOptions.DefFont_Size.ToString();

            this.acb0.BackColor = this.fOptions.BrushColor[0];
            this.acb1.BackColor = this.fOptions.BrushColor[1];
            this.acb2.BackColor = this.fOptions.BrushColor[2];
            this.acb3.BackColor = this.fOptions.BrushColor[3];
            this.acb4.BackColor = this.fOptions.BrushColor[4];
            this.acb5.BackColor = this.fOptions.BrushColor[5];
            this.acb6.BackColor = this.fOptions.BrushColor[6];
            this.acb7.BackColor = this.fOptions.BrushColor[7];
            this.acbText.BackColor = this.fOptions.BrushColor[8];
            this.acbBack.BackColor = this.fOptions.BrushColor[9];
            this.acbLine.BackColor = this.fOptions.BrushColor[10];
        }

        /*private void PanDefFont_Click(object sender, EventArgs e)
        {
            if (this.FontDialog1.ShowDialog() == DialogResult.OK)
            {
                //this.FOptions.ChartOptions.DefFont_Name = this.FontDialog1.Font.Name;
                //this.FOptions.ChartOptions.DefFont_Size = (int)checked((long)Math.Round((double)this.FontDialog1.Font.Size));
            }
            this.UpdateControls();
        }*/

        private void lblColorClick(object sender, MouseEventArgs e)
        {
            Label lbl = sender as Label;
            if (lbl == null)
            {
                return;
            }

            this.ColorDialog1.Color = lbl.BackColor;

            if (this.ColorDialog1.ShowDialog() == DialogResult.OK) {
                lbl.BackColor = this.ColorDialog1.Color;
            }
        }
    }
}
