/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Windows.Forms;
using GKCore;
using GKCore.Locales;
using GKCore.Options;

namespace GKUI.Components
{
    public partial class ACOptionsControl : UserControl, IOptionsControl
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
            SetLocale();
        }

        public void SetLocale()
        {
            Text = LangMan.LS(LSID.MIOptions);

            //SheetAncCircle.Text = LangMan.LS(LSID.AncestorsCircle);
            acb0.Text = LangMan.LS(LSID.Circle) + @" 0";
            acb1.Text = LangMan.LS(LSID.Circle) + @" 1";
            acb2.Text = LangMan.LS(LSID.Circle) + @" 2";
            acb3.Text = LangMan.LS(LSID.Circle) + @" 3";
            acb4.Text = LangMan.LS(LSID.Circle) + @" 4";
            acb5.Text = LangMan.LS(LSID.Circle) + @" 5";
            acb6.Text = LangMan.LS(LSID.Circle) + @" 6";
            acb7.Text = LangMan.LS(LSID.Circle) + @" 7";
            acbText.Text = LangMan.LS(LSID.TextColor);
            acbBack.Text = LangMan.LS(LSID.BackColor);
            acbLine.Text = LangMan.LS(LSID.LinesColor);
            chkHideEmptySegments.Text = LangMan.LS(LSID.HideEmptySegments);
            chkArcText.Text = LangMan.LS(LSID.ArcText);
        }

        public void AcceptChanges()
        {
            if (fOptions == null) return;

            fOptions.BrushColor[ 0] = UIHelper.ConvertColor(acb0.BackColor);
            fOptions.BrushColor[ 1] = UIHelper.ConvertColor(acb1.BackColor);
            fOptions.BrushColor[ 2] = UIHelper.ConvertColor(acb2.BackColor);
            fOptions.BrushColor[ 3] = UIHelper.ConvertColor(acb3.BackColor);
            fOptions.BrushColor[ 4] = UIHelper.ConvertColor(acb4.BackColor);
            fOptions.BrushColor[ 5] = UIHelper.ConvertColor(acb5.BackColor);
            fOptions.BrushColor[ 6] = UIHelper.ConvertColor(acb6.BackColor);
            fOptions.BrushColor[ 7] = UIHelper.ConvertColor(acb7.BackColor);
            fOptions.BrushColor[ 8] = UIHelper.ConvertColor(acbText.BackColor);
            fOptions.BrushColor[ 9] = UIHelper.ConvertColor(acbBack.BackColor);
            fOptions.BrushColor[10] = UIHelper.ConvertColor(acbLine.BackColor);

            fOptions.HideEmptySegments = chkHideEmptySegments.Checked;
            fOptions.ArcText = chkArcText.Checked;
            //fOptions.Apply();
        }

        public void UpdateControls()
        {
            if (fOptions == null) return;
            //PanDefFont.Text = FOptions.ChartOptions.DefFont_Name + ", " + FOptions.ChartOptions.DefFont_Size.ToString();

            acb0.BackColor = UIHelper.ConvertColor(fOptions.BrushColor[0]);
            acb1.BackColor = UIHelper.ConvertColor(fOptions.BrushColor[1]);
            acb2.BackColor = UIHelper.ConvertColor(fOptions.BrushColor[2]);
            acb3.BackColor = UIHelper.ConvertColor(fOptions.BrushColor[3]);
            acb4.BackColor = UIHelper.ConvertColor(fOptions.BrushColor[4]);
            acb5.BackColor = UIHelper.ConvertColor(fOptions.BrushColor[5]);
            acb6.BackColor = UIHelper.ConvertColor(fOptions.BrushColor[6]);
            acb7.BackColor = UIHelper.ConvertColor(fOptions.BrushColor[7]);
            acbText.BackColor = UIHelper.ConvertColor(fOptions.BrushColor[8]);
            acbBack.BackColor = UIHelper.ConvertColor(fOptions.BrushColor[9]);
            acbLine.BackColor = UIHelper.ConvertColor(fOptions.BrushColor[10]);

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

        private async void lblColorClick(object sender, MouseEventArgs e)
        {
            Label lbl = sender as Label;
            if (lbl == null) return;
            lbl.BackColor = UIHelper.ConvertColor(await AppHost.StdDialogs.SelectColor(UIHelper.ConvertColor(lbl.BackColor)));
        }
    }
}
