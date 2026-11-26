/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Locales;
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
            XamlReader.Load(this);
            SetLocale();
        }

        public void SetLocale()
        {
            //Title = LangMan.LS(LSID.MIOptions);

            //SheetAncCircle.Text = LangMan.LS(LSID.AncestorsCircle);
            acb0.Label.Text = LangMan.LS(LSID.Circle) + @" 0";
            acb1.Label.Text = LangMan.LS(LSID.Circle) + @" 1";
            acb2.Label.Text = LangMan.LS(LSID.Circle) + @" 2";
            acb3.Label.Text = LangMan.LS(LSID.Circle) + @" 3";
            acb4.Label.Text = LangMan.LS(LSID.Circle) + @" 4";
            acb5.Label.Text = LangMan.LS(LSID.Circle) + @" 5";
            acb6.Label.Text = LangMan.LS(LSID.Circle) + @" 6";
            acb7.Label.Text = LangMan.LS(LSID.Circle) + @" 7";
            acbText.Label.Text = LangMan.LS(LSID.TextColor);
            acbBack.Label.Text = LangMan.LS(LSID.BackColor);
            acbLine.Label.Text = LangMan.LS(LSID.LinesColor);
            chkHideEmptySegments.Text = LangMan.LS(LSID.HideEmptySegments);
            chkArcText.Text = LangMan.LS(LSID.ArcText);
        }

        public void AcceptChanges()
        {
            if (fOptions == null) return;

            fOptions.BrushColor[ 0] = UIHelper.ConvertColor(acb0.Label.BackgroundColor);
            fOptions.BrushColor[ 1] = UIHelper.ConvertColor(acb1.Label.BackgroundColor);
            fOptions.BrushColor[ 2] = UIHelper.ConvertColor(acb2.Label.BackgroundColor);
            fOptions.BrushColor[ 3] = UIHelper.ConvertColor(acb3.Label.BackgroundColor);
            fOptions.BrushColor[ 4] = UIHelper.ConvertColor(acb4.Label.BackgroundColor);
            fOptions.BrushColor[ 5] = UIHelper.ConvertColor(acb5.Label.BackgroundColor);
            fOptions.BrushColor[ 6] = UIHelper.ConvertColor(acb6.Label.BackgroundColor);
            fOptions.BrushColor[ 7] = UIHelper.ConvertColor(acb7.Label.BackgroundColor);
            fOptions.BrushColor[ 8] = UIHelper.ConvertColor(acbText.Label.BackgroundColor);
            fOptions.BrushColor[ 9] = UIHelper.ConvertColor(acbBack.Label.BackgroundColor);
            fOptions.BrushColor[10] = UIHelper.ConvertColor(acbLine.Label.BackgroundColor);

            fOptions.HideEmptySegments = chkHideEmptySegments.Checked.GetValueOrDefault();
            fOptions.ArcText = chkArcText.Checked.GetValueOrDefault();
            //fOptions.Apply();
        }

        public void UpdateControls()
        {
            if (fOptions == null) return;
            //PanDefFont.Text = FOptions.ChartOptions.DefFont_Name + ", " + FOptions.ChartOptions.DefFont_Size.ToString();

            acb0.Label.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[0]);
            acb1.Label.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[1]);
            acb2.Label.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[2]);
            acb3.Label.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[3]);
            acb4.Label.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[4]);
            acb5.Label.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[5]);
            acb6.Label.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[6]);
            acb7.Label.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[7]);
            acbText.Label.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[8]);
            acbBack.Label.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[9]);
            acbLine.Label.BackgroundColor = UIHelper.ConvertColor(fOptions.BrushColor[10]);

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

        private async void lblColorClick(object sender, EventArgs e)
        {
            var swatch = (GKSwatchPanel)sender;
            Label lbl = swatch.Label;
            lbl.BackgroundColor = UIHelper.ConvertColor(await AppHost.StdDialogs.SelectColor(UIHelper.ConvertColor(lbl.BackgroundColor)));
        }

        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private GKSwatchPanel acb0;
        private GKSwatchPanel acb1;
        private GKSwatchPanel acb2;
        private GKSwatchPanel acb3;
        private GKSwatchPanel acb4;
        private GKSwatchPanel acb5;
        private GKSwatchPanel acb6;
        private GKSwatchPanel acb7;
        private GKSwatchPanel acbText;
        private GKSwatchPanel acbBack;
        private GKSwatchPanel acbLine;
        private CheckBox chkHideEmptySegments;
        private CheckBox chkArcText;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion
    }
}
