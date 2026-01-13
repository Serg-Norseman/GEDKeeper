/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore;
using GKCore.Locales;
using GKCore.Options;
using Xamarin.Forms;

namespace GKUI.Components
{
    public partial class ACOptionsControl : ContentView, IOptionsControl
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

            SetLabelClickEvent(acb0);
            SetLabelClickEvent(acb1);
            SetLabelClickEvent(acb2);
            SetLabelClickEvent(acb3);
            SetLabelClickEvent(acb4);
            SetLabelClickEvent(acb5);
            SetLabelClickEvent(acb6);
            SetLabelClickEvent(acb7);
            SetLabelClickEvent(acbText);
            SetLabelClickEvent(acbBack);
            SetLabelClickEvent(acbLine);

            SetLocale();
        }

        private void SetLabelClickEvent(Label label)
        {
            var tapRecognizer = new TapGestureRecognizer();
            tapRecognizer.Tapped += (s, e) => {
                lblColorClick(s, e);
            };
            label.GestureRecognizers.Add(tapRecognizer);
        }

        public void SetLocale()
        {
            //Title = LangMan.LS(LSID.MIOptions);

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

            fOptions.BrushColor[0] = UIHelper.ConvertColor(acb0.BackgroundColor);
            fOptions.BrushColor[1] = UIHelper.ConvertColor(acb1.BackgroundColor);
            fOptions.BrushColor[2] = UIHelper.ConvertColor(acb2.BackgroundColor);
            fOptions.BrushColor[3] = UIHelper.ConvertColor(acb3.BackgroundColor);
            fOptions.BrushColor[4] = UIHelper.ConvertColor(acb4.BackgroundColor);
            fOptions.BrushColor[5] = UIHelper.ConvertColor(acb5.BackgroundColor);
            fOptions.BrushColor[6] = UIHelper.ConvertColor(acb6.BackgroundColor);
            fOptions.BrushColor[7] = UIHelper.ConvertColor(acb7.BackgroundColor);
            fOptions.BrushColor[8] = UIHelper.ConvertColor(acbText.BackgroundColor);
            fOptions.BrushColor[9] = UIHelper.ConvertColor(acbBack.BackgroundColor);
            fOptions.BrushColor[10] = UIHelper.ConvertColor(acbLine.BackgroundColor);

            fOptions.HideEmptySegments = chkHideEmptySegments.IsChecked;
            fOptions.ArcText = chkArcText.IsChecked;
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

            chkHideEmptySegments.IsChecked = fOptions.HideEmptySegments;
            chkArcText.IsChecked = fOptions.ArcText;
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
            Label lbl = sender as Label;
            if (lbl == null) return;
            lbl.BackgroundColor = UIHelper.ConvertColor(await AppHost.StdDialogs.SelectColor(UIHelper.ConvertColor(lbl.BackgroundColor)));
        }
    }
}
