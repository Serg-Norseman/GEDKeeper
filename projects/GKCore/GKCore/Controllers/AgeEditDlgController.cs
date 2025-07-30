/*
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
using System.Text;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class AgeEditDlgController : DialogController<IAgeEditDlg>
    {
        private GDMCustomEvent fEvent;


        public GDMCustomEvent Event
        {
            get { return fEvent; }
            set {
                if (fEvent != value) {
                    fEvent = value;
                    UpdateView();
                }
            }
        }


        public AgeEditDlgController(IAgeEditDlg view) : base(view)
        {
            for (int i = 0; i < GEDCOMConsts.AgeRelatives.Length; i++) {
                fView.RelativeCombo1.AddItem(GEDCOMConsts.AgeRelatives[i], i - 1);
                fView.RelativeCombo2.AddItem(GEDCOMConsts.AgeRelatives[i], i - 1);
            }

            fView.RelativeCombo1.Activate();
        }

        private static string GetAgeStr(string strAge)
        {
            string mask = LangMan.LS(LSID.AgeInputMask);
            if (mask.Length != strAge.Length)
                throw new Exception("Format invalid");

            var tempRes = new StringBuilder();
            int part = 0;
            bool afterVal = false;
            for (int i = 0; i < mask.Length; i++) {
                var mX = mask[i];
                switch (mX) {
                    case '0':
                        if (char.IsDigit(strAge[i])) {
                            tempRes.Append(strAge[i]);
                            afterVal = true;
                        }
                        break;
                    case ' ':
                        if (afterVal) {
                            part += 1;
                            switch (part) {
                                case 1: tempRes.Append('y'); break;
                                case 2: tempRes.Append('m'); break;
                                case 3: tempRes.Append('d'); break;
                            }
                            tempRes.Append(' ');
                            afterVal = false;
                        }
                        break;
                    default:
                        break;
                }
            }

            return tempRes.ToString().Trim();
        }

        private static void SetAgeStr(ITextBox textBox, GDMAge age)
        {
            string mask = LangMan.LS(LSID.AgeInputMask);

            var maskParts = mask.Split(' ');
            var resParts = new string[maskParts.Length];

            int valIdx = 0;
            for (int i = 0; i < maskParts.Length; i++) {
                var mp = maskParts[i];
                if (mp.StartsWith("0")) {
                    int val = 0;
                    switch (valIdx) {
                        case 0: val = age.Years; break;
                        case 1: val = age.Months; break;
                        case 2: val = age.Days; break;
                    }
                    valIdx++;
                    if (val < 0) val = 0;

                    resParts[i] = val.ToString(mp);
                } else {
                    resParts[i] = mp;
                }
            }

            var tempRes = string.Join(" ", resParts).Replace("0", "_");
            textBox.Text = tempRes;
        }

        public override bool Accept()
        {
            try {
                if (fEvent is GDMIndividualEventDetail indiEvent) {
                    var age = indiEvent.Age;
                    age.StringValue = GetAgeStr(fView.ValueText1.Text);
                    age.Relative = fView.RelativeCombo1.GetSelectedTag<int>();
                } else if (fEvent is GDMFamilyEvent famEvent) {
                    var husbAge = famEvent.HusbandAge;
                    husbAge.StringValue = GetAgeStr(fView.ValueText1.Text);
                    husbAge.Relative = fView.RelativeCombo1.GetSelectedTag<int>();

                    var wifeAge = famEvent.WifeAge;
                    wifeAge.StringValue = GetAgeStr(fView.ValueText2.Text);
                    wifeAge.Relative = fView.RelativeCombo2.GetSelectedTag<int>();
                }

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("AgeEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            if (fEvent is GDMIndividualEventDetail indiEvent) {
                var age = indiEvent.Age;
                fView.RelativeCombo1.SetSelectedTag(age.Relative);
                SetAgeStr(fView.ValueText1, age);

                GetControl<ILabel>("lblAge1").Text = LangMan.LS(LSID.Age);

                fView.RelativeCombo2.Visible = false;
                fView.ValueText2.Visible = false;
                GetControl<ILabel>("lblAge2").Visible = false;
            } else if (fEvent is GDMFamilyEvent famEvent) {
                var husbAge = famEvent.HusbandAge;
                fView.RelativeCombo1.SetSelectedTag(husbAge.Relative);
                SetAgeStr(fView.ValueText1, husbAge);

                GetControl<ILabel>("lblAge1").Text = LangMan.LS(LSID.HusbandAge);

                var wifeAge = famEvent.WifeAge;
                fView.RelativeCombo2.SetSelectedTag(wifeAge.Relative);
                SetAgeStr(fView.ValueText2, wifeAge);

                GetControl<ILabel>("lblAge2").Text = LangMan.LS(LSID.WifeAge);
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Age));

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
