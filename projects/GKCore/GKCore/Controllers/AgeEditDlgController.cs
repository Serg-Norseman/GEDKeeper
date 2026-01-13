/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Text;
using System.Text.RegularExpressions;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
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
            bool hasVal = false;
            for (int i = 0; i < mask.Length; i++) {
                var mX = mask[i];
                switch (mX) {
                    case '0':
                        afterVal = true;
                        if (char.IsDigit(strAge[i])) {
                            hasVal = true;
                            tempRes.Append(strAge[i]);
                        }
                        break;
                    case ' ':
                        if (afterVal) {
                            part += 1;
                            if (hasVal) {
                                switch (part) {
                                    case 1: tempRes.Append('y'); break;
                                    case 2: tempRes.Append('m'); break;
                                    case 3: tempRes.Append('d'); break;
                                }
                                tempRes.Append(' ');
                            }
                            afterVal = false;
                            hasVal = false;
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

                    var xs = val.ToString(mp);
                    // replace 0 to _ only before numbers 1..9
                    resParts[i] = Regex.Replace(xs, @"^0+", match => new string('_', match.Length));
                } else {
                    resParts[i] = mp;
                }
            }

            var tempRes = string.Join(" ", resParts)/*.Replace("0", "_")*/;
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
