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
using BSLib;
using GDModel;
using GKCore.Cultures;
using GKCore.Locales;

namespace GKCore.ExtData
{
    /// <summary>
    /// Preparing tips, recommendations and reminders to display to the user.
    /// </summary>
    public static class TipsCollector
    {
        public static void Collect(BaseContext context, StringList tipsList, bool onlyAlive = true)
        {
            if (tipsList == null)
                throw new ArgumentNullException("tipsList");

            try {
                var tree = context.Tree;
                var dtNow = AppHost.Instance.GetCurrentDateTime();
                string title;

                title = LangMan.LS(LSID.BirthDays);
                bool birtFirstTip = true;
                var indiEnum = tree.GetEnumerator<GDMIndividualRecord>();
                GDMIndividualRecord iRec;
                while (indiEnum.MoveNext(out iRec)) {
                    var lifeEvents = iRec.GetLifeEvents(true);

                    if (onlyAlive && (lifeEvents.DeathEvent != null || lifeEvents.BurialEvent != null)) continue;

                    if (lifeEvents.BirthEvent == null) continue;
                    var dt = lifeEvents.BirthEvent.Date.Value as GDMDate;

                    if (dt != null && dt.IsValidDate()) {
                        int days = GKUtils.GetDaysFor(dt, dtNow, out int years, out bool anniversary);
                        if (days >= 0 && days < 3) {
                            string tip = GetBirthTipMessage(context.Culture, iRec, days, years, anniversary);
                            AddTip(tipsList, title, tip, ref birtFirstTip);
                        }
                    }
                }

                title = LangMan.LS(LSID.WeddingAnniversaries);
                bool marrFirstTip = true;
                var famEnum = tree.GetEnumerator<GDMFamilyRecord>();
                GDMFamilyRecord famRec;
                while (famEnum.MoveNext(out famRec)) {
                    var husb = tree.GetPtrValue(famRec.Husband);
                    var wife = tree.GetPtrValue(famRec.Wife);
                    if (husb == null || wife == null) continue;

                    if (onlyAlive && (!husb.IsLive() || !wife.IsLive())) continue;

                    var marrDate = GKUtils.GetMarriageDate(famRec) as GDMDate;
                    if (marrDate != null && marrDate.IsValidDate()) {
                        int days = GKUtils.GetDaysFor(marrDate, dtNow, out int years, out bool anniversary);
                        if (days >= 0 && days < 3) {
                            string tip = GetMarriageTipMessage(context.Culture, husb, wife, days, years);
                            AddTip(tipsList, title, tip, ref marrFirstTip);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("TipsCollector.Collect()", ex);
            }
        }

        private static void AddTip(StringList tipsList, string title, string tip, ref bool firstTip)
        {
            if (firstTip) {
                tipsList.Add("#" + title);
                firstTip = false;
            }
            tipsList.Add(tip);
        }

        private static string GetBirthTipMessage(ICulture culture, GDMIndividualRecord iRec, int days, int years, bool anniversary)
        {
            string nm = culture.GetPossessiveName(iRec);

            string tip;
            if (!anniversary) {
                switch (days) {
                    case 0:
                        tip = string.Format(LangMan.LS(LSID.BirthdayToday), nm);
                        break;
                    case 1:
                        tip = string.Format(LangMan.LS(LSID.BirthdayTomorrow), nm);
                        break;
                    default:
                        tip = string.Format(LangMan.LS(LSID.DaysRemained), nm, days);
                        break;
                }
            } else {
                switch (days) {
                    case 0:
                        tip = string.Format(LangMan.LS(LSID.AnniversaryToday), nm);
                        break;
                    case 1:
                        tip = string.Format(LangMan.LS(LSID.AnniversaryTomorrow), nm);
                        break;
                    default:
                        tip = string.Format(LangMan.LS(LSID.AnniversaryDaysRemained), nm, days);
                        break;
                }
            }
            return tip;
        }

        private static string GetMarriageTipMessage(ICulture culture, GDMIndividualRecord husbRec, GDMIndividualRecord wifeRec, int days, int years)
        {
            string nm1 = culture.GetPossessiveName(husbRec);
            string nm2 = culture.GetPossessiveName(wifeRec);

            string tip;
            switch (days) {
                case 0:
                    tip = string.Format(LangMan.LS(LSID.WeddingAnniversaryToday), nm1, nm2, years);
                    break;
                case 1:
                    tip = string.Format(LangMan.LS(LSID.WeddingAnniversaryTomorrow), nm1, nm2, years);
                    break;
                default:
                    tip = string.Format(LangMan.LS(LSID.WeddingAnniversaryDaysRemained), nm1, nm2, days, years);
                    break;
            }
            return tip;
        }
    }
}
