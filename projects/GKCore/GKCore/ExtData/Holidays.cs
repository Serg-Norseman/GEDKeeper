/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using BSLib;
using GKCore.Locales;
using GKCore.Utilities;

namespace GKCore.ExtData
{
    public sealed class Holiday
    {
        public string Name;
        public string Date;

        public DateTime XDate;
    }

    internal class HolidaysList
    {
        public string Country { get; set; }
        public Holiday[] Holidays { get; set; }

        public HolidaysList()
        {
            Holidays = new Holiday[0];
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class Holidays
    {
        private HolidaysList fHolidays;

        public Holidays()
        {
            fHolidays = new HolidaysList();
        }

        public void Load(string fileName)
        {
            if (!File.Exists(fileName)) return;

            try {
                // loading database
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    fHolidays = YamlHelper.Deserialize<HolidaysList>(content);
                }

                // processing dates
                for (int i = 0; i < fHolidays.Holidays.Length; i++) {
                    var holiday = fHolidays.Holidays[i];
                    string sdt = holiday.Date;
                    DateTime dtx;
                    if (DateTime.TryParse(sdt, out dtx)) {
                        holiday.XDate = dtx;
                    } else {
                        holiday.XDate = DateTime.FromBinary(0);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("Holidays.Load()", ex);
            }
        }

        public void CollectTips(StringList tipsList)
        {
            CollectTips(tipsList, DateTime.Now.Date);
        }

        public void CollectTips(StringList tipsList, DateTime dtx)
        {
            if (tipsList == null)
                throw new ArgumentNullException(nameof(tipsList));

            try {
                bool firstTip = true;
                for (int i = 0; i < fHolidays.Holidays.Length; i++) {
                    var holiday = fHolidays.Holidays[i];

                    int days = DateHelper.DaysBetween(dtx, holiday.XDate);

                    if (days >= 0 && days < 3) {
                        string tip;

                        if (firstTip) {
                            tipsList.Add("#" + LangMan.LS(LSID.Holidays));
                            firstTip = false;
                        }

                        if (days == 0) {
                            tip = string.Format(
                                LangMan.LS(LSID.HolidayToday), holiday.Name);
                        } else if (days == 1) {
                            tip = string.Format(
                                LangMan.LS(LSID.HolidayTomorrow), holiday.Name);
                        } else {
                            tip = string.Format(
                                LangMan.LS(LSID.DaysRemainedBeforeHoliday), holiday.Name, days);
                        }

                        tipsList.Add(tip);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("Holidays.CollectTips()", ex);
            }
        }
    }
}
