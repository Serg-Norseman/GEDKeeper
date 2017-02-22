/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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
using System.IO;
using GKCommon;
using GKCore.Options;

namespace GKCore
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

            try
            {
                // loading database
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    var rawData = YamlHelper.Deserialize(content, typeof(HolidaysList));
                    fHolidays = rawData[0] as HolidaysList;
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
            }
            catch (Exception ex)
            {
                Logger.LogWrite("Holidays.Load(): " + ex.Message);
            }
        }

        public void CollectTips(StringList tipsList)
        {
            if (tipsList == null)
                throw new ArgumentNullException("tipsList");

            try
            {
                try
                {
                    DateTime dtNow = DateTime.Now.Date;

                    for (int i = 0; i < fHolidays.Holidays.Length; i++) {
                        var holiday = fHolidays.Holidays[i];

                        uint days = SysUtils.DaysBetween(dtNow, holiday.XDate);

                        if (0 == days)
                        {
                            tipsList.Add(string.Format(
                                LangMan.LS(LSID.LSID_HolidayToday), holiday.Name));
                        }
                        else if (1 == days)
                        {
                            tipsList.Add(string.Format(
                                LangMan.LS(LSID.LSID_HolidayTomorrow), holiday.Name));
                        }
                        else if (3 > days)
                        {
                            tipsList.Add(string.Format(
                                LangMan.LS(LSID.LSID_DaysRemainedBeforeHoliday), holiday.Name, days));
                        }
                    }
                }
                finally
                {
                    // temp stub, remove try/finally here?
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("Holidays.CollectTips(): " + ex.Message);
            }
        }
    }
}
