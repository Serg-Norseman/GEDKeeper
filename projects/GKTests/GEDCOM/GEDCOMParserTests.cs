/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using NUnit.Framework;

namespace GKCommon.GEDCOM
{
    [TestFixture]
    public class GEDCOMParserTests
    {
        // access to characters in an array is 3-5% faster than characters in a string

        [Test]
        public void Test_ParseDate()
        {
            string strValue = "ABT @#DJULIAN@ 01 MAR 1980/10";
            string result;

            GEDCOMApproximated approximated;
            GEDCOMCalendar calendar;
            short year;
            bool yearBC;
            string yearModifier;
            byte month, day;
            GEDCOMDateFormat dateFormat;

            result = GEDCOMUtils.ParseDate(strValue, out approximated, out calendar, out year, out yearBC,
                                           out yearModifier, out month, out day, out dateFormat);
        }

        /*[Test]
        public void Test_ParseTimePerf()
        {
            string strValue = "21:17:17:245";
            string result;

            byte hour, minutes, seconds;
            short fraction;

            for (int i = 0; i < 10000; i++) {
                result = GEDCOMUtils.ParseTimeP(strValue, out hour, out minutes, out seconds, out fraction);
                result = GEDCOMUtils.ParseTimeX(strValue, out hour, out minutes, out seconds, out fraction);
            }
        }*/
    }
}
