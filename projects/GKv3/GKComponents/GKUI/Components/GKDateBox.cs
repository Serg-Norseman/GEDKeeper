/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using System.Globalization;
using Eto.Forms;
using GKCore;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKDateBox : MaskedTextBox
    {
        private static readonly string fRegionalDatePattern;


        public string RegionalDatePattern
        {
            get { return fRegionalDatePattern; }
        }

        public string NormalizeDate
        {
            get { return GKUtils.GetNormalizeDate(Text, fRegionalDatePattern); }
            set { Text = GKUtils.GetRegionalDate(value, fRegionalDatePattern); }
        }


        static GKDateBox()
        {
            fRegionalDatePattern = GKUtils.GetShortDatePattern();
            Logger.WriteInfo(string.Format("RegionalDatePattern: {0}", fRegionalDatePattern));
        }

        public GKDateBox()
        {
            Provider = new FixedMaskedTextProvider(GKUtils.GetDateMask(fRegionalDatePattern), CultureInfo.InvariantCulture);
        }
    }
}
