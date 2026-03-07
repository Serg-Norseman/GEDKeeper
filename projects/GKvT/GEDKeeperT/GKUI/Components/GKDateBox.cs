/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using Terminal.Gui;
using Terminal.Gui.TextValidateProviders;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKDateBox : TextValidateField
    {
        private static readonly string fRegionalDatePattern;


        public string RegionalDatePattern
        {
            get { return fRegionalDatePattern; }
        }

        public string NormalizeDate
        {
            get { return GKUtils.GetNormalizeDate(Text.ToString(), fRegionalDatePattern); }
            set { Text = GKUtils.GetRegionalDate(value, fRegionalDatePattern); }
        }


        static GKDateBox()
        {
            fRegionalDatePattern = GKUtils.GetShortDatePattern();
            Logger.WriteInfo(string.Format("RegionalDatePattern: {0}", fRegionalDatePattern));
        }

        public GKDateBox()
        {
            // To turn off the red background
            InvalidIndication = false;

            var mask = GKUtils.GetDateMask(fRegionalDatePattern).Replace("/", @"\/");
            Provider = new NetMaskedTextProvider(mask);
        }
    }
}
