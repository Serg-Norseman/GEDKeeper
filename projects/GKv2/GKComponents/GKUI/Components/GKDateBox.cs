/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.ComponentModel;
using System.Globalization;
using System.Windows.Forms;
using GKCore;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKDateBox : MaskedTextBox
    {
        private static readonly string fRegionalDatePattern;


        [Browsable(false)]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public string RegionalDatePattern
        {
            get { return fRegionalDatePattern; }
        }

        [Browsable(false)]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
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
            Culture = CultureInfo.InvariantCulture;
            TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            Mask = GKUtils.GetDateMask(fRegionalDatePattern);
        }
    }
}
